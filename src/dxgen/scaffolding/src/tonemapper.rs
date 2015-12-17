use winapi::*;
use core::*;
use create_device::*;
use utils::*;
use dx_safe::*;
use structwrappers::*;
use std::ptr;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::str;


pub struct Tonemapper {
  hpass_dheap: DescriptorHeap,
  hpass_cpso: D3D12PipelineState,
  hpass_rs: D3D12RootSignature,
  cpso: D3D12PipelineState,
  root_sig: D3D12RootSignature,
  dheap: DescriptorHeap,
  calloc: D3D12CommandAllocator,
  clist: D3D12GraphicsCommandList,
  gralloc: D3D12CommandAllocator,
  grlist: D3D12GraphicsCommandList,
  fence: D3D12Fence,
  // luid of adapter cpso was generated on. generate_mips asserts that it is the same adapter as passed in DXCore
  luid: Luid,
}

fn compile_shader_and_root_signature(text: &str, fname: &str, rsname: &str) -> Result<(Vec<u8>, Vec<u8>),()> {
  let shader_bytecode = 
    match d3d_compile_from_str(text, fname, "cs_5_1", 0) {
      Ok(sbc) => sbc,
      Err(err) => {
        error!("Shader '{}' compile error: {}", fname, err);
        return Err(());
      },
    };
  let root_sig_bytecode = 
    match d3d_compile_from_str(text, rsname, "rootsig_1_0", 0) {
      Ok(sbc) => sbc,
      Err(err) => {
        error!("Root signature '{}' compile error: {}", rsname, err);
        return Err(());
      },
    };
  Ok((shader_bytecode, root_sig_bytecode))
}

// This utility function will be unsafe. TODO: wrap D3D12_SHADER_BYTECODE
// fn shader_bytecode(src: &[u8]) -> D3D12_SHADER_BYTECODE

// TODO: Use shader reflection to get count of thread groups from shader
const HTGroups: u32 = 128; 
const VTGroups: u32 = 128; 

impl Tonemapper {
  pub fn new(dev: &D3D12Device) -> Tonemapper {
    let hpass_dheap = DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0).expect("Cannot create descriptor heap");
    let dheap = DescriptorHeap::new(&dev, 3, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0).expect("Cannot create descriptor heap");

    let calloc = dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE).expect("Cannot create compute command allocator");
    let clist: D3D12GraphicsCommandList = dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, None).expect("Cannot create compute command list");
    clist.close().expect("Unexpected error while closing empty compute command list");

    let gralloc = dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).expect("Cannot create graphics command list");
    let grlist: D3D12GraphicsCommandList = dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &gralloc, None).expect("Cannot create graphics command list");
    grlist.close().expect("Unexpected error while closing empty graphics command list");

    let fence = dev.create_fence(0, D3D12_FENCE_FLAG_NONE).expect("Cannot create fence");

    let mut f = File::open("tonemap.hlsl").expect("Cannot open tonemapper shader file.");
    let mut content = vec![];
    f.read_to_end(&mut content).expect("Cannot read tonemapper shader file.");
    let shader = str::from_utf8(&content[..]).expect("Shader file content is not a valid UTF-8");

    let (hpass_shader_bc, hpass_rs_bc) =
      compile_shader_and_root_signature(shader.into(), "CSHorizontal", "RSDH").expect("Tonemapper horizontal pass shader compile error");
    
    let hpass_rs = dev.create_root_signature(0, &hpass_rs_bc[..]).expect("Cannot create horisontal pass root signature");

    let hpass_cpsd = D3D12_COMPUTE_PIPELINE_STATE_DESC {
      pRootSignature: hpass_rs.iptr() as *mut _,
      CS: D3D12_SHADER_BYTECODE {
          pShaderBytecode: hpass_shader_bc.as_ptr() as *const _,
          BytecodeLength: hpass_shader_bc.len() as SIZE_T,
        },
      NodeMask: 0,
      CachedPSO: D3D12_CACHED_PIPELINE_STATE {pCachedBlob: ptr::null(), CachedBlobSizeInBytes: 0,},
      Flags: D3D12_PIPELINE_STATE_FLAG_NONE, 
    };
    let hpass_cpso = dev.create_compute_pipeline_state(&hpass_cpsd).expect("Cannot create horiziontal pass compute pipeline state");

    let (shader_bytecode, root_sig_bytecode) = 
      compile_shader_and_root_signature(shader.into(), "CSMain", "RSD").expect("Tonemapper shader compile error");

    trace!("Root signature");
    let root_sig = dev.create_root_signature(0, &root_sig_bytecode[..]).expect("Cannot create root signature");

    let cpsd = D3D12_COMPUTE_PIPELINE_STATE_DESC {
      pRootSignature: ptr::null_mut(),
      CS: D3D12_SHADER_BYTECODE {
        pShaderBytecode: shader_bytecode.as_ptr() as *const _,
        BytecodeLength: shader_bytecode.len() as SIZE_T,
      }, 
      NodeMask: 0,
      CachedPSO: D3D12_CACHED_PIPELINE_STATE {pCachedBlob: ptr::null(), CachedBlobSizeInBytes: 0,},
      Flags: D3D12_PIPELINE_STATE_FLAG_NONE, 
    };
    let cpso = dev.create_compute_pipeline_state(&cpsd).expect("Cannot create compute pipeline state");
    Tonemapper {
      hpass_cpso: hpass_cpso,
      hpass_rs: hpass_rs,
      hpass_dheap: hpass_dheap,
      cpso: cpso,
      root_sig: root_sig,
      luid: Luid(dev.get_adapter_luid()),
      dheap: dheap,
      calloc: calloc,
      clist: clist,
      gralloc: gralloc,
      grlist: grlist,
      fence: fence,
    }
  }

  pub fn tonemap(&self, core: &DXCore, src: &D3D12Resource, dst: &D3D12Resource) -> HResult<()> {
    use std::sync::atomic::Ordering;

    trace!("tonemap");
    trace!("Assert same adapter");
    assert!(&self.luid == &Luid(core.dev.get_adapter_luid()));

    try!(self.calloc.reset());
    try!(self.clist.reset(&self.calloc, Some(&self.cpso)));

    trace!("rdesc");
    let srcdesc = src.get_desc();
    let (cw, ch) = (srcdesc.Width as u32, srcdesc.Height);


    let dstdesc = dst.get_desc();
    trace!("uav_desc");

    // Intermediate texture for horizontal haussian
    let h_tex =
      try!(core.dev.create_committed_resource(
          &heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_tex2d_nomip(cw as u64, ch as u32, 
              srcdesc.Format, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS), 
          D3D12_RESOURCE_STATE_UNORDERED_ACCESS, None));

    let tex_desc = resource_desc_tex2d_nomip(cw as u64, ch as u32, dstdesc.Format, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS);
    trace!("Create intermediate texture");
    // intermediate texture for copying into back-buffer
    let im_tex = try!(core.dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
                              &tex_desc, D3D12_RESOURCE_STATE_UNORDERED_ACCESS, None));

    let clist = &self.clist;
    // Horizontal pass
    let src_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
    core.dev.create_shader_resource_view(Some(&src), Some(&src_desc), self.hpass_dheap.cpu_handle(0));

    let dst_desc = uav_tex2d_desc(srcdesc.Format, 0, 0);
    core.dev.create_unordered_access_view(Some(&h_tex), None, Some(&dst_desc), self.hpass_dheap.cpu_handle(1));

    clist.set_pipeline_state(&self.hpass_cpso);
    clist.set_compute_root_signature(&self.hpass_rs);
    clist.set_descriptor_heaps(&[self.hpass_dheap.get()]);
    clist.set_compute_root_descriptor_table(0, self.hpass_dheap.gpu_handle(0));

    clist.resource_barrier(&[
          *ResourceBarrier::transition(&src,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
        ]);

    clist.dispatch(cw/HTGroups+1, ch, 1);
    clist.resource_barrier(&[
          *ResourceBarrier::transition(&h_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
        ]);

    // Vertical and final pass

    let srv_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
    core.dev.create_shader_resource_view(Some(&src), Some(&srv_desc), self.dheap.cpu_handle(0));

    let h_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
    core.dev.create_shader_resource_view(Some(&h_tex), Some(&h_desc), self.dheap.cpu_handle(1));

    let uav_desc = uav_tex2d_desc(dstdesc.Format, 0, 0);
    core.dev.create_unordered_access_view(Some(&im_tex), None, Some(&uav_desc), self.dheap.cpu_handle(2));


    clist.set_pipeline_state(&self.cpso);
    clist.set_compute_root_signature(&self.root_sig);
    clist.set_descriptor_heaps(&[self.dheap.get()]);
    clist.set_compute_root_descriptor_table(0, self.dheap.gpu_handle(0));

    clist.dispatch(cw, ch/VTGroups+1, 1);
    clist.resource_barrier(&[
          *ResourceBarrier::transition(&src,
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&h_tex, 
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&im_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
        ]);
    trace!("Close compute list");
    try!(clist.close().map_err(|hr|{dump_info_queue(core.info_queue.as_ref()); hr}));
    core.compute_queue.execute_command_lists(&[clist]);

    let next_fence_val = core.next_fence_value();
    try!(core.compute_queue.signal(&self.fence, next_fence_val));

    // Swapchain back buffers are special. Only graphics queue associated with swapchain can write them.
    try!(self.gralloc.reset());
    try!(self.grlist.reset(&self.gralloc, None));
    let grlist = &self.grlist;

    let source_copy_loc = texture_copy_location_index(&im_tex, 0);
    let dest_copy_loc = texture_copy_location_index(&dst, 0);

    let src_box = D3D12_BOX {
      left: 0, top: 0, front: 0,
      right: cw, bottom: ch, back: 1,
    };
    grlist.resource_barrier(&[
          *ResourceBarrier::transition(&im_tex, 
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_SOURCE),
          *ResourceBarrier::transition(&dst, 
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_DEST),
        ]);
    grlist.copy_texture_region(&dest_copy_loc, 0, 0, 0, &source_copy_loc, Some(&src_box));
    grlist.resource_barrier(&[
          *ResourceBarrier::transition(&im_tex, 
            D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&dst, 
            D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON),
        ]);

    trace!("Close graphics list");
    try!(grlist.close().map_err(|hr|{dump_info_queue(core.info_queue.as_ref()); hr}));
    // Instruct graphics queue to wait for compute queue
    try!(core.graphics_queue.wait(&self.fence, next_fence_val));
    core.graphics_queue.execute_command_lists(&[grlist]);

    //TODO: store intermediate texture somewhere to prevent this wait.
    wait_for_graphics_queue(core, &self.fence, &create_event());

    trace!("dump_info_queue");
    dump_info_queue(core.info_queue.as_ref());

    Ok(())
  }
}

