use winapi::*;
use core::*;
use create_device::*;
use utils::*;
use dx_safe::*;
use structwrappers::*;
use std::ptr;

pub struct Downsampler {
  cpso: D3D12PipelineState,
  root_sig: D3D12RootSignature,
  dheap: DescriptorHeap,
  // luid of adapter cpso was generated on. generate_mips asserts that it is the same adapter as passed in DXCore
  luid: Luid,
}

impl Downsampler {
  pub fn new(dev: &D3D12Device) -> Downsampler {
    let dheap = DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0).expect("Cannot create descriptor heap");

    let shader_bytecode = 
      match d3d_compile_from_str(SHADER, "CSMain", "cs_5_1", 0) {
        Ok(sbc) => sbc,
        Err(err) => panic!("Downsampler shader compile error: {}", err),
      };
    let root_sig_bytecode = 
      match d3d_compile_from_str(SHADER, "RSD", "rootsig_1_0", 0) {
        Ok(sbc) => sbc,
        Err(err) => panic!("Downsampler root signature compile error: {}", err),
      };
    debug!("Root signature");
    let root_sig = dev.create_root_signature(0, &root_sig_bytecode[..]).expect("Cannot create root signature");

    //It doesn't work. 
    //let crs = d3d_get_root_signature_from_str(SHADER, dev).expect("Cannot extract root signature");
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
    Downsampler {
      cpso: cpso,
      root_sig: root_sig,
      luid: Luid(dev.get_adapter_luid()),
      dheap: dheap,
    }
  }

  pub fn generate_mips(&self, tex: &D3D12Resource, core: &DXCore) -> HResult<()> {
    use std::sync::atomic::Ordering;

    trace!("generate_mips");
    trace!("Assert same adapter");
    assert!(&self.luid == &Luid(core.dev.get_adapter_luid()));

    trace!("rdesc");
    let rdesc = tex.get_desc();
    let (mut cw, mut ch) = ((rdesc.Width/2) as u32, rdesc.Height/2);

    // Create intermediate texture to hold computed mip-map levels
    // I had no success with generating mip-maps inplace.
    let tex_desc = D3D12_RESOURCE_DESC {
      MipLevels: 1,
      // D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS is required for generating mip-maps (at least it required for my method of generating mip-maps)
      .. resource_desc_tex2d_nomip(cw as u64, ch as u32, DXGI_FORMAT_R8G8B8A8_UNORM, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS)
    };
    trace!("Create intermediate texture");
    let im_tex = try!(core.dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
                              &tex_desc, D3D12_RESOURCE_STATE_COMMON, None));
  
    trace!("uav_desc");
    let uav_desc = uav_tex2d_desc(rdesc.Format, 0, 0);
    core.dev.create_unordered_access_view(Some(&im_tex), None, Some(&uav_desc), self.dheap.cpu_handle(1));

    // callocator and clist are destroyed on function exit. 
    // It is not very efficient, but it's ok for now.
    trace!("callocator");
    let callocator = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
    // DX12 API developers possibly had an idea of type-level separation of graphics and compute command lists, but
    // they (wisely?) decided to keep interface count low and do runtime validation of commands.
    // So, compute and graphics command lists are represented as D3D12GraphicsCommandList.
    trace!("clist");
    let clist: D3D12GraphicsCommandList = try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &callocator, Some(&self.cpso)));
    try!(clist.close());

    let fence = try!(core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE));
    let fence_event = create_event();

    for i in 0..((rdesc.MipLevels-1) as u32) {
      try!(callocator.reset());
      try!(clist.reset(&callocator, None));
      // Create descriptors in descriptor heap.
      // Their layout should match root signature specified in compute shader below.
      // Descriptor for source mip-map slice
      trace!("srv_desc");
      let srv_desc = srv_tex2d_default_slice_mip(rdesc.Format, i, 1);
      core.dev.create_shader_resource_view(Some(&tex), Some(&srv_desc), self.dheap.cpu_handle(0));

      trace!("clist.set_pipeline_state");
      clist.set_pipeline_state(&self.cpso);
      clist.set_compute_root_signature(&self.root_sig);
      trace!("clist.set_descriptor_heaps");
      clist.set_descriptor_heaps(&[self.dheap.get()]);
      trace!("clist.set_compute_root_descriptor_table(0, ...");
      clist.set_compute_root_descriptor_table(0, self.dheap.gpu_handle(0));

      trace!("clist.resource_barrier");
      clist.resource_barrier(&[
          // Transition source mip-map slice
          *ResourceBarrier::transition_subresource(&tex, i,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
          // Transition intermediate texture 
          *ResourceBarrier::transition(&im_tex, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);
      clist.dispatch(cw, ch, 1);
      clist.resource_barrier(&[
          // Transition source mip-map slice
          *ResourceBarrier::transition_subresource(&tex, i,
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          // Transition intermediate texture
          *ResourceBarrier::transition(&im_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE),
          // Transition target slice
          *ResourceBarrier::transition_subresource(&tex, i+1,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_DEST),
        ]);

      let source_copy_loc = texture_copy_location_index(&im_tex, 0);
      let dest_copy_loc = texture_copy_location_index(&tex, i+1);

      let src_box = D3D12_BOX {
        left: 0, top: 0, front: 0,
        right: cw, bottom: ch, back: 1,
      };
      clist.copy_texture_region(&dest_copy_loc, 0, 0, 0, &source_copy_loc, Some(&src_box));
      clist.resource_barrier(&[
          // Transition intermediate texture
          *ResourceBarrier::transition(&im_tex, 
            D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_COMMON),
          // Transition target slice
          *ResourceBarrier::transition_subresource(&tex, i+1,
            D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON),
        ]);
    
      // I forgot to close command list. So I close it now
      try!(clist.close().map_err(|hr|{dump_info_queue(core.info_queue.as_ref()); hr}));

      core.compute_queue.execute_command_lists(&[&clist]);

      let fence_value = core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;
      try!(wait_for_queue(&core.compute_queue, fence_value, &fence, &fence_event));

      cw /= 2;
      ch /= 2;
    };
    dump_info_queue(core.info_queue.as_ref());

    Ok(())
  }
}

static SHADER: &'static str = r#"
#define RSD "RootFlags(0), DescriptorTable(SRV(t0), UAV(u0))" // 2

Texture2D<float4> mip0: register(t0);//4
RWTexture2D<float4> mip1: register(u0);//5

static const float coefs[4] = {1./8., 3./8., 3./8., 1./8.};//7
groupshared float4 color_accum[4][4];//8

[RootSignature(RSD)]//10
[numthreads(4, 4, 1)]//11
void CSMain(uint3 gtid: SV_GroupThreadId, uint3 gid: SV_GroupId) {//12
  int w=0;
  int h=0;
  mip0.GetDimensions(w, h);
  float c = coefs[gtid.x]*coefs[gtid.y];//13
  int2 idx = clamp(gid.xy*2 + gtid.xy + int2(-1,-1), int2(0,0), int2(w-1,h-1));

  color_accum[gtid.x][gtid.y] = mip0[idx]*c;//14
  GroupMemoryBarrierWithGroupSync();//15
  if (gtid.x==1 && gtid.y==1) {//16
    float4 a=float4(0,0,0,0);//17
    for(uint i=0; i<4; ++i) {
      for(uint j=0; j<4; ++j) {
         a += color_accum[i][j];
      };
    };
    mip1[gid.xy] = a;//float4(1, 1, 1, 1);//a;
  };
}
"#;

