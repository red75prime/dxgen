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

pub struct TonemapperResources {
    h_tex: D3D12Resource,
    im_tex: D3D12Resource,
    srcformat: DXGI_FORMAT,
    fence: D3D12Fence,
    calloc: D3D12CommandAllocator,
    clist: D3D12GraphicsCommandList,
    gralloc: D3D12CommandAllocator,
    grlist: D3D12GraphicsCommandList,
    hpass_dheap: DescriptorHeap,
    dheap: DescriptorHeap,
}

// Holds intermediate variables for Tonemapper function.
// To clarify. Tonemapper structure below can be considered to be a function that takes one argument (HDR image) and produces one value (LDR image).
// Creation of temporary intermediate resource on each call to Tonemapper::tonemap() is very slow. This structure hold them.
// Note that the structure must be recreated when dimensions of Tonemapper input change.
impl TonemapperResources {
    pub fn new(dev: &D3D12Device,
               w: u32,
               h: u32,
               srcformat: DXGI_FORMAT,
               dstformat: DXGI_FORMAT)
               -> HResult<TonemapperResources> {
        trace!("Create texture for horizontal gaussian kernel convolution storage");
        let h_tex = try!(dev.create_committed_resource(
          &heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_tex2d_nomip(w as u64, h as u32, 
              srcformat, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS), 
          D3D12_RESOURCE_STATE_COMMON, None));

        let tex_desc = resource_desc_tex2d_nomip(w as u64,
                                                 h as u32,
                                                 dstformat,
                                                 D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS);
        trace!("Create intermediate texture");
        // intermediate texture for copying into back-buffer
        let im_tex = try!(dev.create_committed_resource(&heap_properties_default(),
                                                        D3D12_HEAP_FLAG_NONE,
                                                        &tex_desc,
                                                        D3D12_RESOURCE_STATE_COMMON,
                                                        None));
        trace!("Create fence");
        let fence = try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));
        trace!("Create dheap");
        let dheap = try!(DescriptorHeap::new(&dev, 3, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        trace!("Create hpass_dheap");
        let hpass_dheap = try!(DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        trace!("Create calloc");
        let calloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
        trace!("Create clist");
        let clist: D3D12GraphicsCommandList =
            try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, None));
        trace!("Close clist");
        try!(clist.close());

        trace!("Create gralloc");
        let gralloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
        trace!("Create grlist");
        let grlist: D3D12GraphicsCommandList =
            try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &gralloc, None));
        trace!("Close grlist");
        try!(grlist.close());


        Ok(TonemapperResources {
            h_tex: h_tex,
            im_tex: im_tex,
            srcformat: srcformat,
            fence: fence,
            dheap: dheap,
            hpass_dheap: hpass_dheap,
            calloc: calloc,
            clist: clist,
            gralloc: gralloc,
            grlist: grlist,
        })
    }
}

pub struct Tonemapper {
    hpass_cpso: D3D12PipelineState,
    hpass_rs: D3D12RootSignature,
    cpso: D3D12PipelineState,
    root_sig: D3D12RootSignature,
    // luid of adapter cpso was generated on. generate_mips asserts that it is the same adapter as passed in DXCore
    luid: Luid,
}

// TODO: Use shader reflection to get count of thread groups from shader
const HTGroups: u32 = 128;
const VTGroups: u32 = 128;

impl Tonemapper {
    // TODO: return some error code. Even if nothing meaningful can be done with the error code, at least it allows to shutdown gracefully.
    // I just can't recover from shader compilation error or missing shader file.
    pub fn new(dev: &D3D12Device) -> Tonemapper {

        let mut f = File::open("tonemap.hlsl").expect("Cannot open tonemapper shader file.");
        let mut content = vec![];
        f.read_to_end(&mut content).expect("Cannot read tonemapper shader file.");
        let shader = str::from_utf8(&content[..])
                         .expect("Shader file content is not a valid UTF-8");

        let (hpass_shader_bc, hpass_rs_bc) = compile_shader_and_root_signature(shader.into(),
                                                                               "CSHorizontal",
                                                                               "RSDH")
                                                 .expect("Tonemapper horizontal pass shader \
                                                          compile error");

        let hpass_rs = dev.create_root_signature(0, &hpass_rs_bc[..])
                          .expect("Cannot create horisontal pass root signature");

        let hpass_cpsd = D3D12_COMPUTE_PIPELINE_STATE_DESC {
            pRootSignature: hpass_rs.iptr() as *mut _,
            CS: ShaderBytecode::from_vec(&hpass_shader_bc).get(),
            NodeMask: 0,
            CachedPSO: D3D12_CACHED_PIPELINE_STATE {
                pCachedBlob: ptr::null(),
                CachedBlobSizeInBytes: 0,
            },
            Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
        };
        let hpass_cpso = dev.create_compute_pipeline_state(&hpass_cpsd)
                            .expect("Cannot create horiziontal pass compute pipeline state");

        let (shader_bytecode, root_sig_bytecode) = compile_shader_and_root_signature(shader.into(),
                                                                                     "CSMain",
                                                                                     "RSD")
                                                       .expect("Tonemapper shader compile error");

        trace!("Root signature");
        let root_sig = dev.create_root_signature(0, &root_sig_bytecode[..])
                          .expect("Cannot create root signature");

        let cpsd = D3D12_COMPUTE_PIPELINE_STATE_DESC {
            pRootSignature: ptr::null_mut(),
            CS: ShaderBytecode::from_vec(&shader_bytecode).get(),
            NodeMask: 0,
            CachedPSO: D3D12_CACHED_PIPELINE_STATE {
                pCachedBlob: ptr::null(),
                CachedBlobSizeInBytes: 0,
            },
            Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
        };
        let cpso = dev.create_compute_pipeline_state(&cpsd)
                      .expect("Cannot create compute pipeline state");

        Tonemapper {
            hpass_cpso: hpass_cpso,
            hpass_rs: hpass_rs,
            cpso: cpso,
            root_sig: root_sig,
            luid: Luid(dev.get_adapter_luid()),
        }
    }

    // src - HDR image, dst - swap chain back buffer
    // This function creates GPU "thread", which can be "joined" by using t_fence
    pub fn tonemap(&self,
                   core: &DXCore,
                   res: &mut TonemapperResources,
                   src: &D3D12Resource,
                   dst: &D3D12Resource,
                   t_fence: &mut Fence)
                   -> HResult<()> {
        if cfg!(debug_assertions) { trace!("tonemap") };
        if cfg!(debug_assertions) { trace!("Assert same adapter") };
        assert!(&self.luid == &Luid(core.dev.get_adapter_luid()));

        if cfg!(debug_assertions) { trace!("callor.reset") };
        try!(res.calloc.reset());
        core.dump_info_queue();
        try!(res.clist.reset(&res.calloc, Some(&self.cpso)));

        if cfg!(debug_assertions) { trace!("rdesc") };
        let srcdesc = src.get_desc();
        let (cw, ch) = (srcdesc.Width as u32, srcdesc.Height);


        let dstdesc = dst.get_desc();
        if cfg!(debug_assertions) { trace!("uav_desc") };

        let clist = &res.clist;
        // Horizontal pass
        let src_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev.create_shader_resource_view(Some(&src), Some(&src_desc), res.hpass_dheap.cpu_handle(0));

        let dst_desc = uav_tex2d_desc(srcdesc.Format, 0, 0);
        core.dev.create_unordered_access_view(Some(&res.h_tex), None, Some(&dst_desc), res.hpass_dheap.cpu_handle(1));

        clist.set_pipeline_state(&self.hpass_cpso);
        clist.set_compute_root_signature(&self.hpass_rs);
        clist.set_descriptor_heaps(&[res.hpass_dheap.get()]);
        clist.set_compute_root_descriptor_table(0, res.hpass_dheap.gpu_handle(0));

        clist.resource_barrier(&[
          *ResourceBarrier::transition(&src,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
          *ResourceBarrier::transition(&res.h_tex,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        clist.dispatch(cw / HTGroups + 1, ch, 1);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.h_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
          *ResourceBarrier::transition(&res.im_tex, 
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        // Vertical and final pass

        let srv_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev.create_shader_resource_view(Some(&src), Some(&srv_desc), res.dheap.cpu_handle(0));

        let h_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev
            .create_shader_resource_view(Some(&res.h_tex), Some(&h_desc), res.dheap.cpu_handle(1));

        let uav_desc = uav_tex2d_desc(dstdesc.Format, 0, 0);
        core.dev.create_unordered_access_view(Some(&res.im_tex),
                                              None,
                                              Some(&uav_desc),
                                              res.dheap.cpu_handle(2));


        clist.set_pipeline_state(&self.cpso);
        clist.set_compute_root_signature(&self.root_sig);
        clist.set_descriptor_heaps(&[res.dheap.get()]);
        clist.set_compute_root_descriptor_table(0, res.dheap.gpu_handle(0));

        clist.dispatch(cw, ch / VTGroups + 1, 1);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&src,
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&res.h_tex, 
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&res.im_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
        ]);
        if cfg!(debug_assertions) { trace!("Close compute list") };
        try!(clist.close().map_err(|hr| {
            core.dump_info_queue();
            hr
        }));
        core.compute_queue.execute_command_lists(&[clist]);

        let next_fence_val = core.next_fence_value();
        try!(core.compute_queue.signal(&res.fence, next_fence_val));
        // Instruct graphics queue to wait for compute queue
        try!(core.graphics_queue.wait(&res.fence, next_fence_val));

        // Swapchain back buffers are special. Only graphics queue associated with swapchain can write them.
        if cfg!(debug_assertions) { trace!("grallor.reset") };
        try!(res.gralloc.reset());
        core.dump_info_queue();
        try!(res.grlist.reset(&res.gralloc, None));
        let grlist = &res.grlist;

        let source_copy_loc = texture_copy_location_index(&res.im_tex, 0);
        let dest_copy_loc = texture_copy_location_index(&dst, 0);

        let src_box = D3D12_BOX {
            left: 0,
            top: 0,
            front: 0,
            right: cw,
            bottom: ch,
            back: 1,
        };
        grlist.resource_barrier(&[*ResourceBarrier::transition(&res.im_tex,
                                                               D3D12_RESOURCE_STATE_COMMON,
                                                               D3D12_RESOURCE_STATE_COPY_SOURCE),
                                  *ResourceBarrier::transition(&dst,
                                                               D3D12_RESOURCE_STATE_COMMON,
                                                               D3D12_RESOURCE_STATE_COPY_DEST)]);
        grlist.copy_texture_region(&dest_copy_loc, 0, 0, 0, &source_copy_loc, Some(&src_box));
        grlist.resource_barrier(&[*ResourceBarrier::transition(&res.im_tex,
                                                               D3D12_RESOURCE_STATE_COPY_SOURCE,
                                                               D3D12_RESOURCE_STATE_COMMON),
                                  *ResourceBarrier::transition(&dst,
                                                               D3D12_RESOURCE_STATE_COPY_DEST,
                                                               D3D12_RESOURCE_STATE_COMMON)]);

        if cfg!(debug_assertions) { trace!("Close graphics list") };
        try!(grlist.close().map_err(|hr| {
            core.dump_info_queue();
            hr
        }));
        core.graphics_queue.execute_command_lists(&[grlist]);

        let fence_val = core.next_fence_value();
        try!(t_fence.signal(&core.graphics_queue, fence_val));

        if cfg!(debug_assertions) { trace!("dump_info_queue") };
        core.dump_info_queue();

        Ok(())
    }
}
