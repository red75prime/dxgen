use winapi::*;
use core::*;
use create_device::*;
use utils::*;
use dx_safe::*;
use dx_safe::structwrappers::*;
use std::ptr;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::str;
use std::slice;

pub struct TonemapperResources {
    h_tex: D3D12Resource,
    hv_tex: D3D12Resource,
    im_tex: D3D12Resource,
    rw_total_len: u32,
    rw_total: D3D12Resource,
    rw_buf_total: D3D12Resource,
    rb_total: D3D12Resource,
    rb_one_total: D3D12Resource,
    srcformat: DXGI_FORMAT,
    fence: D3D12Fence,
    calloc: D3D12CommandAllocator,
    clist: D3D12GraphicsCommandList,
    clear_list: D3D12GraphicsCommandList,
    gralloc: D3D12CommandAllocator,
    grlist: D3D12GraphicsCommandList,
    total_dheap: DescriptorHeap,
    total_cuav_dheap: DescriptorHeap, // ClearUnorderedAccessView requires shader inaccessible descriptor heap
    hpass_dheap: DescriptorHeap,
    vpass_dheap: DescriptorHeap,
    dheap: DescriptorHeap,
}

// Holds intermediate variables for Tonemapper function.
// To clarify. Tonemapper structure below can be considered to be a function that takes one argument (HDR image) and produces one value (LDR image).
// Creation of temporary intermediate resource on each call to Tonemapper::tonemap() is very slow. This structure hold them.
// Note that the structure must be recreated when dimensions of Tonemapper input change.
impl TonemapperResources {
    pub fn new(dev: &D3D12Device, tonemapper: &Tonemapper,
               w: u32,
               h: u32,
               srcformat: DXGI_FORMAT,
               dstformat: DXGI_FORMAT)
               -> HResult<TonemapperResources> {
        trace!("Create texture for horizontal gaussian kernel convolution storage");
        let (temp_w, temp_h) = (w/2, h/2);
        let h_tex = try!(dev.create_committed_resource(
          &heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_tex2d_nomip(temp_w as u64, temp_h as u32, 
              srcformat, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS), 
          D3D12_RESOURCE_STATE_COMMON, None));
        try!(h_tex.set_name("h_tex".into()));
        trace!("Create texture for resulting gaussian kernel convolution storage");
        let hv_tex = try!(dev.create_committed_resource(
          &heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_tex2d_nomip(temp_w as u64, temp_h as u32, 
              srcformat, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS), 
          D3D12_RESOURCE_STATE_COMMON, None));
        try!(hv_tex.set_name("hv_tex".into()));

        trace!("Create buffer for brightness values");
        let rw_total_len = ((w-1)/TOTAL_CHUNK_SIZE+1)*((h-1)/TOTAL_CHUNK_SIZE+1);
        let rw_total = try!(dev.create_committed_resource(
          &heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_buffer_uav((rw_total_len*4) as u64), 
          D3D12_RESOURCE_STATE_COMMON, None));
        try!(rw_total.set_name("rw_total".into()));

        trace!("Create buffer for brightness value");
        let rw_buf_total = try!(dev.create_committed_resource(
          &heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_buffer_uav(16), 
          D3D12_RESOURCE_STATE_COMMON, None));
        try!(rw_buf_total.set_name("rw_buf_total".into()));

        trace!("Create read-back buffer for total brightness values");
        let rb_total = try!(dev.create_committed_resource(
          &heap_properties_readback(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_const_buffer(4*rw_total_len as u64), 
          D3D12_RESOURCE_STATE_COPY_DEST, None));
        try!(rb_total.set_name("rb_total".into()));
        debug!("rb_total GPU VA:0x{:x}", rb_total.get_gpu_virtual_address());

        trace!("Create read-back buffer for total brightness value");
        let rb_one_total = try!(dev.create_committed_resource(
          &heap_properties_readback(), D3D12_HEAP_FLAG_NONE, 
          &resource_desc_const_buffer(16),
          D3D12_RESOURCE_STATE_COPY_DEST, None));
        try!(rb_one_total.set_name("rb_one_total".into()));

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
        try!(im_tex.set_name("Intermediate texture for copying into back-buffer".into()));
        trace!("Create fence");
        let fence = try!(dev.create_fence(0, D3D12_FENCE_FLAG_SHARED));
        trace!("Create dheap");
        let dheap = try!(DescriptorHeap::new(&dev, 3, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        trace!("Create hpass_dheap");
        let hpass_dheap = try!(DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        trace!("Create vpass_dheap");
        let vpass_dheap = try!(DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        trace!("Create total_dheap");
        let total_dheap = try!(DescriptorHeap::new(&dev, 4, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        trace!("Create total_cuav_dheap");
        let total_cuav_dheap = try!(DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, false, 0));
        trace!("Create calloc");
        let calloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
        trace!("Create clist");
        let clist: D3D12GraphicsCommandList =
            try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, Some(&tonemapper.total_cpso)));
        trace!("Close clist");
        try!(clist.close());
        trace!("Create clear_list");
        let clear_list: D3D12GraphicsCommandList =
            try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, None));
        trace!("Close clear_list");
        try!(clear_list.close());

        let total_desc = uav_buffer_desc(rw_total_len, 4);
        // Create descriptors in shader visible heap for passing into compute shaders
        dev.create_unordered_access_view(Some(&rw_total), None, Some(&total_desc), total_dheap.cpu_handle(1));
        dev.create_unordered_access_view(Some(&rw_buf_total), None, Some(&uav_buffer_desc(1,4)), total_dheap.cpu_handle(2));
        dev.create_shader_resource_view(Some(&rw_total), Some(&srv_buffer(rw_total_len, 4)), total_dheap.cpu_handle(3));

        // Create descriptors in shader invisible heap for clear_unordered_access_view
        dev.create_unordered_access_view(Some(&rw_total), None, Some(&uav_buffer_desc(rw_total_len, 4)), total_cuav_dheap.cpu_handle(0));
        dev.create_unordered_access_view(Some(&rw_buf_total), None, Some(&uav_buffer_desc(1,4)), total_cuav_dheap.cpu_handle(1));

        trace!("Create gralloc");
        let gralloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
        trace!("Create grlist");
        let grlist: D3D12GraphicsCommandList =
            try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &gralloc, None));
        trace!("Close grlist");
        try!(grlist.close());

        trace!("TonemapperResources created");
        Ok(TonemapperResources {
            h_tex: h_tex,
            hv_tex: hv_tex,
            im_tex: im_tex,
            rw_total_len: rw_total_len,
            rw_total: rw_total,
            rw_buf_total: rw_buf_total,
            rb_total: rb_total,
            rb_one_total: rb_one_total,
            srcformat: srcformat,
            fence: fence,
            dheap: dheap,
            total_dheap: total_dheap,
            total_cuav_dheap: total_cuav_dheap,
            hpass_dheap: hpass_dheap,
            vpass_dheap: vpass_dheap,
            calloc: calloc,
            clist: clist,
            clear_list: clear_list,
            gralloc: gralloc,
            grlist: grlist,
        })
    }

    fn total_brightness(&self) -> f32 {
        let read_range = D3D12_RANGE { Begin: 0, End: 4 };

        let total_brightness = unsafe {
            let mut ptr: *mut u8 = ptr::null_mut();
            self.rb_one_total.map(0, Some(&read_range), Some(&mut ptr)).expect("Cannot map rb_total");
            &*(ptr as *mut f32)
        };
        let ret = *total_brightness;
        self.rb_one_total.unmap(0, None);
        //(ret as f32)/1000.
        ret
    }

    fn total_brightness_full(&self) -> (f32,f32,f32) {
        let read_range = D3D12_RANGE { Begin: 0, End: 4*self.rw_total_len as u64 };

        let arr = unsafe {
            let mut ptr: *mut u8 = ptr::null_mut();
            self.rb_total.map(0, Some(&read_range), Some(&mut ptr)).expect("Cannot map rb_total");
            slice::from_raw_parts(ptr as *const f32, self.rw_total_len as usize)
        };
        let tb = arr.iter().fold(0., |a, &v| a+v);
        let minb = arr.iter().fold(arr[0], |m, &v| f32::min(m, v));
        let maxb = arr.iter().fold(arr[0], |m, &v| f32::max(m, v));
        self.rb_total.unmap(0, None);
        //(*total_brightness as f32)/1000.
        (tb, minb, maxb)
    }
}

pub struct Tonemapper {
    tb_event: Event,
    total_cpso: D3D12PipelineState,
    total_rs: D3D12RootSignature,
    buf_total_cpso: D3D12PipelineState,
    hpass_cpso: D3D12PipelineState,
    hpass_rs: D3D12RootSignature,
    vpass_cpso: D3D12PipelineState,
    vpass_rs: D3D12RootSignature,
    cpso: D3D12PipelineState,
    root_sig: D3D12RootSignature,
    // luid of adapter cpso was generated on. generate_mips asserts that it is the same adapter as passed in DXCore
    luid: Luid,
}

// TODO: Use shader reflection to get count of thread groups from shader
const HTGROUPS: u32 = 128;
const VTGROUPS: u32 = 128;
const COMBINE_GROUPS: u32 = 32;
const TOTAL_CHUNK_SIZE: u32 = 32*2; // Twice the TotalGroups from shader
const BUF_TOTAL_CHUNK_SIZE: u32 = 512; 

impl Tonemapper {
    // TODO: return some error code. Even if nothing meaningful can be done with the error code, at least it allows to shutdown gracefully.
    // I just can't recover from shader compilation error or missing shader file.
    pub fn new(dev: &D3D12Device) -> Tonemapper {

        let mut buf_total_shader_bc = vec![];
        let mut total_shader_bc = vec![];
        let mut total_rs_bc = vec![];
        compile_shaders("reductor.hlsl", 
            &mut[
                ("CSBufTotal", "cs_5_1", &mut buf_total_shader_bc),
                ("CSTotal", "cs_5_1", &mut total_shader_bc),
                ("RSDT", "rootsig_1_0", &mut total_rs_bc),
            ], D3DCOMPILE_OPTIMIZATION_LEVEL3).unwrap();

        let total_rs = dev.create_root_signature(0, &total_rs_bc[..])
                          .expect("Cannot create RSDT root signature");

        let buf_total_cpso = create_default_cpso(dev, &total_rs, buf_total_shader_bc)
                                .expect("Cannot create buffer reduce compute pipeline state");

        let total_cpso = create_default_cpso(dev, &total_rs, total_shader_bc)
                            .expect("Cannot create total brightness compute pipeline state");

        let mut hpass_rs_bc = vec![];
        let mut vpass_rs_bc = vec![];
        let mut final_rs_bc = vec![];
        let mut hpass_shader_bc = vec![];
        let mut vpass_shader_bc = vec![];
        let mut final_shader_bc = vec![];

        compile_shaders("tohemap.hlsl", 
            &mut[
                ("RSDH",        "rootsig_1_0", &mut hpass_rs_bc),
                ("RSDV",        "rootsig_1_0", &mut vpass_rs_bc),
                ("RSD",         "rootsig_1_0", &mut final_rs_bc),
                ("CSHorizontal","cs_5_1", &mut hpass_shader_bc),
                ("CSVertical",  "cs_5_1", &mut vpass_shader_bc),
                ("CSMain",      "cs_5_1", &mut final_shader_bc),
            ], D3DCOMPILE_OPTIMIZATION_LEVEL3).unwrap();

        let hpass_rs = dev.create_root_signature(0, &hpass_rs_bc[..])
                          .expect("Cannot create horisontal pass root signature");

        let hpass_cpso = create_default_cpso(dev, &hpass_rs, hpass_shader_bc)
                            .expect("Cannot create horiziontal pass compute pipeline state");

        let vpass_rs = dev.create_root_signature(0, &vpass_rs_bc[..])
                          .expect("Cannot create vertical pass root signature");

        let vpass_cpso = create_default_cpso(dev, &vpass_rs, vpass_shader_bc)
                            .expect("Cannot create horizontal pass compute pipeline state");

        let final_rs = dev.create_root_signature(0, &final_shader_bc[..])
                          .expect("Cannot create root signature");

        let cpso = create_default_cpso(dev, &final_rs, final_shader_bc)
                      .expect("Cannot create compute pipeline state");

        Tonemapper {
            tb_event: create_event(),
            total_cpso: total_cpso,
            total_rs: total_rs,
            buf_total_cpso: buf_total_cpso,
            hpass_cpso: hpass_cpso,
            hpass_rs: hpass_rs,
            vpass_cpso: vpass_cpso,
            vpass_rs: vpass_rs,
            cpso: cpso,
            root_sig: final_rs,
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
        // TODO: Remove. This is for total brightness calculation bench
        wait_for_compute_queue(core, &res.fence, &self.tb_event);
        wait_for_graphics_queue(core, &res.fence, &self.tb_event);
        wait_for_copy_queue(core, &res.fence, &self.tb_event);

        ::perf_start("tbr");
        if cfg!(debug_assertions) { trace!("calloc.reset") };
        try!(res.calloc.reset());
        core.dump_info_queue();
        if cfg!(debug_assertions) { trace!("clear_list.reset") };
        try!(res.clear_list.reset(&res.calloc, None));

        if cfg!(debug_assertions) { trace!("srcdesc") };
        let srcdesc = src.get_desc();
        let (cw, ch) = (srcdesc.Width as u32, srcdesc.Height);

        if cfg!(debug_assertions) { trace!("dstdesc") };
        let dstdesc = dst.get_desc();

        let clear_list = &res.clear_list;

        try!(clear_list.close());

        let clist = &res.clist;
        if cfg!(debug_assertions) { trace!("clist.reset") };
        try!(clist.reset(&res.calloc, Some(&self.total_cpso)));
        // Total brightness
        clist.set_descriptor_heaps(&[res.total_dheap.get()]);
        //clist.set_pipeline_state(&self.total_cpso);

        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.rw_total,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
          *ResourceBarrier::transition(&res.rw_buf_total,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        // UAV for CPU handle MUST be in shader inaccessible descriptor heap
        if cfg!(debug_assertions) { trace!("Clear unordered access view") };
        //clear_list.clear_unordered_access_view_float(res.total_cuav_dheap.gpu_handle(0), res.total_cuav_dheap.cpu_handle(0), &res.rw_total, &[0.,0.,0.,0.], &[]);
        clist.clear_unordered_access_view_float(res.total_cuav_dheap.gpu_handle(1), res.total_cuav_dheap.cpu_handle(1), &res.rw_buf_total, &[0.,0.,0.,0.], &[]);
        //clear_list.clear_unordered_access_view_uint(res.total_cuav_dheap.gpu_handle(1), res.total_cuav_dheap.cpu_handle(1), &res.rw_buf_total, &[0,0,0,0], &[]);
        clist.resource_barrier(&[
          *ResourceBarrier::uav(&res.rw_total),
          *ResourceBarrier::uav(&res.rw_buf_total),
          //*ResourceBarrier::transition(&res.rw_total,
          //  D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
          //*ResourceBarrier::transition(&res.rw_buf_total,
          //  D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
        ]);

        let src_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev.create_shader_resource_view(Some(&src), Some(&src_desc), res.total_dheap.cpu_handle(0));


        clist.set_compute_root_signature(&self.total_rs);
        clist.set_compute_root_descriptor_table(1, res.total_dheap.gpu_handle(0));

        clist.resource_barrier(&[
          *ResourceBarrier::transition(&src,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
          //*ResourceBarrier::transition(&res.rw_total,
          //  D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
          //*ResourceBarrier::transition(&res.rw_buf_total,
          //  D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        let hdispatch = (cw-1) / TOTAL_CHUNK_SIZE + 1;
        let vdispatch = (ch-1) / TOTAL_CHUNK_SIZE + 1;
        clist.set_compute_root32_bit_constant(0, hdispatch, 0);
        clist.set_compute_root32_bit_constant(0, cw, 1);
        clist.set_compute_root32_bit_constant(0, ch, 2);
        clist.dispatch(hdispatch, vdispatch, 1);

        //clist.resource_barrier(&[
        //  *ResourceBarrier::uav(&res.rw_total),
        //]);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.rw_total,
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE),
        ]);
        clist.copy_resource(&res.rb_total, &res.rw_total);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.rw_total,
            D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        clist.set_pipeline_state(&self.buf_total_cpso);
        clist.set_compute_root_signature(&self.total_rs);
        clist.set_descriptor_heaps(&[res.total_dheap.get()]);
        clist.set_compute_root32_bit_constant(0, hdispatch, 0);
        clist.set_compute_root_descriptor_table(1, res.total_dheap.gpu_handle(0));
        let bdispatch = (hdispatch*vdispatch-1) / BUF_TOTAL_CHUNK_SIZE + 1;
        clist.dispatch(bdispatch, 1, 1);

        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.rw_buf_total,
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE),
        ]);
        // copy_resource cannot be used here. buffers have different sizes.
        clist.copy_buffer_region(&res.rb_one_total, 0, &res.rw_buf_total, 0, 4);
        //clist.copy_buffer_region(&res.rb_total, 0, &res.rw_total, 0, 4);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.rw_total,
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&res.rw_buf_total,
            D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_COMMON),
        ]);
        if cfg!(debug_assertions) { trace!("clist.close") };
        try!(clist.close());
        core.dump_info_queue_tagged("Before execute total brightness");
        if cfg!(debug_assertions) { trace!("Execute total brightness") };
        core.compute_queue.execute_command_lists(&[clist]);
        core.dump_info_queue_tagged("After execute total brightness");

        wait_for_compute_queue(core, &res.fence, &self.tb_event);
        //wait_for_graphics_queue(core, &res.fence, &self.tb_event);
        //wait_for_copy_queue(core, &res.fence, &self.tb_event);
        ::perf_end("tbr");

        let total_one = res.total_brightness();
        let total_brightness = res.total_brightness_full();
        let avg_brightness = total_brightness.0 / cw as f32 / ch as f32;
        print!("Total brightness: {:10.1}/{:?} Average brightness: {:.3} Dispatch treads: {}*{}={} rw_total size: {:4} bdispatch: {}         \r", total_one, total_brightness, avg_brightness, hdispatch, vdispatch, hdispatch*vdispatch, res.rw_total.get_desc().Width/4, bdispatch);
        let _ = ::std::io::Write::flush(&mut ::std::io::stdout());

        try!(clist.reset(&res.calloc, Some(&self.hpass_cpso)));

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
          *ResourceBarrier::transition(&res.h_tex,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        clist.dispatch(cw / HTGROUPS + 1, ch/2, 1);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.h_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
        ]);

        // Vertical pass
        let src_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev.create_shader_resource_view(Some(&res.h_tex), Some(&src_desc), res.vpass_dheap.cpu_handle(0));

        let dst_desc = uav_tex2d_desc(srcdesc.Format, 0, 0);
        core.dev.create_unordered_access_view(Some(&res.hv_tex), None, Some(&dst_desc), res.vpass_dheap.cpu_handle(1));

        clist.set_pipeline_state(&self.vpass_cpso);
        clist.set_compute_root_signature(&self.vpass_rs);
        clist.set_descriptor_heaps(&[res.vpass_dheap.get()]);
        clist.set_compute_root_descriptor_table(0, res.vpass_dheap.gpu_handle(0));

        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.hv_tex,
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        clist.dispatch(cw/2, ch/2/VTGROUPS+1, 1);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&res.h_tex, 
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&res.hv_tex, 
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
          *ResourceBarrier::transition(&res.im_tex, 
            D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
        ]);

        // Final pass

        let srv_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev.create_shader_resource_view(Some(&src), Some(&srv_desc), res.dheap.cpu_handle(0));

        let hv_desc = srv_tex2d_default_slice_mip(srcdesc.Format, 0, 1);
        core.dev
            .create_shader_resource_view(Some(&res.hv_tex), Some(&hv_desc), res.dheap.cpu_handle(1));

        let uav_desc = uav_tex2d_desc(dstdesc.Format, 0, 0);
        core.dev.create_unordered_access_view(Some(&res.im_tex),
                                              None,
                                              Some(&uav_desc),
                                              res.dheap.cpu_handle(2));


        clist.set_pipeline_state(&self.cpso);
        clist.set_compute_root_signature(&self.root_sig);
        clist.set_descriptor_heaps(&[res.dheap.get()]);
        clist.set_compute_root_descriptor_table(0, res.dheap.gpu_handle(0));

        clist.dispatch(cw / COMBINE_GROUPS + 1, ch / COMBINE_GROUPS + 1, 1);
        clist.resource_barrier(&[
          *ResourceBarrier::transition(&src,
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
          *ResourceBarrier::transition(&res.hv_tex, 
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
        if cfg!(debug_assertions) { trace!("gralloc.reset") };
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
