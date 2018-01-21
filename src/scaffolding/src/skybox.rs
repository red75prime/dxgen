extern crate image;

use core::{self, DXCore, DumpOnError};
use create_device as cdev;
use downsampler::Downsampler;
use dxsafe::*;
use dxsafe::structwrappers::*;
use self::image::hdr;
use std::fs;
use std::io;
use std::path::Path;
use std::ptr;
use utils;
use winapi::*;

// Skybox per frame resources, to be exact
#[allow(unused)]
pub struct SkyboxResources {
}

pub struct Skybox {
    root_sig: D3D12RootSignature,
    pso: D3D12PipelineState,
    _skytex: D3D12Resource,
    skydesc: D3D12_SHADER_RESOURCE_VIEW_DESC,
    dheap: DescriptorHeap,
}

impl Skybox {
    pub fn new(core: &DXCore, downsampler: &Downsampler, rt_format: DXGI_FORMAT) -> HResult<Skybox> {
        core.dump_info_queue_tagged("Skybox::new() start. Infoqueue cleanup");
        trace!("load_skybox_texture()");
        let (tex, srv_desc) = load_skybox_texture(&core.dev, &core.compute_queue, &core.copy_queue, downsampler).dump(core, "load skybox")?;

        let dheap = try!(DescriptorHeap::new(&core.dev, 1, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));

        core.dev.create_shader_resource_view(Some(&tex), Some(&srv_desc), dheap.cpu_handle(0));

        let mut root_sig_bc = vec![];
        let mut v_shader_bc = vec![];
        let mut p_shader_bc = vec![];

        trace!("compile_shaders(\"skybox.hlsl\", ...)");
        try!(cdev::compile_shaders("skybox.hlsl", 
                &mut[
                    ("SBRS", "rootsig_1_0", &mut root_sig_bc),
                    ("VSMain", "vs_5_0", &mut v_shader_bc),
                    ("PSMain", "ps_5_0", &mut p_shader_bc),
                ], D3DCOMPILE_OPTIMIZATION_LEVEL3)
            .map_err(|err|{ error!("{}", err); DXGI_ERROR_INVALID_CALL }));

        trace!("create_root_signature()");
        let root_signature = try!(core.dev.create_root_signature(0, &root_sig_bc[..]));
        try!(root_signature.set_name("skybox SBRS"));

        let mut pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
            pRootSignature: root_signature.iptr() as *mut _,
            VS: ShaderBytecode::from_vec(&v_shader_bc).get(),
            PS: ShaderBytecode::from_vec(&p_shader_bc).get(),
            RasterizerState: D3D12_RASTERIZER_DESC {
                CullMode: D3D12_CULL_MODE_NONE,
                ..rasterizer_desc_default()
            },
            DepthStencilState: D3D12_DEPTH_STENCIL_DESC {
                DepthFunc: D3D12_COMPARISON_FUNC_LESS_EQUAL,
                ..depth_stencil_state_default()
            },
            InputLayout: D3D12_INPUT_LAYOUT_DESC {
                pInputElementDescs: ptr::null(),
                NumElements: 0,
            },
            PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
            NumRenderTargets: 1,
            DSVFormat: DXGI_FORMAT_D32_FLOAT,
            Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
            ..graphics_pipeline_state_desc_default()
        };
        pso_desc.RTVFormats[0] = rt_format;

        let pso = try!(core.dev.create_graphics_pipeline_state(&pso_desc));

        Ok(Skybox {
            _skytex: tex,
            skydesc: srv_desc,
            root_sig: root_signature,
            pso: pso,
            dheap: dheap,
        })
    }

    pub fn populate_command_list(&self, glist: &D3D12GraphicsCommandList, view_const: &D3D12Resource) {
        glist.set_pipeline_state(&self.pso);
        glist.set_graphics_root_signature(&self.root_sig);
        glist.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
        glist.ia_set_vertex_buffers(0, None);
        glist.ia_set_index_buffer(None);
        glist.set_descriptor_heaps(&[self.dheap.get()]);
        glist.set_graphics_root_constant_buffer_view(0, view_const.get_gpu_virtual_address());
        glist.set_graphics_root_descriptor_table(1, self.dheap.gpu_handle(0));

        glist.draw_instanced(4, 1, 0, 0);

    }

    pub fn texture(&self) -> (&D3D12Resource, &D3D12_SHADER_RESOURCE_VIEW_DESC) {
        (&self._skytex, &self.skydesc)
    }
}

fn load_skybox_texture(dev: &D3D12Device, comp_queue: &D3D12CommandQueue, copy_queue: &D3D12CommandQueue, downsampler: &Downsampler) 
                        -> HResult<(D3D12Resource, D3D12_SHADER_RESOURCE_VIEW_DESC)> {
    let paths = ["./assets","../../src/assets/","./","../../src/"];
    let maybe_f = paths
                    .iter() // Skip all paths, which cannot be opened 
                    .filter_map(|&p|fs::File::open(&Path::new(p).join("skybox.hdr")).ok())
                    .next(); // Take first one, which can be opened
    let f = 
        if let Some(file) = maybe_f { 
            file
        } else {
            error!("skybox.hdr not found");
            return Err(ERROR_FILE_NOT_FOUND as HRESULT);
        };
    let reader = io::BufReader::new(f);
    let decoder = try!(hdr::HDRDecoder::new(reader)
            .map_err(|err|{ error!("Cannot parse 'skybox.hdr': {}", err); ERROR_INVALID_DATA as HRESULT }));
    let meta = decoder.metadata();
    let data = try!(decoder.read_image_native()
            .map_err(|err|{ error!("Cannot read 'skybox.hdr': {}", err); ERROR_INVALID_DATA as HRESULT }));

    println!("First data point: {} {} {} {}", data[0].c[0], data[0].c[1], data[0].c[2], data[0].e);

    let rgbe8_tex_desc = resource_desc_tex2d_nomip(
                                    meta.width as u64,
                                    meta.height as u32,
                                    DXGI_FORMAT_R32_UINT,
                                    D3D12_RESOURCE_FLAG_NONE);

    let rgbe8_tex = try!(dev.create_committed_resource(
                                    &heap_properties_default(),
                                    D3D12_HEAP_FLAG_NONE,
                                    &rgbe8_tex_desc,
                                    D3D12_RESOURCE_STATE_COMMON,
                                    None));

    let rgbe8_srv_desc = srv_tex2d_default_slice_mip(rgbe8_tex_desc.Format, 0, 1); 
    
    try!(utils::upload_into_texture(dev, copy_queue, &rgbe8_tex, meta.width, meta.height, &data[..]));

    trace!("Check DXGI_FORMAT_R9G9B9E5_SHAREDEXP support");
    let shexp_support = try!(dev.check_feature_support_format_support(DXGI_FORMAT_R9G9B9E5_SHAREDEXP));

    if shexp_support.Support1.0 & D3D12_FORMAT_SUPPORT1_TEXTURE2D.0 == 0
        //|| shexp_support.Support1.0 & D3D12_FORMAT_SUPPORT1_SHADER_SAMPLE.0 == 0
        //|| shexp_support.Support2.0 & D3D12_FORMAT_SUPPORT2_UAV_TYPED_STORE.0 == 0 
    {
        trace!("Use DXGI_FORMAT_R16G16B16A16_FLOAT");
        let tex_desc = D3D12_RESOURCE_DESC {
                MipLevels: 3,
                ..resource_desc_tex2d_nomip(meta.width as u64,
                                            meta.height as u32,
                                            DXGI_FORMAT_R16G16B16A16_FLOAT,
                                            //DXGI_FORMAT_R32G32B32A32_FLOAT,
                                            D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS)
            };
        trace!("create skybox texture");
        let tex = try!(create_resource_default(dev, &tex_desc));
        tex.set_name("Skybox texture DXGI_FORMAT_R16G16B16A16_FLOAT")?;
        trace!("texture created");
        let tex_uav_desc = uav_tex2d_desc(tex_desc.Format, 0, 0);
        try!(convert_to_rgb(dev, comp_queue, &rgbe8_tex, &rgbe8_srv_desc, &tex, &tex_uav_desc));
        try!(downsampler.generate_mips(dev, comp_queue, &tex));
        let tex_srv_desc = srv_tex2d_default_slice_mip(tex_desc.Format, 0, tex_desc.MipLevels as u32);
        Ok((tex, tex_srv_desc))
    } else {
        trace!("Use DXGI_FORMAT_R9G9B9E5_SHAREDEXP");
        let tex_desc = D3D12_RESOURCE_DESC {
                MipLevels: 1, // Downsampler will not work with sharedexp
                ..resource_desc_tex2d_nomip(meta.width as u64,
                                            meta.height as u32,
                                            DXGI_FORMAT_R9G9B9E5_SHAREDEXP,
                                            D3D12_RESOURCE_FLAG_NONE)
            };
        trace!("create skybox texture");
        let tex = try!(create_resource_default(dev, &tex_desc));
        try!(tex.set_name("Skybox texture DXGI_FORMAT_R9G9B9E5_SHAREDEXP"));
        try!(convert_to_rgb_sharedexp(dev, comp_queue, &rgbe8_tex, &rgbe8_srv_desc, &tex));
        let tex_srv_desc = srv_tex2d_default_slice_mip(tex_desc.Format, 0, tex_desc.MipLevels as u32);
        Ok((tex, tex_srv_desc))
    }
}

fn create_resource_default(dev: &D3D12Device, desc: &D3D12_RESOURCE_DESC) -> HResult<D3D12Resource> {
    dev.create_committed_resource(
        &heap_properties_default(),
        D3D12_HEAP_FLAG_NONE,
        desc,
        D3D12_RESOURCE_STATE_COMMON,
        None)
}

fn convert_to_rgb(dev: &D3D12Device, cqueue: &D3D12CommandQueue, src: &D3D12Resource, src_srv_desc: &D3D12_SHADER_RESOURCE_VIEW_DESC, 
                    dst: &D3D12Resource, dst_uav_desc: &D3D12_UNORDERED_ACCESS_VIEW_DESC) -> HResult<()> {
    // One time conversion, so I don't keep anything
    let mut root_sig_bc = vec![];
    let mut convert_bc = vec![];
    let mut smooth_bc = vec![];

    if dev.check_feature_support_options()?.TypedUAVLoadAdditionalFormats == 0 {
        error!("Device doesn't support additional formats in typed UAV load");
        return Err(DXGI_ERROR_INVALID_CALL);
    };

    trace!("convert_to_rgb compile_shaders");
    cdev::compile_shaders("skybox.hlsl", 
            &mut[
                ("TCRS", "rootsig_1_0", &mut root_sig_bc),
                ("CSTexConvert", "cs_5_0", &mut convert_bc),
                ("CSSmooth", "cs_5_0", &mut smooth_bc),
            ], D3DCOMPILE_OPTIMIZATION_LEVEL3)
        .map_err(|err|{ error!("{}", err) ; DXGI_ERROR_INVALID_CALL })?;

    trace!("convert_to_rgb create root signature");
    let root_sig = dev.create_root_signature(0, &root_sig_bc[..])?;
    try!(root_sig.set_name("convert_to_rgb TCRS"));

    let convert_pso = try!(create_default_cpso(&dev, &root_sig, convert_bc));
    let smooth_pso = try!(create_default_cpso(&dev, &root_sig, smooth_bc));

    trace!("convert_to_rgb create_fence");
    let fence = try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));

    let dheap = try!(DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));

    dev.create_shader_resource_view(Some(src), Some(src_srv_desc), dheap.cpu_handle(0));
    dev.create_unordered_access_view(Some(dst), None, Some(dst_uav_desc), dheap.cpu_handle(1));

    let calloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
    let clist: D3D12GraphicsCommandList =
        try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, Some(&convert_pso)));
    
    clist.set_descriptor_heaps(&[dheap.get()]);

    clist.set_compute_root_signature(&root_sig);
    clist.set_compute_root_descriptor_table(0, dheap.gpu_handle(0));

    clist.resource_barrier(&[
        *ResourceBarrier::transition(src,
        D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
        *ResourceBarrier::transition(dst,
        D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
    ]);

    let src_desc = src.get_desc();

    clist.dispatch(src_desc.Width as u32, src_desc.Height, 1);

    trace!("convert_to_rgb clist.close 1");
    try!(clist.close());

    cqueue.execute_command_lists(&[&clist]);
    

    for i in 0..10 {
        trace!("convert_to_rgb clist.reset smooth {}", i);
        clist.reset(&calloc, Some(&smooth_pso))?;
        clist.set_descriptor_heaps(&[dheap.get()]);

        clist.set_compute_root_signature(&root_sig);
        clist.set_compute_root_descriptor_table(0, dheap.gpu_handle(0));
        //clist.resource_barrier(&[*ResourceBarrier::uav(dst)]);
        let groups = 8;
        clist.dispatch(src_desc.Width as u32/groups, src_desc.Height/groups, 1);
        trace!("convert_to_rgb clist.close smooth {}", i);
        clist.close()?;
        cqueue.execute_command_lists(&[&clist]);
    };

    // clist.reset(&calloc, None)?;

    // clist.resource_barrier(&[
    //     *ResourceBarrier::transition(src,
    //     D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
    //     *ResourceBarrier::transition(dst,
    //     D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
    // ]);

    // trace!("convert_to_rgb clist.close");
    // try!(clist.close());

    // cqueue.execute_command_lists(&[&clist]);

    trace!("convert_to_rgb wait");
    try!(core::wait(cqueue, &fence, &core::create_event()));

    // core.dump_info_queue_tagged("Dump at the end of convert_to_rgb");

    Ok(())
}

fn convert_to_rgb_sharedexp(dev: &D3D12Device, cqueue: &D3D12CommandQueue, src: &D3D12Resource, src_srv_desc: &D3D12_SHADER_RESOURCE_VIEW_DESC, 
                    dst: &D3D12Resource) -> HResult<()> {
    // One time conversion, so I don't keep anything
    let mut root_sig_bc = vec![];
    let mut convert_bc = vec![];

    trace!("convert_to_rgb compile_shaders");
    try!(cdev::compile_shaders("skybox.hlsl", 
            &mut[
                ("TCRS", "rootsig_1_0", &mut root_sig_bc),
                ("CSTexConvertShexp", "cs_5_0", &mut convert_bc),
            ], D3DCOMPILE_OPTIMIZATION_LEVEL3)
        .map_err(|err|{ error!("{}", err) ; DXGI_ERROR_INVALID_CALL }));

    let root_sig = try!(dev.create_root_signature(0, &root_sig_bc[..]));
    try!(root_sig.set_name("convert_to_rgb_sharedexp TCRS"));

    let convert_pso = try!(create_default_cpso(&dev, &root_sig, convert_bc));

    let dst_desc = dst.get_desc();
    let tex_desc = D3D12_RESOURCE_DESC {
        MipLevels: dst_desc.MipLevels,
        ..resource_desc_tex2d_nomip(dst_desc.Width,
                                    dst_desc.Height,
                                    DXGI_FORMAT_R32_UINT,
                                    D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS)
    };

    trace!("create intermediate skybox texture");
    let tex = try!(dev.create_committed_resource(
                                    &heap_properties_default(),
                                    D3D12_HEAP_FLAG_NONE,
                                    &tex_desc,
                                    D3D12_RESOURCE_STATE_COMMON,
                                    None));
    try!(tex.set_name("intermediate Skybox texture"));
    let tex_uav_desc = uav_tex2d_desc(tex_desc.Format, 0, 0);

    trace!("convert_to_rgb create_fence");
    let fence = try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));

    let dheap = try!(DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));

    dev.create_shader_resource_view(Some(src), Some(src_srv_desc), dheap.cpu_handle(0));
    dev.create_unordered_access_view(Some(&tex), None, Some(&tex_uav_desc), dheap.cpu_handle(1));

    let calloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
    let clist: D3D12GraphicsCommandList =
        try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, Some(&convert_pso)));
    
    clist.set_descriptor_heaps(&[dheap.get()]);

    clist.set_compute_root_signature(&root_sig);
    clist.set_compute_root_descriptor_table(0, dheap.gpu_handle(0));

    clist.resource_barrier(&[
        *ResourceBarrier::transition(src,
        D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
        *ResourceBarrier::transition(&tex,
        D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
    ]);

    let src_desc = src.get_desc();

    clist.dispatch(src_desc.Width as u32, src_desc.Height, 1);

    clist.resource_barrier(&[
        *ResourceBarrier::transition(src,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
        *ResourceBarrier::transition(&tex,
        D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
    ]);

    clist.copy_resource(dst, &tex);

    clist.resource_barrier(&[
        *ResourceBarrier::transition(dst,
        D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON),
    ]);

    trace!("convert_to_rgb clist.close");
    try!(clist.close());

    cqueue.execute_command_lists(&[&clist]);

    trace!("convert_to_rgb wait");
    try!(core::wait(cqueue, &fence, &core::create_event()));

    // core.dump_info_queue_tagged("Dump at the end of convert_to_rgb");

    Ok(())
}
