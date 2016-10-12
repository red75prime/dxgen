extern crate image;

use core::{self, DXCore};
use create_device as cdev;
use downsampler::Downsampler;
use dxsafe::*;
use dxsafe::structwrappers::*;
use self::image::hdr;
use std::fs;
use std::io;
use utils;
use winapi::*;

// Skybox per frame resources to be exact
pub struct SkyboxResources {
    dheap: D3D12DescriptorHeap,
}

pub struct Skybox {
    root_sig: D3D12RootSignature,
    pso: D3D12PipelineState,
    skytex: D3D12Resource,
}

impl Skybox {
    pub fn new(core: &DXCore, downsampler: &Downsampler) -> HResult<Skybox> {
        let (tex, srv_desc) = try!(load_skybox_texture(core, downsampler));

        unimplemented!()
    }
}

fn load_skybox_texture(core: &DXCore, downsampler: &Downsampler) -> HResult<(D3D12Resource, D3D12_SHADER_RESOURCE_VIEW_DESC)> {
    let f = try!(fs::File::open("assets/skybox.hdr").or(fs::File::open("skybox.hdr"))
            .map_err(|_|{ error!("'skybox.hdr' is not found"); ERROR_FILE_NOT_FOUND as HRESULT }));
    let reader = io::BufReader::new(f);
    let decoder = try!(hdr::HDRDecoder::new(reader)
            .map_err(|err|{ error!("Cannot parse 'skybox.hdr': {}", err); ERROR_INVALID_DATA as HRESULT }));
    let meta = decoder.metadata();
    let data = try!(decoder.read_image_native()
            .map_err(|err|{ error!("Cannot read 'skybox.hdr': {}", err); ERROR_INVALID_DATA as HRESULT }));

    let rgbe8_tex_desc = resource_desc_tex2d_nomip(
                                    meta.width as u64,
                                    meta.height as u32,
                                    DXGI_FORMAT_R32_UINT,
                                    D3D12_RESOURCE_FLAG_NONE);

    let rgbe8_tex = try!(core.dev.create_committed_resource(
                                    &heap_properties_default(),
                                    D3D12_HEAP_FLAG_NONE,
                                    &rgbe8_tex_desc,
                                    D3D12_RESOURCE_STATE_COMMON,
                                    None));

    let rgbe8_srv_desc = srv_tex2d_default_slice_mip(rgbe8_tex_desc.Format, 0, 1); 
    
    try!(utils::upload_into_texture(core, &rgbe8_tex, meta.width, meta.height, &data[..]));

    let mipcnt = 8;
    let tex_desc = D3D12_RESOURCE_DESC {
        MipLevels: mipcnt,
        ..resource_desc_tex2d_nomip(meta.width as u64,
                                    meta.height as u32,
                                    DXGI_FORMAT_R16G16B16A16_FLOAT,
                                    D3D12_RESOURCE_FLAG_NONE)
    };

    let tex = try!(core.dev.create_committed_resource(
                                    &heap_properties_default(),
                                    D3D12_HEAP_FLAG_NONE,
                                    &tex_desc,
                                    D3D12_RESOURCE_STATE_COMMON,
                                    None));

    let tex_uav_desc = uav_tex2d_desc(tex_desc.Format, 0, 0);
    try!(convert_to_rgb(core, &rgbe8_tex, &rgbe8_srv_desc, &tex, &tex_uav_desc));

    let tex_srv_desc = srv_tex2d_default_slice_mip(tex_desc.Format, 0, mipcnt as u32);
    try!(downsampler.generate_mips(&tex, core));
    
    Ok((tex, tex_srv_desc))
}

fn convert_to_rgb(core: &DXCore, src: &D3D12Resource, src_srv_desc: &D3D12_SHADER_RESOURCE_VIEW_DESC, 
                    dst: &D3D12Resource, dst_uav_desc: &D3D12_UNORDERED_ACCESS_VIEW_DESC) -> HResult<()> {
    // One time conversion, so I don't keep anything
    let mut root_sig_bc = vec![];
    let mut convert_bc = vec![];

    try!(cdev::compile_shaders("skybox.hlsl", 
            &mut[
                ("TCRS", "rootsig_1_0", &mut root_sig_bc),
                ("CSTexConvert", "cs_5_1", &mut convert_bc),
            ], D3DCOMPILE_OPTIMIZATION_LEVEL3)
        .map_err(|err|{ error!("{}", err) ; DXGI_ERROR_INVALID_CALL }));

    let root_sig = try!(core.dev.create_root_signature(0, &root_sig_bc[..]));

    let convert_pso = try!(create_default_cpso(&core.dev, &root_sig, convert_bc));

    let dheap = try!(DescriptorHeap::new(&core.dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));

    core.dev.create_shader_resource_view(Some(src), Some(src_srv_desc), dheap.cpu_handle(0));
    core.dev.create_unordered_access_view(Some(dst), None, Some(dst_uav_desc), dheap.cpu_handle(1));

    let calloc = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
    let clist: D3D12GraphicsCommandList =
        try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COMPUTE, &calloc, Some(&convert_pso)));
    
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

    clist.resource_barrier(&[
        *ResourceBarrier::transition(src,
        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
        *ResourceBarrier::transition(dst,
        D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COMMON),
    ]);

    try!(clist.close());

    core.compute_queue.execute_command_lists(&[&clist]);

    let fence = try!(core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE));

    utils::wait_for_compute_queue(core, &fence, &core::create_event());

    Ok(())
}
