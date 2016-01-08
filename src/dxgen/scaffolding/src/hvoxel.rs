use std::fmt;
use std::mem;
use std::ptr;
use winapi::*;
use dx_safe::*;
use core::*;
use utils::*;
use cgmath::*;
use structwrappers::*;
use window::*;
use rand;
use crossbeam;
use create_device::*;
use std::cmp::{min, max};

use camera::*;
use std::sync::atomic::Ordering;

dx_vertex!( Vertex {
  (POSITION, 0, DXGI_FORMAT_R32G32B32_FLOAT) pos: [f32;3],
  (COLOR   , 0, DXGI_FORMAT_R32G32B32_FLOAT) c: [f32;3],
  (PSIZE   , 0, DXGI_FORMAT_R32_FLOAT      ) r: f32,
});

const SCN: usize = 4;
enum SparseCell3dN<T> {
    Homogenous(T),
    Detailed(Box<[[[SparseCell3dN<T>; SCN]; SCN]; SCN]>),
}

enum CellType {
    Air,
    Earth(u32),
}

pub struct AppData {
    core: DXCore,
    schain: DXSwapChain,
    ds_buf: D3D12Resource,
    tex: D3D12Resource,
    dsv_heap: D3D12DescriptorHeap,
    srv_heap: D3D12DescriptorHeap,
    w: u32,
    h: u32,
    camera: Camera,
    calloc: D3D12CommandAllocator,
    clist: D3D12GraphicsCommandList,
}

pub fn on_init<T: Parameters>(wnd: &Window,
                              adapter: Option<&DXGIAdapter1>,
                              frame_count: u32,
                              parameters: &T)
                              -> Result<AppData, String> {
    let hwnd = wnd.get_hwnd();
    let (w, h) = wnd.size();

    // core::create_core creates structure that contains D3D12Device, command queues and so on
    // which are required for creation of all other objects
    let core = try!(create_core(adapter, D3D_FEATURE_LEVEL_11_0, cfg!(debug))
                        .map_err(|err| format!("Cannot create DXCore: {}", err)));

    let sc_desc = DXGI_SWAP_CHAIN_DESC1 {
        Width: w,
        Height: h,
        Format: DXGI_FORMAT_R8G8B8A8_UNORM, /* note that create_swap_chain fails when Format is DXGI_FORMAT_R8G8B8A8_UNORM_SRGB */
        Stereo: 0,
        SampleDesc: sample_desc_default(),
        BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
        BufferCount: frame_count,
        Scaling: DXGI_SCALING_NONE,
        SwapEffect: DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
        AlphaMode: DXGI_ALPHA_MODE_UNSPECIFIED,
        Flags: 0, // DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH.0,
    };

    // so I pass DXGI_FORMAT_R8G8B8A8_UNORM_SRGB separately for use in render target view
    let swap_chain = try!(create_swap_chain(&core,
                                            &sc_desc,
                                            DXGI_FORMAT_R8G8B8A8_UNORM_SRGB,
                                            hwnd,
                                            Some(&fsd),
                                            None)
                              .map_err(|msg| format!("{}", msg)));

    // Descriptor heaps store information that GPU needs to access resources like textures, constant buffers, unordered access views
    // Root signature binds descriptor heap entries to shader registers.
    // This particular descriptor heap will hold information about a texture.
    let srv_hd = D3D12_DESCRIPTOR_HEAP_DESC {
        NumDescriptors: 1,
        Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
        Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
        NodeMask: 0,
    };
    let srv_heap = try!(core.dev
                            .create_descriptor_heap(&srv_hd)
                            .map_err(|hr| format!("SRV descriptor heap creation error {}", hr)));

    // This heap will contain an entry for depth-stencil resource
    let dsv_hd = D3D12_DESCRIPTOR_HEAP_DESC {
        NumDescriptors: 1,
        Type: D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
        Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
        NodeMask: 0,
    };
    let dsv_heap = try!(core.dev
                            .create_descriptor_heap(&dsv_hd)
                            .map_err(|hr| format!("DSV descriptor heap creation error {}", hr)));

    // Command allocator manages storage for command lists.
    // TODO: remove this one, I don't use it
    let callocator = try!(core.dev
                              .create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT)
                              .map_err(|hr| format!("Command allocator creation error {}", hr)));

    let compile_flags = 0;
    debug!("Vertex shader");
    // shaders.hlsl should be in process current directory
    // vshader and pshader are Vec<u8> containing shader byte-code
    let vshader = d3d_compile_from_file("shaders.hlsl", "VSMain", "vs_5_0", compile_flags).unwrap();
    debug!("Pixel shader");
    let pshader = d3d_compile_from_file("shaders.hlsl", "PSMain", "ps_5_0", compile_flags).unwrap();

    let tex_drange = D3D12_DESCRIPTOR_RANGE {
        // Bind as shader resource view (shader register 't')
        RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
        // One descriptor in range
        NumDescriptors: 1,
        // Bind as 't0'
        BaseShaderRegister: 0,
        // Shader model 5.0 adds register spaces, e.g. this texture can be refered to as 'Texture2D<float4> tex: register(t0,  space0)'
        RegisterSpace: 0,
        // First descriptor in a descriptors heap
        // You can put D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND here to compute offsets automatically.
        OffsetInDescriptorsFromTableStart: 0,
    };


    // Root parameters:
    let rs_parms = vec![
    //   Slot 0: register(t0, space0)
    //     Attach resource to this slot by calling
    //       D3D12GraphicsCommandList::set_graphics_root_descriptor_table(slot_number, gpu_virtual_address_of_first_descriptor)
    //     Note, that firstly you need to attach descriptor heaps to command list
    //        D3D12GraphicsCommandList::set_descriptor_heaps(heaps: &[&D3D12DescriptorHeap])
    *RootParameter::descriptor_table(&[tex_drange], D3D12_SHADER_VISIBILITY_PIXEL),
    //   Slot 1: register(c0, space0), contains constants (uniforms in GL terminology)
    //     Call D3D12GraphicsCommandList::set_graphics_root_constant_buffer_view(slot_number, gpu_virtual_address_of_constants)
    //     to bind data to this slot. You don't need descriptor heaps to do that.
    *RootParameter::cbv(0, 0, D3D12_SHADER_VISIBILITY_ALL),
  ];
    // D3D12_ROOT_PARAMETER structure have different sizes for 32-bit and 64-bit code.
    // I had a fun time adapting wrapper generator to this class of cases.
    debug!("Size of D3D12_ROOT_PARAMETER:{}",
           ::std::mem::size_of_val(&rs_parms[0]));

    let static_samplers = vec![static_sampler_anisotropic_default()];

    // It is the place where all input slots are bound to shader registers.
    // except for vertex ('v' registers) and index buffers, which do not require binding
    // TODO: make a wrapper
    let rsd = D3D12_ROOT_SIGNATURE_DESC {
        NumParameters: rs_parms.len() as UINT,
        pParameters: rs_parms[..].as_ptr(),
        NumStaticSamplers: static_samplers.len() as u32,
        pStaticSamplers: static_samplers.as_ptr(),
        Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
    };

    debug!("Serialize root signature");
    // Root signature can be embedded into HLSL. You can extract blob from shader text.
    // https://msdn.microsoft.com/en-us/library/windows/desktop/dn913202(v=vs.85).aspx
    let blob = try!(d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1)
                        .map_err(|_| "Serialize root signature error"));

    debug!("Root signature");
    // It creates D3D12RoosSignature
    let root_sign = try!(core.dev
                             .create_root_signature(0, blob_as_slice(&blob))
                             .map_err(|hr| format!("Root signature creation error {}", hr)));

    // Graphics pipeline state (GPS) contains settings for all stages in graphics pipeline.
    // Also, it containt root signature that binds descriptor heaps to shader registers
    // Shaders are part of GPS too. D3D12Device::create_graphics_pipeline_state() performs compilation of provided shader bytecode.
    // MSDN suggests to create GPS on separate thread, as this is CPU intensive operation.
    let gps = try!(create_static_sampler_gps::<Vertex>(&core, &[], &[], &root_sign)
                       .map_err(|hr| format!("Graphics pipeline state creation error {}", hr)));

    // Command lists hold the list of commands to be executed by GPU. D3D12CommanQueue::execute_command_lists() initiates actual execution.
    debug!("Command list");
    let clist: D3D12GraphicsCommandList = core.dev
                                              .create_command_list(0,
                                                                   D3D12_COMMAND_LIST_TYPE_DIRECT,
                                                                   &callocator,
                                                                   Some(&gps))
                                              .unwrap();

    // ------------------- Vertex and index buffer resource init begin ----------------------------

    // get vertices and indices for cube
    let (vtc, idx) = shape_gen::cube_indexed::<Vertex>(0.5);

    // Size in bytes of array of vertices
    let vbuf_size = mem::size_of_val(&vtc[..]);

    // Vertex buffer will be created in upload head,
    // Using D3D12Resource::map we can get CPU write access to this heap.
    let heap_prop = heap_properties_upload();

    // The only thing DX12 needs to create Buffer Resource is its length
    let res_desc = resource_desc_buffer(vbuf_size as u64);
    // info!("Resource desc: {:#?}", &res_desc);

    // Create resource in upload heap with initial state GENERIC_READ, without optimized clear value.
    // create_committed_resource() creates implicit heap large enough to contain resource.
    // GENERIC_READ here is GPU side. It means that all shaders can read the resource and
    // resource can be source of copy operation.
    debug!("Vertex buffer");
    let vbuf = try!(core.dev
                        .create_committed_resource(&heap_prop,
                                                   D3D12_HEAP_FLAG_NONE,
                                                   &res_desc,
                                                   D3D12_RESOURCE_STATE_GENERIC_READ,
                                                   None)
                        .map_err(|_| core.info_queue.clone()));

    // Transfer data from vtc into GPU memory allocated for vbuf
    try!(upload_into_buffer(&vbuf, &vtc[..]).map_err(|_| core.info_queue.clone()));

    // Unlike shader resource views or constant buffer views,
    // vertex buffer views don't go into descriptor heaps.
    // D3D12GraphicsCommandList::ia_set_vertex_buffers() binds them directly.
    let vbview = D3D12_VERTEX_BUFFER_VIEW {
        BufferLocation: vbuf.get_gpu_virtual_address(),
        StrideInBytes: mem::size_of::<Vertex>() as u32,
        SizeInBytes: vbuf_size as u32,
    };

    // Creation and filling of index buffer is similar to that of vertex buffer.
    let ibuf_size = mem::size_of_val(&idx[..]);
    let idesc = resource_desc_buffer(ibuf_size as u64);
    debug!("Index buffer");
    let ibuf = try!(core.dev
                        .create_committed_resource(&heap_prop,
                                                   D3D12_HEAP_FLAG_NONE,
                                                   &idesc,
                                                   D3D12_RESOURCE_STATE_GENERIC_READ,
                                                   None)
                        .map_err(|_| core.info_queue.clone()));
    try!(upload_into_buffer(&ibuf, &idx[..]).map_err(|_| core.info_queue.clone()));

    // D3D12GraphicsCommandList::ia_set_index_buffer() binds index buffer to graphics pipeline.
    let i_view = D3D12_INDEX_BUFFER_VIEW {
        BufferLocation: ibuf.get_gpu_virtual_address(),
        SizeInBytes: ibuf_size as UINT,
        Format: DXGI_FORMAT_R32_UINT,
    };

    // ------------------- Vertex and index buffer resource init end ----------------------------

    // ------------------- Constant buffer resource init begin ----------------------------
    let world_matrix: [[f32; 4]; 4] = [[1., 0., 0., 0.],
                                       [0., 1., 0., 0.],
                                       [0., 0., 1., 0.],
                                       [0., 0., 0., 1.]];
    let view_matrix: [[f32; 4]; 4] = [[1., 0., 0., 0.],
                                      [0., 1., 0., 0.],
                                      [0., 0., 1., 0.],
                                      [0., 0., 0., 1.]];
    let proj_matrix: [[f32; 4]; 4] = [[1., 0., 0., 0.],
                                      [0., 1., 0., 0.],
                                      [0., 0., 1., 0.],
                                      [0., 0., 0., 1.]];
    let cbuf_data = Constants {
        model: world_matrix,
        view: view_matrix,
        proj: proj_matrix,
        n_model: world_matrix,
        light_pos: [-1., 0., 0.],
    };
    let cbsize = mem::size_of_val(&cbuf_data);

    // Resource creation for constant buffer is the same as resource creation for vertex buffer
    let cbuf = core.dev
                   .create_committed_resource(&heap_properties_upload(),
                                              D3D12_HEAP_FLAG_NONE,
                                              &resource_desc_buffer(cbsize as u64),
                                              D3D12_RESOURCE_STATE_GENERIC_READ,
                                              None)
                   .unwrap();
    try!(upload_into_buffer(&cbuf, &[cbuf_data]).map_err(|_| core.info_queue.clone()));

    // But constant buffer view goes into descriptor heap.
    let cbview = D3D12_CONSTANT_BUFFER_VIEW_DESC {
        BufferLocation: cbuf.get_gpu_virtual_address(),
        SizeInBytes: ((cbsize + 255) & !255) as u32, /* size of constants buffer in bytes should be divisible by 256. */
    };

    // Create descriptor heap to hold one constant buffer view
    let cbv_hd = D3D12_DESCRIPTOR_HEAP_DESC {
        NumDescriptors: 1,
        Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
        Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
        NodeMask: 0,
    };
    debug!("CBV descriptor heap");
    let cbvd_heap = core.dev.create_descriptor_heap(&cbv_hd).unwrap();

    // CPU side of descriptor heap is an array of unspecified structures of cbv_dsize size.
    let _cbv_dsize = core.dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
    // const_dh receives pointer to the start of descriptors array
    let const_dh = cbvd_heap.get_cpu_descriptor_handle_for_heap_start();
    // create_constant_buffer_view creates hardware specific representation of constant buffer view at const_dh address.
    // In this case const_dh corresponds to first entry in cbvd_heap.
    debug!("Create shader resource view: constants buffer");
    core.dev.create_constant_buffer_view(Some(&cbview), const_dh);

    // ------------------- Constant buffer resource init end  -----------------------------

    // ------------------- Texture resource init begin  -----------------------------

    let tex_w = 256usize;
    let tex_h = 256usize;

    // I create resource with 4 mip levels, but fill only one yet.
    let tex_desc = D3D12_RESOURCE_DESC {
        MipLevels: 4,
        ..resource_desc_tex2d_nomip(tex_w as u64,
                                    tex_h as u32,
                                    DXGI_FORMAT_R8G8B8A8_UNORM,
                                    D3D12_RESOURCE_FLAG_NONE)
    };

    // I create texture in default heap, CPU can't get access to this heap.
    // I set initial state of the resource to COPY_DEST,
    // to prepare it for data transfer thru intermediate resource placed in upload heap
    debug!("Texture resource");
    let tex_resource = try!(core.dev
                                .create_committed_resource(&heap_properties_default(),
                                                           D3D12_HEAP_FLAG_NONE,
                                                           &tex_desc,
                                                           D3D12_RESOURCE_STATE_COPY_DEST,
                                                           None)
                                .map_err(|_| core.info_queue.clone()));

    // Create pattern to set as texture data
    let mut texdata: Vec<u32> = vec![0xff00ffff; tex_w*tex_h as usize];
    let mut i = 0;
    for v in &mut texdata[..] {
        let (x, y) = (i % tex_w, i / tex_w);
        *v = if x % 10 == 0 || y % 10 == 0 {
            0xff204f4f // yellowish
        } else {
            0xff909090 // light grey
        };
        i += 1;
    }

    // utils::upload_into_texture() transfers texdata into upload buffer resource,
    // then executed copy command on core.copy_queue
    try!(upload_into_texture(&core, &tex_resource, tex_w, tex_h, &texdata[..])
             .map_err(|_| core.info_queue.clone()));
    // upload_into_texture transition tex_resource into common state (it uses copy queue, so it can't set pixel_shader_resource state)
    command_list.resource_barrier(&mut [*ResourceBarrier::transition(&tex_resource, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE)]);

    // create shader resource view in srv_heap descriptor heap
    let srv_desc = shader_resource_view_tex2d_default_mip(DXGI_FORMAT_R8G8B8A8_UNORM, 4);

    let srv_dh = srv_heap.get_cpu_descriptor_handle_for_heap_start();
    debug!("Create shader resource view: texture");
    core.dev.create_shader_resource_view(Some(&tex_resource), Some(&srv_desc), srv_dh);

    // ------------------- Texture resource init end  -----------------------------

    // ------------------- Depth stencil buffer init begin ------------------------

    // utils::create_depth_stencil() functions handles creation of depth-stencil resource
    // and depth-stencil view, and it places depth-stencil view into dsd_heap descriptor heap
    let ds_format = DXGI_FORMAT_D32_FLOAT;
    let ds_res = try!(create_depth_stencil(w as u64, h as u32, ds_format, &core.dev, &dsd_heap, 0)
                          .map_err(|_| core.info_queue.clone()));

    // ------------------- Depth stencil buffer init end --------------------------

    // Fences synchronize CPU and GPU.
    // You can place a fence in the GPU's command queue by calling D3D12CommandQueue::signal().
    // When GPU reaches the fence, it notifies CPU.
    // If you want GPU to wait for signal, call D3D12CommandQueue::wait(),
    // then tell GPU to continue by calling D3D12CommandQueue::signal().
    debug!("fence");
    let fence = core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE).unwrap();

    // fence_event communicates GPU's notification to CPU.
    // Use fence.set_event_on_completion(fence_value, fence_event) to bind fence to fence_event
    // Use utils::wait_for_single_object() to wait until GPU reaches marker set by D3D12CommandQueue::signal().
    // fence_value identifies particular call to D3D12CommandQueue::signal()
    debug!("fence_event");
    let fence_event = create_event();


    // Pack all created objects into AppData
    let mut ret = AppData {
        core: core,
        schain: swap_chain,
        w: w,
        h: h,
        root_signature: root_sign,
        srv_heap: srv_heap,
        dsv_heap: dsd_heap,
        pipeline_state: gps,
        _vertex_buffer: vbuf,
        vertex_buffer_view: vbview,
        _index_buffer: ibuf,
        index_buffer_view: i_view,
        _constant_buffer: cbuf,
        _tex_resource: tex_resource,
        ds_res: ds_res,
        frame_index: 0,
        fence_event: fence_event,
        fence: fence,
        tick: 0.0,
        minimized: false,
        camera: Camera::new(),
    };

    ret.camera.go(-3., 0., 0.);
    ret.camera.aspect = (w as f32) / (h as f32);

    // TODO: remove
    debug!("Command list Close");
    command_list.close().unwrap();
    debug!("Command queue execute");
    ret.core.graphics_queue.execute_command_lists(&[&command_list]);
    debug!("wait_for_prev_frame");
    wait_for_graphics_queue(&ret.core, &ret.fence, &ret.fence_event);

    // Done
    Ok(ret)
}
