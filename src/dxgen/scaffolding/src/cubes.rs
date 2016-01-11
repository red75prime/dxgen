use std::fmt;
use std::mem;
use std::ptr;
use winapi::*;
use dx_safe::*;
use core;
use core::{DXCore, DXSwapChain, Event};
use utils::*;
use cgmath::*;
use structwrappers::*;
use window::*;
use crossbeam;
use create_device::*;

use camera::*;
use shape_gen;
use shape_gen::GenVertex;

use downsampler::Downsampler;
use tonemapper::{Tonemapper, TonemapperResources};
use plshadow::PLShadow;

use std::io::prelude::*;
use std::fs::File;
use std::str;
use std::slice;
use std::sync::Arc;
use cubestate::{State, StateUpdateAgent};

// dx_vertex! macro implement dxsems::VertexFormat trait for given structure
// VertexFormat::generate(&self, register_space: u32) -> Vec<D3D12_INPUT_ELEMENT_DESC>
// There's no #[repr(C)], because dx_vertex takes care of measuring field's offsets
dx_vertex!( Vertex {
  (POSITION, 0, DXGI_FORMAT_R32G32B32_FLOAT) pos: [f32;3],
  (COLOR   , 0, DXGI_FORMAT_R32G32B32_FLOAT) color: [f32;3],
  (TEXCOORD, 0, DXGI_FORMAT_R32G32_FLOAT   ) texc0: [f32;2],
  (NORMAL  , 0, DXGI_FORMAT_R32G32B32_FLOAT) norm: [f32;3],
});

// implementing shape_gen::GenVertex for Vertex allows its use in shape_gen's functions
impl GenVertex for Vertex {
    fn new_vertex(p: Vector3<f32>) -> Vertex {
        Vertex {
            pos: [p.x, p.y, p.z],
            color: [1., 0.5, 0.5],
            texc0: [0., 0.],
            norm: [1., 0., 0.],
        }
    }

    fn set_uv(self, u: f32, v: f32) -> Vertex {
        Vertex { texc0: [u, v], ..self }
    }

    fn set_normal(self, n: Vector3<f32>) -> Vertex {
        Vertex { norm: [n.x, n.y, n.z], ..self }
    }
}

type M4 = [[f32; 4]; 4];

// This struct contains data to be passed into shader,
// thus its memory layout should be defined. repr(C) does that.
#[repr(C)]
#[derive(Clone)]
struct Constants {
    view: M4,
    proj: M4,
    eye_pos: [f32; 3],
    // HLSL shader constants have some alignment rules. I need padding here
    // TODO: Use shader reflection API
    _padding1: f32,
    light_pos: [f32; 3],
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct InstanceData {
    world: [[f32; 4]; 4],
    n_world: [[f32; 3]; 3],
}

#[derive(Clone, Copy, Debug)]
pub struct CubeParms {
    pub object_count: u32,
    pub thread_count: u32,
    pub speed_mult: f32,
    pub concurrent_state_update: bool,
}

static CLEAR_COLOR: [f32; 4] = [0.01, 0.01, 0.15, 1.0];

pub fn create_hdr_render_target
    (dev: &D3D12Device,
     w: u32,
     h: u32)
     -> HResult<(D3D12Resource, DescriptorHeap, TonemapperResources)> {
    trace!("DescriptorHeap::new");
    let dheap = try!(DescriptorHeap::new(dev, 1, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, false, 0));

    let hdr_format = DXGI_FORMAT_R32G32B32A32_FLOAT;

    let tex_desc = resource_desc_tex2d_nomip(w as u64,
                                             h as u32,
                                             hdr_format,
                                             D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET);

    trace!("create_commited_resource");
    let render_target =
        try!(dev.create_committed_resource(&heap_properties_default(),
                                           D3D12_HEAP_FLAG_NONE,
                                           &tex_desc,
                                           D3D12_RESOURCE_STATE_COMMON,
                                           Some(&rt_rgba_f32_clear_value(CLEAR_COLOR))));
    trace!("create_render_target_view");
    dev.create_render_target_view(Some(&render_target),
                                  Some(&render_target_view_desc_tex2d_default(hdr_format)),
                                  dheap.cpu_handle(0));

    trace!("Create tonemapper_resource");
    let tonemapper_resource = try!(TonemapperResources::new(dev,
                                                            w,
                                                            h,
                                                            hdr_format,
                                                            DXGI_FORMAT_R8G8B8A8_UNORM));

    trace!("create_hdr_render_target done");
    Ok((render_target, dheap, tonemapper_resource))
}

// This is not for encapsulation. This is for logical grouping.
struct CommonResources {
    pipeline_state: D3D12PipelineState,
    root_signature: D3D12RootSignature,
    srv_heap: DescriptorHeap,
    // rustc thinks that _vertex_buffer is never user, but it holds corresponding resource alive,
    // while vertex_buffer_view keeps GPU virtual address of the buffer.
    // underscore prevents warning
    // TODO: bundle vertex buffer and vertex buffer view into one structure?
    _vertex_buffer: D3D12Resource,
    vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
    // same as for _vertex_buffer
    _index_buffer: D3D12Resource,
    index_buffer_view: D3D12_INDEX_BUFFER_VIEW,
    index_count: u32,
    tex_resource: D3D12Resource,
    downsampler: Downsampler,
    tonemapper: Tonemapper,
    plshadow: PLShadow<Vertex>,
    thread_cnt: u32,
}

impl CommonResources {
    fn new(dev: &D3D12Device, tex_w: u32, tex_h: u32, thread_cnt: u32) -> HResult<CommonResources> {
        trace!("Open 'shaders.hlsl'");
        let mut f = try!(File::open("shaders.hlsl").map_err(|_| DXGI_ERROR_NOT_FOUND));
        let mut content = vec![];
        trace!("Read 'shaders.hlsl'");
        try!(f.read_to_end(&mut content).map_err(|_| DXGI_ERROR_NOT_FOUND));
        trace!("Try interpret 'shaders.hlsl' content as UTF-8");
        let shader = try!(str::from_utf8(&content[..]).map_err(|_| DXGI_ERROR_INVALID_CALL));

        // shaders.hlsl should be in process current directory
        // vshader_bc and pshader_bc are Vec<u8> containing shader byte-code
        trace!("Vertex shader compilation");
        let vshader_bc = try!(d3d_compile_from_str(shader, "VSMain", "vs_5_0", 0).map_err(|err| {
            error!("Vertex shader compilation error: {}", err);
            E_FAIL
        }));
        trace!("Pixel shader compilation");
        let pshader_bc = try!(d3d_compile_from_str(shader, "PSMain", "ps_5_0", 0).map_err(|err| {
            error!("Pixel shader compilation error: {}", err);
            E_FAIL
        }));
        trace!("Root signature compilation");
        let root_signature_bc = try!(d3d_compile_from_str(shader, "RSD", "rootsig_1_0", 0)
                                         .map_err(|err| {
                                             error!("Root signature compilation error: {}", err);
                                             E_FAIL
                                         }));

        trace!("Root signature creation");
        let root_signature = try!(dev.create_root_signature(0, &root_signature_bc[..]));

        // Here was initialization of root signature, but I replaced it with root signature embedded into shader source.
        // There also were comments about resource bindings. Please, refer to https://github.com/red75prime/dxgen/blob/17cb52dcb4ca8fdd1dbe0e3993d6da47c195a2e2/src/dxgen/scaffolding/src/cubes.rs#L370

        // Graphics pipeline state (GPS) contains settings for all stages in graphics pipeline.
        // Also, it containt root signature that binds descriptor heaps to shader registers
        // Shaders are part of GPS too. D3D12Device::create_graphics_pipeline_state() performs compilation of provided shader bytecode.
        // MSDN suggests to create GPS on separate thread, as this is CPU intensive operation.
        trace!("Graphics pipeline state");
        let pipeline_state = try!(create_static_sampler_gps::<Vertex>(dev,
                                                                      &vshader_bc[..],
                                                                      &pshader_bc[..],
                                                                      &root_signature));

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
        trace!("Create vertex buffer");
        let vertex_buffer = try!(dev.create_committed_resource(&heap_prop,
                                                               D3D12_HEAP_FLAG_NONE,
                                                               &res_desc,
                                                               D3D12_RESOURCE_STATE_GENERIC_READ,
                                                               None));

        // Transfer data from vtc into GPU memory allocated for vbuf
        trace!("Upload vertices");
        try!(upload_into_buffer(&vertex_buffer, &vtc[..]));

        // Unlike shader resource views or constant buffer views,
        // vertex buffer views don't go into descriptor heaps.
        // D3D12GraphicsCommandList::ia_set_vertex_buffers() binds them directly.
        let vertex_buffer_view = D3D12_VERTEX_BUFFER_VIEW {
            BufferLocation: vertex_buffer.get_gpu_virtual_address(),
            StrideInBytes: mem::size_of::<Vertex>() as u32,
            SizeInBytes: vbuf_size as u32,
        };

        // Creation and filling of index buffer is similar to that of vertex buffer.
        let ibuf_size = mem::size_of_val(&idx[..]);
        let idesc = resource_desc_buffer(ibuf_size as u64);
        trace!("Create index buffer");
        let index_buffer = try!(dev.create_committed_resource(&heap_prop,
                                                              D3D12_HEAP_FLAG_NONE,
                                                              &idesc,
                                                              D3D12_RESOURCE_STATE_GENERIC_READ,
                                                              None));
        trace!("Upload indices");
        try!(upload_into_buffer(&index_buffer, &idx[..]));

        // D3D12GraphicsCommandList::ia_set_index_buffer() binds index buffer to graphics pipeline.
        let index_buffer_view = D3D12_INDEX_BUFFER_VIEW {
            BufferLocation: index_buffer.get_gpu_virtual_address(),
            SizeInBytes: ibuf_size as UINT,
            Format: DXGI_FORMAT_R32_UINT,
        };

        // ------------------- Vertex and index buffer resource init end ----------------------------

        // ------------------- Texture resource init begin  -----------------------------

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
        trace!("Create texture resource");
        let tex_resource = try!(dev.create_committed_resource(&heap_properties_default(),
                                                                   D3D12_HEAP_FLAG_NONE,
                                                                   &tex_desc,
                                                                   D3D12_RESOURCE_STATE_COMMON,
                                                                   None));

        let srv_heap = try!(DescriptorHeap::new(dev, 1, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0));
        let srv_desc = srv_tex2d_default_slice_mip(tex_desc.Format, 0, 4);
        dev.create_shader_resource_view(Some(&tex_resource), Some(&srv_desc), srv_heap.cpu_handle(0));

        // ------------------- Texture resource init end  -----------------------------
        trace!("Downsampler::new");
        let downsampler = Downsampler::new(dev);

        trace!("Create tonemapper");
        let tonemapper = Tonemapper::new(dev);

        trace!("Create plshadow");
        let plshadow = try!(PLShadow::<Vertex>::new(dev));


        Ok(CommonResources {
            pipeline_state: pipeline_state,
            root_signature: root_signature,
            srv_heap: srv_heap,
            // rustc thinks that _vertex_buffer is never user, but it holds corresponding resource alive,
            // while vertex_buffer_view keeps GPU virtual address of the buffer.
            // underscore prevents warning
            // TODO: bundle vertex buffer and vertex buffer view into one structure?
            //
            _vertex_buffer: vertex_buffer,
            vertex_buffer_view: vertex_buffer_view,
            // same as for _vertex_buffer
            _index_buffer: index_buffer,
            index_buffer_view: index_buffer_view,
            index_count: idx.len() as u32,
            tex_resource: tex_resource,
            downsampler: downsampler,
            tonemapper: tonemapper,
            plshadow: plshadow,
            thread_cnt: thread_cnt,
        })
    }
}

struct FrameResources {
    viewport: D3D12_VIEWPORT,
    sci_rect: D3D12_RECT,
    calloc: D3D12CommandAllocator,
    glist: D3D12GraphicsCommandList,
    fence: Fence,
    tm_fence: D3D12Fence, // for tonemapper sync
    i_buffer: D3D12Resource, // instances data
    // I don't know easy way of saying "this borrow lifetime equals lifetime of the structure"
    // I can propagate lifetime parameter all the way up. Later.
    // TODO: replace 'static
    instance_data_mapped: &'static mut [InstanceData],
    gpu_i_ptr: u64,
    c_buffer: D3D12Resource, // constants data
    // TODO: replace 'static
    constants_mapped: &'static mut Constants,
    gpu_c_ptr: u64,
    hdr_render_target: D3D12Resource,
    hdr_rtv_heap: DescriptorHeap,
    _depth_stencil: D3D12Resource,
    dsd_heap: DescriptorHeap,
    tonemapper_resources: TonemapperResources,
    object_cnt: u32,
}

impl FrameResources {
    // Creates frame resources for rendering onto back-buffer of given dimensions and format
    fn new(dev: &D3D12Device,
               w: u32,
               h: u32,
               format: DXGI_FORMAT,
               object_cnt: u32)
               -> HResult<FrameResources> {
        let object_cnt = object_cnt as usize;
        trace!("FrameResources::new()");
        trace!("Create calloc");
        let calloc = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
        trace!("Create glist");
        let glist: D3D12GraphicsCommandList =
            try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &calloc, None));
        try!(glist.close());
        trace!("Create fence");
        let fence = Fence::new(try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE)));
        trace!("Create tm_fence");
        let tm_fence = try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));

        let i_buffer_size = mem::size_of::<InstanceData>() * object_cnt;
        trace!("Create i_buffer");
        let i_buffer = try!(dev.create_committed_resource(&heap_properties_upload(),
                                                          D3D12_HEAP_FLAG_NONE,
                                                          &resource_desc_buffer(i_buffer_size as u64),
                                                          D3D12_RESOURCE_STATE_GENERIC_READ,
                                                          None));
        try!(i_buffer.set_name("Instance data buffer".into()));

        // Indicate that CPU doesn't read from buffer
        let read_range = D3D12_RANGE { Begin: 0, End: 0 };
        trace!("Map i_buffer");
        let cpu_i_ptr = unsafe {
            let mut ptr: *mut u8 = ptr::null_mut();
            try!(i_buffer.map(0, Some(&read_range), Some(&mut ptr)));
            slice::from_raw_parts_mut(ptr as *mut InstanceData, object_cnt)
        };
        let gpu_i_ptr = i_buffer.get_gpu_virtual_address();


        let c_buffer_size = mem::size_of::<Constants>();
        trace!("Create c_buffer");
        let c_buffer = try!(dev.create_committed_resource(&heap_properties_upload(),
                                                          D3D12_HEAP_FLAG_NONE,
                                                          &resource_desc_buffer(c_buffer_size as u64),
                                                          D3D12_RESOURCE_STATE_GENERIC_READ,
                                                          None));
        try!(c_buffer.set_name("Constants buffer".into()));

        trace!("Map c_buffer");
        let cpu_c_ptr = unsafe {
            let mut ptr: *mut u8 = ptr::null_mut();
            try!(c_buffer.map(0, Some(&read_range), Some(&mut ptr)));
            &mut *(ptr as *mut Constants)
        };
        let gpu_c_ptr = c_buffer.get_gpu_virtual_address();

        trace!("Create depth stencil descriptor heap");
        let dsd_heap = try!(DescriptorHeap::new(dev, 1, D3D12_DESCRIPTOR_HEAP_TYPE_DSV, false, 0));

        let ds_format = DXGI_FORMAT_D32_FLOAT;
        // utils::create_depth_stencil() functions handles creation of depth-stencil resource
        // and depth-stencil view, and it places depth-stencil view into dsd_heap descriptor heap
        trace!("Create depth stencil");
        let depth_stencil = try!(create_depth_stencil(dev,
                                                      w as u64,
                                                      h as u32,
                                                      ds_format,
                                                      dsd_heap.cpu_handle(0)));
        trace!("Create HDR render target");
        let (hdr_render_target, hdr_rtv_heap, tonemapper_resources) =
            try!(create_hdr_render_target(dev, w, h));

        let viewport = D3D12_VIEWPORT {
            TopLeftX: 0.,
            TopLeftY: 0.,
            MinDepth: 0.,
            Width: w as f32,
            Height: h as f32,
            MaxDepth: 1.0,
        };
        let sci_rect = D3D12_RECT {
            right: w as i32,
            bottom: h as i32,
            left: 0,
            top: 0,
        };

        trace!("Create FrameResources done");
        Ok(FrameResources {
            viewport: viewport,
            sci_rect: sci_rect,
            calloc: calloc,
            glist: glist,
            fence: fence,
            tm_fence: tm_fence,
            i_buffer: i_buffer,
            instance_data_mapped: cpu_i_ptr,
            gpu_i_ptr: gpu_i_ptr,
            c_buffer: c_buffer,
            constants_mapped: cpu_c_ptr,
            gpu_c_ptr: gpu_c_ptr,
            hdr_render_target: hdr_render_target,
            hdr_rtv_heap: hdr_rtv_heap,
            _depth_stencil: depth_stencil,
            dsd_heap: dsd_heap,
            tonemapper_resources: tonemapper_resources,
            object_cnt: object_cnt as u32,
        })
    }
    // rtvh - render target view descriptor handle
    fn render(&mut self, core: &DXCore, rt: &D3D12Resource, rtvh: D3D12_CPU_DESCRIPTOR_HANDLE, 
                cr: &CommonResources, st: &State, camera: &Camera)
                -> HResult<()> {
        ::perf_wait_start();
        try!(self.fence.wait_for_gpu());
        ::perf_wait_end();
    
        ::perf_fillbuf_start();

        *self.constants_mapped = Constants {
            view: matrix4_to_4x4(&camera.view_matrix()),
            proj: matrix4_to_4x4(&camera.projection_matrix()),
            eye_pos: camera.eye.clone().into(),
            _padding1: 0.,
            light_pos: st.light_pos.clone().into(),
        };

        //// Experiments showed no gain from filling instance data using multiple threads
        //crossbeam::scope(|scope| {
        //    let chunk_size = (self.instance_data_mapped.len() / cr.thread_cnt as usize)+1;
        //    for (ch_idx, chunk) in self.instance_data_mapped.chunks_mut(chunk_size).enumerate() {
        //        scope.spawn(move || {
        //            let start = ch_idx * chunk_size;
        //            for (idx, inst_ref) in chunk.iter_mut().enumerate() {
        //                let cs = &st.cubes[start+idx];
        //                *inst_ref = InstanceData {
        //                    world: rotshift3_to_4x4(&cs.rot, &cs.pos),
        //                    n_world: rot3_to_3x3(&cs.rot),
        //                };
        //            };
        //        });
        //    };
        //});
        for (inst_ref, cs) in self.instance_data_mapped.iter_mut().zip(st.cubes[..].iter()) {
            unsafe { // 
                // memory at inst_ref is write-only. ptr::write ensures that it is not read.
                // There should be no performance inprovement compared to "*inst_ref =", but somehow there is small speedup.
                ptr::write(inst_ref as *mut _, InstanceData {
                    world: rotshift3_to_4x4(&cs.rot, &cs.pos),
                    n_world: rot3_to_3x3(&cs.rot),
                });
            };
            //*inst_ref = InstanceData {
            //    world: rotshift3_to_4x4(&cs.rot, &cs.pos),
            //    n_world: rot3_to_3x3(&cs.rot),
            //};
        };
        ::perf_fillbuf_end();
        ::perf_start("listfill");
        try!(self.calloc.reset());
        try!(self.glist.reset(&self.calloc, Some(&cr.pipeline_state)));

        let hdr_rtvh = self.hdr_rtv_heap.cpu_handle(0);
        let dsvh = self.dsd_heap.cpu_handle(0);
        let glist = &self.glist;
        glist.set_graphics_root_signature(&cr.root_signature);
        glist.set_descriptor_heaps(&[cr.srv_heap.get()]);
        glist.rs_set_viewports(&[self.viewport]);
        glist.rs_set_scissor_rects(&[self.sci_rect]);
        glist.om_set_render_targets(1, &hdr_rtvh, Some(&dsvh));
        glist.set_graphics_root_descriptor_table(0, cr.srv_heap.gpu_handle(0));
        glist.set_graphics_root_constant_buffer_view(1, self.gpu_c_ptr);
        glist.set_graphics_root_shader_resource_view(2, self.gpu_i_ptr);
        glist.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        glist.ia_set_vertex_buffers(0, Some(&[cr.vertex_buffer_view]));
        glist.ia_set_index_buffer(Some(&cr.index_buffer_view));
        glist.resource_barrier(
            &[*ResourceBarrier::transition(&self.hdr_render_target,
               D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_RENDER_TARGET)]);
        glist.clear_render_target_view(hdr_rtvh, &CLEAR_COLOR, &[]);
        glist.clear_depth_stencil_view(dsvh, D3D12_CLEAR_FLAG_DEPTH, 1.0, 0, &[]);
        glist.draw_indexed_instanced(cr.index_count, st.cubes.len() as u32, 0, 0, 0);
        glist.resource_barrier(
            &[*ResourceBarrier::transition(&self.hdr_render_target,
                D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_COMMON)]);
        try!(glist.close());
        ::perf_end("listfill");
        ::perf_exec_start();
        core.graphics_queue.execute_command_lists(&[glist]);
        ::perf_exec_end();
        ::perf_start("tonemap");
        let fence_val = core.next_fence_value();
        try!(core.graphics_queue.signal(&self.tm_fence, fence_val));
        try!(core.compute_queue.wait(&self.tm_fence, fence_val));
        try!(cr.tonemapper.tonemap(core, &mut self.tonemapper_resources, &self.hdr_render_target, rt, &mut self.fence));
        ::perf_end("tonemap");

        Ok(())
    }
}

impl Drop for FrameResources {
    fn drop(&mut self) {
        self.c_buffer.unmap(0, None);
        self.i_buffer.unmap(0, None);
    }
}


// This struct contains all data the rendering needs.
pub struct AppData {
    // core contains D3D12Device, D3D12CommandQueue and few other objects
    // which are required for creating and drawing D3D12 objects such as vertex buffers,
    // textures, command lists and so on.
    // Also, it provides central source for all fence values.
    core: DXCore,
    // swap_chain contains DXGISwapChain3, back-buffer resources,
    // render target view heap
    swap_chain: DXSwapChain,
    frame_index: UINT,
    fence_event: Event,
    fence: D3D12Fence,
    minimized: bool,
    object_count: u32,
    frame_count: u32,
    update_step: f32,
    parameters: CubeParms,
    state: Arc<State>,
    common_resources: CommonResources,
    frame_resources: Vec<FrameResources>,
    camera: Camera,
    su_agent: StateUpdateAgent,
}

// This impl allows me to write data.on_init() instead of on_init(&data). Well, at least 'dot' operator performs autodereference.
// TODO: create trait for use in all rendering modules
impl AppData {
    // The purpose is self-explanatory.
    //   wnd - window,
    //   adapter - GPU to use for rendering, or None for default.
    //   frame_count - count of backbuffers in swapchain
    //   parameters - object's count and thread's count
    pub fn on_init(wnd: &Window,
                                  adapter: Option<&DXGIAdapter1>,
                                  frame_count: u32,
                                  parameters: &CubeParms)
                                  -> HResult<AppData> {
        on_init(wnd, adapter, frame_count, parameters)
    }

    // Allows rendering module to change backbuffers'/depth-stencil buffer's size when window size is changed
    // Also it sets AppData::minimized field
    pub fn on_resize(&mut self, w: u32, h: u32, c: u32) {
        on_resize(self, w, h, c)
    }

    // renders and presents scene
    // TODO: remove x,y
    pub fn on_render(&mut self) {
        on_render(self)
    }

    pub fn is_minimized(&self) -> bool {
        self.minimized
    }

    pub fn set_fullscreen(&self, fullscreen: bool) -> HResult<HRESULT> {
        self.swap_chain.swap_chain.set_fullscreen_state(if fullscreen {
                                                            1
                                                        } else {
                                                            0
                                                        },
                                                        None)
    }

    pub fn wait_frame(&self) {
        wait_for_graphics_queue(&self.core, &self.fence, &self.fence_event);
    }

    pub fn take_info_queue(&self) -> Option<D3D12InfoQueue> {
        self.core.info_queue.clone()
    }

    // allows message loop fiddle with camera
    pub fn camera(&mut self) -> &mut Camera {
        &mut self.camera
    }
}

// TODO: implement or remove
impl fmt::Debug for AppData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "struct AppData")
    }
}

fn get_display_mode_list1(output: &DXGIOutput4,
                          format: DXGI_FORMAT,
                          flags: UINT)
                          -> HResult<Vec<DXGI_MODE_DESC1>> {
    // In rare cases get_display_mode_list1 can return Err(DXGI_ERROR_MORE_DATA). Modes should be reenumerated in this case.
    // loop takes care of this
    loop {
        match output.get_display_mode_list1(format, flags, None) {
            Ok(mode_count) => {
                let mut modes = Vec::with_capacity(mode_count as usize);
                // DXGI_MODE_DESC1 is POD. Uninitialized content will be overwritten immediately
                // So it's safe to set_len here
                unsafe {
                    modes.set_len(mode_count as usize);
                };
                match output.get_display_mode_list1(format, flags, Some(&mut modes[..])) {
                    Ok(mode_count_actual) => {
                        assert!(mode_count == mode_count_actual);
                        // Uniinitialized data in modes is safely overwritten.
                        // return is required here to escape the loop
                        return Ok(modes);
                    }
                    // I don't match against Err(DXGI_ERROR_MORE_DATA), because a typo here will cause just a warning
                    // and I have loads of warning. I need to get rid of them.
                    Err(hr) if hr == DXGI_ERROR_MORE_DATA => {
                        // Do nothing and fallthru to the loop
                    }
                    Err(hr) => {
                        return Err(hr);
                    }
                }; // match query modes
            } // Ok(mode_count)
            Err(hr) => {
                return Err(hr);
            }
        } // match query mode count
    }
}


pub fn create_static_sampler_gps<T: VertexFormat>(dev: &D3D12Device,
                                                  vshader: &[u8],
                                                  pshader: &[u8],
                                                  root_sign: &D3D12RootSignature)
                                                  -> HResult<D3D12PipelineState> {
    // dx_vertex! macro implements VertexFormat trait, which allows to
    // automatically generate description of vertex data
    let input_elts_desc = T::generate(0);

    // Finally, I combine all the data into pipeline state object description
    // TODO: make wrapper. Each pointer in this structure can potentially outlive pointee.
    //       Pointer/length pairs shouldn't be exposed too.
    let mut pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
        // DX12 runtime doesn't AddRef root_sign, so I need to keep root_sign myself.
        // I return root_sign from this function and I keep it inside AppData.
        pRootSignature: root_sign.iptr() as *mut _,
        VS: ShaderBytecode::from_slice(vshader).get(),
        PS: ShaderBytecode::from_slice(pshader).get(),
        RasterizerState: D3D12_RASTERIZER_DESC {
            CullMode: D3D12_CULL_MODE_BACK,
            ..rasterizer_desc_default()
        },
        InputLayout: D3D12_INPUT_LAYOUT_DESC {
            pInputElementDescs: input_elts_desc.as_ptr(),
            NumElements: input_elts_desc.len() as u32,
        },
        PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
        NumRenderTargets: 1,
        DSVFormat: DXGI_FORMAT_D32_FLOAT,
        Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
        // Take other fields from default gps
        ..graphics_pipeline_state_desc_default()
    };
    // This assignment reduces amount of typing. But I could have included
    // all 8 members of RTVFormats in the struct initialization above.
    pso_desc.RTVFormats[0] = DXGI_FORMAT_R32G32B32A32_FLOAT;

    // Note that creation of pipeline state object is time consuming operation.
    // Compilation of shader byte-code occurs here.
    let ps = try!(dev.create_graphics_pipeline_state(&pso_desc));
    // DirectX 12 offload resource management to the programmer.
    // So I need to keep root_sign around, as DX12 runtime doesn't AddRef it.
    Ok(ps)
}

trait DumpOnError {
    fn dump(self, core: &DXCore) -> Self;
}

impl<T> DumpOnError for HResult<T> {
    fn dump(self, core: &DXCore) -> Self {
        match self {
            Ok(_) => (),
            Err(_) => {
                core.dump_info_queue();
                ()
            },
        };
        self
    }
}

pub fn on_init(wnd: &Window,
                              adapter: Option<&DXGIAdapter1>,
                              frame_count: u32,
                              parameters: &CubeParms)
                              -> HResult<AppData> {
    let hwnd = wnd.get_hwnd();
    let (w, h) = wnd.size();

    // core::create_core creates structure that contains D3D12Device, command queues and so on
    // which are required for creation of all other objects
    let core = 
        match core::create_core(adapter, D3D_FEATURE_LEVEL_11_0, cfg!(debug_assertions)) {
            Ok(core) => core,
            Err(desc) => {
                error!("{}", desc);
                return Err(E_FAIL);
            },
        };

    // Get adapter the device was created on
    let adapter_luid = core.dev.get_adapter_luid();
    if let Ok(adapter) = core.dxgi_factory.enum_adapter_by_luid::<DXGIAdapter3>(adapter_luid) {
        let mut i = 0;
        // Enumerate adapter's outputs
        while let Ok(output) = adapter.enum_outputs(i)
                                      .and_then(|o| o.query_interface::<DXGIOutput4>()) {
            if let Ok(output_desc) = output.get_desc() {
                let output_device_name = wchar_array_to_string_lossy(&output_desc.DeviceName[..]);
                info!("Output {}: {}", i, output_device_name);
            } else {
                error!("Output {}: Unbelievable! DXGIOutput4::get_desc() returned an error.",
                       i);
            };
            // Enumerate display modes
            if let Ok(modes) = get_display_mode_list1(&output, DXGI_FORMAT_R8G8B8A8_UNORM, 0) {
                for mode in &modes {
                    info!("  {}x{} @ {}/{}Hz {:?} {:?} ",
                          mode.Width,
                          mode.Height,
                          mode.RefreshRate.Numerator,
                          mode.RefreshRate.Denominator,
                          mode.ScanlineOrdering,
                          mode.Scaling);
                }
            };
            i += 1;
        }
    };

    let sc_desc = DXGI_SWAP_CHAIN_DESC1 {
        Width: w as u32,
        Height: h as u32,
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

    let swap_chain = try!(core::create_swap_chain(&core,
                                            &sc_desc,
                                            DXGI_FORMAT_R8G8B8A8_UNORM,
                                            hwnd,
                                            None,
                                            None)
                              .dump(&core));

    let (tex_w, tex_h) = (256u32, 256u32);

    let common_resources = try!(CommonResources::new(&core.dev, tex_w, tex_h, parameters.thread_count).dump(&core));


    // Create pattern to set as texture data
    let mut texdata: Vec<u32> = vec![0xff00ffff; (tex_w*tex_h) as usize];
    let mut i = 0;
    for v in &mut texdata[..] {
        let (x, y) = (i % tex_w, i / tex_w);
        let (tex_w, tex_h) = (tex_w as f32, tex_h as f32);
        let (fx, fy) = (x as f32, y as f32);
        let (fx, fy) = ((2. * fx - tex_w) / tex_w, (2. * fy - tex_h) / tex_h);
        let d = (fx * fx + fy * fy).sqrt() * 4.0;
        *v = if x % 10 == 0 || y % 10 == 0 {
            0xff10ff10 // bright green
        } else {
            0xff009090 + (((d.sin() * 0.5 + 0.5) * 255.) as u32) * 65536
        };
        i += 1;
    }

    // Fences synchronize CPU and GPU.
    // You can place a fence in the GPU's command queue by calling D3D12CommandQueue::signal().
    // When GPU reaches the fence, it notifies CPU.
    // If you want GPU to wait for signal, call D3D12CommandQueue::wait(),
    // then tell GPU to continue by calling D3D12CommandQueue::signal().
    trace!("fence");
    let fence = try!(core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE).dump(&core));

    // fence_event communicates GPU's notification to CPU.
    // Use fence.set_event_on_completion(fence_value, fence_event) to bind fence to fence_event
    // Use utils::wait_for_single_object() to wait until GPU reaches marker set by D3D12CommandQueue::signal().
    // fence_value identifies particular call to D3D12CommandQueue::signal()
    trace!("fence_event");
    let fence_event = core::create_event();

    // tex!() expands to &common_resources.tex_resource
    alias!(tex, &common_resources.tex_resource);
    // utils::upload_into_texture() transfers texdata into upload buffer resource,
    // then executed copy command on core.copy_queue
    trace!("upload_into_texture");
    try!(upload_into_texture(&core, tex!(), tex_w, tex_h, &texdata[..]).dump(&core));
    trace!("Downsampler::generate_mips");
    try!(common_resources.downsampler.generate_mips(tex!(), &core).dump(&core));

    // Temporary allocator and command list to transition texture into appropriate state
    trace!("Create calloc");
    let calloc = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
    trace!("Create glist");
    let glist: D3D12GraphicsCommandList =
        try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &calloc, None));
    trace!("resource_barrier");
    glist.resource_barrier(&[*ResourceBarrier::transition(tex!(), D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE)]);

    trace!("Command list Close");
    try!(glist.close().dump(&core));
    trace!("Command queue execute");
    core.graphics_queue.execute_command_lists(&[&glist]);

    trace!("wait_for_prev_frame");
    wait_for_graphics_queue(&core, &fence, &fence_event);

    let object_count = parameters.object_count;

    let mut frame_resources = vec![];
    for _ in 0 .. frame_count {
        frame_resources.push(try!(FrameResources::new(&core.dev, w, h, DXGI_FORMAT_R8G8B8A8_UNORM, object_count).dump(&core)));
    };


    // Pack all created objects into AppData
    let mut ret = AppData {
        core: core,
        swap_chain: swap_chain,
        frame_index: 0,
        fence_event: fence_event,
        fence: fence,
        minimized: false,
        object_count: object_count,
        frame_count: frame_count,
        common_resources: common_resources,
        frame_resources: frame_resources,
        update_step: parameters.speed_mult,
        parameters: *parameters,
        state: Arc::new(State::new(parameters.object_count)),
        camera: Camera::new(),
        su_agent: StateUpdateAgent::new(parameters.thread_count),
    };

    ret.camera.go(-3., 0., 0.);
    ret.camera.aspect = (w as f32) / (h as f32);

    // Done
    Ok(ret)
}

// -----------------------------------------------------------------------------------------------------
// ----------------- on_render -------------------------------------------------------------------------
// -----------------------------------------------------------------------------------------------------
pub fn on_render(data: &mut AppData) {
    use std::thread::sleep_ms;
    if cfg!(debug_assertions) {
        trace!("on_render")
    };
    let maybe_future_state = 
        if data.parameters.concurrent_state_update {
            ::perf_start("state_update_start");
            let ret = Some(data.su_agent.start_update(data.state.clone(), data.update_step));
            ::perf_end("state_update_start");
            ret
        } else {
            None
        };

    // update index of current back buffer
    let fi = data.swap_chain.swap_chain.get_current_back_buffer_index();
    data.frame_index = fi;

    // Render scene
    data.frame_resources[fi as usize].render(
        &data.core, 
        data.swap_chain.render_target(fi), 
        data.swap_chain.rtv_cpu_handle(fi), 
        &data.common_resources, 
        &data.state,
        &data.camera
    ).dump(&data.core).unwrap();

    ::perf_present_start();

    let pparms = DXGI_PRESENT_PARAMETERS {
        DirtyRectsCount: 0,
        pDirtyRects: ptr::null_mut(),
        pScrollRect: ptr::null_mut(),
        pScrollOffset: ptr::null_mut(),
    };


    // I hope that present waits for back buffer transition into STATE_PRESENT
    // I didn't find anything about this in MSDN.
    if cfg!(debug_assertions) {
        trace!("present")
    };
    match data.swap_chain.swap_chain.present1(1, 0, &pparms) {
        Err(hr) => {
            data.core.dump_info_queue();
            // TODO: Maybe I can handle DEVICE_REMOVED error by recreating DXCore and DXSwapchain
            if hr == DXGI_ERROR_DEVICE_REMOVED {
                if let Err(reason) = data.core.dev.get_device_removed_reason() {
                    match reason {
                        DXGI_ERROR_DEVICE_HUNG => error!("Device hung"),
                        DXGI_ERROR_DEVICE_REMOVED => error!("Device removed"),
                        DXGI_ERROR_DEVICE_RESET => error!("Device reset"),
                        DXGI_ERROR_DRIVER_INTERNAL_ERROR => error!("Driver internal error"),
                        DXGI_ERROR_INVALID_CALL => error!("Invalid call"),
                        _ => error!("Device removed reason: 0x{:x}",reason),
                    }
                };
            };
            panic!("Present failed with 0x{:x}", hr);
        }
        // TODO: Use STATUS_OCCLUDED to reduce frame rate
        _ => (),
    }
    sleep_ms(100);
    ::perf_present_end();
    ::perf_start("state_update");
    if let Some(mut future_state) = maybe_future_state {
        data.state = Arc::new(future_state.get());
    } else {
        data.state = Arc::new(data.state.update(data.update_step, data.common_resources.thread_cnt));
    }
    ::perf_end("state_update");
    data.core.dump_info_queue();
}


// ---------------------------

// This function gets called by message loop, when WM_SIZE message arrives.
// Normally WM_SIZE goes to window procedure, but I found that this can happen
// inside DXGISwapchain::present() call I do in on_render() function.
// It causes panic, because in window procedure I borrow RefCell<AppData>, which was
// already borrowed for on_render().
// So I tweaked window procedure and window::PollEventIterator to
// insert WM_SIZE into message queue.
// TODO: return status.
pub fn on_resize(data: &mut AppData, w: u32, h: u32, c: u32) {
    debug!("Resize to {},{},{}.", w, h, c);
    if w == 0 || h == 0 {
        data.minimized = true;
        return;
    };
    data.minimized = false;
    // Before we can resize back buffers, we need to make sure that GPU
    // no longer draws on them.
    wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);
    wait_for_compute_queue(&data.core, &data.fence, &data.fence_event);
    wait_for_copy_queue(&data.core, &data.fence, &data.fence_event);

    data.swap_chain.resize(&data.core.dev, w, h, DXGI_FORMAT_R8G8B8A8_UNORM).expect("swap_chain.resize failed");

    data.camera().aspect = (w as f32) / (h as f32);

    data.frame_resources.truncate(0);
    for _ in 0 .. data.frame_count {
        data.frame_resources.push(
            FrameResources::new(&data.core.dev, w, h, DXGI_FORMAT_R8G8B8A8_UNORM, data.object_count).unwrap());
    }

    // on_render(data, 1, 1);
}

