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
use std::cmp::{min,max};

use camera::*;
use std::sync::atomic::Ordering;
use shape_gen;
use shape_gen::GenVertex;

use downsampler::*;

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
  fn new_vertex(p: &Vector3<f32>) -> Vertex {
    Vertex {
      pos: [p.x, p.y, p.z],
      color: [1.,0.5,0.5,],
      texc0: [0., 0.],
      norm: [1., 0., 0.],
    }
  }
  
  fn set_uv(self, u: f32, v: f32) -> Vertex {
    Vertex {texc0: [u,v], ..self}
  }

  fn set_normal(self, n: &Vector3<f32>) -> Vertex {
    Vertex {norm: [n.x, n.y, n.z], ..self}
  }
}

type M4 = [[f32;4];4];

// This struct contains data to be passed into shader,
// thus its memory layout should be defined. repr(C) does that.
#[repr(C)] #[derive(Clone)]
struct Constants {
  model: M4,
  view: M4,
  proj: M4,
  n_model: M4,
  light_pos: [f32;3],
}

// This trait isn't very useful yet. Maybe it'll never be.
pub trait Parameters {
  fn object_count(&self) -> &u32;
  fn thread_count(&self) -> &u32;
  fn speed_mult(&self) -> f32;
}

pub struct CubeParms {
  pub object_count: u32,
  pub thread_count: u32,
  pub speed_mult: f32,
}

impl Parameters for CubeParms {
  fn object_count(&self) -> &u32 {
    &self.object_count
  }
  fn thread_count(&self) -> &u32 {
    &self.thread_count
  }
  fn speed_mult(&self) -> f32 {
    self.speed_mult
  }
}

// This struct contains all data the rendering needs.
// TODO: split this into something more manageable.
pub struct AppData {
  // core contains D3D12Device, D3D12CommandQueue and few other objects
  // which are required for creating and drawing D3D12 objects such as vertex buffers,
  // textures, command lists and so on. 
  // Also, it provides central source for all fence values.
  core: DXCore,
  // swap_chain contains DXGISwapChain3, back-buffer resources,
  // render target view heap
  swap_chain: DXSwapChain,
  viewport: D3D12_VIEWPORT,
  scissor_rect: D3D12_RECT,
  root_signature: D3D12RootSignature,
  srv_heap: D3D12DescriptorHeap,
  dsd_heap: D3D12DescriptorHeap,
  pipeline_state: D3D12PipelineState,
  // rustc thinks that _vertex_buffer is never user, but it holds corresponding resource alive,
  // while vertex_buffer_view keeps GPU virtual address of the buffer.
  // underscore prevents warning
  // TODO: bundle vertex buffer and vertex buffer view into one structure?
  // 
  _vertex_buffer: D3D12Resource,
  vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
  // same as for _vertex_buffer
  _index_buffer: D3D12Resource,
  index_buffer_view: D3D12_INDEX_BUFFER_VIEW,
  _constant_buffer: D3D12Resource,
  _tex_resource: D3D12Resource,
  depth_stencil: Option<D3D12Resource>,
  frame_index: UINT,
  fence_event: Event,
  fence: D3D12Fence,
  tick: f64,
  parallel_submission: Vec<ParallelSubmissionData<Constants>>,
  cur_ps_num: usize,
  minimized: bool,
  camera: Camera,
  rot_spd: Vec<(Vector3<f32>, Vector3<f32>, Vector3<f32>, f32)>,
  downsampler: Downsampler,
}

// Parallel GPU workload submission requires some data from AppData.
// So I need to implement Sync for it to be able to pass references to
// different threads.
// TODO: rewrite parrallel part to pass only required parts of AppData
unsafe impl Sync for AppData {}


// This impl allows me to write data.on_init() instead of on_init(&data). Well, at least 'dot' operator performs autodereference.
// TODO: create trait for use in all rendering modules
impl AppData {
  // The purpose is self-explanatory. 
  //   wnd - window,
  //   adapter - GPU to use for rendering, or None for default.
  //   frame_count - count of backbuffers in swapchain
  //   parameters - object's count and thread's count
  pub fn on_init<T: Parameters>(wnd: &Window, adapter: Option<&DXGIAdapter1>, frame_count: u32, parameters: &T) -> Result<AppData, Option<D3D12InfoQueue>> {
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
    self.swap_chain.swap_chain.set_fullscreen_state(if fullscreen {1} else {0} , None)
  }

  pub fn wait_frame(&self) {
    wait_for_graphics_queue(&self.core, &self.fence, &self.fence_event);
  }

  // returns queue of debug layer's messages
  pub fn info_queue(&self) -> &Option<D3D12InfoQueue> {
    &self.core.info_queue
  }

  // allows message loop fiddle with camera
  pub fn camera(&mut self) -> &mut Camera {
    &mut self.camera
  }
}

// TODO: implement or remove
impl fmt::Debug for AppData {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f,"struct AppData")
  }
}

fn get_display_mode_list1(output: &DXGIOutput4, format: DXGI_FORMAT, flags: UINT) -> HResult<Vec<DXGI_MODE_DESC1>> {
  // In rare cases get_display_mode_list1 can return Err(DXGI_ERROR_MORE_DATA). Modes should be reenumerated in this case.
  // loop takes care of this
  loop {
    match output.get_display_mode_list1(format, flags, None) {
      Ok(mode_count) => {
        let mut modes = Vec::with_capacity(mode_count as usize);
        // DXGI_MODE_DESC1 is POD. Uninitialized content will be overwritten immidiately
        // So it's safe to set_len here
        unsafe{ modes.set_len(mode_count as usize); };
        match output.get_display_mode_list1(format, flags, Some(&mut modes[..])) {
          Ok(mode_count_actual) => {
            assert!(mode_count == mode_count_actual);
            // Uniinitialized data in modes is safely overwritten.
            // return is required here to escape the loop
            return Ok(modes);
          },
          // I don't match against Err(DXGI_ERROR_MORE_DATA), because a typo here will cause just a warning
          // and I have loads of warning. I need to get rid of them.
          Err(hr) if hr==DXGI_ERROR_MORE_DATA => {
            // Do nothing and fallthru to the loop
          },
          Err(hr) => { return Err(hr); },
        }; // match query modes
      }, // Ok(mode_count)
      Err(hr) => { return Err(hr); },
    } // match query mode count
  }
}

pub fn on_init<T: Parameters>(wnd: &Window, adapter: Option<&DXGIAdapter1>, frame_count: u32, parameters: &T) -> Result<AppData, Option<D3D12InfoQueue>> {
  // TODO: get window width and height from wnd
  let hwnd=wnd.get_hwnd();
  let (w, h)=wnd.size();
  debug!("HWND");
  let viewport=D3D12_VIEWPORT {
    TopLeftX: 0.,
    TopLeftY: 0.,
    MinDepth: 0.,
    Width: w as f32,
    Height: h as f32,
    MaxDepth: 1.0,
  };
  let sci_rect=D3D12_RECT {
    right: w as i32,
    bottom: h as i32,
    left: 0,
    top: 0,
  };
  
  // core::create_core creates structure that contains D3D12Device, command queues and so on
  // which are required for creation of all other objects
  let core = try!(create_core(adapter, D3D_FEATURE_LEVEL_11_0, cfg!(debug_assertions)).map_err(|err|panic!("Cannot create DXCore: {}", err)));

  // Get adapter the device was created on
  let adapter_luid = core.dev.get_adapter_luid();
  if let Ok(adapter) = core.dxgi_factory.enum_adapter_by_luid::<DXGIAdapter3>(adapter_luid) {
    let mut i=0;
    // Enumerate adapter's outputs
    while let Ok(output) = adapter.enum_outputs(i).and_then(|o|o.query_interface::<DXGIOutput4>()) {
      if let Ok(output_desc) = output.get_desc() {
        let output_device_name = wchar_array_to_string_lossy(&output_desc.DeviceName[..]);
        info!("Output {}: {}", i, output_device_name);
      } else {
        error!("Output {}: Unbelievable! DXGIOutput4::get_desc() returned an error.", i);
      };
      // Enumerate display modes
      if let Ok(modes) = get_display_mode_list1(&output, DXGI_FORMAT_R8G8B8A8_UNORM, 0) {
        for mode in &modes {
          info!("  {}x{} @ {}/{}Hz {:?} {:?} ", mode.Width, mode.Height, mode.RefreshRate.Numerator, mode.RefreshRate.Denominator, mode.ScanlineOrdering, mode.Scaling);
        }
      };
      i += 1;
    }
  };

  let sc_desc = DXGI_SWAP_CHAIN_DESC1 {
    Width: w as u32,
    Height: h as u32,
    Format: DXGI_FORMAT_R8G8B8A8_UNORM, // note that create_swap_chain fails when Format is DXGI_FORMAT_R8G8B8A8_UNORM_SRGB
    Stereo: 0,
    SampleDesc: sample_desc_default(),
    BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
    BufferCount: frame_count,
    Scaling: DXGI_SCALING_NONE,
    SwapEffect: DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
    AlphaMode: DXGI_ALPHA_MODE_UNSPECIFIED,
    Flags: 0, //DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH.0,
  };

  // I tried to get fullscreen with tearing for unbounded FPS. Unsuccessfully.
  // Yours truly.
  // P.S. I found out the cause. I need to enumerate display modes, not to create the struct,
  // hoping that it will match display mode exactly.
  // https://msdn.microsoft.com/en-us/library/windows/desktop/bb205075(v=vs.85).aspx#Flipping
  let fsd = DXGI_SWAP_CHAIN_FULLSCREEN_DESC {
    RefreshRate: DXGI_RATIONAL {Numerator: 59940, Denominator: 1000},
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE,
    Scaling: DXGI_MODE_SCALING_UNSPECIFIED,
    Windowed: 1,
  };
  // so I pass DXGI_FORMAT_R8G8B8A8_UNORM_SRGB separately for use in render target view
  let swap_chain = try!(create_swap_chain(&core, &sc_desc, DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, hwnd, None, None)
                      .map_err(|msg|{error!("{}",msg);core.info_queue.clone()}));

  // Descriptor heaps store information that GPU needs to access resources like textures, constant buffers, unordered access views
  // Root signature binds descriptor heap entries to shader registers.
  // This particular descriptor heap will hold information about a texture.
  let srv_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  trace!("SRV descriptor heap");
  let srv_heap = core.dev.create_descriptor_heap(&srv_hd).unwrap();

  // This heap will contain an entry for depth-stencil resource
  let dsd_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let dsd_heap = core.dev.create_descriptor_heap(&dsd_hd).unwrap();

  let compile_flags=0;
  debug!("Vertex shader");
  // shaders.hlsl should be in process current directory
  // vshader and pshader are Vec<u8> containing shader byte-code
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  debug!("Pixel shader");
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();

  // Descriptor range is one of several ways of binding descriptor heaps content to
  // shader registers. It describes type, starting position and count of descriptors in
  // descriptors heap, as well as shader register(s) to bind to.
  // Descriptor range goes into RootParameter::descriptor_table, which goes into root signature.
  let desc_ranges = vec![
    // This one binds first entry from SRV/CBV/UAV descriptor heap to shader register t0
    D3D12_DESCRIPTOR_RANGE {
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
    },
    // In older version I used this entry to bind constant buffer
    // In this version I use RootParameter::cbv below and 
    // D3D12GraphicsCommandList::set_graphics_root_constant_buffer_view()
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: 1,
    },
    // Another remnant of older version.
    // In this verion sampler goes directly into root signature
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: 0,
    },
  ];

  // Root parameters:
  let rs_parms = vec![
    //   Slot 0: register(t0, space0)
    //     Attach resource to this slot by calling
    //       D3D12GraphicsCommandList::set_graphics_root_descriptor_table(slot_number, gpu_virtual_address_of_first_descriptor)
    //     Note, that firstly you need to attach descriptor heaps to command list
    //        D3D12GraphicsCommandList::set_descriptor_heaps(heaps: &[&D3D12DescriptorHeap])
    *RootParameter::descriptor_table(&desc_ranges[0..1], D3D12_SHADER_VISIBILITY_PIXEL),
    //   Slot 1: register(c0, space0), contains constants (uniforms in GL terminology)
    //     Call D3D12GraphicsCommandList::set_graphics_root_constant_buffer_view(slot_number, gpu_virtual_address_of_constants)
    //     to bind data to this slot. You don't need descriptor heaps to do that.
    *RootParameter::cbv(0, 0, D3D12_SHADER_VISIBILITY_ALL),
  ];
  // D3D12_ROOT_PARAMETER structure have different sizes for 32-bit and 64-bit code.
  // I had a fun time adapting wrapper generator to this class of cases.
  debug!("Size of D3D12_ROOT_PARAMETER:{}", ::std::mem::size_of_val(&rs_parms[0]));

  // Static sampler goes directly into root signature.
  // It doesn't require descriptor in sampler descriptor heap.
  let static_samplers = vec![static_sampler_anisotropic_default()];

  // It is the place where all input slots are bound to shader registers.
  // except for vertex ('v' registers) and index buffers, which do not require binding
  // TODO: make a wrapper
  let rsd=D3D12_ROOT_SIGNATURE_DESC{
    NumParameters: rs_parms.len() as UINT,
    pParameters: rs_parms[..].as_ptr(),
    NumStaticSamplers: static_samplers.len() as u32,
    pStaticSamplers: static_samplers.as_ptr(),
    Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
  };

  trace!("Serialize root signature");
  // Root signature can be embedded into HLSL. You can extract blob from shader text.
  // https://msdn.microsoft.com/en-us/library/windows/desktop/dn913202(v=vs.85).aspx
  let blob=try!(d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1).map_err(|_|core.info_queue.clone()));

  trace!("Root signature");
  // It creates D3D12RoosSignature
  let root_sign = try!(core.dev.create_root_signature(0, blob_as_slice(&blob)).map_err(|_|core.info_queue.clone()));
  // Graphics pipeline state (GPS) contains settings for all stages in graphics pipeline.
  // Also, it containt root signature that binds descriptor heaps to shader registers
  // Shaders are part of GPS too. D3D12Device::create_graphics_pipeline_state() performs compilation of provided shader bytecode.
  // MSDN suggests to create GPS on separate thread, as this is CPU intensive operation.
  trace!("Graphics pipeline state");
  let gps=try!(create_static_sampler_gps::<Vertex>(&core, &vshader[..], &pshader[..], &root_sign).map_err(|_|core.info_queue.clone()));

  // Command allocator manages storage for command lists.
  trace!("Command allocator");
  let callocator = core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).unwrap();

  // Command lists hold the list of commands to be executed by GPU. D3D12CommanQueue::execute_command_lists() initiates actual execution.
  // I use this command list once to transition texture to pixel shader resource state.
  // TODO: don't store callocator and command_list in AppData
  trace!("Command list");
  let command_list: D3D12GraphicsCommandList = core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, Some(&gps)).unwrap();
  
  // ------------------- Vertex and index buffer resource init begin ----------------------------

  // get vertices and indices for cube
  let (vtc,idx) = shape_gen::cube_indexed::<Vertex>(0.5);

  // Size in bytes of array of vertices
  let vbuf_size = mem::size_of_val(&vtc[..]);

  // Vertex buffer will be created in upload head,
  // Using D3D12Resource::map we can get CPU write access to this heap.
  let heap_prop = heap_properties_upload();

  // The only thing DX12 needs to create Buffer Resource is its length
  let res_desc = resource_desc_buffer(vbuf_size as u64);
  //info!("Resource desc: {:#?}", &res_desc);

  // Create resource in upload heap with initial state GENERIC_READ, without optimized clear value.
  // create_committed_resource() creates implicit heap large enough to contain resource.
  // GENERIC_READ here is GPU side. It means that all shaders can read the resource and 
  // resource can be source of copy operation.
  trace!("Vertex buffer");
  let vbuf = try!(core.dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &res_desc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|core.info_queue.clone()));

  //Transfer data from vtc into GPU memory allocated for vbuf
  try!(upload_into_buffer(&vbuf, &vtc[..]).map_err(|_|core.info_queue.clone()));
  
  // Unlike shader resource views or constant buffer views, 
  // vertex buffer views don't go into descriptor heaps.
  // D3D12GraphicsCommandList::ia_set_vertex_buffers() binds them directly.
  let vbview = 
    D3D12_VERTEX_BUFFER_VIEW {
      BufferLocation: vbuf.get_gpu_virtual_address(),
      StrideInBytes: mem::size_of::<Vertex>() as u32,
      SizeInBytes: vbuf_size as u32,
  };

  // Creation and filling of index buffer is similar to that of vertex buffer.
  let ibuf_size = mem::size_of_val(&idx[..]);
  let idesc = resource_desc_buffer(ibuf_size as u64);
  trace!("Index buffer");
  let ibuf = try!(core.dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &idesc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|core.info_queue.clone()));
  try!(upload_into_buffer(&ibuf, &idx[..]).map_err(|_|core.info_queue.clone()));

  // D3D12GraphicsCommandList::ia_set_index_buffer() binds index buffer to graphics pipeline.
  let i_view = D3D12_INDEX_BUFFER_VIEW {
    BufferLocation: ibuf.get_gpu_virtual_address(),
    SizeInBytes: ibuf_size as UINT,
    Format: DXGI_FORMAT_R32_UINT,
  };

  // ------------------- Vertex and index buffer resource init end ----------------------------

  // ------------------- Constant buffer resource init begin ----------------------------
  let world_matrix: [[f32;4];4] = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.],];
  let view_matrix: [[f32;4];4] = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.],];
  let proj_matrix: [[f32;4];4] = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.],];
  let cbuf_data = Constants{ model: world_matrix, view: view_matrix, proj: proj_matrix, n_model: world_matrix, light_pos: [-1.,0.,0.]};
  let cbsize = mem::size_of_val(&cbuf_data);

  // Resource creation for constant buffer is the same as resource creation for vertex buffer
  let cbuf = core.dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(cbsize as u64), D3D12_RESOURCE_STATE_GENERIC_READ, None).unwrap();
  try!(upload_into_buffer(&cbuf, &[cbuf_data]).map_err(|_|core.info_queue.clone()));

  // But constant buffer view goes into descriptor heap.
  let cbview = 
    D3D12_CONSTANT_BUFFER_VIEW_DESC {
      BufferLocation: cbuf.get_gpu_virtual_address(),
      SizeInBytes: ((cbsize+255) & !255) as u32, // size of constants buffer in bytes should be divisible by 256.
  };

  // Create descriptor heap to hold one constant buffer view
  let cbv_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1, 
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  trace!("CBV descriptor heap");
  let cbvd_heap = core.dev.create_descriptor_heap(&cbv_hd).unwrap();

  // CPU side of descriptor heap is an array of unspecified structures of cbv_dsize size.
  let _cbv_dsize = core.dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
  // const_dh receives pointer to the start of descriptors array
  let const_dh = cbvd_heap.get_cpu_descriptor_handle_for_heap_start();
  // create_constant_buffer_view creates hardware specific representation of constant buffer view at const_dh address.
  // In this case const_dh corresponds to first entry in cbvd_heap.
  trace!("Create shader resource view: constants buffer");
  core.dev.create_constant_buffer_view(Some(&cbview), const_dh);

  // ------------------- Constant buffer resource init end  -----------------------------

  // ------------------- Texture resource init begin  -----------------------------

  let tex_w=256usize;
  let tex_h=256usize;

  // I create resource with 4 mip levels, but fill only one yet.
  let tex_desc = D3D12_RESOURCE_DESC {
    MipLevels: 4,
    .. resource_desc_tex2d_nomip(tex_w as u64, tex_h as u32, DXGI_FORMAT_R8G8B8A8_UNORM, D3D12_RESOURCE_FLAG_NONE)
  };

  // I create texture in default heap, CPU can't get access to this heap.
  // I set initial state of the resource to COPY_DEST, 
  // to prepare it for data transfer thru intermediate resource placed in upload heap
  trace!("Texture resource");
  let tex_resource = try!(core.dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, 
                            &tex_desc, D3D12_RESOURCE_STATE_COPY_DEST, None).map_err(|_|core.info_queue.clone()));
  
  // Create pattern to set as texture data
  let mut texdata: Vec<u32> = vec![0xff00ffff; tex_w*tex_h as usize];
  let mut i = 0;
  for v in &mut texdata[..] {
    let (x,y) = (i%tex_w, i/tex_w);
    *v =
      if x%10 == 0 || y%10 == 0 {
        0xff10ff10 // bright green
      } else {
        0xff909090 // light grey
      };
    i+=1;
  }

  // utils::upload_into_texture() transfers texdata into upload buffer resource,
  // then executed copy command on core.copy_queue
  trace!("upload_into_texture");
  try!(upload_into_texture(&core, &tex_resource, tex_w, tex_h, &texdata[..]).map_err(|_|core.info_queue.clone()));

  trace!("Downsampler::new");
  let downsampler = Downsampler::new(&core.dev);
  //generate mips
  trace!("Downsampler::generate_mips");
  try!(downsampler.generate_mips(&tex_resource, &core).map_err(|_|core.info_queue.clone()));
  // upload_into_texture transition tex_resource into common state (it uses copy queue, so it can't set pixel_shader_resource state)
  trace!("resource_barrier");
  command_list.resource_barrier(&[*ResourceBarrier::transition(&tex_resource, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE)]);

  trace!("Command list Close");
  command_list.close().unwrap();
  trace!("Command queue execute");
  core.graphics_queue.execute_command_lists(&[&command_list]);  
  // Fences synchronize CPU and GPU.
  // You can place a fence in the GPU's command queue by calling D3D12CommandQueue::signal().
  // When GPU reaches the fence, it notifies CPU.
  // If you want GPU to wait for signal, call D3D12CommandQueue::wait(),
  // then tell GPU to continue by calling D3D12CommandQueue::signal().
  trace!("fence");
  let fence=core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE).unwrap();

  // fence_event communicates GPU's notification to CPU. 
  // Use fence.set_event_on_completion(fence_value, fence_event) to bind fence to fence_event
  // Use utils::wait_for_single_object() to wait until GPU reaches marker set by D3D12CommandQueue::signal().
  // fence_value identifies particular call to D3D12CommandQueue::signal()
  trace!("fence_event");
  let fence_event = create_event();
  
  trace!("wait_for_prev_frame");
  wait_for_graphics_queue(&core, &fence, &fence_event);

  // create shader resource view in srv_heap descriptor heap
  let srv_desc = shader_resource_view_tex2d_default_mip(DXGI_FORMAT_R8G8B8A8_UNORM, 4);

  let srv_dh=srv_heap.get_cpu_descriptor_handle_for_heap_start();
  trace!("Create shader resource view: texture");
  core.dev.create_shader_resource_view(Some(&tex_resource), Some(&srv_desc), srv_dh);

  // ------------------- Texture resource init end  -----------------------------

  // ------------------- Depth stencil buffer init begin ------------------------
  
  // utils::create_depth_stencil() functions handles creation of depth-stencil resource 
  // and depth-stencil view, and it places depth-stencil view into dsd_heap descriptor heap
  let ds_format = DXGI_FORMAT_D32_FLOAT;
  let ds_res = try!(create_depth_stencil(w as u64, h as u32, ds_format, &core.dev, &dsd_heap, 0).map_err(|_|core.info_queue.clone()));

  // ------------------- Depth stencil buffer init end --------------------------

  let mut par_sub = vec![];
  // Fill data required to submit GPU workload in parallel
  // I create three copies of that data to be able to keep GPU busy.
  // While GPU renders previous frame, I can submit new command lists.
  trace!("init_parallel_submission");
  for _ in 0..3 {
    par_sub.push(try!(init_parallel_submission(&core, *parameters.thread_count(), *parameters.object_count())
            .map_err(|hr|{error!("init_parallel_submission failed with 0x{:x}", hr); core.info_queue.clone()})));
  }
  // State of the cubes. Tuple (position, speed, rotation axis, rotation speed)
  let mut rot_spd = Vec::with_capacity(*parameters.object_count() as usize);
  for _ in 0 .. *parameters.object_count() {
    rot_spd.push((v3((rand::random::<f32>()-0.5)*200., (rand::random::<f32>()-0.5)*200., (rand::random::<f32>()-1.)*200.), 
        v3(rand::random::<f32>()-0.5, rand::random::<f32>()-0.5, rand::random::<f32>()-0.5).mul_s(parameters.speed_mult()),
          v3(rand::random::<f32>()-0.5, rand::random::<f32>()-0.5, rand::random::<f32>()-0.5).normalize(), rand::random::<f32>()*1.0));
  }

  // Pack all created objects into AppData
  let mut ret=
    AppData {
      core: core,
      swap_chain: swap_chain,
      viewport: viewport,
      scissor_rect: sci_rect,
      root_signature: root_sign,
      srv_heap: srv_heap,
      dsd_heap: dsd_heap,
      pipeline_state: gps,
      _vertex_buffer: vbuf,
      vertex_buffer_view: vbview,
      _index_buffer: ibuf,
      index_buffer_view: i_view,
      _constant_buffer: cbuf,
      _tex_resource: tex_resource,
      depth_stencil: Some(ds_res),
      frame_index: 0,
      fence_event: fence_event,
      fence: fence,
      tick: 0.0,
      parallel_submission: par_sub,
      cur_ps_num: 0,
      minimized: false,
      camera: Camera::new(),
      rot_spd: rot_spd,
      downsampler: downsampler,
    };

  // I can't use 'camera' variable here. I moved it into ret.camera.
  ret.camera.go(-3., 0., 0.);
  ret.camera.aspect = (w as f32)/(h as f32);
  
  // Done
  Ok(ret)
}

//-----------------------------------------------------------------------------------------------------
//----------------- on_render -------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------
pub fn on_render(data: &mut AppData) {
  use std::f64; 

  if cfg!(debug_assertions) {trace!("on_render")};

  // advance position of cubes
  for rot in &mut data.rot_spd {
    rot.0 = rot.0.add_v(&rot.1);
  }

  // measure timings
  ::perf_wait_start();
  {
    // data.cur_ps_num is number of currently active parallel submission
    let ps = &data.parallel_submission[data.cur_ps_num];
    let fence = &ps.fence;
    // wait for GPU to execute all command lists, which was previously submitted by current parallel submission
    fence.set_event_on_completion(ps.fence_val, data.fence_event.handle()).unwrap();
    wait_for_single_object(&data.fence_event, INFINITE);
  }
  ::perf_wait_end();
  ::perf_fillbuf_start();
  // update index of current back buffer
  data.frame_index = data.swap_chain.swap_chain.get_current_back_buffer_index();

  // Setup world, normal, view and projection matrices
  let mut wm: Matrix4<f32> = Matrix4::one();
  let mut wm_normal=wm;
  wm_normal.invert_self();
  wm_normal.transpose_self();
  let world_normal_matrix = matrix4_to_4x4(&wm_normal);
  let world_matrix = matrix4_to_4x4(&wm); 
  let view_matrix = matrix4_to_4x4(&data.camera.view_matrix());
  let proj_matrix = matrix4_to_4x4(&data.camera.projection_matrix());

  let (lx,ly) = f64::sin_cos(data.tick);
  let (lx,ly) = (lx as f32, ly as f32);

  // Parallel submission threads use view, proj and light_pos
  let consts = Constants{ model: world_matrix, view: view_matrix, proj: proj_matrix, n_model: world_normal_matrix, light_pos: [lx,ly,-3.0]};

  crossbeam::scope(|scope| {
    // It's overkill to run those two threads in parallel.
    // command_list_rt_clear() just creates command list to clear render target and depth-stencil buffer.
    // Thread creation overhead probably eats all bonuses.
    // TODO: Compare this and sequential version
    scope.spawn(|| { 
        let ps = &data.parallel_submission[data.cur_ps_num];
        match command_list_rt_clear(&ps.clear_list, &ps.ps_allocator, data) {
          Err(_) => {
            dump_info_queue(data.core.info_queue.as_ref());
            panic!();
          },
          _ => {},
        }
      });
    scope.spawn(|| {
      // this function fills several graphics command lists, each list is filled in its own thread.
      submit_in_parallel(data, &consts).unwrap();
    });
  });
  ::perf_fillbuf_end();
  ::perf_clear_start();
  // Clear render targets
  data.core.graphics_queue.execute_command_lists(&[&data.parallel_submission[data.cur_ps_num].clear_list]);

  // I'm not sure if GPU parallelizes execution within execute_command_lists only, or across calls too.
  // TODO: make sure that this sync is not needed
  //wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);

  ::perf_clear_end();
  ::perf_exec_start();
  // Scope prevents 'ps', that borrows part of 'data', from blocking subsequent mutable borrows.
  {
    let ps = &data.parallel_submission[data.cur_ps_num];
    // ParallelSubmission::gc_lists contains Vec of tuples (D3D12GraphicsCommandList, D3D12CommandAllocator)
    // This line creates vector of references to (borrows of) command lists from gc_lists
    // TODO: check if one command allocator is sufficient for parallel lists creation
    let cls: Vec<&D3D12GraphicsCommandList> = ps.gc_lists.iter().map(|&(ref clist, _)|clist).collect();
    data.core.graphics_queue.execute_command_lists(&cls[..]);
  }

  {
    let ps = &data.parallel_submission[data.cur_ps_num];
    ps.transition_list.reset(&ps.ps_allocator, None).unwrap();
    // Resource barrier in this command list tells GPU to make current back buffer ready for presentation
    ps.transition_list.resource_barrier(
        &[*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
        D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT)]);
    ps.transition_list.close().unwrap();
    data.core.graphics_queue.execute_command_lists(&[&ps.transition_list]);
  }

  let fence_val_next=data.core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;
  data.parallel_submission[data.cur_ps_num].fence_val = fence_val_next;
  // When GPU reaches this point in its command queue it will set completed value of given fence to fence_val_next
  // I tell CPU to wait for it near the beginning of on_render() to be able to reuse command lists
  data.core.graphics_queue.signal(&data.parallel_submission[data.cur_ps_num].fence, fence_val_next).unwrap();

  // Advance to next parallel submission struct. I need a couple of parallel submission structs to
  // avoid waiting for GPU to finish processing command lists.
  data.cur_ps_num = (data.cur_ps_num+1) % data.parallel_submission.len();
  ::perf_exec_end();
  ::perf_present_start();

  let pparms = DXGI_PRESENT_PARAMETERS {
    DirtyRectsCount: 0,
    pDirtyRects: ptr::null_mut(),
    pScrollRect: ptr::null_mut(),
    pScrollOffset: ptr::null_mut(),
  };
  // I hope that present waits for back buffer transition into STATE_PRESENT
  // I didn't find anything about this in MSDN.
  match data.swap_chain.swap_chain.present1(0, 0, &pparms) {
    Err(hr) => {
      dump_info_queue(data.core.info_queue.as_ref());
      //TODO: Maybe I can handle DEVICE_REMOVED error by recreating DXCore and DXSwapchain
      panic!("Present failed with 0x{:x}", hr);
    },
    //TODO: Use STATUS_OCCLUDED to reduce frame rate
    _ => (),
  }
  ::perf_present_end();
  data.tick += 0.01;
  dump_info_queue(data.core.info_queue.as_ref());
}


//--------------------------------
// This function fills graphics command list with instructions
// for clearing render target and depth-stencil view.
fn command_list_rt_clear(command_list: &D3D12GraphicsCommandList, command_allocator: &D3D12CommandAllocator, data: &AppData) -> HResult<()> {
  if cfg!(debug_assertions) {trace!("Command allocator reset")};
  // I unwrap() result because it shouldn't panic if I wrote the program correctly.
  // Hmm. I think DEVICE_REMOVED _can_ occur here. Ok. I replaced it with try!.
  // D3D12CommandAllocator::reset() indicates that it is safe to reuse allocated memory.
  // GPU mustn't be using any of command lists associated with this allocator.
  // I ensure this near the beginning of on_render() by waiting for fence.
  try!(command_allocator.reset());
  if cfg!(debug_assertions) {trace!("Command list reset")};
  // Command lists can be reused immediately after call to D3DCommandQueue::execute_command_lists(),
  // but I don't use this property yes.
  try!(command_list.reset(command_allocator, Some(&data.pipeline_state)));

  if cfg!(debug_assertions) {trace!("Resource barrier")};
  // It tells GPU to prepare back buffer for use as render target.
  command_list.resource_barrier(
      &[*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
      D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET).flags(D3D12_RESOURCE_BARRIER_FLAG_NONE)]);

  // Essentially, the following is a pointer arithmetics.
  // TODO: wrap it somehow
  // rtvh is a pointer to CPU side of render target view descriptor heap
  let mut rtvh=data.swap_chain.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  // advance pointer to a descriptor of current back buffer
  rtvh.ptr += (data.frame_index as SIZE_T)*data.swap_chain.rtv_dsize;
  // depth-stencil view descriptor heap holds just one descriptor
  // TODO: check if this impacts performance
  let dsvh = data.dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  if cfg!(debug_assertions) {trace!("OM set render targets")};
  // ID3D12GraphicsCommandList::OMSetRenderTargets has 4 parameters.
  // I split this method into two: om_set_render_targets() 
  // and om_set_render_targets_arr(), eliminating RTsSingleHandleToDescriptorRange parameter.
  command_list.om_set_render_targets(1, &rtvh, Some(&dsvh));

  let clear_color = [0.01, 0.01, 0.15, 1.0];
  if cfg!(debug_assertions) {trace!("clear render target view")};
  command_list.clear_render_target_view(rtvh, &clear_color, &[]);
  command_list.clear_depth_stencil_view(dsvh, D3D12_CLEAR_FLAG_DEPTH, 1.0, 0, &[]);
  
  match command_list.close() {
    Ok(_) => Ok(()),
    Err(hr) => {
      dump_info_queue(data.core.info_queue.as_ref());
      panic!("command_list.close() failed with 0x{:x}", hr);
    },
  }
}

//---------------------------

// This function gets called by message loop, when WM_SIZE message arrives.
// Normally WM_SIZE goes to window procedure, but I found that this can happen
// inside DXGISwapchain::present() call I do in on_render() function.
// It causes panic, because in window procedure I borrow RefCell<AppData>, which was 
// already borrowed for on_render().
// So I tweaked window procedure and window::PollEventIterator to 
// insert WM_SIZE into message queue.
// TODO: return status.
pub fn on_resize(data: &mut AppData, w: u32, h: u32, c: u32) {
   debug!("Resize to {},{},{}.",w,h,c);
   if w==0 || h==0 {
      data.minimized = true;
      return;
   };
   data.minimized = false;
   // Before we can resize back buffers, we need to make sure that GPU
   // no longer draws on them.
   wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);
   // For buffer resize to succeed there should be no outstanding references to back buffers
   drop_render_targets(&mut data.swap_chain);
   // Resize back buffers to new size. 
   //Number of back buffers, back buffer format and swapchain flags remain unchanged.
   let res = data.swap_chain.swap_chain.resize_buffers(0, w, h, DXGI_FORMAT_UNKNOWN, 0);
   match res {
     Err(hr) => {
       // TODO: Return custom error to facilitate error recovery
       dump_info_queue(data.core.info_queue.as_ref());
       error!("resize_buffers returned 0x{:x}", hr);
     },
     _ => (),
   };
   // 
   reaquire_render_targets(&data.core, &mut data.swap_chain).unwrap();
   // create new depth stencil
   let ds_format = DXGI_FORMAT_D32_FLOAT;
   data.depth_stencil = Some(create_depth_stencil(w as u64, h as u32, ds_format, &data.core.dev, &data.dsd_heap, 0).unwrap());
  
   data.camera.aspect = (w as f32)/(h as f32);

   data.viewport=D3D12_VIEWPORT {
     TopLeftX: 0.,
     TopLeftY: 0.,
     MinDepth: 0.,
     Width: w as f32,
     Height: h as f32,
     MaxDepth: 1.0,
   };
   data.scissor_rect=D3D12_RECT {
     right: w as i32,
     bottom: h as i32,
     left: 0,
     top: 0,
   };
  //on_render(data, 1, 1);
}

struct PtrConstants<T>(*mut T);

unsafe impl<T> Send for PtrConstants<T> {}
unsafe impl<T> Sync for PtrConstants<T> {}

struct ParallelSubmissionData<T> {
  c_buffer: D3D12Resource,
  // Buffer is kept mapped into CPU address space. Drop unmaps it
  // cpu_buf_ptr.0 points to buf_alignment aligned array of buf_count Ts
  cpu_buf_ptr: PtrConstants<T>,
  gpu_buf_ptr: u64,
  buf_alignment: usize,
  buf_count: usize,
  gc_lists: Vec<(D3D12GraphicsCommandList, D3D12CommandAllocator)>,
  ps_allocator: D3D12CommandAllocator,
  clear_list: D3D12GraphicsCommandList,
  transition_list: D3D12GraphicsCommandList,
  fence: D3D12Fence,
  fence_val: u64,
}

impl<T> Drop for ParallelSubmissionData<T> {
  fn drop(&mut self) {
    self.c_buffer.unmap(0, None);
  }
}

impl<T> ParallelSubmissionData<T> {
  // returns (pointer to i-th constant buffer, gpu virtual address of i-th constant buffer)
  fn get_buf_ptr_mut(&self, i: usize) -> (*mut T, u64) {
    assert!(i<self.buf_count);
    (((self.cpu_buf_ptr.0 as usize) + i*self.buf_alignment) as *mut T, 
        self.gpu_buf_ptr + ((i*self.buf_alignment) as u64))
  }

  // returns vec of (chunk_start, chunk_size)
  fn get_chunks(&self, threads: usize) -> Vec<(usize, usize)> {
    let chunk_size = max(1, self.buf_count/threads);
    let mut c_start = 0;
    let mut remaining = self.buf_count;
    let mut chunks = vec![];
    for _ in 0..threads {
      if remaining == 0 {
        chunks.push((0,0));
      } else {
        let cur_chunk_size = min(remaining, chunk_size);
        chunks.push((c_start, cur_chunk_size));
        c_start += cur_chunk_size;
        remaining -= cur_chunk_size;
      }
    }
    assert!(chunks.len() == threads);
    chunks
  }

  fn thread_count(&self) -> usize {
    self.gc_lists.len()
  }
}

fn init_parallel_submission(core: &DXCore, thread_count: u32, object_count: u32) -> HResult<ParallelSubmissionData<Constants>> {
  trace!("cbsize");
  let cbsize = unsafe {
    let cbuf_data: Constants = ::std::mem::uninitialized();
    let cbsize = mem::size_of_val(&cbuf_data);
    mem::forget(cbuf_data);
    cbsize
  };

  let alignment = (cbsize+255) & !255; // TODO: Remove duplication. This is used in D3D12_CONSTANT_BUFFER_VIEW_DESC also.

  let buffer_size = (object_count as usize)*alignment;
  // create resource to hold all constant buffers
  trace!("create cbuf");
  let cbuf = try!(core.dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(buffer_size as u64), D3D12_RESOURCE_STATE_GENERIC_READ, None));
  let mut ptr: *mut u8 = ptr::null_mut();
  // Indicate that CPU doesn't read from buffer
  let read_range = D3D12_RANGE {Begin: 0, End: 0,};
  trace!("map cbuf");
  unsafe {
    try!(cbuf.map(0, Some(&read_range), Some(&mut ptr)));
    debug!("cbuf.iptr(): 0x{:x}, ptr: 0x{:x}", cbuf.iptr() as usize, ptr as usize);
  }
  let cpu_buf_ptr = PtrConstants(ptr as *mut Constants);

  let mut clists = vec![];
  for _ in 0 .. thread_count {
    trace!("create_command_allocator");
    let callocator = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
    trace!("create_command_list");
    let clist: D3D12GraphicsCommandList = try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, None));
    clist.close().unwrap();
    clists.push((clist, callocator));
  }

  let gpu_buf_ptr = cbuf.get_gpu_virtual_address();

  let fence = try!(core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE));

  let ps_allocator = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
  let clear_list: D3D12GraphicsCommandList = try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &ps_allocator, None));
  clear_list.close().unwrap();
  let transition_list: D3D12GraphicsCommandList = try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &ps_allocator, None));
  transition_list.close().unwrap();


  Ok(ParallelSubmissionData {
    c_buffer: cbuf,
    cpu_buf_ptr: cpu_buf_ptr,
    gpu_buf_ptr: gpu_buf_ptr,
    buf_alignment: alignment,
    buf_count: object_count as usize,
    gc_lists: clists,
    ps_allocator: ps_allocator,
    clear_list: clear_list,
    transition_list: transition_list,
    fence: fence,
    fence_val: 0,
  })
}

fn submit_in_parallel(data: &AppData, consts: &Constants) -> HResult<()> {
  let ps = &data.parallel_submission[data.cur_ps_num];
  let thread_count = ps.thread_count();
  let dsvh = data.dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  let mut rtvh=data.swap_chain.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  rtvh.ptr += (data.frame_index as SIZE_T)*data.swap_chain.rtv_dsize;
  let chunks = ps.get_chunks(thread_count);
  //debug!("{:?}", chunks);

  crossbeam::scope(|scope|{
    let mut jhs = vec![];
    for ((&(start, count), &(ref clist, ref callocator)), th_n) in chunks.iter().zip(ps.gc_lists.iter()).zip(1..) {
      let jh =
        scope.spawn::<_,HResult<()>>(move|| {
          if count == 0 {
              return Ok(());
          };
          let seed: &[_] = &[th_n, 3, 3, 4];
          let mut rng: rand::StdRng = rand::SeedableRng::from_seed(seed);

          try!(callocator.reset());
          try!(clist.reset(callocator, Some(&data.pipeline_state)));
          clist.set_graphics_root_signature(&data.root_signature);
          clist.set_descriptor_heaps(&[&data.srv_heap]);
          clist.rs_set_viewports(&[data.viewport]);
          clist.rs_set_scissor_rects(&[data.scissor_rect]);
          clist.om_set_render_targets(1, &rtvh, Some(&dsvh));
          clist.set_graphics_root_descriptor_table(0, data.srv_heap.get_gpu_descriptor_handle_for_heap_start());
          clist.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
          clist.ia_set_vertex_buffers(0, Some(&[data.vertex_buffer_view]));
          clist.ia_set_index_buffer(Some(&data.index_buffer_view));

          assert!(data.index_buffer_view.Format==DXGI_FORMAT_R32_UINT);
          let num_vtx = data.index_buffer_view.SizeInBytes/4; //Index size depends on Format

          for i in start .. start+count {
            let mut c: Constants = consts.clone();
            let (pos, _, dir, spd) = data.rot_spd[i].clone();
            let rot = Basis3::from_axis_angle(&dir, rad(1.0 + spd*(data.tick as f32)));
            let asfa = rot.as_ref();
            for j in 0..3 {
                for k in 0..3 {
                  c.model[j][k] = asfa[k][j];
                  c.n_model[j][k] = asfa[k][j];
                }
            }
            c.model[0][3] = pos.x;
            c.model[1][3] = pos.y;
            c.model[2][3] = pos.z;
            let (constants, cb_address) = ps.get_buf_ptr_mut(i);
            unsafe {
                // Write to memory-mapped constant buffer
                *(constants) = c;
            };
            //debug!("cb_address: 0x{:x}", cb_address);
            clist.set_graphics_root_constant_buffer_view(1, cb_address);
            clist.draw_indexed_instanced(num_vtx, 1, 0, 0, 0);
          }
          try!(clist.close());
          Ok(())
        });
      jhs.push(jh);
    }
  });
  Ok(())
}

