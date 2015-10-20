use std::fmt;
use std::mem;
use std::ptr;
use winapi::*;
use kernel32::*;
use d3d12_safe::*;
use core::*;
use utils::*;
use cgmath::*;
use structwrappers::*;
use window::*;
use rand::Rng;
use rand;
use cgmath;
use crossbeam;
use create_device::*;
use std::cmp::{min,max};

use camera::*;
use shape_gen::*;
use std::sync::atomic::Ordering;

pub type M3 = [[f32;3];3];
pub type M4 = [[f32;4];4];


dx_vertex!( Vertex {
  (POSITION, 0, DXGI_FORMAT_R32G32B32_FLOAT) pos: [f32;3],
  (COLOR   , 0, DXGI_FORMAT_R32G32B32_FLOAT) color: [f32;3],
  (TEXCOORD, 0, DXGI_FORMAT_R32G32_FLOAT   ) texc0: [f32;2],
  (NORMAL  , 0, DXGI_FORMAT_R32G32B32_FLOAT) norm: [f32;3],
});

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

#[repr(C)] #[derive(Clone)]
struct Constants {
  model: M4,
  view: M4,
  proj: M4,
  n_model: M4,
  light_pos: [f32;3],
}

pub trait Parameters {
  fn object_count(&self) -> &u32;
  fn thread_count(&self) -> &u32;
}

pub struct CubeParms {
  pub object_count: u32,
  pub thread_count: u32,
}

impl Parameters for CubeParms {
  fn object_count(&self) -> &u32 {
    &self.object_count
  }
  fn thread_count(&self) -> &u32 {
    &self.thread_count
  }
}

// TODO: split this into something more manageable.
pub struct AppData {
  core: DXCore,
  swap_chain: DXSwapChain,
  viewport: D3D12_VIEWPORT,
  scissor_rect: D3D12_RECT,
  root_signature: D3D12RootSignature,
  srv_heap: D3D12DescriptorHeap,
  sam_heap: D3D12DescriptorHeap,
  dsd_heap: D3D12DescriptorHeap,
  cbvd_heap: D3D12DescriptorHeap,
  pipeline_state: D3D12PipelineState,
  vertex_buffer: D3D12Resource,
  vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
  constant_buffer: D3D12Resource,
  tex_resource: D3D12Resource,
  depth_stencil: Option<D3D12Resource>,
  frame_index: UINT,
  fence_event: HANDLE,
  fence: D3D12Fence,
  tick: f64,
  parallel_submission: Vec<ParallelSubmissionData<Constants>>,
  cur_ps_num: usize,
  minimized: bool,
  camera: Camera,
  rot_spd: Vec<(Vector3<f32>, Vector3<f32>, f32)>,
}

unsafe impl Sync for AppData {}

impl AppData {
  pub fn on_init<T: Parameters>(wnd: &Window, adapter: Option<&DXGIAdapter1>, frame_count: u32, parameters: &T) -> Result<AppData, Option<D3D12InfoQueue>> {
    on_init(wnd, adapter, frame_count, parameters)
  }

  pub fn on_resize(&mut self, w: u32, h: u32, c: u32) {
    on_resize(self, w, h, c)
  }

  pub fn on_render(&mut self, x: i32, y: i32) {
    on_render(self, x, y)
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

  pub fn info_queue(&self) -> &Option<D3D12InfoQueue> {
    &self.core.info_queue
  }

  pub fn camera(&mut self) -> &mut Camera {
    &mut self.camera
  }
}

impl fmt::Debug for AppData {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f,"struct AppData")
  }
}

pub fn on_init<T: Parameters>(wnd: &Window, adapter: Option<&DXGIAdapter1>, frame_count: u32, parameters: &T) -> Result<AppData, Option<D3D12InfoQueue>> {
  let (w, h)=(512., 256.);
  let hwnd=wnd.get_hwnd();
  debug!("HWND");
  let viewport=D3D12_VIEWPORT {
    TopLeftX: 0.,
    TopLeftY: 0.,
    MinDepth: 0.,
    Width: w,
    Height: h,
    MaxDepth: 1.0,
  };
  let sci_rect=D3D12_RECT {
    right: w as i32,
    bottom: h as i32,
    left: 0,
    top: 0,
  };

//#[cfg(debug)]
  let core = try!(create_core(adapter, D3D_FEATURE_LEVEL_12_0, cfg!(debug)).map_err(|err|panic!("Cannot create DXCore: {}", err)));

  //core.dev.set_stable_power_state(0);

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

  let fsd = DXGI_SWAP_CHAIN_FULLSCREEN_DESC {
    RefreshRate: DXGI_RATIONAL {Numerator: 60, Denominator: 1},
    ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE,
    Scaling: DXGI_MODE_SCALING_UNSPECIFIED,
    Windowed: 1,
  };
  // so I pass DXGI_FORMAT_R8G8B8A8_UNORM_SRGB separately for use in render target view
  let swap_chain = try!(create_swap_chain(&core, &sc_desc, DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, hwnd, Some(&fsd), None)
                      .map_err(|msg|{error!("{}",msg);core.info_queue.clone()}));

  let srv_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 2, // first for texture, second for constants
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("SRV descriptor heap");
  let srv_heap = core.dev.create_descriptor_heap(&srv_hd).unwrap();

  let sam_hd = D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("Sampler descriptor heap");
  let sam_heap = core.dev.create_descriptor_heap(&sam_hd).unwrap();

  let dsd_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let dsd_heap = core.dev.create_descriptor_heap(&dsd_hd).unwrap();

  let sampler_desc = D3D12_SAMPLER_DESC {
    Filter: D3D12_FILTER_ANISOTROPIC,
    AddressU: D3D12_TEXTURE_ADDRESS_MODE_BORDER,
    AddressV: D3D12_TEXTURE_ADDRESS_MODE_BORDER,
    AddressW: D3D12_TEXTURE_ADDRESS_MODE_BORDER,
    MinLOD: 0.0,
    MaxLOD: D3D12_FLOAT32_MAX,
    MipLODBias: 0.0,
    MaxAnisotropy: 16,
    ComparisonFunc: D3D12_COMPARISON_FUNC_ALWAYS,
    BorderColor: [1.0,1.0,1.0,1.0],
  };
  debug!("Sampler");
  core.dev.create_sampler(&sampler_desc, sam_heap.get_cpu_descriptor_handle_for_heap_start());

  debug!("Command allocator");
  let callocator = core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).unwrap();

  debug!("Graphics pipeline state");
  let (gps, root_sign)=try!(create_static_sampler_gps::<Vertex>(&core).map_err(|_|core.info_queue.clone()));
  debug!("Command list");
  let command_list: D3D12GraphicsCommandList = core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, Some(&gps)).unwrap();
  
  // ------------------- Vertex buffer resource init begin ----------------------------

  let (_,vtc) = cube::<Vertex>(0.5);

  let vbuf_size = mem::size_of_val(&vtc[..]);

  let heap_prop = heap_properties_upload();

  let res_desc = resource_desc_buffer(vbuf_size as u64);
  //info!("Resource desc: {:#?}", &res_desc);

  debug!("Vertex buffer");
  let vbuf = try!(core.dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &res_desc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|core.info_queue.clone()));

  unsafe{
    upload_into_buffer(&vbuf, &vtc[..]);
  };
  
  let vbview = 
    D3D12_VERTEX_BUFFER_VIEW {
      BufferLocation: vbuf.get_gpu_virtual_address(),
      StrideInBytes: mem::size_of::<Vertex>() as u32,
      SizeInBytes: vbuf_size as u32,
  };

  // ------------------- Vertex buffer resource init end ----------------------------

  // ------------------- Constant buffer resource init begin ----------------------------
  let world_matrix: [[f32;4];4] = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.],];
  let view_matrix: [[f32;4];4] = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.],];
  let proj_matrix: [[f32;4];4] = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.],];
  let cbuf_data = Constants{ model: world_matrix, view: view_matrix, proj: proj_matrix, n_model: world_matrix, light_pos: [-1.,0.,0.]};
  let cbsize = mem::size_of_val(&cbuf_data);

  let cbuf = core.dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(cbsize as u64), D3D12_RESOURCE_STATE_GENERIC_READ, None).unwrap();
  unsafe {
    upload_into_buffer(&cbuf, &[cbuf_data]);
  };

  let cbview = 
    D3D12_CONSTANT_BUFFER_VIEW_DESC {
      BufferLocation: cbuf.get_gpu_virtual_address(),
      SizeInBytes: ((cbsize+255) & !255) as u32,
  };

  let cbv_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1, 
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("CBV descriptor heap");
  let cbvd_heap = core.dev.create_descriptor_heap(&cbv_hd).unwrap();

  let cbv_dsize = core.dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
  let const_dh = cbvd_heap.get_cpu_descriptor_handle_for_heap_start();
  //const_dh.ptr += srv_dsize; 
  debug!("Create shader resource view: constants buffer");
  core.dev.create_constant_buffer_view(Some(&cbview), const_dh);

  // ------------------- Constant buffer resource init end  -----------------------------

  // ------------------- Texture resource init begin  -----------------------------

  let tex_w=256usize;
  let tex_h=256usize;

  let tex_desc = resource_desc_tex2d_nomip(tex_w as u64, tex_h as u32, DXGI_FORMAT_R8G8B8A8_UNORM, D3D12_RESOURCE_FLAG_NONE);
  //info!("Texture desc: {:#?}", &res_desc);

  debug!("Texture resource");
  let tex_resource = try!(core.dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, &tex_desc, D3D12_RESOURCE_STATE_COPY_DEST, None).map_err(|_|core.info_queue.clone()));
  
  let mut texdata: Vec<u32> = vec![0xff00ffff; tex_w*tex_h as usize];
  let mut i = 0;
  for v in &mut texdata[..] {
    let (x,y) = (i%tex_w, i/tex_w);
    *v =
      if x%10 == 0 || y%10 == 0 {
        0xff204f4f
      } else {
        0xff909090
      };
//    *v=rand::random();
    i+=1;
  }
  // temporary buffer should live until the start of command list execution
  //#[allow(unused_variables)]
  try!(upload_into_texture(&core, &tex_resource, tex_w, tex_h, &texdata[..]).map_err(|_|core.info_queue.clone()));
  // upload_into_texture transition tex_resource into common state (it uses copy queue, so it can't set pixel_shader_resource state)
  command_list.resource_barrier(&mut [*ResourceBarrier::transition(&tex_resource, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE)]);

  let srv_desc = shader_resource_view_tex2d_default(DXGI_FORMAT_R8G8B8A8_UNORM);

  let srv_dh=srv_heap.get_cpu_descriptor_handle_for_heap_start();
  debug!("Create shader resource view: texture");
  core.dev.create_shader_resource_view(Some(&tex_resource), Some(&srv_desc), srv_dh);

  // ------------------- Texture resource init end  -----------------------------

  // ------------------- Depth stencil buffer init begin ------------------------
  
  let ds_format = DXGI_FORMAT_D32_FLOAT;
  let ds_res = try!(create_depth_stencil(w as u64, h as u32, ds_format, &core.dev, &dsd_heap, 0).map_err(|_|core.info_queue.clone()));

  // ------------------- Depth stencil buffer init end --------------------------

  debug!("fence");
  let fence=core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE).unwrap();

  let fence_event = unsafe{ CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  debug!("fence_event");
  if fence_event == ptr::null_mut() {
    panic!("Cannot create event");
  }
  
  let mut par_sub = vec![];
  for _ in 0..3 {
    par_sub.push(try!(init_parallel_submission(&core, *parameters.thread_count(), *parameters.object_count()).map_err(|_|core.info_queue.clone())));
  }
  let mut rot_spd = Vec::with_capacity(*parameters.object_count() as usize);
  for _ in 0 .. *parameters.object_count() {
    rot_spd.push((v3((rand::random::<f32>()-0.5)*200., (rand::random::<f32>()-0.5)*200., (rand::random::<f32>()-1.)*200.), 
          v3(rand::random::<f32>()-0.5, rand::random::<f32>()-0.5, rand::random::<f32>()-0.5).normalize(), rand::random::<f32>()));
  }


  let mut ret=
    AppData {
      core: core,
      swap_chain: swap_chain,
      viewport: viewport,
      scissor_rect: sci_rect,
      root_signature: root_sign,
      srv_heap: srv_heap,
      sam_heap: sam_heap,
      dsd_heap: dsd_heap,
      cbvd_heap: cbvd_heap,
      pipeline_state: gps,
      vertex_buffer: vbuf,
      vertex_buffer_view: vbview,
      constant_buffer: cbuf,
      tex_resource: tex_resource,
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
    };

  ret.camera.go(-3., 0., 0.);
  ret.camera.aspect = (w as f32)/(h as f32);
  
  debug!("Command list Close");
  command_list.close().unwrap();

  debug!("Command queue execute");
  ret.core.graphics_queue.execute_command_lists(&[&command_list]);
  
  debug!("wait_for_prev_frame");
  wait_for_graphics_queue(&ret.core, &ret.fence, &ret.fence_event);

  Ok(ret)
}

//----------------- on_render ------------------------------------------------------------------------------------------------
pub fn on_render(data: &mut AppData, x: i32, y: i32) {
  use std::f64; 

  if cfg!(debug) {debug!("on_render")};
  ::perf_wait_start();
  {
    let ps = &data.parallel_submission[data.cur_ps_num];
    let fence = &ps.fence;
    // wait for GPU to execute all command lists, which was previously submitted by current parallel submission
    fence.set_event_on_completion(ps.fence_val, data.fence_event).unwrap();
    wait_for_single_object(data.fence_event, INFINITE);
  }
  ::perf_wait_end();
  ::perf_fillbuf_start();
  data.frame_index = data.swap_chain.swap_chain.get_current_back_buffer_index();
  //let tick=data.tick;
  let aspect=data.viewport.Height/data.viewport.Width;
  let (x,y) = (x as f32, y as f32);
  let (x,y) = (x-data.viewport.Width/2., y-data.viewport.Height/2.);
  let tick=f64::atan2(x as f64, y as f64);

  let (s,c)=f64::sin_cos(-tick); // f32 is too small for time counting
  let (s,c)=(s as f32, c as f32);
  
  let z=(f64::sin(data.tick*10.)*2.) as f32;

  let mut d=f32::sqrt(x*x+y*y);
  d = if d == 0. {1.} else {d};

  let mut wm: Matrix4<f32> = Matrix4::from(Matrix3::from(Basis3::from_axis_angle(&Vector3::new(y/d, x/d, 0.0),rad(d/200.))));
  wm.w.w = 1.;

  let mut wm_normal=wm;
  wm_normal.invert_self();
  wm_normal.transpose_self();
  
  let world_normal_matrix = matrix4_to_4x4(&wm_normal);

  let world_matrix = matrix4_to_4x4(&wm); //[[c, s, 0.,0.],[-s, c, 0.,0.],[0.,c ,1.,0.],[0.,0.,0.,1.],];

  let (vx,vy) = f64::sin_cos(data.tick*0.01);
  let (vx,vy) = (vx as f32, vy as f32);

  let view_matrix = matrix4_to_4x4(&data.camera.view_matrix());

  let proj_matrix = matrix4_to_4x4(&data.camera.projection_matrix());

  let (lx,ly) = f64::sin_cos(data.tick);
  let (lx,ly) = (lx as f32, ly as f32);

  let consts = Constants{ model: world_matrix, view: view_matrix, proj: proj_matrix, n_model: world_normal_matrix, light_pos: [lx,ly,-3.0]};

  crossbeam::scope(|scope| {
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
      submit_in_parallel(data, &consts).unwrap();
    });
  });
  ::perf_fillbuf_end();
  ::perf_clear_start();
  data.core.graphics_queue.execute_command_lists(&[&data.parallel_submission[data.cur_ps_num].clear_list]);

  //wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);

  ::perf_clear_end();
  ::perf_exec_start();
  {
    let ps = &data.parallel_submission[data.cur_ps_num];
    let cls: Vec<&D3D12GraphicsCommandList> = ps.gc_lists.iter().map(|&(ref clist, _)|clist).collect();
    data.core.graphics_queue.execute_command_lists(&cls[..]);
  }

  {
    let ps = &data.parallel_submission[data.cur_ps_num];
    ps.transition_list.reset(&ps.ps_allocator, None).unwrap();
    ps.transition_list.resource_barrier(
        &mut [*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
        D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT)]);
    ps.transition_list.close().unwrap();
    data.core.graphics_queue.execute_command_lists(&[&ps.transition_list]);
  }

  let fence_val_next=data.core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;
  data.parallel_submission[data.cur_ps_num].fence_val = fence_val_next;
  data.core.graphics_queue.signal(&data.parallel_submission[data.cur_ps_num].fence, fence_val_next);
  data.cur_ps_num = (data.cur_ps_num+1) % data.parallel_submission.len();
  ::perf_exec_end();
  ::perf_present_start();
  match data.swap_chain.swap_chain.present(0, 0) {
    Err(hr) => {
      dump_info_queue(data.core.info_queue.as_ref());
      panic!("Present failed with 0x{:x}", hr);
    },
    _ => (),
  }
  ::perf_present_end();
  data.tick += 0.01;
  dump_info_queue(data.core.info_queue.as_ref());
}

fn command_list_rt_clear(command_list: &D3D12GraphicsCommandList, command_allocator: &D3D12CommandAllocator, data: &AppData) -> HResult<()> {
  if cfg!(debug) {debug!("Command allocator reset")};
  command_allocator.reset().unwrap();
  if cfg!(debug) {debug!("Command list reset")};
  command_list.reset(command_allocator, Some(&data.pipeline_state)).unwrap();


  if cfg!(debug) {debug!("Resource barrier")};
  command_list.resource_barrier(
      &mut [*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
      D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET).flags(D3D12_RESOURCE_BARRIER_FLAG_NONE)]);

  let mut rtvh=data.swap_chain.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  rtvh.ptr += (data.frame_index as SIZE_T)*data.swap_chain.rtv_dsize;
  let dsvh = data.dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  if cfg!(debug) {debug!("OM set render targets")};
  command_list.om_set_render_targets(1, &rtvh, Some(&dsvh));

  let clear_color = [0.01, 0.01, 0.15, 1.0];
  if cfg!(debug) {debug!("clear render target view")};
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

fn create_static_sampler_gps<T: VertexFormat>(core: &DXCore) -> HResult<(D3D12PipelineState,D3D12RootSignature)> {
  let static_samplers = vec![
    D3D12_STATIC_SAMPLER_DESC {
      Filter: D3D12_FILTER_ANISOTROPIC,
      AddressU: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
      AddressV: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
      AddressW: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
      MipLODBias: 0.0,
      MaxAnisotropy: 16,
      ComparisonFunc: D3D12_COMPARISON_FUNC_LESS_EQUAL,
      BorderColor: D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE,
      MinLOD: 0.0,
      MaxLOD: D3D12_FLOAT32_MAX,
      ShaderRegister: 0,
      RegisterSpace: 0,
      ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
    },
  ];

  let desc_ranges = vec![
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: 0,
    },
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: 1,
    },
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: 0,
    },
  ];

  let rs_parms = vec![
    *RootParameter::descriptor_table(&desc_ranges[0..1], D3D12_SHADER_VISIBILITY_PIXEL),
    *RootParameter::cbv(0, 0, D3D12_SHADER_VISIBILITY_ALL),
//    *RootParameter::descriptor_table(&desc_ranges[2..3], D3D12_SHADER_VISIBILITY_PIXEL),
  ];
  debug!("Size of D3D12_ROOT_PARAMETER:{}", ::std::mem::size_of_val(&rs_parms[0]));

  let rsd=D3D12_ROOT_SIGNATURE_DESC{
    NumParameters: rs_parms.len() as UINT,
    pParameters: rs_parms[..].as_ptr(),
    NumStaticSamplers: static_samplers.len() as u32,
    pStaticSamplers: static_samplers.as_ptr(),
    Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
  };

  debug!("Serialize root signature");
  let blob=try!(d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1));

  debug!("Root signature");
  let root_sign = try!(core.dev.create_root_signature(0, blob_as_slice(&blob)));

  let compile_flags=0;
  debug!("Vertex shader");
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  debug!("Pixel shader");
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();

  let input_elts_desc = T::generate(0);

  let mut pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
    pRootSignature: root_sign.iptr() as *mut _,
    VS: D3D12_SHADER_BYTECODE {
      pShaderBytecode: vshader.as_ptr() as *const _,
      BytecodeLength: vshader.len() as SIZE_T,
    },
    PS: D3D12_SHADER_BYTECODE {
      pShaderBytecode: pshader.as_ptr() as *const _ ,
      BytecodeLength: pshader.len() as SIZE_T, 
    },
    RasterizerState: D3D12_RASTERIZER_DESC {
      CullMode: D3D12_CULL_MODE_NONE,
      .. rasterizer_desc_default()
    },
    InputLayout: D3D12_INPUT_LAYOUT_DESC {
      pInputElementDescs: input_elts_desc.as_ptr(),
      NumElements: input_elts_desc.len() as u32,
    },
    PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
    NumRenderTargets: 1,
    //RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    DSVFormat: DXGI_FORMAT_D32_FLOAT,
    .. graphics_pipeline_state_desc_default()
  };
  pso_desc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM_SRGB;

  let ps = try!(core.dev.create_graphics_pipeline_state(&pso_desc));
  Ok((ps, root_sign))
}

pub fn on_resize(data: &mut AppData, w: u32, h: u32, c: u32) {
   debug!("Resize to {},{},{}.",w,h,c);
   if w==0 || h==0 {
      data.minimized = true;
      return;
   };
   data.minimized = false;
   wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);
   drop_render_targets(&mut data.swap_chain);
   let desc = data.swap_chain.swap_chain.get_desc().unwrap();
   let res = data.swap_chain.swap_chain.resize_buffers(0, w, h, DXGI_FORMAT_UNKNOWN, 0);
   match res {
     Err(hr) => {
       dump_info_queue(data.core.info_queue.as_ref());
       error!("resize_buffers returned 0x{:x}", hr);
     },
     _ => (),
   };
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
    for i in 0..threads {
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
  let cbsize = unsafe {
    let cbuf_data: Constants = ::std::mem::uninitialized();
    let cbsize = mem::size_of_val(&cbuf_data);
    mem::forget(cbuf_data);
    cbsize
  };

  let alignment = (cbsize+255) & !255; // TODO: Remove duplication. This is used in D3D12_CONSTANT_BUFFER_VIEW_DESC also.

  let buffer_size = (object_count as usize)*alignment;
  // create resource to hold all constant buffers
  let cbuf = try!(core.dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(buffer_size as u64), D3D12_RESOURCE_STATE_GENERIC_READ, None));
  let mut ptr: *mut u8 = ptr::null_mut();
  unsafe {
    try!(cbuf.map(0, None, Some(&mut ptr)));
    debug!("cbuf.iptr(): 0x{:x}, ptr: 0x{:x}", cbuf.iptr() as usize, ptr as usize);
  }
  let cpu_buf_ptr = PtrConstants(ptr as *mut Constants);

  let mut clists = vec![];
  for i in 0 .. thread_count {
    debug!("create_command_allocator");
    let callocator = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
    debug!("create_command_list");
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
          let num_vtx = data.vertex_buffer_view.SizeInBytes/data.vertex_buffer_view.StrideInBytes;

          for i in start .. start+count {
            let mut c: Constants = consts.clone();
            let (pos, dir, spd) = data.rot_spd[i].clone();
            let rot = Basis3::from_axis_angle(&dir, rad(spd*(data.tick as f32)));
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
            clist.draw_instanced(num_vtx, 1, 0, 0);
          }
          try!(clist.close());
          Ok(())
        });
      jhs.push(jh);
    }
  });
  Ok(())
}

