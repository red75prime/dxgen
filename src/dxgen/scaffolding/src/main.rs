#![feature(optin_builtin_traits)]
#![feature(clone_from_slice)]

//#[macro_use] extern crate gfx;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate libc;
extern crate winapi;
extern crate d3d12_safe;
//extern crate dxguid_sys;
extern crate dxgi_sys;
extern crate kernel32;
//extern crate d3d12_sys;
extern crate rand;
extern crate clock_ticks;
extern crate cgmath;

#[macro_use] mod macros;
mod create_device;
mod window;
mod structwrappers;
mod utils;
mod gfx_d3d12;
mod shape_gen;

use winapi::*;
use d3d12_safe::*;
use create_device::*;
use kernel32::{CreateEventW, WaitForSingleObject};
use std::ptr;
use std::fmt;
use std::mem;
use std::cell::RefCell;
use std::rc::Rc;
use window::*;
use structwrappers::*;
use utils::*;
use clock_ticks::*;
use cgmath::*;
use shape_gen::*;

#[link(name="d3dcompiler")]
extern {}

const FRAME_COUNT : u32 = 2;

type M4 = [[f32;4];4];

#[derive(Clone,Copy,Debug)]
struct Vertex {
  pos: [f32;3],
  color: [f32;3],
  texc0: [f32;2],
  norm: [f32;3],
}

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

#[repr(C)]
struct Constants {
  model: M4,
  view: M4,
  proj: M4,
  n_model: M4,
  light_pos: [f32;3],
}

fn main() {
  env_logger::init().unwrap();

  let vm = Matrix4::look_at(&Point3::new(0., 0., -3.), &Point3::new(0., 0., 0.), &Vector3::new(0., 1., 0.));
  println!("Look at: {:?}", vm);
  let pm = cgmath::perspective(deg(30.), 1.5, 0.1, 10.);
  println!("Persp: {:?}", pm);

  let factory: DXGIFactory4 = create_dxgi_factory2().expect("Cannot create DXGIFactory1. No can do.");
  let mut i=0;
  let mut adapters = vec![];
  while let Ok(adapter)=factory.enum_adapters1(i) {
    let descr=adapter.get_desc1().unwrap();
    println!("Adapter {}: {}", i, wchar_array_to_string_lossy(&descr.Description));
    println!("   Dedicated video memory: {}MiB", descr.DedicatedVideoMemory/1024/1024);
    adapters.push(adapter);
    i+=1;
  }
  let res: Vec<_> = adapters.into_iter().map(|a|::std::thread::spawn(move ||main_prime(a))).collect();
  for jh in res {
    let _ = jh.join();
  }
}

fn main_prime(adapter: DXGIAdapter1) {
  let descr=adapter.get_desc1().unwrap();
  let title=format!("D3D12 Hello, rusty world! ({})", wchar_array_to_string_lossy(&descr.Description));

  let wnd=create_window(&title, 512, 256);
  let data=
    match create_appdata(&wnd, Some(&adapter)) { //Some(&factory.enum_adapters1(2).unwrap().query_interface().unwrap())) {
      Ok(appdata) => {
        Rc::new(RefCell::new(appdata))
      },
      Err(mut iq) => {
        dump_info_queue(&mut iq);
        return ();
      },
    };
  
  let rdata=data.clone();
  wnd.set_resize_fn(Box::new(move |w,h,c|{
    on_resize(&mut *rdata.borrow_mut(), w, h, c);
  }));

  let mut x=0;
  let mut y=0;
  let mut frame_count = 0;
  let mut start = precise_time_s();
  for mmsg in wnd.poll_events() {
    if let Some(msg) = mmsg {
      //println!("{} ", msg.message);
      match msg.message {
        WM_MOUSEMOVE => {
          x=GET_X_LPARAM(msg.lParam);
          y=GET_Y_LPARAM(msg.lParam);
        },
        _ => {},
      } 
    } else {
      on_render(&mut *data.borrow_mut(), x, y);
      //std::thread::sleep_ms(1);
      frame_count += 1;
      let now = precise_time_s();
      if now<start || now>=(start+1.0) {
        start = now;
        print!("FPS: {}   \r" , frame_count);
        ::std::io::Write::flush(&mut ::std::io::stdout());
        frame_count = 0;
      }
    }
  }
  // Application should exit fullscreen state before terminating. 
  data.borrow().swap_chain.set_fullscreen_state(0, None).unwrap();
  // copy info queue
  let iq=data.borrow().iq.clone();
  // release resources
  wnd.set_resize_fn(Box::new(|_,_,_|{}));
  wait_for_prev_frame(&mut *data.borrow_mut());
  drop(data);
  // maybe debug layer has something to say
  dump_info_queue(&iq);  
}

fn on_render(data: &mut AppData, x: i32, y: i32) {
  use std::f64; 

  wait_for_prev_frame(data);
  //let tick=data.tick;
  data.tick += 0.01;
  let aspect=data.viewport.Height/data.viewport.Width;
  let (x,y) = (x as f32, y as f32);
  let (x,y) = (x-data.viewport.Width/2., y-data.viewport.Height/2.);
  let tick=f64::atan2(x as f64, y as f64);

  let (s,c)=f64::sin_cos(-tick); // f32 is too small for time counting
  let (s,c)=(s as f32, c as f32);
//  let vtc=vec![
//    Vertex {pos: [(0.5*s+0.0*c), (0.5*(-c)+0.0*s), 0.0],  color: [1.0,0.0,0.0], texc0: [1.0, 1.0], norm: [0.0, 0.0, -1.0]},
//    Vertex {pos: [(-0.5*s-0.5*c), (-0.5*(-c)-0.5*s), 0.0],color: [0.0,0.0,1.0], texc0: [0.0, 1.0], norm: [0.0, 0.0, -1.0]},
//    Vertex {pos: [(-0.5*s+0.5*c), (-0.5*(-c)+0.5*s), 0.0],color: [0.0,1.0,0.0], texc0: [1.0, 0.0], norm: [0.0, 0.0, -1.0]},
//    Vertex {pos: [(-1.0*s+0.0*c), (-1.0*(-c)+0.0*s), 0.0],color: [0.0,0.5,0.5], texc0: [0.0, 0.0], norm: [0.0, 0.0, -1.0]},
//  ];
//
//  unsafe {
//    upload_into_buffer(&data.vertex_buffer, &vtc[..]);
//  }
  let z=(f64::sin(data.tick*10.)*2.) as f32;

  let mut d=f32::sqrt(x*x+y*y);
  d = if d == 0. {1.} else {d};

  let mut wm: Matrix4<f32> = Matrix4::from(Matrix3::from(Basis3::from_axis_angle(&Vector3::new(y/d, x/d, 0.0),rad(d/200.))));
  wm.w.w = 1.;

  let mut wm_normal=wm;
  wm_normal.invert_self();
  wm_normal.transpose_self();
  
  let world_normal_matrix = matrix4_to_4x4(&wm_normal);

//  wm.x.w=rand::random::<f32>()-0.5;
//  wm.y.w=rand::random::<f32>()-0.5;
//  wm.z.w=(rand::random::<f32>()-0.5)*4.;
  
  let world_matrix = matrix4_to_4x4(&wm); //[[c, s, 0.,0.],[-s, c, 0.,0.],[0.,c ,1.,0.],[0.,0.,0.,1.],];

  
//  let vm = Matrix4::look_at(&Point3::new(0., 0., 3.), &Point3::new(0., 0., 0.), &Vector3::new(0., 1., 0.));

//  let view_matrix:  [[f32;4];4] = vm.into();
  let view_matrix:  [[f32;4];4] = [[1., 0., 0.,0.],[0., 1., 0.,0.],[0.,0.,1.,3.],[0.,0.,0.,1.],];

//  println!("view: {:?}", &view_matrix);
  
//  let pm = cgmath::perspective(deg(30.), aspect, 0.1, 20.);
  let zfar=20.;
  let znear=0.1;
  let q = zfar/(zfar-znear);
  let proj_matrix: [[f32;4];4] = [[aspect, 0., 0., 0.,], [0., 1., 0., 0., ], [0., 0., q, -q*znear], [0., 0., 1., 0.], ];

//  println!("proj: {:?}", &proj_matrix);
  let (lx,ly) = f64::sin_cos(data.tick);
  let (lx,ly) = (lx as f32, ly as f32);

  let consts = Constants{ model: world_matrix, view: view_matrix, proj: proj_matrix, n_model: world_normal_matrix, light_pos: [lx,ly,0.0]};
  unsafe {
    upload_into_buffer(&data.constant_buffer, &[consts]);
  };

  populate_command_list(&data.command_list, &data.command_allocator, &data.srv_heap, data);

  data.command_queue.execute_command_lists(&[&data.command_list]);
  match data.swap_chain.present(0, 0) {
    Err(hr) => {
      dump_info_queue(&data.iq);
      panic!("Present failed with 0x{:x}", hr);
    },
    _ => (),
  }
  dump_info_queue(&data.iq);
}

// TODO: split this into something more manageable.
struct AppData {
  viewport: D3D12_VIEWPORT,
  scissor_rect: D3D12_RECT,
  swap_chain: DXGISwapChain3,
  device: D3D12Device,
  render_targets: Vec<D3D12Resource>,
  command_allocator: D3D12CommandAllocator,
  command_queue: D3D12CommandQueue,
  root_signature: D3D12RootSignature,
  rtv_heap: D3D12DescriptorHeap,
  srv_heap: D3D12DescriptorHeap,
  sam_heap: D3D12DescriptorHeap,
  dsd_heap: D3D12DescriptorHeap,
  pipeline_state: D3D12PipelineState,
  command_list: D3D12GraphicsCommandList,
  rtv_descriptor_size: SIZE_T,
  vertex_buffer: D3D12Resource,
  vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
  constant_buffer: D3D12Resource,
  tex_resource: D3D12Resource,
  depth_stencil: Option<D3D12Resource>,
  frame_index: UINT,
  fence_event: HANDLE,
  fence: D3D12Fence,
  fence_value: UINT64,
  tick: f64,
  iq: D3D12InfoQueue,
}

impl fmt::Debug for AppData {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f,"struct AppData")
  }
}

fn wait_for_prev_frame(data: &mut AppData) {
  let fence_value=data.fence_value;
  data.command_queue.signal(&mut data.fence, fence_value).unwrap();
  data.fence_value += 1;
  if data.fence.get_completed_value() < fence_value {
    match data.fence.set_event_on_completion(fence_value, data.fence_event) {
      Ok(_) => (),
      Err(hr) => {
        dump_info_queue(&mut data.iq);
        panic!("set_event_on_completion error: 0x{:x}",hr);
      },
    }
    unsafe { WaitForSingleObject(data.fence_event, INFINITE) };
  }
  data.frame_index = data.swap_chain.get_current_back_buffer_index();
}

fn populate_command_list(command_list: &D3D12GraphicsCommandList, command_allocator: &D3D12CommandAllocator, srv_heap: &D3D12DescriptorHeap, data: &AppData) {
  let d_info=false;
  if d_info {debug!("Command allocator reset")};
  command_allocator.reset().unwrap();
  if d_info {debug!("Command list reset")};
  command_list.reset(&data.command_allocator, Some(&data.pipeline_state)).unwrap();

  if d_info {debug!("Set graphics root signature")};
  command_list.set_graphics_root_signature(&data.root_signature);

  if d_info {debug!("Set descriptor heaps")};
  command_list.set_descriptor_heaps(&[srv_heap, &data.sam_heap]);

  if d_info {debug!("Set viepowrts")};
  command_list.rs_set_viewports(&[data.viewport]);
  if d_info {debug!("Set scissor rects")};
  command_list.rs_set_scissor_rects(&[data.scissor_rect]);

  if d_info {debug!("Resource barrier")};
  command_list.resource_barrier(
      &mut [*ResourceBarrier::transition(&data.render_targets[data.frame_index as usize],
      D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET).flags(D3D12_RESOURCE_BARRIER_FLAG_NONE)]);

  let mut rtvh=data.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  rtvh.ptr += (data.frame_index as SIZE_T)*data.rtv_descriptor_size;
  let dsvh = data.dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  if d_info {debug!("OM set render targets")};
  command_list.om_set_render_targets(1, &rtvh, Some(&dsvh));

  let mut clear_color = [0.01, 0.01, 0.15, 1.0];
  if d_info {debug!("clear render target view")};
  command_list.clear_render_target_view(rtvh, &clear_color, &[]);
  command_list.clear_depth_stencil_view(dsvh, D3D12_CLEAR_FLAG_DEPTH, 1.0, 0, &[]);
  
  let mut gpu_dh=srv_heap.get_gpu_descriptor_handle_for_heap_start();
  if d_info {debug!("Set graphics root descriptor table 0")};
  command_list.set_graphics_root_descriptor_table(0, gpu_dh);

  if d_info {debug!("Set graphics root descriptor table 1")};
//  gpu_dh.ptr += data.device.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
  command_list.set_graphics_root_descriptor_table(1, gpu_dh);

  if d_info {debug!("Set graphics root descriptor table 2")};
  command_list.set_graphics_root_descriptor_table(2, data.sam_heap.get_gpu_descriptor_handle_for_heap_start());

//  command_list.set_graphics_root_shader_resource_view(0, data.tex_resource.get_gpu_virtual_address());

  command_list.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
  command_list.ia_set_vertex_buffers(0, Some(&[data.vertex_buffer_view]));

  let num_vtx=data.vertex_buffer_view.SizeInBytes/data.vertex_buffer_view.StrideInBytes;
  command_list.draw_instanced(num_vtx, 1, 0, 0);
  
  command_list.resource_barrier(
      &mut [*ResourceBarrier::transition(&data.render_targets[data.frame_index as usize],
      D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT)]);

  match command_list.close() {
    Ok(_) => {},
    Err(hr) => {
      dump_info_queue(&data.iq);
      panic!("command_list.close() failed with 0x{:x}", hr);
    },
  }
}

fn on_resize(data: &mut AppData, w: u32, h: u32, c: u32) {
   wait_for_prev_frame(data);
   drop_render_targets(data);
   let desc=data.swap_chain.get_desc().unwrap();
   let res=data.swap_chain.resize_buffers(0, w, h, desc.BufferDesc.Format, desc.Flags);
   println!("Resize to {},{}. Result:{:?}",w,h,res);
   match res {
     Err(hr) => {
       dump_info_queue(&data.iq);
       error!("resize_buffers returned 0x{:x}", hr);
     },
     _ => (),
   };
   reaquire_render_targets(data).unwrap();
   // create new depth stencil
   let ds_format = DXGI_FORMAT_D32_FLOAT;
   data.depth_stencil = Some(create_depth_stencil(w as u64, h as u32, ds_format, &data.device, &data.dsd_heap, 0).unwrap());
  
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

fn drop_render_targets(data: &mut AppData) {
  data.render_targets.truncate(0);
}

fn reaquire_render_targets(data: &mut AppData) -> HResult<()> {
  wait_for_prev_frame(data);
  let mut cdh=data.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  data.render_targets.truncate(0);
  for i in 0..FRAME_COUNT {
    let buf=try!(data.swap_chain.get_buffer::<D3D12Resource>(i as u32));
    data.device.create_render_target_view(Some(&buf), None, cdh);
    cdh.ptr += data.rtv_descriptor_size;
    data.render_targets.push(buf);
  }
  Ok(())
}

unsafe fn upload_into_buffer<T>(buf: &D3D12Resource, data: &[T]) {
  let sz = mem::size_of_val(data);
//  debug!("Map buffer");
  let mut p_buf: *mut u8 = ptr::null_mut();
  buf.map(0, None, Some(&mut p_buf)).unwrap();
  let buf_slice=std::slice::from_raw_parts_mut(p_buf, sz);
  buf_slice.clone_from_slice(std::slice::from_raw_parts(data.as_ptr() as *const u8, sz)); 
//  debug!("Unmap buffer");
  buf.unmap(0, None);
}

fn upload_into_texture(clist: &D3D12GraphicsCommandList, tex: &D3D12Resource, w: usize, h: usize, data: &[u32]) -> HResult<D3D12Resource> {
  debug!("upload_into_texture");
  debug!("get_desc tex.iptr(): 0x{:x}, vtbl: 0x{:x}, vtbl[0]: 0x{:x}", tex.iptr() as usize, unsafe{((*tex.iptr()).lpVtbl) as usize}, unsafe{(*(*tex.iptr()).lpVtbl).QueryInterface as usize});
  let desc = tex.get_desc();
  debug!("get_device");
  let dev: D3D12Device = try!(tex.get_device());
  let mut num_rows = [0];
  let mut row_size_bytes = [0];
  let mut total_size_bytes = 0;
  let mut psfp: [D3D12_PLACED_SUBRESOURCE_FOOTPRINT; 1] = [unsafe{ ::std::mem::uninitialized() }];
  debug!("get_copyable_footprints");
  // Microsoft sample application uses HeapAlloc for footprint and other arguments
  // TODO: investigate
  dev.get_copyable_footprints(&desc, 0, 0, Some(&mut psfp), Some(&mut num_rows), Some(&mut row_size_bytes), Some(&mut total_size_bytes));
  let rows = num_rows[0] as usize;
  let row_len = (row_size_bytes[0]/4) as usize;
  let row_pitch = (psfp[0].Footprint.RowPitch/4) as usize;
  let total_len = (total_size_bytes/4) as usize;
  debug!("Placed subres. footprint:{:?}", psfp[0]);
  debug!("Rows:{}, Row len:{}, Row pitch:{}  Total len:{}", rows, row_len, row_pitch, total_len);

  debug!("create_committed_resource");
  let res_buf=try!(dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(total_size_bytes), D3D12_RESOURCE_STATE_GENERIC_READ, None));
  res_buf.set_name("Temporary texture buffer".into());  

  let mut temp_buf = Vec::with_capacity(total_len);
  unsafe {
    temp_buf.set_len(total_len);
  };
  for y in 0..rows {
    &temp_buf[y*row_pitch..y*row_pitch+w].clone_from_slice(&data[y*w..y*w+w]);
  }

  unsafe {
    upload_into_buffer(&res_buf, &temp_buf[..]);
  };

  let dest = texture_copy_location_index(&tex, 0);
  let src = texture_copy_location_footprint(&res_buf, &psfp[0]);
  clist.copy_texture_region(&dest, 0, 0, 0, &src, None); 

  // TODO: make wrapper for D3D12GraphicsCommand list to keep referenced resources alive
  Ok(res_buf)
}

fn create_appdata(wnd: &Window, adapter: Option<&DXGIAdapter1>) -> Result<AppData,D3D12InfoQueue> {
  let w=512.;
  let h=256.;
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

  debug!("Debug");
  let debug=get_debug_interface().unwrap();
  debug.enable_debug_layer();

  debug!("Factory");
  let factory: DXGIFactory4 = create_dxgi_factory2().unwrap();
  debug!("Device");
  let dev = 
    match d3d12_create_device(adapter, D3D_FEATURE_LEVEL_11_0) {
      Ok(dev) => dev,
      Err(hr) => {
        panic!("create device failes with 0x{:x}", hr)
//        warn!("Fallback to warp adapter");
//        debug!("Warp");
//        let warp: DXGIAdapter = factory.enum_warp_adapter().unwrap();
//        d3d12_create_device(Some(&warp), D3D_FEATURE_LEVEL_11_0).expect("Cannot create warp device")
      },
    };
  debug!("LUID");
  let luid=dev.get_adapter_luid();
  debug!("Adapter LUID {:?}", luid);

  debug!("Info queue");
  let info_queue: D3D12InfoQueue = dev.query_interface().unwrap();

  let mut opts: D3D12_FEATURE_DATA_D3D12_OPTIONS = unsafe{ mem::uninitialized::<_>() };
  debug!("Check feature support: options");
  dev.check_feature_support_options(&mut opts).unwrap();
  info!("{:#?}",opts);

  let fl_array=[D3D_FEATURE_LEVEL_9_1, D3D_FEATURE_LEVEL_9_2, D3D_FEATURE_LEVEL_9_3, 
                D3D_FEATURE_LEVEL_10_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_11_0, 
                D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_12_0,
                D3D_FEATURE_LEVEL_12_1, ];
  let mut feat_levels=D3D12_FEATURE_DATA_FEATURE_LEVELS {
      NumFeatureLevels: fl_array.len() as UINT,
      pFeatureLevelsRequested: fl_array.as_ptr(),
      MaxSupportedFeatureLevel: D3D_FEATURE_LEVEL_9_1,
    };
  debug!("Check feature support: levels");
  dev.check_feature_support_feature_levels(&mut feat_levels).unwrap();
  info!("Max supported feature level: {:?}", feat_levels.MaxSupportedFeatureLevel);

  let qd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
    Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
    Priority: 0,
    NodeMask: 0,
  };
  debug!("Command queue");
  let cqueue=dev.create_command_queue(&qd).unwrap();

  let mut scd=DXGI_SWAP_CHAIN_DESC{
    BufferCount: FRAME_COUNT,
    BufferDesc: DXGI_MODE_DESC{
        Width: w as UINT,
        Height: h as UINT,
        RefreshRate: DXGI_RATIONAL {Numerator:0, Denominator: 0},
        Format: DXGI_FORMAT_R8G8B8A8_UNORM,
        ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED,
        Scaling: DXGI_MODE_SCALING_UNSPECIFIED,
      },
    SampleDesc: DXGI_SAMPLE_DESC {Count:1, Quality: 0,},
    BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
    SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
    OutputWindow: hwnd,
    Windowed: 1,
    Flags: 0,
  };
  debug!("Swap chain");
  let swap_chain: DXGISwapChain3 = 
    match factory.create_swap_chain(&cqueue, &mut scd) {
      Err(hr) => {
        println!("create_swap_chain failed with 0x{:x}",hr);
        return Err(info_queue.clone());
      },
      Ok(i) => 
        i.query_interface().unwrap(),
    };
//  info!("Maximum frame latency: {}", swap_chain.get_maximum_frame_latency().unwrap());
//  debug!("Swap chain: get_maximum_frame_latency");
  //println!("{:?}", &scd);
  let frame_index=swap_chain.get_current_back_buffer_index();
  println!("Frame index: {}", &frame_index);
  let rtvhd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: FRAME_COUNT,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  debug!("RTV descriptor heap");
  let rtvheap=dev.create_descriptor_heap(&rtvhd).unwrap(); 
  let rtvdsize=dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_RTV) as SIZE_T;
  let mut cdh=rtvheap.get_cpu_descriptor_handle_for_heap_start();
  let mut render_targets=vec![];
  for i in 0..FRAME_COUNT {
    let buf=swap_chain.get_buffer::<D3D12Resource>(i as u32).unwrap();
    dev.create_render_target_view(Some(&buf), None, cdh);
    cdh.ptr += rtvdsize;
    render_targets.push(buf);
  }

  let srv_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 2, // first for texture, second for constants
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("SRV descriptor heap");
  let srv_heap=dev.create_descriptor_heap(&srv_hd).unwrap();

  let sam_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("Sampler descriptor heap");
  let sam_heap=dev.create_descriptor_heap(&sam_hd).unwrap();

  let dsd_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let dsd_heap=dev.create_descriptor_heap(&dsd_hd).unwrap();

  let sampler_desc = D3D12_SAMPLER_DESC {
    Filter: D3D12_FILTER_MIN_MAG_MIP_LINEAR,
    AddressU: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
    AddressV: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
    AddressW: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
    MinLOD: 0.0,
    MaxLOD: D3D12_FLOAT32_MAX,
    MipLODBias: 0.0,
    MaxAnisotropy: 16,
    ComparisonFunc: D3D12_COMPARISON_FUNC_ALWAYS,
    BorderColor: [1.0,1.0,1.0,1.0],
  };
  debug!("Sampler");
  dev.create_sampler(&sampler_desc, sam_heap.get_cpu_descriptor_handle_for_heap_start());

  debug!("Command allocator");
  let callocator=dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).unwrap();

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

  let mut rs_parms = vec![
    *RootParameter::descriptor_table(&desc_ranges[0..1], D3D12_SHADER_VISIBILITY_PIXEL),
    *RootParameter::descriptor_table(&desc_ranges[1..2], D3D12_SHADER_VISIBILITY_ALL),
    *RootParameter::descriptor_table(&desc_ranges[2..3], D3D12_SHADER_VISIBILITY_PIXEL),
  ];
  debug!("Size of D3D12_ROOT_PARAMETER:{}", ::std::mem::size_of_val(&rs_parms[0]));

  let rsd=D3D12_ROOT_SIGNATURE_DESC{
    NumParameters: rs_parms.len() as UINT,
    pParameters: rs_parms[..].as_ptr(),
    NumStaticSamplers: 0,
    pStaticSamplers: ptr::null(),
    Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
  };

  debug!("Serialize root signature");
  let blob=try!(d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1)
        .map_err(|hr|{
          error!("HRESULT=0x{:x}",hr);
          info_queue.clone()}));

  debug!("Root signature");
  let root_sign=
    unsafe{
      let blob_slice:&[u8]=::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8, blob.get_buffer_size() as usize);
      let root_sign=dev.create_root_signature(0, blob_slice).unwrap();
      root_sign
    };
  let compile_flags=0;
  debug!("Vertex shader");
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  debug!("Pixel shader");
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();

  let w_pos=str_to_cstring("POSITION");
  let w_color=str_to_cstring("COLOR");
  let w_texc0=str_to_cstring("TEXCOORD");
  let w_norm=str_to_cstring("NORMAL");
  let input_elts_desc=[
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_pos.as_ptr(), 
      SemanticIndex: 0,
      Format: DXGI_FORMAT_R32G32B32_FLOAT, 
      InputSlot: 0, 
      AlignedByteOffset: offset_of!(Vertex, pos) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_color.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32B32_FLOAT, 
      InputSlot: 0, 
      AlignedByteOffset: offset_of!(Vertex, color) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_texc0.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32_FLOAT, 
      InputSlot: 0,
      AlignedByteOffset: offset_of!(Vertex, texc0) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_norm.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32B32_FLOAT, 
      InputSlot: 0,
      AlignedByteOffset: offset_of!(Vertex, norm) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
  ];
  let def_stencil_op_desc=D3D12_DEPTH_STENCILOP_DESC{
      StencilFunc: D3D12_COMPARISON_FUNC_ALWAYS,
      StencilDepthFailOp: D3D12_STENCIL_OP_KEEP,
      StencilPassOp: D3D12_STENCIL_OP_KEEP,
      StencilFailOp: D3D12_STENCIL_OP_KEEP,
    };
  let blend_desc_def=
    D3D12_RENDER_TARGET_BLEND_DESC{
      BlendEnable: 0,
      LogicOpEnable: 0,
      SrcBlend: D3D12_BLEND_ONE,
      DestBlend: D3D12_BLEND_ZERO,
      BlendOp: D3D12_BLEND_OP_ADD,
      SrcBlendAlpha: D3D12_BLEND_ONE,
      DestBlendAlpha: D3D12_BLEND_ZERO,
      BlendOpAlpha: D3D12_BLEND_OP_ADD,
      LogicOp: D3D12_LOGIC_OP_NOOP,
      RenderTargetWriteMask: 15,
    };

  let pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
    pRootSignature: root_sign.iptr() as *mut _,
    VS: D3D12_SHADER_BYTECODE{pShaderBytecode: vshader.as_ptr() as *const _ ,BytecodeLength: vshader.len() as SIZE_T, },
    PS: D3D12_SHADER_BYTECODE{pShaderBytecode: pshader.as_ptr() as *const _ ,BytecodeLength: pshader.len() as SIZE_T, },
    DS: D3D12_SHADER_BYTECODE{pShaderBytecode: ptr::null_mut() ,BytecodeLength: 0, },
    HS: D3D12_SHADER_BYTECODE{pShaderBytecode: ptr::null_mut() ,BytecodeLength: 0, },
    GS: D3D12_SHADER_BYTECODE{pShaderBytecode: ptr::null_mut() ,BytecodeLength: 0, },
    StreamOutput: D3D12_STREAM_OUTPUT_DESC {pSODeclaration: ptr::null(), NumEntries: 0, pBufferStrides: ptr::null(), NumStrides: 0, RasterizedStream: 0,},
    BlendState: D3D12_BLEND_DESC {AlphaToCoverageEnable: 0, IndependentBlendEnable: 0, 
        RenderTarget: [
          D3D12_RENDER_TARGET_BLEND_DESC{
            BlendEnable: 0,
            LogicOpEnable: 0,
            SrcBlend: D3D12_BLEND_ONE,
            DestBlend: D3D12_BLEND_ZERO,
            BlendOp: D3D12_BLEND_OP_ADD,
            SrcBlendAlpha: D3D12_BLEND_ONE,
            DestBlendAlpha: D3D12_BLEND_ZERO,
            BlendOpAlpha: D3D12_BLEND_OP_ADD,
            LogicOp: D3D12_LOGIC_OP_NOOP,
            RenderTargetWriteMask: D3D12_COLOR_WRITE_ENABLE_ALL.0 as u8,
          },
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
         ],},
    SampleMask: 0xffffffff,
    RasterizerState : D3D12_RASTERIZER_DESC{
      FillMode: D3D12_FILL_MODE_SOLID,
      CullMode: D3D12_CULL_MODE_NONE,
      FrontCounterClockwise: 0,
      DepthBias: D3D12_DEFAULT_DEPTH_BIAS as i32,
      SlopeScaledDepthBias: D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS,
      DepthBiasClamp: D3D12_DEFAULT_DEPTH_BIAS_CLAMP,
      DepthClipEnable: 1,
      MultisampleEnable: 0,
      AntialiasedLineEnable: 0,
      ForcedSampleCount: 0,
      ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF,
    },
    DepthStencilState : D3D12_DEPTH_STENCIL_DESC{
      DepthEnable: 1,
      DepthWriteMask: D3D12_DEPTH_WRITE_MASK_ALL,
      DepthFunc: D3D12_COMPARISON_FUNC_LESS,
      StencilEnable: 0,
      StencilReadMask: D3D12_DEFAULT_STENCIL_READ_MASK as u8,
      StencilWriteMask: D3D12_DEFAULT_STENCIL_READ_MASK as u8,
      FrontFace: def_stencil_op_desc,
      BackFace: def_stencil_op_desc,
      },
    InputLayout : D3D12_INPUT_LAYOUT_DESC{
      pInputElementDescs: input_elts_desc.as_ptr(),
      NumElements: input_elts_desc.len() as u32,
      },
    IBStripCutValue : D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED,
    PrimitiveTopologyType : D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
    NumRenderTargets : 1,
    RTVFormats : [DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, ],
    DSVFormat : DXGI_FORMAT_D32_FLOAT, 
    SampleDesc : DXGI_SAMPLE_DESC{Count:1, Quality: 0,},
    NodeMask : 0,
    CachedPSO : D3D12_CACHED_PIPELINE_STATE{pCachedBlob: ptr::null(), CachedBlobSizeInBytes: 0,},
    Flags : D3D12_PIPELINE_STATE_FLAG_NONE,         
  };
  debug!("Graphics pipeline state");
  let gps=match dev.create_graphics_pipeline_state(&pso_desc) {
      Ok(gps) => gps,
      Err(_) => {return Err(info_queue);},
    };
  debug!("Command list");
  let command_list : D3D12GraphicsCommandList = dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, Some(&gps)).unwrap();
  
  // ------------------- Vertex buffer resource init begin ----------------------------

//  let vtc=vec![
//    Vertex {pos: [0.0, 0.5, 2.0],   color: [1.0,0.0,0.0], texc0: [0.0, 0.16667], norm: [0.0, 0.0, -1.0]},
//    Vertex {pos: [0.5, -0.5, 2.0],  color: [0.0,1.0,0.0], texc0: [0.66667, 0.0], norm: [0.0, 0.0, -1.0]},
//    Vertex {pos: [-0.5, -0.5, 2.0], color: [0.0,0.0,1.0], texc0: [0.66667, 0.33333], norm: [0.0, 0.0, -1.0]},
//    Vertex {pos: [0.0, -1.0, 2.0],  color: [0.0,0.7,0.7], texc0: [1.0, 0.16667], norm: [0.0, 0.0, -1.0]},
//  ];

  let (_,vtc) = cube::<Vertex>(0.5);

  let vbuf_size = mem::size_of_val(&vtc[..]);

  let heap_prop = heap_properties_upload();
//  let heap_prop=dev.get_custom_heap_properties(0, D3D12_HEAP_TYPE_UPLOAD);

//  debug!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
//  debug!("Heap properties: {:#?}", &heap_prop);
  
  let res_desc = resource_desc_buffer(vbuf_size as u64);
  info!("Resource desc: {:#?}", &res_desc);

  debug!("Vertex buffer");
  let vbuf=try!(dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &res_desc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|info_queue.clone()));

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

  let cbuf = dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(cbsize as u64), D3D12_RESOURCE_STATE_GENERIC_READ, None).unwrap();
  unsafe {
    upload_into_buffer(&cbuf, &[cbuf_data]);
  };

  let cbview = 
    D3D12_CONSTANT_BUFFER_VIEW_DESC {
      BufferLocation: cbuf.get_gpu_virtual_address(),
      SizeInBytes: ((cbsize+255) & !255) as u32,
  };

  let srv_dsize=dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
  let mut const_dh=srv_heap.get_cpu_descriptor_handle_for_heap_start();
  const_dh.ptr += srv_dsize; // constant buffer descriptor is second in descriptor heap
  debug!("Create shader resource view: texture");
  dev.create_constant_buffer_view(Some(&cbview), const_dh);

  // ------------------- Constant buffer resource init end  -----------------------------

  // ------------------- Texture resource init begin  -----------------------------

  let tex_w=256usize;
  let tex_h=256usize;

  let tex_desc = resource_desc_tex2d_nomip(tex_w as u64, tex_h as u32, DXGI_FORMAT_R8G8B8A8_UNORM, D3D12_RESOURCE_FLAG_NONE);
  info!("Texture desc: {:#?}", &res_desc);

  debug!("Texture resource");
  let tex_resource=try!(dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, &tex_desc, D3D12_RESOURCE_STATE_COPY_DEST, None).map_err(|_|info_queue.clone()));
  
  let mut texdata: Vec<u32> = vec![0xff00ffff; tex_w*tex_h as usize];
  for v in &mut texdata[..] {
    *v=rand::random();
  }
  // temporary buffer should live until the start of command list execution
  //#[allow(unused_variables)]
  let _temp_buf=try!(upload_into_texture(&command_list, &tex_resource, tex_w, tex_h, &texdata[..]).map_err(|_|info_queue.clone()));

  command_list.resource_barrier(&mut [*ResourceBarrier::transition(&tex_resource, D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE)]);

  let mut srv_desc = shader_resource_view_tex2d_default(DXGI_FORMAT_R8G8B8A8_UNORM);

  let mut srv_dh=srv_heap.get_cpu_descriptor_handle_for_heap_start();
  debug!("Create shader resource view: texture");
  dev.create_shader_resource_view(Some(&tex_resource), Some(&srv_desc), srv_dh);

  // Data transfer from upload buffer into texture will be performed at execute_command_lists below.
  // ------------------- Texture resource init end  -----------------------------

  // ------------------- Depth stencil buffer init begin ------------------------
  
  let ds_format = DXGI_FORMAT_D32_FLOAT;
  let ds_res = try!(create_depth_stencil(w as u64, h as u32, ds_format, &dev, &dsd_heap, 0).map_err(|_|info_queue.clone()));

  // ------------------- Depth stencil buffer init end --------------------------

  debug!("fence");
  let fence=dev.create_fence(0, D3D12_FENCE_FLAG_NONE).unwrap();

  let fence_val=1u64;
  let fence_event = unsafe{ CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  debug!("fence_event");
  if fence_event == ptr::null_mut() {
    panic!("Cannot create event");
  }
  
  let mut ret=
    AppData {
      viewport: viewport,
      scissor_rect: sci_rect,
      swap_chain: swap_chain,
      device: dev,
      render_targets: render_targets,
      command_allocator: callocator,
      command_queue: cqueue,
      root_signature: root_sign,
      rtv_heap: rtvheap,
      srv_heap: srv_heap,
      sam_heap: sam_heap,
      dsd_heap: dsd_heap,
      pipeline_state: gps,
      command_list: command_list,
      rtv_descriptor_size: rtvdsize,
      vertex_buffer: vbuf,
      vertex_buffer_view: vbview,
      constant_buffer: cbuf,
      tex_resource: tex_resource,
      depth_stencil: Some(ds_res),
      frame_index: frame_index,
      fence_event: fence_event,
      fence: fence,
      fence_value: fence_val,
      tick: 0.0,
      iq: info_queue,
    };
  
  debug!("Command list Close");
  ret.command_list.close().unwrap();

  debug!("Command queue execute");
  ret.command_queue.execute_command_lists(&[&ret.command_list]);
  
  debug!("wait_for_prev_frame");
  wait_for_prev_frame(&mut ret);
  Ok(ret)
}

fn dump_info_queue(iq: &D3D12InfoQueue) {
  let iq=iq;
  let mnum=iq.get_num_stored_messages_allowed_by_retrieval_filter();
  //println!("Number of debug messages is {}", mnum);
  for i in 0..mnum {
    let mut sz = 0;
    let _ = iq.get_message(i, None,&mut sz);
    //info!("get_message returned {:?}", hr); // It is the case when hr!=0 and it's ok
    // create buffer to receive message
    let mut arr: Vec<u8> = Vec::with_capacity(sz as usize); 
    let mut sz1=sz; 
    unsafe {
      // get_message expects Option<&mut D3D12_MESSAGE> as second parameter
      // it should be Option<&[u8]>, but I don't have annotation for that yet.
      let _ = iq.get_message(i, Some(mem::transmute(arr.as_ptr())), &mut sz1); 
      assert_eq!(sz, sz1); // just to be sure. hr is Err(1) on success.
      // Reinterpret first chunk of arr as D3D12_MESSAGE, and byte-copy it into msg
      let msg: D3D12_MESSAGE = mem::transmute_copy(&(*arr.as_ptr()));
      // msg contains pointer into memory occupied by arr. arr should be borrowed now, but it is unsafe code.
      let cdescr = ::std::ffi::CStr::from_ptr(msg.pDescription as *const i8);
      let descr = String::from_utf8_lossy(cdescr.to_bytes()).to_owned();
      match msg.Severity {
        D3D12_MESSAGE_SEVERITY_CORRUPTION |
        D3D12_MESSAGE_SEVERITY_ERROR =>
          {error!("{:}", descr)},
        D3D12_MESSAGE_SEVERITY_WARNING =>
          {warn!("{:}", descr)},
        _ =>
          {debug!("{:}", descr)},
      }
    }
  };
  iq.clear_stored_messages();
}

fn create_depth_stencil(w: u64, h: u32, ds_format: DXGI_FORMAT, dev: &D3D12Device, 
                        dsd_heap: &D3D12DescriptorHeap, heap_offset: u32) -> HResult<D3D12Resource> {
  let offset = 
    if heap_offset == 0 { 0 } 
    else {
      dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_DSV)*heap_offset
    };
  let ds_desc = resource_desc_tex2d_nomip(w, h, ds_format, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);
  debug!("Depth stencil resource");
  let ds_res=try!(dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, &ds_desc, 
                                          D3D12_RESOURCE_STATE_DEPTH_WRITE, Some(&depth_stencil_clear_value_depth_f32())));

  let dsv_desc = depth_stencil_view_desc_tex2d_default(ds_format);
  let mut handle = dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  handle.ptr += offset as SIZE_T;
  dev.create_depth_stencil_view(Some(&ds_res), Some(&dsv_desc), handle);

  Ok(ds_res)
}

fn matrix4_to_4x4(m: &Matrix4<f32>) -> [[f32;4];4] {
  [ [m.x.x, m.y.x, m.z.x, m.w.x],
    [m.x.y, m.y.y, m.z.y, m.w.y],
    [m.x.z, m.y.z, m.z.z, m.w.z],
    [m.x.w, m.y.w, m.z.w, m.w.w] ]
}
