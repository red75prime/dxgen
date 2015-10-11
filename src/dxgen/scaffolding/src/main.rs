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
mod dxsems;
mod core;

use dxsems::VertexFormat;
use winapi::*;
use d3d12_safe::*;
use create_device::*;
use kernel32::{CreateEventW, WaitForSingleObject};
use std::ptr;
use std::fmt;
use std::mem;
use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use window::*;
use structwrappers::*;
use utils::*;
use clock_ticks::*;
use cgmath::*;
use shape_gen::*;
use core::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use rand::Rng;

#[link(name="d3dcompiler")]
extern {}

const FRAME_COUNT : u32 = 3;

type M3 = [[f32;3];3];
type M4 = [[f32;4];4];

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

fn main() {
  env_logger::init().unwrap();

  let atomic_boom = AtomicUsize::new(usize::max_value());
  let da_boom_p = atomic_boom.fetch_add(1, Ordering::Relaxed);
  let da_boom_2 = atomic_boom.fetch_add(1, Ordering::Relaxed);
  println!("0x{:x} then 0x{:x}", da_boom_p, da_boom_2);
  // It seems atomics don't panic on overflow

  let mut adapters_to_test = vec![];
  for arg in env::args() {
    if let Ok(n) = arg.parse::<u32>() {
      adapters_to_test.push(n);
    }
  }

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
    if adapters_to_test.len()==0 || adapters_to_test[..].contains(&i) {
      adapters.push(adapter);
    }
    i+=1;
  }

  let mutex=Arc::new(Mutex::new(()));
  let join_handles: Vec<_> = 
    adapters.into_iter()
      .map(|a|{
        let mutex = mutex.clone();
        ::std::thread::spawn(move||{
          main_prime(a, mutex)
        })
      }).collect();
  for jh in join_handles {
    let _ = jh.join();
  }
}

fn main_prime(adapter: DXGIAdapter1, mutex: Arc<Mutex<()>>) {

  let descr=wchar_array_to_string_lossy(&adapter.get_desc1().unwrap().Description);
  let title=format!("D3D12 Hello, rusty world! ({})", descr);
  let wnd=create_window(&title, 512, 256);

  {
    if let Ok(dev)=d3d12_create_device(Some(&adapter), D3D_FEATURE_LEVEL_12_1) {
      let format = DXGI_FORMAT_R8G8B8A8_UNORM_SRGB;
      let mut fsup = D3D12_FEATURE_DATA_FORMAT_SUPPORT {
        Format: format,
        Support1: D3D12_FORMAT_SUPPORT1_NONE,
        Support2: D3D12_FORMAT_SUPPORT2_NONE,
      };
      if let Ok(_) = dev.check_feature_support_format_support(&mut fsup) {
        let lock=mutex.lock();
        println!("{}", descr);
        println!("Format support for {:?} is {:x}, {:x}", format, fsup.Support1.0, fsup.Support2.0);
      }
    }
  }

  let data=
    match create_appdata(&wnd, Some(&adapter)) { 
      Ok(appdata) => {
        Rc::new(RefCell::new(appdata))
      },
      Err(iq) => {
        dump_info_queue(iq.map(|iq|iq).as_ref());
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
  let mut avg_fps = 0;
  let mut start = precise_time_s();
  let mut mstart = start;
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
      let frametime = now - mstart;
      mstart = now;
      print!("FPS: {:4.3}\tAvg. FPS: {}        \r" , 1.0/frametime, avg_fps);
      if now<start || now>=(start+1.0) {
        start = now;
        let _ = ::std::io::Write::flush(&mut ::std::io::stdout()); 
        avg_fps = frame_count;
        frame_count = 0;
      }
    }
  }
  // Application should exit fullscreen state before terminating. 
  data.borrow().swap_chain.swap_chain.set_fullscreen_state(0, None).unwrap();
  // copy info queue
  let iq=data.borrow().core.info_queue.clone();
  // release resources
  wnd.set_resize_fn(Box::new(|_,_,_|{}));
  {
    let data=data.borrow();
    wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);
  }
  drop(data);
  // maybe debug layer has something to say
  dump_info_queue(iq.map(|iq|iq).as_ref());
}

fn on_render(data: &mut AppData, x: i32, y: i32) {
  use std::f64; 

  wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);
  data.frame_index = data.swap_chain.swap_chain.get_current_back_buffer_index();
  //let tick=data.tick;
  data.tick += 0.01;
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

//  wm.x.w=rand::random::<f32>()-0.5;
//  wm.y.w=rand::random::<f32>()-0.5;
//  wm.z.w=(rand::random::<f32>()-0.5)*4.;
  
  let world_matrix = matrix4_to_4x4(&wm); //[[c, s, 0.,0.],[-s, c, 0.,0.],[0.,c ,1.,0.],[0.,0.,0.,1.],];

  
  let mut vm = Matrix4::look_at(&Point3::new(0., 0., -3.), &Point3::new(0., 0., 0.), &Vector3::new(0., 1., 0.));
  lhs_to_rhs(&mut vm);
  let view_matrix = matrix4_to_4x4(&vm);

//  println!("view: {:?}", &view_matrix);
  
  let mut pm = cgmath::perspective(deg(90.), 1.0/aspect, 0.1, 20.);
  //lhs_to_rhs(&mut pm);
  let proj_matrix = matrix4_to_4x4(&pm);

//  println!("proj: {:?}", &proj_matrix);
  let (lx,ly) = f64::sin_cos(data.tick);
  let (lx,ly) = (lx as f32, ly as f32);

  let consts = Constants{ model: world_matrix, view: view_matrix, proj: proj_matrix, n_model: world_normal_matrix, light_pos: [lx,ly,0.0]};
  unsafe {
    upload_into_buffer(&data.constant_buffer, &[consts.clone()]);
  };

  match populate_command_list(&data.command_list, &data.command_allocator, &data.srv_heap, data, &consts) {
    Err(_) => {
      dump_info_queue(data.core.info_queue.as_ref());
      panic!();
    },
    _ => {},
  }

  data.core.graphics_queue.execute_command_lists(&[&data.command_list]);

  wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);

  let cls: Vec<&D3D12GraphicsCommandList> = data.parallel_submission.gc_lists.iter().map(|&(ref clist, _)|clist).collect();
  data.core.graphics_queue.execute_command_lists(&cls[..]);

  wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);

  data.command_list.reset(&data.command_allocator, Some(&data.pipeline_state)).unwrap();
  data.command_list.resource_barrier(
      &mut [*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
      D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT)]);
  data.command_list.close().unwrap();
  data.core.graphics_queue.execute_command_lists(&[&data.command_list]);

  match data.swap_chain.swap_chain.present(0, 0) {
    Err(hr) => {
      dump_info_queue(data.core.info_queue.as_ref());
      panic!("Present failed with 0x{:x}", hr);
    },
    _ => (),
  }
  dump_info_queue(data.core.info_queue.as_ref());
}

// TODO: split this into something more manageable.
struct AppData {
  core: DXCore,
  swap_chain: DXSwapChain,
  viewport: D3D12_VIEWPORT,
  scissor_rect: D3D12_RECT,
  command_allocator: D3D12CommandAllocator,
  root_signature: D3D12RootSignature,
  srv_heap: D3D12DescriptorHeap,
  sam_heap: D3D12DescriptorHeap,
  dsd_heap: D3D12DescriptorHeap,
  cbvd_heap: D3D12DescriptorHeap,
  pipeline_state: D3D12PipelineState,
  command_list: D3D12GraphicsCommandList,
  vertex_buffer: D3D12Resource,
  vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
  constant_buffer: D3D12Resource,
  tex_resource: D3D12Resource,
  depth_stencil: Option<D3D12Resource>,
  frame_index: UINT,
  fence_event: HANDLE,
  fence: D3D12Fence,
  tick: f64,
  parallel_submission: ParallelSubmissionData,
}

unsafe impl Send for AppData {}

unsafe impl Sync for AppData {}

impl fmt::Debug for AppData {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f,"struct AppData")
  }
}

fn wait_for_graphics_queue(core: &DXCore, fence: &D3D12Fence, fence_event: &HANDLE) {
  // TODO: Look for overflow behaviour of atomics
  let fence_value = core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;

  core.graphics_queue.signal(fence, fence_value).unwrap();
  if fence.get_completed_value() < fence_value {
    match fence.set_event_on_completion(fence_value, *fence_event) {
      Ok(_) => (),
      Err(hr) => {
        dump_info_queue(core.info_queue.as_ref());
        panic!("set_event_on_completion error: 0x{:x}",hr);
      },
    }
    unsafe { WaitForSingleObject(*fence_event, INFINITE) };
  }
}

fn populate_command_list(command_list: &D3D12GraphicsCommandList, command_allocator: &D3D12CommandAllocator, srv_heap: &D3D12DescriptorHeap, data: &AppData, consts: &Constants) -> HResult<()> {
  let d_info=false;
  if d_info {debug!("Command allocator reset")};
  command_allocator.reset().unwrap();
  if d_info {debug!("Command list reset")};
  command_list.reset(&data.command_allocator, Some(&data.pipeline_state)).unwrap();

  if d_info {debug!("Set graphics root signature")};
  command_list.set_graphics_root_signature(&data.root_signature);

  if d_info {debug!("Set descriptor heaps")};
  command_list.set_descriptor_heaps(&[srv_heap]);

  if d_info {debug!("Set viepowrts")};
  command_list.rs_set_viewports(&[data.viewport]);
  if d_info {debug!("Set scissor rects")};
  command_list.rs_set_scissor_rects(&[data.scissor_rect]);

  if d_info {debug!("Resource barrier")};
  command_list.resource_barrier(
      &mut [*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
      D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET).flags(D3D12_RESOURCE_BARRIER_FLAG_NONE)]);

  let mut rtvh=data.swap_chain.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  rtvh.ptr += (data.frame_index as SIZE_T)*data.swap_chain.rtv_dsize;
  let dsvh = data.dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  if d_info {debug!("OM set render targets")};
  command_list.om_set_render_targets(1, &rtvh, Some(&dsvh));

  let clear_color = [0.01, 0.01, 0.15, 1.0];
  if d_info {debug!("clear render target view")};
  command_list.clear_render_target_view(rtvh, &clear_color, &[]);
  command_list.clear_depth_stencil_view(dsvh, D3D12_CLEAR_FLAG_DEPTH, 1.0, 0, &[]);
  
  let mut gpu_dh=srv_heap.get_gpu_descriptor_handle_for_heap_start();
  if d_info {debug!("Set graphics root descriptor table 0")};
  command_list.set_graphics_root_descriptor_table(0, gpu_dh);

  if d_info {debug!("Set graphics root descriptor table 1")};
//  gpu_dh.ptr += data.device.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
//  command_list.set_graphics_root_descriptor_table(1, gpu_dh);

  command_list.set_graphics_root_constant_buffer_view(1, data.constant_buffer.get_gpu_virtual_address());

//  if d_info {debug!("Set graphics root descriptor table 2")};
//  command_list.set_graphics_root_descriptor_table(2, data.sam_heap.get_gpu_descriptor_handle_for_heap_start());

//  command_list.set_graphics_root_shader_resource_view(0, data.tex_resource.get_gpu_virtual_address());

  command_list.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
  command_list.ia_set_vertex_buffers(0, Some(&[data.vertex_buffer_view]));

  let num_vtx=data.vertex_buffer_view.SizeInBytes/data.vertex_buffer_view.StrideInBytes;
  command_list.draw_instanced(num_vtx, 1, 0, 0);
  
  try!(submit_in_parallel(data, consts));
//  command_list.resource_barrier(
//      &mut [*ResourceBarrier::transition(&data.swap_chain.render_targets[data.frame_index as usize],
//      D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT)]);

  match command_list.close() {
    Ok(_) => Ok(()),
    Err(hr) => {
      dump_info_queue(data.core.info_queue.as_ref());
      panic!("command_list.close() failed with 0x{:x}", hr);
    },
  }
}

struct PtrConstants(*mut Constants);

unsafe impl Send for PtrConstants {}
unsafe impl Sync for PtrConstants {}

struct ParallelSubmissionData {
  // Buffers are kept mapped into CPU address space. Drop unmaps them
  c_buffers: Vec<(D3D12Resource, PtrConstants)>,
  gc_lists: Vec<(D3D12GraphicsCommandList, D3D12CommandAllocator)>,
}


impl Drop for ParallelSubmissionData {
  fn drop(&mut self) {
    for &(ref res, _) in &self.c_buffers {
      res.unmap(0, None);
    }
  }
}

fn init_parallel_submission(core: &DXCore, thread_count: u32, object_count: u32) -> HResult<ParallelSubmissionData> {
  let cbsize = unsafe {
    let cbuf_data: Constants = ::std::mem::uninitialized();
    let cbsize = mem::size_of_val(&cbuf_data);
    mem::forget(cbuf_data);
    cbsize
  };

  let mut c_buffers = vec![];
  for i in 0 .. object_count {
    let cbuf = try!(core.dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(cbsize as u64), D3D12_RESOURCE_STATE_GENERIC_READ, None));
    let mut ptr: *mut u8 = ptr::null_mut();
    unsafe {
      try!(cbuf.map(0, None, Some(&mut ptr)));
      debug!("cbuf.iptr(): 0x{:x}, ptr: 0x{:x}", cbuf.iptr() as usize, ptr as usize);
      c_buffers.push((cbuf, PtrConstants(ptr as *mut Constants))); // unsafe cast from pointer to reference. Actually, it should be safe, if I am not mistaken.
    }
  }

  let mut clists = vec![];
  for i in 0 .. thread_count {
    debug!("create_command_allocator");
    let callocator = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
    debug!("create_command_list");
    let clist: D3D12GraphicsCommandList = try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, None));
    clist.close().unwrap();
    clists.push((clist, callocator));
  }

  Ok(ParallelSubmissionData {
    c_buffers: c_buffers,
    gc_lists: clists,
  })
}

fn submit_in_parallel(data: &AppData, consts: &Constants) -> HResult<()> {

  let sub_size = data.parallel_submission.c_buffers.len() / data.parallel_submission.gc_lists.len();
  let chunks = (&data.parallel_submission.c_buffers[..]).chunks(sub_size);
  let dsvh = data.dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  let mut rtvh=data.swap_chain.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  rtvh.ptr += (data.frame_index as SIZE_T)*data.swap_chain.rtv_dsize;

  let mut join_handles: Vec<::std::thread::JoinHandle<HResult<()>>> = vec![];
  for ((submission, &(ref clist, ref callocator)), th_n) in chunks.zip(data.parallel_submission.gc_lists.iter()).zip(1..) {
    let submission: &'static [(D3D12Resource, PtrConstants)] = unsafe{ mem::transmute(submission) };
    let clist = clist.clone();
    let callocator = callocator.clone();
    let consts: Constants = consts.clone();
    let pipeline_state = data.pipeline_state.clone();
    let root_signature = data.root_signature.clone();
    let srv_heap = data.srv_heap.clone();
    let vertex_buffer_view = data.vertex_buffer_view;
    let viewport = data.viewport;
    let scissor_rect = data.scissor_rect;
    join_handles.push(
      ::std::thread::spawn(move|| {
        let seed: &[_] = &[th_n, 2, 3, 4];
        let mut rng: rand::StdRng = rand::SeedableRng::from_seed(seed);

        try!(clist.reset(&callocator, Some(&pipeline_state)));
        clist.set_graphics_root_signature(&root_signature);
        clist.set_descriptor_heaps(&[&srv_heap]);
        clist.rs_set_viewports(&[viewport]);
        clist.rs_set_scissor_rects(&[scissor_rect]);
        clist.om_set_render_targets(1, &rtvh, Some(&dsvh));
        clist.set_graphics_root_descriptor_table(0, srv_heap.get_gpu_descriptor_handle_for_heap_start());
        clist.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        clist.ia_set_vertex_buffers(0, Some(&[vertex_buffer_view]));
        let num_vtx = vertex_buffer_view.SizeInBytes/vertex_buffer_view.StrideInBytes;

        for &(ref constant_buffer, ref constants) in submission {
          let mut c: Constants = consts.clone();
          c.model[0][3]=(rng.gen::<f32>()-0.5)*20.;
          c.model[1][3]=(rng.gen::<f32>()-0.5)*20.;
          c.model[2][3]=(rng.gen::<f32>()-1.0)*20.;
          unsafe {
            // Write to memory-mapped constant buffer
            *(constants.0) = c;
          };
          let cb_address = constant_buffer.get_gpu_virtual_address();
          //debug!("cb_address: 0x{:x}", cb_address);
          clist.set_graphics_root_constant_buffer_view(1, cb_address);
          clist.draw_instanced(num_vtx, 1, 0, 0);
        }
        try!(clist.close());
        Ok(())
      }));
  }
  for jh in join_handles {
    let _ = jh.join();
  }
  Ok(())
}

fn on_resize(data: &mut AppData, w: u32, h: u32, _: u32) {
   wait_for_graphics_queue(&data.core, &data.fence, &data.fence_event);
   drop_render_targets(&mut data.swap_chain);
   let desc = data.swap_chain.swap_chain.get_desc().unwrap();
   let res = data.swap_chain.swap_chain.resize_buffers(0, w, h, DXGI_FORMAT_UNKNOWN, 0);
   //info!("Resize to {},{}. Result:{:?}",w,h,res);
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

fn drop_render_targets(sc: &mut DXSwapChain) {
  sc.render_targets.truncate(0);
}

fn reaquire_render_targets(core: &DXCore, sc: &mut DXSwapChain) -> HResult<()> {
  let mut cdh = sc.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  sc.render_targets.truncate(0);
  for i in 0 .. sc.frame_count {
    let buf=try!(sc.swap_chain.get_buffer::<D3D12Resource>(i as u32));
    core.dev.create_render_target_view(Some(&buf), Some(&render_target_view_desc_tex2d_default(sc.rtv_format)), cdh);
    cdh.ptr += sc.rtv_dsize;
    sc.render_targets.push(buf);
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

fn upload_into_texture(core: &DXCore, tex: &D3D12Resource, w: usize, h: usize, data: &[u32]) -> HResult<()> {
  debug!("upload_into_texture");
  debug!("get_desc tex.iptr(): 0x{:x}, vtbl: 0x{:x}, vtbl[0]: 0x{:x}", tex.iptr() as usize, unsafe{((*tex.iptr()).lpVtbl) as usize}, unsafe{(*(*tex.iptr()).lpVtbl).QueryInterface as usize});
  let desc = tex.get_desc();
  debug!("get_device");
  let dev = &core.dev;
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
  res_buf.set_name("Temporary texture buffer".into()).unwrap();

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

  debug!("create_command_allocator");
  let callocator = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COPY));
  debug!("create_command_list");
  let clist: D3D12GraphicsCommandList = try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COPY, &callocator, None));
  clist.copy_texture_region(&dest, 0, 0, 0, &src, None); 
  clist.resource_barrier(&mut [*ResourceBarrier::transition(&tex, D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON)]);

  debug!("Command list Close");
  try!(clist.close());

  debug!("Command queue execute");
  core.copy_queue.execute_command_lists(&[&clist]);
  debug!("fence");
  let fence=try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));
  debug!("fence_event");
  let fence_event = unsafe{ CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  if fence_event == ptr::null_mut() {
    panic!("Cannot create event");
  }

  let fv=core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;
  debug!("signal");
  try!(core.copy_queue.signal(&fence, fv));
  //try!(core.copy_queue.wait(&fence, fv)); // D3D12CommandQueue::wait doesn't work, or I do something wrong.
  if fence.get_completed_value() < fv {
    debug!("set_event_on_completion");
    try!(fence.set_event_on_completion(fv, fence_event));
    debug!("wait");
    unsafe { WaitForSingleObject(fence_event, INFINITE) };
  }
  Ok(())
}

fn create_appdata(wnd: &Window, adapter: Option<&DXGIAdapter1>) -> Result<AppData, Option<D3D12InfoQueue>> {
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

  let core = try!(create_core(adapter, D3D_FEATURE_LEVEL_11_0, true).map_err(|err|panic!("Cannot create DXCore: {}", err)));

  let sc_desc = DXGI_SWAP_CHAIN_DESC1 {
    Width: w as u32,
    Height: h as u32,
    Format: DXGI_FORMAT_R8G8B8A8_UNORM, // note that create_swap_chain fails when Format is DXGI_FORMAT_R8G8B8A8_UNORM_SRGB
    Stereo: 0,
    SampleDesc: sample_desc_default(),
    BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
    BufferCount: FRAME_COUNT,
    Scaling: DXGI_SCALING_NONE,
    SwapEffect: DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
    AlphaMode: DXGI_ALPHA_MODE_UNSPECIFIED,
    Flags: DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH.0,
  };

  // so I pass DXGI_FORMAT_R8G8B8A8_UNORM_SRGB separately for use in render target view
  let swap_chain = try!(create_swap_chain(&core, &sc_desc, DXGI_FORMAT_R8G8B8A8_UNORM_SRGB, hwnd, None, None)
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
  for v in &mut texdata[..] {
    *v=rand::random();
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
  
  let par_sub = try!(init_parallel_submission(&core, 8, 16000).map_err(|_|core.info_queue.clone()));

  let ret=
    AppData {
      core: core,
      swap_chain: swap_chain,
      viewport: viewport,
      scissor_rect: sci_rect,
      command_allocator: callocator,
      root_signature: root_sign,
      srv_heap: srv_heap,
      sam_heap: sam_heap,
      dsd_heap: dsd_heap,
      cbvd_heap: cbvd_heap,
      pipeline_state: gps,
      command_list: command_list,
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
    };
  
  debug!("Command list Close");
  ret.command_list.close().unwrap();

  debug!("Command queue execute");
  ret.core.graphics_queue.execute_command_lists(&[&ret.command_list]);
  
  debug!("wait_for_prev_frame");
  wait_for_graphics_queue(&ret.core, &ret.fence, &ret.fence_event);
  Ok(ret)
}

fn create_main_graphics_pipeline_state(core: &DXCore) -> HResult<(D3D12PipelineState,D3D12RootSignature)> {
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
  let blob=try!(d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1));

  debug!("Root signature");
  let root_sign = try!(core.dev.create_root_signature(0, blob_as_slice(&blob)));

  let compile_flags=0;
  debug!("Vertex shader");
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  debug!("Pixel shader");
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();

  let input_elts_desc = Vertex::generate(0);

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
