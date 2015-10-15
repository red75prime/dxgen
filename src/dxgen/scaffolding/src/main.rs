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
extern crate crossbeam;

#[macro_use] mod macros;
mod create_device;
mod window;
mod structwrappers;
mod utils;
mod gfx_d3d12;
mod shape_gen;
mod dxsems;
mod core;
mod app;
mod cubes;

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

const FRAME_COUNT : u32 = 4;

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

  let factory: DXGIFactory4 = create_dxgi_factory2().expect("Cannot create DXGIFactory4. No can do.");
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
  crossbeam::scope(|scope| {
    for a in adapters {
      let mutex = mutex.clone();
      scope.spawn(move||{
        main_prime(a, mutex)
      });
    };
  });
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
    match cubes::AppData::on_init(&wnd, Some(&adapter), FRAME_COUNT) { 
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
    cubes::on_resize(&mut *rdata.borrow_mut(), w, h, c);
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
      let do_not_render = data.borrow().is_minimized();
      if do_not_render {
        std::thread::sleep_ms(10);
      } else {
        cubes::on_render(&mut *data.borrow_mut(), x, y);
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
  }
  // Application should exit fullscreen state before terminating. 
  data.borrow().set_fullscreen(false).unwrap();
  // copy info queue
  let iq=data.borrow().info_queue().clone();
  // release resources
  wnd.set_resize_fn(Box::new(|_,_,_|{}));
  {
    let data=data.borrow();
    data.wait_frame();
  }
  drop(data);
  // maybe debug layer has something to say
  dump_info_queue(iq.map(|iq|iq).as_ref());
}

