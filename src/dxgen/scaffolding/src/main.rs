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
//mod gfx_d3d12;
mod shape_gen;
mod dxsems;
mod core;
mod app;
mod cubes;
mod camera;


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
use cubes::{Parameters, CubeParms};

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

  let mut parms = CubeParms {thread_count: 2, object_count: 2_000,};
  let mut adapters_to_test = vec![];
  for arg in env::args() {
    if let Ok(n) = arg.parse::<u32>() {
      adapters_to_test.push(n);
    }
    if arg.starts_with("-o") {
      if let Ok(n) = (&arg[2..]).parse::<u32>() {
        parms.object_count = n;
      }
    }
    if arg.starts_with("-t") {
      if let Ok(n) = (&arg[2..]).parse::<u32>() {
        parms.thread_count = n;
      }
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
      let parms = &parms;
      scope.spawn(move||{
        main_prime(a, mutex, parms)
      });
    };
  });
}

fn main_prime<T: Parameters>(adapter: DXGIAdapter1, mutex: Arc<Mutex<()>>, parms: &T) {

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
    match cubes::AppData::on_init(&wnd, Some(&adapter), FRAME_COUNT, parms) { 
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

  let mut x:i32=0;
  let mut y:i32=0;
  // TODO: Make this horror go away
  let (mut wdown, mut sdown, mut adown, mut ddown, mut qdown, mut edown) = (false, false, false, false, false, false);
  let mut mouse_down = false;
  let mut frame_count = 0;
  let mut avg_fps = 0;
  let mut start = precise_time_s();
  let mut mstart = start;
  for mmsg in wnd.poll_events() {
    if let Some(msg) = mmsg {
      //println!("{} ", msg.message);
      match msg.message {
        WM_MOUSEMOVE => {
          let x1 = GET_X_LPARAM(msg.lParam) as i32;
          let y1 = GET_Y_LPARAM(msg.lParam) as i32;
          let (dx, dy) = (x1 - x, y - y1);
          x = x1;
          y = y1;
          if mouse_down {
            let mut data = data.borrow_mut();
            let camera = data.camera();
            camera.rotx(-dx as f32/6.);
            camera.roty(-dy as f32/6.);
          }
        },
        WM_KEYDOWN => {
          match msg.wParam as u8 {
            b'A' => adown = true,
            b'S' => sdown = true,
            b'D' => ddown = true,
            b'W' => wdown = true,
            b'Q' => qdown = true,
            b'E' => edown = true,
            _ => {},
          }
        },
        WM_KEYUP => {
          match msg.wParam as u8 {
            b'A' => adown = false,
            b'S' => sdown = false,
            b'D' => ddown = false,
            b'W' => wdown = false,
            b'Q' => qdown = false,
            b'E' => edown = false,
            _ => {},
          }
        },
        WM_LBUTTONDOWN => {
          mouse_down = true;
        },
        WM_LBUTTONUP => {
          mouse_down = false;
        },
        _ => {},
      };
    } else {
      let do_not_render = data.borrow().is_minimized();
      if do_not_render {
        std::thread::sleep_ms(10);
      } else {
        {
          let mut data = data.borrow_mut();
          let camera = data.camera();
          if wdown {
            camera.go(0.1, 0., 0.);
          };
          if sdown {
            camera.go(-0.1, 0., 0.);
          };
          if adown {
            camera.go(0., 0.1, 0.); // Something wrong with signs. Negative value should translate camera to the left
          };
          if ddown {
            camera.go(0., -0.1, 0.);
          };
          if qdown {
            camera.rotz(-0.1);
          };
          if edown {
            camera.rotz(0.1);
          };
        }
        cubes::on_render(&mut *data.borrow_mut(), x, y);
        ::perf_frame();
        frame_count += 1;
        let now = precise_time_s();
        let frametime = now - mstart;
        mstart = now;
        if now<start || now>=(start+1.0) {
          let (clear, fill, exec, present, wait) = unsafe {
            (perf_data.clear*1000./perf_data.frames as f64, perf_data.fillbuf*1000./perf_data.frames as f64, 
              perf_data.exec*1000./perf_data.frames as f64, perf_data.present*1000./perf_data.frames as f64, 
              perf_data.wait*1000./perf_data.frames as f64, )
          };
          print!("FPS: {:3} clear:{:4.2} fill:{:4.2} exec:{:4.2} present:{:4.2} wait:{:4.2}   \r", avg_fps, clear, fill, exec, present, wait);
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

pub struct PerfData {
  frames: u64,
  clear: f64,
  fillbuf: f64,
  exec: f64,
  present: f64,
  wait: f64,
}

static mut perf_data: PerfData = PerfData {
  frames: 0,
  clear: 0.,
  fillbuf: 0.,
  exec: 0.,
  present: 0.,
  wait: 0.,
};

pub fn perf_frame() {
  unsafe { perf_data.frames += 1; }
}

pub fn perf_clear_start() {
  unsafe { perf_data.clear -= precise_time_s(); }
}

pub fn perf_clear_end() {
  unsafe { perf_data.clear += precise_time_s(); }
}

pub fn perf_fillbuf_start() {
  unsafe { perf_data.fillbuf -= precise_time_s(); }
}

pub fn perf_fillbuf_end() {
  unsafe { perf_data.fillbuf += precise_time_s(); }
}

pub fn perf_exec_start() {
  unsafe { perf_data.exec -= precise_time_s(); }
}

pub fn perf_exec_end() {
  unsafe { perf_data.exec += precise_time_s(); }
}

pub fn perf_present_start() {
  unsafe { perf_data.present -= precise_time_s(); }
}

pub fn perf_present_end() {
  unsafe { perf_data.present += precise_time_s(); }
}

pub fn perf_wait_start() {
  unsafe { perf_data.wait -= precise_time_s(); }
}

pub fn perf_wait_end() {
  unsafe { perf_data.wait += precise_time_s(); }
}

