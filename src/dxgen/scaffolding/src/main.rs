#![feature(optin_builtin_traits)]
#![feature(clone_from_slice)]

//#[macro_use] extern crate gfx;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate libc;
extern crate winapi;
extern crate dx_safe;
//extern crate dxguid_sys;
extern crate dxgi_sys;
extern crate kernel32;
extern crate user32;
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
use dx_safe::*;
use create_device::*;
use kernel32::{CreateEventW, WaitForSingleObject};
use std::ptr;
use std::fmt;
use std::mem;
use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use window::*;
use utils::*;
use clock_ticks::*;
use cgmath::*;
use core::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use rand::Rng;
use cubes::{Parameters, CubeParms};

#[link(name="d3dcompiler")]
extern {}

const FRAME_COUNT : u32 = 4;

fn main() {
  // Initialize logger
  env_logger::init().unwrap();

  // Set default values of cubes module parameters
  let mut parms = CubeParms {thread_count: 2, object_count: 2_000,};
  let mut adapters_to_test = vec![];
  for arg in env::args() {
    if let Ok(n) = arg.parse::<u32>() {
      // If command line parameter is a number, treat it as a number of graphics adapter.
      adapters_to_test.push(n);
    }
    if arg.starts_with("-o") {
      // Command line parameter -o<N> sets number of objects to draw
      // Note, that 2 in arg[2..] is a number of bytes, not characters. 
      if let Ok(n) = (&arg[2..]).parse::<u32>() {
        parms.object_count = n;
      }
    }
    if arg.starts_with("-t") {
      // Command line parameter -t<N> sets number of threads and graphics command lists
      // to use for sending workload to GPU
      if let Ok(n) = (&arg[2..]).parse::<u32>() {
        parms.thread_count = n;
      }
    }
  }

  let factory: DXGIFactory4 = create_dxgi_factory2().expect("Cannot create DXGIFactory4. No can do.");
  let mut i=0;
  let mut adapters = vec![];
  // Iterate over available GPUs 
  while let Ok(adapter)=factory.enum_adapters1(i) {
    let descr=adapter.get_desc1().unwrap();
    println!("Adapter {}: {}", i, wchar_array_to_string_lossy(&descr.Description));
    println!("   Dedicated video memory: {}MiB", descr.DedicatedVideoMemory/1024/1024);
    if adapters_to_test.len()==0 || adapters_to_test[..].contains(&i) {
      // If there's no numbers in command line add each and every available adapter,
      // otherwise use adapter numbers from command line
      adapters.push(adapter);
    }
    // It's easy to forget this.
    i+=1;
  }

  // I used this mutex to sync console output.
  let mutex=Arc::new(Mutex::new(()));
  crossbeam::scope(|scope| {
    for (id,a) in adapters.into_iter().enumerate() {
      let mutex = mutex.clone();
      let parms = &parms;
      scope.spawn(move||{
        // Spawn a thread for each adapter
        main_prime(id, a, mutex, parms)
      });
    };
  });
}

fn main_prime<T: Parameters>(id: usize, adapter: DXGIAdapter1, mutex: Arc<Mutex<()>>, parms: &T) {
  // Setup window. Currently window module supports only one window per thread.
  let descr=wchar_array_to_string_lossy(&adapter.get_desc1().unwrap().Description);
  let title=format!("D3D12 Hello, rusty world! ({})", descr);
  let wnd=create_window(&title, 512, 256);

  {
    // This block of code is not required. I just checked some stuff
    if let Ok(dev)=d3d12_create_device(Some(&adapter), D3D_FEATURE_LEVEL_12_1) {
      let format = DXGI_FORMAT_R8G8B8A8_UNORM_SRGB;
      let mut fsup = D3D12_FEATURE_DATA_FORMAT_SUPPORT {
        Format: format,
        Support1: D3D12_FORMAT_SUPPORT1_NONE,
        Support2: D3D12_FORMAT_SUPPORT2_NONE,
      };
      if let Ok(_) = dev.check_feature_support_format_support(&mut fsup) {
        // We could have few threads running, so take a lock to prevent line interleaving
        let lock=mutex.lock();
        println!("{}", descr);
        println!("Format support for {:?} is {:x}, {:x}", format, fsup.Support1.0, fsup.Support2.0);
      }
    }
  }

  // Initialization of cubes module data required to render all stuff
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
  
  // x and y store last mouse coords from WM_MOUSEMOVE
  let mut x:i32=0;
  let mut y:i32=0;
  // TODO: Make this horror go away
  // Simple and crude way to track state of keyboard keys
  let (mut wdown, mut sdown, mut adown, mut ddown, mut qdown, mut edown) = (false, false, false, false, false, false);
  // and state of left mouse button
  let mut mouse_down = false;
  // Profiling stuff
  let mut frame_count = 0;
  let mut avg_fps = 0;
  let mut start = precise_time_s();
  let mut mstart = start;
  // Window::poll_events() returns non-blocking iterator, that return Option<MSG>
  for mmsg in wnd.poll_events() {
    // "if let Some(msg)" extracts msg from mmsg
    // if mmsg is None, then 'else' branch is taken
    if let Some(msg) = mmsg {
      // Instead of passing messages into cubes module, I process them here
      // It is not well-thought-out design decision, it's just slightly simpler now, and cost of changing it is low.
      match msg.message {
        // Usual message processing stuff
        WM_SIZE => {
          // Normally this message goes to wndproc, in window.rs I repost it into message queue to prevent reentrancy problems
          debug!("WM_SIZE {}, {}  ", msg.wParam, msg.lParam);
          data.borrow_mut().on_resize(LOWORD(msg.lParam as u32) as u32, HIWORD(msg.lParam as u32) as u32, msg.wParam as u32); 
        },
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
          set_capture(wnd.get_hwnd());
        },
        WM_LBUTTONUP => {
          mouse_down = false;
          release_capture();
        },
        _ => {},
      };
    } else {
      let do_not_render = data.borrow().is_minimized();
      if do_not_render {
        // MSDN suggest to use MsgWaitForMultipleObjects here, but 10ms sleep shouldn't create problems
        std::thread::sleep_ms(10);
      } else {
        {
          // data is Rc<RefCell<cubes::AppData>>
          // Rc is not really needed. I didn't pass it around.
          // Take a mutable reference to cubes::AppData
          let mut data = data.borrow_mut();
          // Process WASD keys
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
          // Process Q and E keys. They control camera's roll
          if qdown {
            camera.rotz(-0.5);
          };
          if edown {
            camera.rotz(0.5);
          };
        }
        // For this simple program I don't separate update and render steps.
        // State change and rendering is done inside on_render.
        // Error handling isn't implemented yet. on_render panics, if it needs to.
        // TODO: remove x,y parameters
        // TODO: process error
        data.borrow_mut().on_render(x, y);
        // register rendered frame in performance collector
        ::perf_frame();
        // fps counting stuff
        let now = precise_time_s();
        let frames = PERFDATA.with(|p_data| p_data.borrow().frames);
        if frames>0 && now<start || now>=(start+1.0) {
          // Once per second show stats
          let (clear, fill, exec, present, wait) =
            PERFDATA.with(|p_data| {
              let p_data = p_data.borrow();
              let frames = p_data.frames as f64;
              (p_data.perf.get("clear").unwrap()*1000./frames, 
                p_data.perf.get("fillbuf").unwrap()*1000./frames, 
                p_data.perf.get("exec").unwrap()*1000./frames, 
                p_data.perf.get("present").unwrap()*1000./frames, 
                p_data.perf.get("wait").unwrap()*1000./frames, )
            });
          println!("Adapter {} FPS: {:3} clear:{:4.2} fill:{:4.2} exec:{:4.2} present:{:4.2} wait:{:4.2}   \r", id, frames, clear, fill, exec, present, wait);
          let _ = ::std::io::Write::flush(&mut ::std::io::stdout()); 
          perf_reset();
          start = now;
        }
      }
    }
  }
  // Application should exit fullscreen state before terminating. 
  data.borrow().set_fullscreen(false).unwrap();
  // copy info queue
  let iq=data.borrow().info_queue().clone();
  // wait for all GPU processing to stop
  data.borrow().wait_frame();
  // release resources
  drop(data);
  // maybe debug layer has something to say
  dump_info_queue(iq.map(|iq|iq).as_ref());
}


// Simple render statistics collector
pub struct PerfData {
  frames: u64,
  perf: HashMap<&'static str, f64>,
}

impl PerfData {
  fn new() -> PerfData {
    let mut pd = PerfData {
      frames: 0,
      perf: HashMap::new(),
    };
    pd.perf.insert("clear", 0.);
    pd.perf.insert("fillbuf", 0.);
    pd.perf.insert("exec", 0.);
    pd.perf.insert("present", 0.);
    pd.perf.insert("wait", 0.);
    pd
  }
}

thread_local!(static PERFDATA: RefCell<PerfData> = 
  RefCell::new(PerfData::new()));

pub fn perf_frame() {
  PERFDATA.with(|pd| {
    pd.borrow_mut().frames += 1;
  });
}

pub fn perf_reset() {
  PERFDATA.with(|pd| {
    let mut pd=pd.borrow_mut();
    *pd = PerfData::new();
  });
}

pub fn perf_start(name: &'static str) {
  PERFDATA.with(|pd| {
    *pd.borrow_mut().perf.get_mut(name).unwrap() -= precise_time_s();
  });
}

pub fn perf_end(name: &'static str) {
  PERFDATA.with(|pd| {
    *pd.borrow_mut().perf.get_mut(name).unwrap() += precise_time_s();
  });
}

pub fn perf_clear_start() {
  perf_start("clear");
}

pub fn perf_clear_end() {
  perf_end("clear");
}

pub fn perf_fillbuf_start() {
  perf_start("fillbuf");
}

pub fn perf_fillbuf_end() {
  perf_end("fillbuf");
}

pub fn perf_exec_start() {
  perf_start("exec");
}

pub fn perf_exec_end() {
  perf_end("exec");
}

pub fn perf_present_start() {
  perf_start("present");
}

pub fn perf_present_end() {
  perf_end("present");
}

pub fn perf_wait_start() {
  perf_start("wait");
}

pub fn perf_wait_end() {
  perf_end("wait");
}
