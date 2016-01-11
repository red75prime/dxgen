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
//extern crate dxgi_sys;
extern crate kernel32;
extern crate user32;
//extern crate d3d12_sys;
extern crate rand;
extern crate clock_ticks;
extern crate cgmath;
extern crate crossbeam;
extern crate obj;
extern crate itertools;

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
//mod hvoxel;
mod downsampler;
mod tonemapper;
mod plshadow;
mod cubestate;



use winapi::*;
use dx_safe::*;
use create_device::*;
use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use window::*;
use utils::*;
use clock_ticks::*;
use cubes::CubeParms;
use std::time::Duration;
use itertools::Itertools;

const FRAME_COUNT : u32 = 3;

fn main() {
  // Initialize logger
  env_logger::init().unwrap();

  // Set default values of cubes module parameters
  let mut parms = CubeParms {thread_count: 2, object_count: 2_000, speed_mult: 0.01, concurrent_state_update: true};
  let mut adapters_to_test = vec![];
  let mut adapters_info = false;
  for arg in env::args() {
    if let Ok(n) = arg.parse::<u32>() {
      // If command line parameter is a number, treat it as a number of graphics adapter.
      adapters_to_test.push(n);
    }
    if arg == "-i" || arg == "--info" {
      adapters_info = true;
    }
    if arg == "--seq" {
        parms.concurrent_state_update = false;
    };
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
    if arg.starts_with("-s") {
      // Command line parameter -s<f32> sets cube speed multiplier
      if let Ok(s) = (&arg[2..]).parse::<f32>() {
        parms.speed_mult = s;
      }
    }
  }

  let factory: DXGIFactory4 = 
    match create_dxgi_factory2() {
      Ok(fact) => fact,
      Err(hr) => panic!("Cannot create DXGIFactory4 (0x{:x}). No can do.",hr),
    };
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
      if adapters_info {
        print_adapter_info(&adapter);
      }
      adapters.push(adapter);
    }
    // It's easy to forget this.
    i+=1;
  }

  if adapters_info {
    return;
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

fn print_adapter_info(adapter: &DXGIAdapter1) {
  if let Ok(dev)=d3d12_create_device(Some(&adapter), D3D_FEATURE_LEVEL_11_0) {
    let mut data = unsafe { ::std::mem::uninitialized() };
    if let Ok(_) = dev.check_feature_support_virtual_address(&mut data) {
      // data is guarantied to be initialized here
      println!("   {:#?}", data);
    }
    let mut data = unsafe { ::std::mem::uninitialized() };
    if let Ok(_) = dev.check_feature_support_options(&mut data) {
      // data is guarantied to be initialized here
      println!("   {:#?}", data);
    }
  }
}

const VK_A: i32 = b'A' as i32;
const VK_S: i32 = b'S' as i32;
const VK_D: i32 = b'D' as i32;
const VK_F: i32 = b'F' as i32;
const VK_Q: i32 = b'Q' as i32;
const VK_W: i32 = b'W' as i32;
const VK_E: i32 = b'E' as i32;
const VK_R: i32 = b'R' as i32;

fn main_prime(id: usize, adapter: DXGIAdapter1, mutex: Arc<Mutex<()>>, parms: &CubeParms) {
    // Setup window. Currently window module supports only one window per thread.
    let descr=wchar_array_to_string_lossy(&adapter.get_desc1().unwrap().Description);
    let title=format!("D3D12 Hello, rusty world! ({})", descr);
    let wnd=create_window(&title, 512, 256);

    {
    // This block of code is not required. I just checked some stuff
    if let Ok(dev)=d3d12_create_device(Some(&adapter), D3D_FEATURE_LEVEL_12_0) {
        let format = DXGI_FORMAT_R8G8B8A8_UNORM;
        let mut fsup = D3D12_FEATURE_DATA_FORMAT_SUPPORT {
        Format: format,
        Support1: D3D12_FORMAT_SUPPORT1_NONE,
        Support2: D3D12_FORMAT_SUPPORT2_NONE,
        };
        if let Ok(_) = dev.check_feature_support_format_support(&mut fsup) {
        // We could have few threads running, so take a lock to prevent line interleaving
        let _ = mutex.lock();
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
        Err(err) => {
        error!("AppData creation failed with error code 0x{:x}", err);
        return ();
        },
    };
  
    // x and y store last mouse coords from WM_MOUSEMOVE
    let mut x:i32=0;
    let mut y:i32=0;
    // TODO: Make this horror go away
    // Simple and crude way to track state of keyboard keys
    let (mut wdown, mut sdown, mut adown, mut ddown, mut qdown, mut edown, mut rdown, mut fdown, mut shdown, mut ctdown) = (false, false, false, false, false, false, false, false, false, false);
    // and state of left mouse button
    let mut mouse_down = false;
    // Profiling stuff
    let mut start = precise_time_s();
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
            camera.roty(dy as f32/6.);
            }
        },
        WM_KEYDOWN => {
            match msg.wParam as i32 {
            VK_A => adown = true,
            VK_S => sdown = true,
            VK_D => ddown = true,
            VK_W => wdown = true,
            VK_Q => qdown = true,
            VK_E => edown = true,
            VK_R => rdown = true,
            VK_F => fdown = true,
            VK_SHIFT => shdown = true,
            VK_CONTROL => ctdown = true,
            _ => {},
            }
        },
        WM_KEYUP => {
            match msg.wParam as i32 {
            VK_A => adown = false,
            VK_S => sdown = false,
            VK_D => ddown = false,
            VK_W => wdown = false,
            VK_Q => qdown = false,
            VK_E => edown = false,
            VK_R => rdown = false,
            VK_F => fdown = false,
            VK_SHIFT => shdown = false,
            VK_CONTROL => ctdown = false,
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
    } else { // There's no pending window message.
        let do_not_render = data.borrow().is_minimized();
        if do_not_render {
        // MSDN suggest to use MsgWaitForMultipleObjects here, but 10ms sleep shouldn't create problems
        std::thread::sleep(Duration::from_millis(10));
        } else {
        {
            // data is Rc<RefCell<cubes::AppData>>
            // Rc is not really needed. I didn't pass it around.
            // Take a mutable reference to cubes::AppData
            let mut data = data.borrow_mut();
            // Process WASD keys
            let camera = data.camera();

            let step=if shdown {1.0} else if ctdown {0.01} else {0.1};

            if wdown {
            camera.go(step, 0., 0.);
            };
            if sdown {
            camera.go(-step, 0., 0.);
            };
            if adown {
            camera.go(0., -step, 0.); 
            };
            if ddown {
            camera.go(0., step, 0.);
            };
            if rdown {
            camera.go(0., 0., step); 
            };
            if fdown {
            camera.go(0., 0.0, -step);
            };
            // Process Q and E keys. They control camera's roll
            if qdown {
            camera.rotz(-step);
            };
            if edown {
            camera.rotz(step);
            };
        }
        // For this simple program I don't separate update and render steps.
        // State change and rendering is done inside on_render.
        // Error handling isn't implemented yet. on_render panics, if it needs to.
        // TODO: process error
        ::perf_start("total");
        data.borrow_mut().on_render();
        ::perf_end("total");
        // register rendered frame in performance collector
        ::perf_frame();
        // fps counting stuff
        let now = precise_time_s();
        let frames = PERFDATA.with(|p_data| p_data.borrow().frames);
        if frames>0 && now<start || now>=(start+1.0) {
            // Once per second show stats
            print!("Adapter {} FPS: {:4}", id, frames);
            PERFDATA.with(|p_data| {
                let p_data = p_data.borrow();
                let frames = p_data.frames as f64;
                for (&pname, val) in p_data.perf.iter().sorted_by(|&(&n1,_),&(&n2,_)| n1.cmp(n2)) {
                    print!(" {}:{:.2}", pname, val*1000./frames);
                };
                println!("");
            });
            //println!("Adapter {} FPS: {:3} clear:{:.2} fill:{:.2} exec:{:.2} present:{:.2} wait:{:.2}  ", id, frames, clear, fill, exec, present, wait);
            let _ = ::std::io::Write::flush(&mut ::std::io::stdout()); 
            perf_reset();
            start = now;
        }
        }
    }
    }
    // Application should exit fullscreen state before terminating. 
    data.borrow().set_fullscreen(false).expect("Fullscreen mode isn't supported");
    // wait for all GPU processing to stop
    data.borrow().wait_frame();
    // Save info_queue before final release of resources
    let maybe_iq = data.borrow_mut().take_info_queue();
    // release resources
    drop(data);
    // Let debug layer say its last words
    if let Some(iq) = maybe_iq {
        core::dump_info_queue(&iq);
    };
}


// Simple render statistics collector
pub struct PerfData {
  frames: u64,
  perf: HashMap<&'static str, f64>,
}

impl PerfData {
  fn new() -> PerfData {
    PerfData {
      frames: 0,
      perf: HashMap::new(),
    }
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
    *pd.borrow_mut().perf.entry(name).or_insert(0.) -= precise_time_s();
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
