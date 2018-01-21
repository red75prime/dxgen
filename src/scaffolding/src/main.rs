#![allow(unused_imports)]
//#![feature(optin_builtin_traits)]
//#![feature(clone_from_slice)]
//#![feature(question_mark)]

//#[macro_use] extern crate gfx;
extern crate dxsafe;
extern crate env_logger;
#[macro_use]
extern crate lazy_static;
extern crate libc;
#[macro_use]
extern crate log;
extern crate winapi;
//extern crate dxguid_sys;
extern crate d3dcompiler as d3dcompiler_sys;
extern crate dxgi as dxgi_sys;
#[macro_use]
extern crate failure;
extern crate kernel32;
extern crate user32;
//extern crate d3d12 as d3d12_sys; // I moved this into winapi in my fork, but it expected to be so in winapi 0.3
extern crate cgmath;
extern crate crossbeam;
extern crate d2d1 as d2d1_sys;
extern crate dwrite as dwrite_sys;
extern crate getopts;
extern crate itertools;
extern crate nalgebra;
extern crate ncollide;
extern crate obj;
extern crate rand;
extern crate time;

#[macro_use]
mod macros;
mod create_device;
mod error_conversion;
mod window;
mod utils;
//mod gfx_d3d12;
mod shape_gen;
mod dxsems;
mod core;
mod cubes;
mod camera;
//mod hvoxel;
mod downsampler;
mod tonemapper;
mod plshadow;
//mod collision;
mod cubestate;
mod light;
//mod d2d1test;
mod drawtext;
mod skybox;
mod seh;
mod wmtext;
mod ws;

#[cfg(feature = "openal")]
mod sound;

#[cfg(not(feature = "openal"))]
mod nosound;
#[cfg(not(feature = "openal"))]
use nosound as sound;

use create_device::*;
use cubes::CubeParms;
use dxsafe::*;
use failure::{err_msg, Error, ResultExt};
use error_conversion::ResultExtHr;
use getopts::Options;
use itertools::Itertools;
use std::env;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::process;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use utils::*;
use winapi::*;
use window::*;

const FRAME_COUNT: u32 = 3;

fn parse_non_zero_u32(s: &str) -> Result<u32, Error> {
    let n = s.parse()?;
    if n > 0 {
        Ok(n)
    } else {
        Err(err_msg("number shouldn't be zero"))
    }
}

#[derive(Debug, Copy, Clone)]
enum IncExc {
    Inc(f32),
    Exc(f32),
}

use IncExc::{Exc, Inc};

fn parse_f32_range(s: &str, mn: IncExc, mx: IncExc) -> Result<f32, Error> {
    let x = s.parse()?;
    let in_range = match (mn, mx) {
        (Inc(mn), Inc(mx)) => mn <= x && x <= mx,
        (Exc(mn), Inc(mx)) => mn < x && x <= mx,
        (Inc(mn), Exc(mx)) => mn <= x && x < mx,
        (Exc(mn), Exc(mx)) => mn < x && x < mx,
    };
    if in_range {
        Ok(x)
    } else {
        Err(err_msg(format!(
            "number {} not in range {:?} .. {:?}",
            x, mn, mx
        )))
    }
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [0] [1] [N] [options]", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    match mainr() {
        Err(err) => {
            for c in err.causes() {
                println!("{}", c);
            }
            //println!("{}", err.backtrace());
            process::exit(101);
        }
        _ => (),
    }
}

fn mainr() -> Result<(), Error> {
    // install SEH guard to continue on continuable structured exception
    let _seh_guard = seh::SehGuard::new();
    // install ctrl-C handler to shutdown gracefully
    seh::install_ctrl_c_handler();
    // Initialize logger
    env_logger::init().unwrap();

    println!("sound::init()");
    sound::init();

    // Set default values of cubes module parameters
    let mut parms = CubeParms {
        thread_count: 2,
        object_count: 2_000,
        backbuffer_count: FRAME_COUNT,
        speed_mult: 1.0,
        concurrent_state_update: true, // unused
        debug_layer: cfg!(debug_assertions),
        rt_format: DXGI_FORMAT_R16G16B16A16_FLOAT,
        enable_srgb: false,
        render_trace: false,
        fovy_deg: 60.,
        render_text: true,
    };
    let mut adapters_to_test = vec![];
    let mut adapters_info = false;

    let program_name = env::args().next().unwrap().clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "Usage");
    opts.optflag("i", "info", "Adapters information");
    opts.optflag("", "f32-color", "Render using R32G32B32_FLOAT");
    opts.optflag("d", "debug", "Enable debug layer");
    opts.optflag("", "nodebug", "Disable debug layer");
    opts.optflag("", "notext", "Disable DWrite text output");
    opts.optflag("", "trace", "Enable logging in render cycle");
    opts.optflag(
        "",
        "srgb",
        "Pretend that backbuffer uses sRGB (doesn't seem to affect anything)",
    );
    opts.optopt("o", "objects", "Number of cubes", "NUM");
    opts.optopt("t", "", "Number of state update threads", "NUM");
    opts.optopt("b", "", "Number of backbuffers", "NUM");
    opts.optopt("s", "", "Time speed multiplier", "FLOAT");
    opts.optopt("", "fov", "Vertical FOV", "FLOAT");
    let ms = opts.parse(env::args().skip(1))?;
    // help
    if ms.opt_present("h") {
        print_usage(&program_name, opts);
        return Ok(());
    };
    // adapters to use
    for anum in &ms.free {
        let n = anum.parse::<u32>()
            .with_context(|_| format!("Unrecognized command line argument \"{}\"", anum))?;
        adapters_to_test.push(n);
    }
    // adapters info
    if ms.opt_present("i") {
        adapters_info = true;
    };
    // use f32 color
    if ms.opt_present("f32-color") {
        parms.rt_format = DXGI_FORMAT_R32G32B32A32_FLOAT;
    };
    // enable sRGB
    if ms.opt_present("srgb") {
        parms.enable_srgb = true;
    };
    if ms.opt_present("d") {
        parms.debug_layer = true;
    };
    if ms.opt_present("nodebug") {
        parms.debug_layer = false;
    };
    if ms.opt_present("notext") {
        parms.render_text = false;
    };
    if ms.opt_present("trace") {
        parms.render_trace = true;
    };
    // object count
    if let Some(num) = ms.opt_str("o") {
        parms.object_count = parse_non_zero_u32(&num).with_context(|_| {
            format!(
                "Object count in -o should be positive non-zero integer, not '{}'",
                num
            )
        })?;
    };

    if let Some(num) = ms.opt_str("t") {
        parms.thread_count = parse_non_zero_u32(&num).with_context(|_| {
            format!(
                "Thread count in -t should be positive non-zero integer, not '{}'",
                num
            )
        })?;
    };
    if let Some(num) = ms.opt_str("b") {
        parms.backbuffer_count = parse_non_zero_u32(&num).with_context(|_| {
            format!(
                "Backbuffer count in -b should be positive non-zero integer, not '{}'",
                num
            )
        })?;
    };
    if let Some(num) = ms.opt_str("s") {
        parms.speed_mult = parse_f32_range(&num, Inc(0.), Inc(::std::f32::INFINITY))
            .with_context(|_| {
                format!(
                    "Time speed multiplier in -s should be non-negative floating number, not '{}'",
                    num
                )
            })?;
    };
    if let Some(num) = ms.opt_str("fov") {
        parms.fovy_deg = parse_f32_range(&num, Exc(0.), Exc(180.)).with_context(|_| {
            format!(
                "FOV in --fov should be floating number in (0., 180.) range, not '{}'",
                num
            )
        })?;
    };

    let factory: DXGIFactory4 = match create_dxgi_factory2(parms.debug_layer) {
        Ok(fact) => fact,
        Err(hr) => bail!("Cannot create DXGIFactory4 (0x{:x}). No can do. {}", hr, utils::hr2msg(hr)),
    };
    let mut adapters = vec![];
    // Iterate over available GPUs
    for (i, adapter) in &factory {
        let descr = adapter.get_desc1().unwrap();
        println!(
            "Adapter {}: {}",
            i,
            wchar_array_to_string_lossy(&descr.Description)
        );
        println!(
            "   Dedicated video memory: {}MiB",
            descr.DedicatedVideoMemory / 1024 / 1024
        );

        if adapters_to_test.is_empty() || adapters_to_test[..].contains(&i) {
            // If there's no numbers in command line add each and every available adapter,
            // otherwise use adapter numbers from command line
            if adapters_info {
                print_adapter_info(&adapter);
            }
            adapters.push(adapter);
        }
    }

    if adapters_info {
        return Ok(());
    }

    // I used this mutex to sync console output.
    let mutex = Arc::new(Mutex::new(()));
    crossbeam::scope(|scope| {
        //// d2d1 test window
        // scope.spawn(|| {
        //   if let Err(err) = d2d1test::main() {
        //     error!("d2d1test::main() error 0x{:x}", err);
        //   }
        // });

        for (id, a) in adapters.into_iter().enumerate() {
            let mutex = mutex.clone();
            let parms = &parms;
            let dxgi_factory = factory.clone();
            scope.spawn(move || {
                // Spawn a thread for each adapter
                match main_prime(id, dxgi_factory, a, mutex, parms) {
                    Err(err) => {
                        let stdout = ::std::io::stdout();
                        let mut handle = stdout.lock();
                        for c in err.causes() {
                            let _ = writeln!(handle, "{}", c);
                        }
                    }
                    _ => (),
                }
            });
        }
    });
    seh::uninstall_ctrl_c_handler();
    Ok(())
}

fn print_adapter_info(adapter: &DXGIAdapter1) {
    if let Ok(dev) = d3d12_create_device(Some(adapter), D3D_FEATURE_LEVEL_11_0) {
        if let Ok(data) = dev.check_feature_support_virtual_address() {
            println!("   {:#?}", data);
        }
        if let Ok(data) = dev.check_feature_support_options() {
            println!("   {:#?}", data);
        }

        let rdesc = D3D12_RESOURCE_DESC {
            Dimension: D3D12_RESOURCE_DIMENSION_TEXTURE1D,
            Alignment: 0,
            Width: 1024,
            Height: 1,
            DepthOrArraySize: 1,
            MipLevels: 10,
            Format: DXGI_FORMAT_R32G32B32A32_FLOAT,
            SampleDesc: DXGI_SAMPLE_DESC {
                Count: 1,
                Quality: 0,
            },
            Layout: D3D12_TEXTURE_LAYOUT_UNKNOWN,
            Flags: D3D12_RESOURCE_FLAG_NONE,
        };
        let rai = dev.get_resource_allocation_info(0, &[rdesc]);
        println!("   {:#?}", rai);
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
const VK_P: i32 = b'P' as i32;

const KEYS_LEN: usize = 256;
struct KeyState {
    keys: [bool; KEYS_LEN],
}

impl KeyState {
    pub fn set(&mut self, vk: i32) {
        if vk >= 0 && vk < self.keys.len() as i32 {
            self.keys[vk as usize] = true;
        }
    }
    pub fn unset(&mut self, vk: i32) {
        if vk >= 0 && vk < self.keys.len() as i32 {
            self.keys[vk as usize] = false;
        }
    }

    pub fn pressed(&self, vk: i32) -> bool {
        if vk >= 0 && vk < self.keys.len() as i32 {
            self.keys[vk as usize]
        } else {
            false
        }
    }
}

impl Default for KeyState {
    fn default() -> KeyState {
        KeyState {
            keys: [false; KEYS_LEN],
        }
    }
}

fn main_prime(
    id: usize,
    dxgi_factory: DXGIFactory4,
    adapter: DXGIAdapter1,
    _mutex: Arc<Mutex<()>>,
    parms: &CubeParms,
) ->  Result<(), Error>
{
    // Setup window. Currently window module supports only one window per thread.
    let descr = wchar_array_to_string_lossy(&adapter.get_desc1().into_error_context("Cannot get adapter description")?.Description);
    let title = format!("D3D12 Hello, rusty world! ({})", descr);
    let wnd = create_window(&title, 512, 256);

    let mut fps = 0.0;
    // Initialization of cubes module data required to render all stuff
    let data = 
        RefCell::new(
            cubes::AppData::on_init(&wnd, &dxgi_factory, &adapter, parms)
            .with_context(|_| format!("Adapter '{}' failed initialization", descr))?);

    // x and y store last mouse coords from WM_MOUSEMOVE
    let mut x: i32 = 0;
    let mut y: i32 = 0;

    let mut keys = KeyState::default();

    // state of left mouse button
    let mut mouse_down = false;
    let mut pause = false;
    // Profiling stuff
    let mut start = time::precise_time_s();
    let mut prev_frame_time = time::precise_time_s();
    // Window::poll_events() returns non-blocking iterator, which returns Option<MSG>
    for mmsg in wnd.poll_events() {
        // "if let Some(msg)" extracts msg from mmsg
        // if mmsg is None, then 'else' branch is taken
        if let Some(msg) = mmsg {
            //trace!("{:?} {} {:x} {:x}", msg.time, wmtext::wm_str(msg.message), msg.wParam, msg.lParam);
            // Instead of passing messages into cubes module, I process them here
            // It is not well-thought-out design decision, it's just slightly simpler now, and cost of changing it is low.
            match msg.message {
                // Usual message processing stuff
                WM_SIZE => {
                    // Normally this message goes to wndproc, in window.rs I repost it into message queue to prevent reentrancy problems
                    debug!("WM_SIZE {}, {}  ", msg.wParam, msg.lParam);
                    data.borrow_mut().on_resize(
                        LOWORD(msg.lParam as u32) as u32,
                        HIWORD(msg.lParam as u32) as u32,
                        msg.wParam as u32,
                    );
                }
                WM_MOUSEMOVE => {
                    let x1 = GET_X_LPARAM(msg.lParam) as i32;
                    let y1 = GET_Y_LPARAM(msg.lParam) as i32;
                    let (dx, dy) = (x1 - x, y - y1);
                    x = x1;
                    y = y1;
                    if mouse_down {
                        let mut data = data.borrow_mut();
                        let camera = data.camera();
                        let fov = camera.fov();
                        camera.roty(dy as f32 * fov / 300.);
                        camera.rotx(-dx as f32 * fov / 300.);
                    }
                }
                WM_MOUSEWHEEL => {
                    let dz = (GET_WHEEL_DELTA_WPARAM(msg.wParam) as f32) / (WHEEL_DELTA as f32);
                    let mut data = data.borrow_mut();
                    let camera = data.camera();
                    let mut vfov = camera.fov();
                    if vfov < 5.0 {
                        vfov -= dz * 0.2;
                    } else {
                        vfov -= dz * 2.0;
                    };
                    vfov = f32::max(vfov, 0.2);
                    vfov = f32::min(vfov, 120.);
                    camera.set_fov(vfov);
                }
                WM_KEYDOWN => {
                    keys.set(msg.wParam as i32);
                }
                WM_KEYUP => {
                    let vk = msg.wParam as i32;
                    keys.unset(vk);
                    if vk == VK_P {
                        pause = !pause;
                    };
                }
                WM_LBUTTONDOWN => {
                    mouse_down = true;
                    set_capture(wnd.get_hwnd());
                }
                WM_LBUTTONUP => {
                    mouse_down = false;
                    release_capture();
                }
                _ => {}
            };
        } else {
            // There's no pending window message.
            let do_not_render = data.borrow().is_minimized();
            if do_not_render {
                // MSDN suggest to use MsgWaitForMultipleObjects here, but 10ms sleep shouldn't create problems
                std::thread::sleep(Duration::from_millis(10));
            } else {
                let cur_frame_time = time::precise_time_s();
                let frame_dt = cur_frame_time - prev_frame_time;
                prev_frame_time = cur_frame_time;
                // data is Rc<RefCell<cubes::AppData>>
                // Rc is not really needed. I didn't pass it around.
                // Take a mutable reference to cubes::AppData
                let mut data = data.borrow_mut();
                {
                    // Process WASD keys
                    let camera = data.camera();

                    camera.restore_up((180. * frame_dt) as f32);

                    let step = match (keys.pressed(VK_SHIFT), keys.pressed(VK_CONTROL)) {
                        (false, false) => 0.1,
                        (true, false) => 1.0,
                        (false, true) => 0.01,
                        (true, true) => 0.001,
                    };

                    if keys.pressed(VK_W) {
                        camera.go(step, 0., 0.);
                    };
                    if keys.pressed(VK_S) {
                        camera.go(-step, 0., 0.);
                    };
                    if keys.pressed(VK_A) {
                        camera.go(0., -step, 0.);
                    };
                    if keys.pressed(VK_D) {
                        camera.go(0., step, 0.);
                    };
                    if keys.pressed(VK_R) {
                        camera.go(0., 0., step);
                    };
                    if keys.pressed(VK_F) {
                        camera.go(0., 0., -step);
                    };
                    // Process Q and E keys. They control camera's roll
                    if keys.pressed(VK_Q) {
                        camera.rotz(-step);
                    };
                    if keys.pressed(VK_E) {
                        camera.rotz(step);
                    };
                }
                // For this simple program I don't separate update and render steps.
                // State change and rendering is done inside on_render.
                ::perf_start("total");
                let render_dt = if frame_dt > 0.1 { 0.1 } else { frame_dt as f32 };
                data.on_render(pause, fps, render_dt)?;
                ::perf_end("total");
                // register rendered frame in performance collector
                ::perf_frame();
                // fps counting stuff
                let now = time::precise_time_s();
                let frames = PERFDATA.with(|p_data| p_data.borrow().frames);
                if frames > 0 && now < start || now >= (start + 1.0) {
                    // Once per second show stats
                    print!("Adapter {} FPS: {:4}", id, frames);
                    fps = frames as f32;
                    PERFDATA.with(|p_data| {
                        let p_data = p_data.borrow();
                        let frames = p_data.frames as f64;
                        for (&pname, val) in p_data
                            .perf
                            .iter()
                            .sorted_by(|&(&n1, _), &(&n2, _)| n1.cmp(n2))
                        {
                            print!(" {}:{:.2}", pname, val * 1000. / frames);
                        }
                        println!("");
                    });
                    let _ = ::std::io::Write::flush(&mut ::std::io::stdout());
                    perf_reset();
                    start = now;
                } // stats
            } // do not render
        } // no window message
        if seh::ctrl_c_is_triggered() {
            break;
        };
    } // message loop
      // Application should exit fullscreen state before terminating.
    data.borrow()
        .set_fullscreen(false)
        .expect("Cannot exit fullscreen mode");
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
    Ok(())
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
        let mut pd = pd.borrow_mut();
        *pd = PerfData::new();
    });
}

pub fn perf_start(name: &'static str) {
    PERFDATA.with(|pd| {
        *pd.borrow_mut().perf.entry(name).or_insert(0.) -= time::precise_time_s();
    });
}

pub fn perf_end(name: &'static str) {
    PERFDATA.with(|pd| {
        *pd.borrow_mut().perf.get_mut(name).unwrap() += time::precise_time_s();
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
