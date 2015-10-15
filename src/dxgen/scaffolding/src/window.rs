extern crate winapi;
extern crate user32;
extern crate kernel32;

use winapi::*;
use self::user32::{CreateWindowExW,RegisterClassExW, GetClassInfoExW, DefWindowProcW, PostQuitMessage, LoadCursorW };
use self::kernel32::{GetModuleHandleW};
use std::ptr;
use libc;

use utils::*;
use std::mem;
use std::cell::RefCell;

use self::user32::{GetMessageW,TranslateMessage,DispatchMessageW,PeekMessageW};
//use self::kernel32::{Sleep};

pub struct Window {
  hwnd: HWND,
}

pub struct PollEventIterator<'a> {
  wnd: &'a Window,
}

impl<'a> Iterator for PollEventIterator<'a> {
  type Item = Option<MSG>;
  
  fn next(&mut self) -> Option<Self::Item> {
    let mut msg = unsafe{ mem::uninitialized::<MSG>() };
    let ret = unsafe{ PeekMessageW(&mut msg, ptr::null_mut(), 0, 0, PM_REMOVE) };
    if ret == 0 {
      Some(None)
    } else {
      //debug!("Windows message:{}", msg.message);
      if msg.message == WM_QUIT {
        None
      } else if msg.hwnd == self.wnd.hwnd {
        unsafe{TranslateMessage(&mut msg)};
        unsafe{DispatchMessageW(&mut msg)};
        Some(Some(msg))
      } else {
        Some(None)
      }
    }
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    (0, None)
  }
}

impl Window {
  pub fn get_hwnd(&self) -> HWND {
    self.hwnd
  }
  pub fn poll_events<'a>(&'a self) -> PollEventIterator<'a> {
    PollEventIterator{wnd: &self}
  }
  pub fn set_resize_fn(&self, resize_fn: ResizeFn) {
    WINDOW.with(|rc| {
      let mtd=&mut *rc.borrow_mut();
      match mtd {
        &mut Some(ref mut td) => {
          td.resize_fn = Some(resize_fn);
        },
        &mut None => panic!(),
      };
    });
  }
}

impl Clone for Window {
  fn clone(&self) -> Self {
    Window {hwnd: self.hwnd}
  }
}
impl Copy for Window {}
impl !Send for Window {}
impl !Sync for Window {}

type ResizeFn = Box<FnMut(u32, u32, u32) -> ()>;

struct ThreadLocalData {
  hwnd: HWND,
  resize_fn: Option<ResizeFn>,
}

thread_local!(static WINDOW: RefCell<Option<ThreadLocalData>> = RefCell::new(None) );

unsafe extern "system" fn wnd_callback(hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM ) -> LRESULT {
  match msg {
    WM_DESTROY => {
      println!("WM_DESTROY");
      PostQuitMessage(0);
      return 0;
    },
    WM_SIZE => {
      WINDOW.with(|rc| {
        if let Some(ThreadLocalData {resize_fn: Some(ref mut resize), ..}) = *rc.borrow_mut() {
          (*resize)(LOWORD(lparam as u32) as u32, HIWORD(lparam as u32) as u32, wparam as u32 );
        }
      });
      return 0;
    },
    _ => ()
  }
  DefWindowProcW(hwnd, msg, wparam, lparam)
}

// No more than 1 (one) window for now
pub fn create_window(title: &str, width: i32, height: i32) -> Window {
  if let Some(wnd) = WINDOW.with(|rc|{match *rc.borrow() {Some(ref td) => Some(Window{hwnd: td.hwnd}), _ => None}}) {
    return wnd;
  };
  let class_name=str_to_vec_u16("RustMainWndClass_14f22");
  let h_module=unsafe{GetModuleHandleW(ptr::null_mut())};
  let mut class=winapi::WNDCLASSEXW{
    cbSize: mem::size_of::<winapi::WNDCLASSEXW>() as UINT,
    style: winapi::CS_DBLCLKS | winapi::CS_HREDRAW | winapi::CS_VREDRAW,
    lpfnWndProc: Some(wnd_callback),
    cbClsExtra: 0,
    cbWndExtra: 0,
    hInstance: h_module,
    hIcon: ptr::null_mut(),
    hCursor: unsafe{ LoadCursorW(ptr::null_mut(), IDC_ARROW) },
    hbrBackground: ptr::null_mut(),
    lpszMenuName: ptr::null_mut(),
    lpszClassName: class_name.as_ptr(),
    hIconSm: ptr::null_mut(),
  };
  if unsafe{GetClassInfoExW(h_module, class_name.as_ptr(), &mut class as *mut _)} == 0 {
    unsafe{RegisterClassExW(&mut class as *mut _)};
  }

  let w_title=str_to_vec_u16(title);
  let hwnd=unsafe {CreateWindowExW(0, class_name.as_ptr() ,w_title.as_ptr(), 
                winapi::WS_VISIBLE | winapi::WS_OVERLAPPEDWINDOW,
                winapi::CW_USEDEFAULT, winapi::CW_USEDEFAULT, 
                width as libc::c_int, height as libc::c_int, 
                ptr::null_mut(), ptr::null_mut(), h_module, ptr::null_mut())};
  if hwnd == ptr::null_mut() {
    panic!("CreateWindowExW failed.");
  }
  WINDOW.with(|rc| {
    let td=&mut *rc.borrow_mut();
    *td = Some(ThreadLocalData{hwnd: hwnd, resize_fn: None });
  });
  Window {
    hwnd: hwnd,
  }
}
