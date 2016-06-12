use winapi::*;
use user32::{CreateWindowExW, RegisterClassExW, GetClassInfoExW, DefWindowProcW, PostQuitMessage,
             LoadCursorW, GetClientRect};
use kernel32::GetModuleHandleW;
use std::ptr;
use libc;

use utils::*;
use std::mem;
use std::cell::RefCell;
use std::rc::Rc;
use std::marker::PhantomData;

use user32::{TranslateMessage, DispatchMessageW, PeekMessageW, GetMessageW};
// use kernel32::{Sleep};

pub struct Window {
    hwnd: HWND,
    _nosync_nosend: PhantomData<Rc<u32>>,
}

#[allow(dead_code)]
pub struct BlockingEventIterator<'a> {
    wnd: &'a Window,
}

impl<'a> Iterator for BlockingEventIterator<'a> {
    type Item = MSG;

    fn next(&mut self) -> Option<Self::Item> {
        let mut maybe_injected_msg = None;
        // Extract WM_SIZE message from thread-local data associated with this window
        WINDOW.with(|rc| {
            if let Some(ThreadLocalData {inject_msg: ref mut imsg, ..}) = *rc.borrow_mut() {
                maybe_injected_msg = imsg.take();
            }
        });
        if let Some(imsg) = maybe_injected_msg {
            return Some(imsg);
        }
        let mut msg = unsafe { mem::uninitialized::<MSG>() };
        loop {
            let ret = unsafe { GetMessageW(&mut msg, ptr::null_mut(), 0, 0) };
            if ret == 0 {
                return None;
            }
            if msg.message == WM_QUIT {
                return None;
            } else if msg.message == WM_PAINT {
                return Some(msg);
            } else if msg.hwnd == self.wnd.hwnd {
                unsafe { TranslateMessage(&mut msg) };
                unsafe { DispatchMessageW(&mut msg) };
                return Some(msg);
            } else {
                // Wrong window. Broadcast?
                // skip this message and wait next
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, None)
    }
}

pub struct PollEventIterator<'a> {
    wnd: &'a Window,
}

impl<'a> Iterator for PollEventIterator<'a> {
    type Item = Option<MSG>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut maybe_injected_msg = None;
        // Extract WM_SIZE message from thread-local data associated with this window
        WINDOW.with(|rc| {
            if let Some(ThreadLocalData {inject_msg: ref mut imsg, ..}) = *rc.borrow_mut() {
                maybe_injected_msg = imsg.take();
            }
        });
        if let Some(imsg) = maybe_injected_msg {
            return Some(Some(imsg));
        }
        let mut msg = unsafe { mem::uninitialized::<MSG>() };
        let ret = unsafe { PeekMessageW(&mut msg, ptr::null_mut(), 0, 0, PM_REMOVE) };
        if ret == 0 {
            Some(None)
        } else {
            // debug!("Windows message:{}", msg.message);
            if msg.message == WM_QUIT {
                None
            } else if msg.hwnd == self.wnd.hwnd {
                unsafe { TranslateMessage(&mut msg) };
                unsafe { DispatchMessageW(&mut msg) };
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
        PollEventIterator { wnd: &self }
    }
    #[allow(dead_code)]
    pub fn events<'a>(&'a self) -> BlockingEventIterator<'a> {
        BlockingEventIterator { wnd: &self }
    }
    pub fn size(&self) -> (u32, u32) {
        let mut rect = RECT {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
        };
        if unsafe { GetClientRect(self.hwnd, &mut rect) } == 0 {
            (0, 0)
        } else {
            (rect.right as u32, rect.bottom as u32)
        }
    }
}

impl Clone for Window {
    fn clone(&self) -> Self {
        Window { hwnd: self.hwnd, _nosync_nosend: self._nosync_nosend }
    }
}
impl Copy for Window {}
//Doesn't work in stable Rust 1.7.0
//impl !Send for Window {}
//impl !Sync for Window {}

type ResizeFn = Box<FnMut(u32, u32, u32) -> ()>;

struct ThreadLocalData {
    hwnd: HWND,
    inject_msg: Option<MSG>, // MSG for injection into message queue
}

thread_local!(static WINDOW: RefCell<Option<ThreadLocalData>> = RefCell::new(None) );

unsafe extern "system" fn wnd_callback(hwnd: HWND,
                                       msg: UINT,
                                       wparam: WPARAM,
                                       lparam: LPARAM)
                                       -> LRESULT {
    match msg {
        WM_DESTROY => {
            PostQuitMessage(0);
            return 0;
        }
        WM_SIZE => {
            // WM_SIZE can be received while calling DXGISwapChain::present
            // so, it's better to postpone processing
            trace!("WM_SIZE wndproc");
            WINDOW.with(|rc| {
                if let Some(ThreadLocalData {inject_msg: ref mut imsg, ..}) = *rc.borrow_mut() {
                    *imsg = Some(MSG {
                        hwnd: hwnd,
                        message: msg,
                        wParam: wparam,
                        lParam: lparam,
                        time: 0,
                        pt: POINT { x: 0, y: 0 },
                    });
                }
            });
            return 0;
        }
        _ => (),
    }
    DefWindowProcW(hwnd, msg, wparam, lparam)
}

// No more than 1 (one) window per thread for now
pub fn create_window(title: &str, width: i32, height: i32) -> Window {
    if let Some(wnd) = WINDOW.with(|rc| {
        match *rc.borrow() {
            Some(ref td) => Some(Window { hwnd: td.hwnd, _nosync_nosend: PhantomData }),
            _ => None,
        }
    }) {
        return wnd;
    };
    let class_name = str_to_vec_u16("RustMainWndClass_14f22");
    let h_module = unsafe { GetModuleHandleW(ptr::null_mut()) };
    let mut class = WNDCLASSEXW {
        cbSize: mem::size_of::<WNDCLASSEXW>() as UINT,
        style: CS_DBLCLKS | CS_HREDRAW | CS_VREDRAW,
        lpfnWndProc: Some(wnd_callback),
        cbClsExtra: 0,
        cbWndExtra: 0,
        hInstance: h_module,
        hIcon: ptr::null_mut(),
        hCursor: unsafe { LoadCursorW(ptr::null_mut(), IDC_ARROW) },
        hbrBackground: ptr::null_mut(),
        lpszMenuName: ptr::null_mut(),
        lpszClassName: class_name.as_ptr(),
        hIconSm: ptr::null_mut(),
    };
    if unsafe { GetClassInfoExW(h_module, class_name.as_ptr(), &mut class as *mut _) } == 0 {
        unsafe { RegisterClassExW(&mut class as *mut _) };
    }

    let w_title = str_to_vec_u16(title);
    let hwnd = unsafe {
        CreateWindowExW(0,
                        class_name.as_ptr(),
                        w_title.as_ptr(),
                        WS_VISIBLE | WS_OVERLAPPEDWINDOW,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        width as libc::c_int,
                        height as libc::c_int,
                        ptr::null_mut(),
                        ptr::null_mut(),
                        h_module,
                        ptr::null_mut())
    };
    if hwnd == ptr::null_mut() {
        panic!("CreateWindowExW failed.");
    }
    WINDOW.with(|rc| {
        let td = &mut *rc.borrow_mut();
        *td = Some(ThreadLocalData {
            hwnd: hwnd,
            inject_msg: None,
        });
    });
    Window { hwnd: hwnd, _nosync_nosend: PhantomData }
}
