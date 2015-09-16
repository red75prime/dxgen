use winapi::*;
use d3d12_safe::*;
//use d3d12_sys::*;
use dxguid_sys::*;
use dxgi_sys::*;
use std::ptr;
use libc;

#[link(name="d3d12")]
extern "system" {
    pub fn D3D12CreateDevice(pAdapter: *mut IUnknown, MinimumFeatureLevel: D3D_FEATURE_LEVEL, riid: REFGUID, ppDevice: *mut *mut c_void) -> HRESULT;
    pub fn D3D12CreateRootSignatureDeserializer(pSrcData: LPCVOID, SrcDataSizeInBytes: SIZE_T, pRootSignatureDeserializerInterface: REFGUID, ppRootSignatureDeserializer: *mut *mut c_void) -> HRESULT;
    pub fn D3D12GetDebugInterface(riid: REFGUID, ppvDebug: *mut *mut c_void) -> HRESULT;
    pub fn D3D12SerializeRootSignature(pRootSignature: *const D3D12_ROOT_SIGNATURE_DESC, Version: D3D_ROOT_SIGNATURE_VERSION, ppBlob: *mut *mut ID3DBlob, ppErrorBlob: *mut *mut ID3DBlob) -> HRESULT;
}

#[link(name="dxgi")]
extern "system" {
    pub fn CreateDXGIFactory2(Flags: UINT, riid: REFGUID, ppFactory: *mut *mut c_void) -> HRESULT;
    pub fn DXGIGetDebugInterface1(Flags: UINT, riid: REFGUID, pDebug: *mut *mut c_void) -> HRESULT;
}


pub fn d3d12_create_device(adapter: Option<&DXGIAdapter>, min_feat_level: D3D_FEATURE_LEVEL) -> HResult<D3D12Device> {
  let mut p_dev : *mut IUnknown = ptr::null_mut();
  let hr=unsafe {D3D12CreateDevice(adapter.map(|p|p.iptr()).unwrap_or(ptr::null_mut()), min_feat_level, &IID_ID3D12Device, &mut p_dev as *mut *mut _ as *mut *mut c_void) }; 
  if hr==0 {
    Ok(D3D12Device::new(p_dev))
  } else {
    Err(hr)
  }
}

pub fn create_dxgi_factory1() -> HResult<DXGIFactory1> {
  let mut p_fac : *mut IUnknown = ptr::null_mut();
  let hr=unsafe {CreateDXGIFactory1(&IID_IDXGIFactory1, &mut p_fac as *mut *mut _ as *mut *mut _) }; 
  if hr==0 {
    Ok(DXGIFactory1::new(p_fac))
  } else {
    Err(hr)
  }
}

pub fn get_debug_interface() -> HResult<D3D12Debug> {
  let mut p_fac : *mut IUnknown = ptr::null_mut();
  let hr=unsafe {D3D12GetDebugInterface(&IID_ID3D12Debug, &mut p_fac as *mut *mut _ as *mut *mut _) }; 
  if hr==0 {
    Ok(D3D12Debug::new(p_fac))
  } else {
    Err(hr)
  }
}

pub fn d3d12_serialize_root_signature(root_signature: &D3D12_ROOT_SIGNATURE_DESC, ver: D3D_ROOT_SIGNATURE_VERSION) -> HResult<D3D10Blob> {
  let mut p_blob : *mut IUnknown = ptr::null_mut();
  let hr=unsafe {D3D12SerializeRootSignature(root_signature, ver, &mut p_blob as *mut *mut _ as *mut *mut _, ptr::null_mut()) }; 
  if hr==0 {
    Ok(D3D10Blob::new(p_blob))
  } else {
    Err(hr)
  }
}

extern "system" {
  fn D3DCompileFromFile(file_name: LPCWSTR, defines: *const D3D_SHADER_MACRO, include: *mut ID3DInclude, entry: LPCSTR, target: LPCSTR, flags1: UINT, flags2: UINT, code: *mut *mut ID3D10Blob, errs: *mut *mut ID3D10Blob) -> HRESULT;
}

pub fn d3d_compile_from_file(file_name: &str, entry: &str, target: &str, flags: UINT) -> Result<Vec<u8>, String> {
  let w_file_name=str_to_vec_u16(file_name);
  let w_entry=str_to_cstring(entry);
  let w_target=str_to_cstring(target);
  let mut blob1: *mut ID3D10Blob = ptr::null_mut();
  let mut blob2: *mut ID3D10Blob = ptr::null_mut();
  let hr=unsafe{D3DCompileFromFile(w_file_name.as_ptr(), ptr::null_mut(), ptr::null_mut(), w_entry.as_ptr(), w_target.as_ptr(), flags, 0, &mut blob1, &mut blob2)};
  if blob2 != ptr::null_mut() {
    let mut blob=D3D10Blob::new(blob2 as *mut _);
    unsafe{
      let blob_slice:&[u8]=::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8, blob.get_buffer_size() as usize);
      let mut ret=vec![];
      ret.extend(blob_slice);
      mem::forget(blob_slice);
      Err(String::from_utf8_lossy(&ret[..]).into_owned())
    }
  } else {
    if hr==0 {
      assert!(blob1 != ptr::null_mut());
      let mut blob=D3D10Blob::new(blob1 as *mut _);
      let mut ret=vec![];
      unsafe {
        let blob_slice:&[u8]=::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8, blob.get_buffer_size() as usize);
        ret.extend(blob_slice);
        mem::forget(blob_slice);
      }
      Ok(ret)
    } else {
      Err("".into())
    }
  }
}

extern crate winapi;
extern crate user32;
extern crate kernel32;

use self::user32::{CreateWindowExW,RegisterClassExW, GetClassInfoExW, DefWindowProcW, PostQuitMessage};
use self::kernel32::{GetModuleHandleW};

use std::ffi::{OsStr, OsString};
use std::os::windows::ffi::OsStrExt;
use std::mem;
use std::borrow::Cow;
use std::cell::RefCell;

fn str_to_vec_u16(s : &str) -> Vec<u16> {
  let osstr = OsString::from(s);
  osstr.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
}

use std::ffi::CString;

pub fn str_to_cstring(s : &str) -> CString {
  CString::new(s).unwrap()
}

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
  static ref WND_MAP: Mutex<HashMap<HWnd, WndWindow>> = {Mutex::new(HashMap::new())};
}

pub type ResizeFn=Option<Box<FnMut(i16,i16,u8)->()>>;

struct WndWindow {
  destroyed: bool,
  hwnd: winapi::HWND,
  resize_fn: ResizeFn,
}

fn loword(lparam: winapi::LPARAM) -> i16 {
  (lparam & 0xFFFF) as i16
}

fn hiword(lparam: winapi::LPARAM) -> i16 {
  ((lparam/65536) & 0xFFFF) as i16
}


unsafe impl Sync for WndWindow {}
unsafe impl Send for WndWindow {}

#[derive(Debug,PartialEq,Eq,Hash,Clone,Copy)]
pub struct HWnd(usize);

unsafe extern "system" fn wnd_callback(hwnd: winapi::HWND, uMsg: UINT, wParam: winapi::WPARAM, lParam: winapi::LPARAM ) -> winapi::LRESULT {
  {
    if let Some(rwnd)=WND_MAP.lock().unwrap().get_mut(&HWnd(hwnd as usize)) {
      match uMsg {
        winapi::WM_DESTROY => {
          println!("WM_DESTROY");
          unsafe {PostQuitMessage(0)};
          return 0;
        },
        winapi::WM_SIZE => {
          if let Some(ref mut boxedfn)=rwnd.resize_fn {
            (*boxedfn)(loword(lParam),hiword(lParam),wParam as u8);
          }
          return 0;
        },
        _ => { },
      }
    } else {
      // Window is being created
    }
  }
  DefWindowProcW(hwnd, uMsg, wParam, lParam)
}

// We cannot place WndWindow on stack, as window contains pointer to this struct
pub fn create_window(title: &str, width: i32, height: i32) -> HWnd {
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
      hCursor: ptr::null_mut(),
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
    panic!();
  }
  let rwnd=WndWindow {
    destroyed: false,
    hwnd: hwnd,
    resize_fn: None,
    };
  let rhwnd=HWnd(hwnd as usize);
  WND_MAP.lock().unwrap().insert(rhwnd, rwnd);
  rhwnd
}

use self::user32::{GetMessageW,TranslateMessage,DispatchMessageW,PeekMessageW};
use self::kernel32::{Sleep};

pub fn message_loop() -> winapi::WPARAM {
  let mut msg: winapi::MSG = unsafe{mem::uninitialized::<_>()};
  loop {
    let ret = unsafe{PeekMessageW(&mut msg, ptr::null_mut(), 0, 0, winapi::PM_REMOVE)};
    //println!("GetMessage returned {:?}. wParam={:?}", ret, msg.wParam);
    if ret == 0 {
      unsafe{Sleep(1)};
    } else {
      if msg.message == winapi::WM_QUIT {return msg.wParam};
      unsafe{TranslateMessage(&mut msg)};
      unsafe{DispatchMessageW(&mut msg)};
    }
  }
}

pub fn set_resize_fn(rhwnd: HWnd, resize_fn: ResizeFn) {
  if let Some(rwnd)=WND_MAP.lock().unwrap().get_mut(&rhwnd) {
    rwnd.resize_fn=resize_fn;
  }
}

pub fn get_hwnd(rhwnd: HWnd) -> Option<HWND> {
  if let Some(rwnd)=WND_MAP.lock().unwrap().get(&rhwnd) {
    Some(rwnd.hwnd as HWND)
  } else {
    None
  }
}
