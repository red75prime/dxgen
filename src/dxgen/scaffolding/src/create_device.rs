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

pub fn create_dxgi_factory1<T: HasIID>() -> HResult<T> {
  let mut p_fac : *mut IUnknown = ptr::null_mut();
  let hr=unsafe {CreateDXGIFactory2(DXGI_CREATE_FACTORY_DEBUG, T::iid(), &mut p_fac as *mut *mut _ as *mut *mut _) }; 
  if hr==0 {
    Ok(T::new(p_fac))
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
    let blob=D3D10Blob::new(blob2 as *mut _);
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
      let blob=D3D10Blob::new(blob1 as *mut _);
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

use self::user32::{CreateWindowExW,RegisterClassExW, GetClassInfoExW, DefWindowProcW, PostQuitMessage, LoadCursorW };
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

