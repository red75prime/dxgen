use d3d12_sys::*;
use d3d12::*;
use iid::{HResult,HasIID,IUnknown};
use std::ptr::{null_mut};
use libc::c_void;

pub fn d3d12_create_device(min_feat_level: D3D_FEATURE_LEVEL) -> HResult<D3D12Device> {
  let mut pp_dev : *mut IUnknown = null_mut();
  let hr=unsafe {D3D12CreateDevice(null_mut(), min_feat_level, &IID_ID3D12Device, &mut pp_dev as *mut *mut _ as *mut *mut c_void) }; 
  if hr==0 {
    Ok(D3D12Device::new(pp_dev))
  } else {
    Err(hr)
  }
}
