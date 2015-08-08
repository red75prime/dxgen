#![feature(libc)]

use std::fmt;

use libc::{GUID,c_void};

pub type IID=GUID;

pub mod iids {
  use super::IID;
  
  pub static IID_IUnknown : IID = IID {Data1:0, Data2:0, Data3:0, Data4:[0,0,0,0,0,0,0,0,],};  
}

pub type HRESULT=u32;

pub type UINT64=u64;

pub type ULONG=u32;

pub type REFGUID=&'static GUID;

pub type HResult<T>=Result<T, HRESULT>;

#[allow(non_snake_case)}
#[repr(C)]
pub struct IUnknownVtbl {
  QueryInterface : extern "system" fn (This : *mut *mut IUnknownVtbl, riid : REFGUID, ppvObject : *mut *mut c_void) -> HRESULT,
  AddRef : extern "system" fn (This : *mut *mut IUnknownVtbl) -> ULONG,
  Release : extern "system" fn (This : *mut *mut IUnknownVtbl) -> ULONG,
}

pub trait HasIID {
  fn iid() -> REFGUID;
  fn new(ppVtbl : *mut *mut IUnknownVtbl) -> Self;
  fn expose_iptr(&self) -> *mut *mut IUnknownVtbl;
}

struct UnknownPtr(*mut *mut IUnknownVtbl);

impl HasIID for UnknownPtr {
  fn iid() -> REFGUID { &iids::IID_IUnknown }
  fn new(pp_vtbl : *mut *mut IUnknownVtbl) -> Self {
    UnknownPtr(pp_vtbl)
  }
  fn expose_iptr(&self) -> *mut *mut IUnknownVtbl {
    self.0
  }
}

pub fn release_com_ptr<T:HasIID>(obj : &mut T) {
  let iunk=obj.expose_iptr();
  if iunk==::std::ptr::null_mut() {
    // 
  } else {
    unsafe {
      ((**iunk).Release)(iunk);
    }
  }
}

pub fn clone_com_ptr<T:HasIID>(obj: &T) -> T {
  let iunk=obj.expose_iptr();
  let ret=T::new(iunk);
  unsafe {
    ((**iunk).AddRef)(iunk);
  }
  ret
}

impl Drop for UnknownPtr {
  fn drop(&mut self) {
    release_com_ptr(self);
  }
}

impl Clone for UnknownPtr {
  fn clone(&self) -> Self {
    clone_com_ptr(self) 
  }
}

pub trait QueryInterface {
  fn query_interface<T:HasIID>(&self) -> HResult<T>;
}

impl<T:HasIID> QueryInterface for T {
  fn query_interface<O:HasIID>(&self) -> HResult<O> {
    let iunk=self.expose_iptr();
    let mut ounk : *mut IUnknownVtbl=::std::ptr::null_mut();
    let hr=unsafe {
      ((**iunk).QueryInterface)(iunk, O::iid(), &mut ounk as *mut *mut _ as *mut *mut c_void)
    };
    if hr==0 {
      Ok(O::new(&mut ounk))
    } else {
      Err(hr)
    }
  }
}
