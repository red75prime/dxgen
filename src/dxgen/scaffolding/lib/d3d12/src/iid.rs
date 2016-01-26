use winapi::{HRESULT,REFGUID,c_void};

pub mod iids {
  use winapi::IID;

  #[allow(non_upper_case_globals)]  
  pub static IID_IUnknown : IID = IID {Data1:0, Data2:0, Data3:0, Data4:[0,0,0,0,0,0,0,0,],};  
}

pub type HResult<T>=Result<T, HRESULT>;

use winapi::IUnknown;

pub trait HasIID {
  fn iid() -> REFGUID;
  fn new(ppVtbl : *mut IUnknown) -> Self;
  fn iptr(&self) -> *mut IUnknown;
}

pub struct Unknown(*mut IUnknown);

impl HasIID for Unknown {
  fn iid() -> REFGUID { &iids::IID_IUnknown }
  fn new(pp_vtbl : *mut IUnknown) -> Self {
    Unknown(pp_vtbl)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0
  }
}

pub fn release_com_ptr<T:HasIID>(obj : &mut T) {
  let iunk=obj.iptr();
  if iunk==::std::ptr::null_mut() {
    // 
  } else {
    unsafe {
      (*iunk).Release();
    }
  }
}

pub fn clone_com_ptr<T:HasIID>(obj: &T) -> T {
  let iunk=obj.iptr();
  let ret=T::new(iunk);
  unsafe {
    (*iunk).AddRef();
  }
  ret
}

impl Drop for Unknown {
  fn drop(&mut self) {
    release_com_ptr(self);
  }
}

impl Clone for Unknown {
  fn clone(&self) -> Self {
    clone_com_ptr(self) 
  }
}

pub trait QueryInterface {
  fn query_interface<T:HasIID>(&self) -> HResult<T>;
}

impl<T:HasIID> QueryInterface for T {
  fn query_interface<O:HasIID>(&self) -> HResult<O> {
    let iunk=self.iptr();
    let mut ounk : *mut IUnknown=::std::ptr::null_mut();
    let hr=unsafe {
      (*iunk).QueryInterface(O::iid(), &mut ounk as *mut *mut _ as *mut *mut c_void)
    };
    if hr==0 {
      Ok(O::new(ounk))
    } else {
      Err(hr)
    }
  }
}
