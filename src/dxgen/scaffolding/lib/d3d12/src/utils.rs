pub use winapi::*;
pub use dxguid::*;
pub use std::ptr;
pub use std::mem;
pub use std::borrow::Cow;
pub use std::ffi::{OsStr,OsString};
pub use std::os::windows::ffi::OsStrExt;

pub use iid::*;

pub trait TDWriteGeometrySink : ::d2d1_safe::TD2D1SimplifiedGeometrySink {}

#[allow(dead_code)]
pub fn os_str_to_vec_u16(s : &OsStr) -> Vec<u16> {
  s.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
}

pub fn opt_arr_as_mut_ptr<T>(opt: &Option<&mut [T]>) -> *mut T {
  opt.as_ref().map(|v|(*v).as_ptr() as *mut _).unwrap_or(ptr::null_mut())
}

pub fn opt_arr_as_ptr<T>(opt: &Option<&[T]>) -> *const T {
  opt.as_ref().map(|v|(*v).as_ptr()).unwrap_or(ptr::null())
}

pub fn opt_as_mut_ptr<T>(opt: &Option<&mut T>) -> *mut T {
  opt.as_ref().map(|v|*v as *const _ as *mut _).unwrap_or(ptr::null_mut())
}

pub fn slice_as_ptr<T>(s: &[T]) -> *const T {
  if s.len()==0 {ptr::null()} else {s.as_ptr()}
}

pub fn slice_as_mut_ptr<T>(s: &mut [T]) -> *mut T {
  if s.len()==0 {ptr::null_mut()} else {s.as_mut_ptr()}
}

pub fn str_to_vec_u16(s : Cow<str>) -> Vec<u16> {
  let osstr = OsString::from(s.into_owned());
  osstr.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
}

// Utility function. 
// Compares optional numbers
// returns Some(len) if all inputs are Some(len) or None (or Some(0) if all inputs are None), 
//         None otherwise
pub fn same_length(lens:&[Option<usize>]) -> Option<usize> {
    if lens.len() == 1 {
        Some(lens[0].unwrap_or(0))
    } else {
        let res=lens.iter().fold(Ok(None), 
            |sz, mlen| { 
                match sz { 
                    Err(_) => sz, 
                    Ok(None) => Ok(mlen.map(|l1|l1)), 
                    Ok(Some(l)) => 
                        match *mlen {
                            None => sz, 
                            Some(l1) => 
                                if l1==l {
                                    sz
                                } else {
                                    Err(())
                                }
                        }
                }
            });
        res.map(|ms|{ms.unwrap_or(0)}).ok()
    }
}

pub fn hr2ret<T>(hr : HRESULT, res:T) -> HResult<T> {
  if SUCCEEDED(hr) {
    Ok(res)
  } else {
    Err(hr)
  }
}

#[allow(dead_code)]
pub fn opt_slice_to_mut_ptr<T>(os: Option<&mut [T]>) -> *mut T {
  os.map(|s|s.as_mut_ptr()).unwrap_or(::std::ptr::null_mut())
}

#[allow(dead_code)]
pub fn opt_slice_to_ptr<T>(os: Option<&[T]>) -> *const T {
  os.map(|s|s.as_ptr()).unwrap_or(::std::ptr::null())
}

