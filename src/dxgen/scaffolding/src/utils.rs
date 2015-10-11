use std::ffi::{OsString};
use std::os::windows::ffi::OsStringExt;

pub fn wchar_array_to_string_lossy(ws: &[u16]) -> String {
  match ws.iter().position(|c|*c==0) {
    Some(p) => {
      OsString::from_wide(&ws[0..p]).to_string_lossy().into_owned()
    },
    None => {
      OsString::from_wide(ws).to_string_lossy().into_owned()
    },
  }
}

use cgmath::*;

pub fn matrix4_to_4x4(m: &Matrix4<f32>) -> [[f32;4];4] {
  [ [m.x.x, m.y.x, m.z.x, m.w.x],
    [m.x.y, m.y.y, m.z.y, m.w.y],
    [m.x.z, m.y.z, m.z.z, m.w.z],
    [m.x.w, m.y.w, m.z.w, m.w.w] ]
}

pub fn lhs_to_rhs(m: &mut Matrix4<f32>) {
  m.z.x = - m.z.x;
  m.z.y = - m.z.y;
  m.z.z = - m.z.z;
  m.z.w = - m.z.w;
}

use d3d12_safe::*;
use winapi::*;
use std::mem;

pub fn dump_info_queue(iq: Option<&D3D12InfoQueue>) {
  if let Some(iq)=iq {
    let mnum=iq.get_num_stored_messages_allowed_by_retrieval_filter();
    //println!("Number of debug messages is {}", mnum);
    for i in 0..mnum {
      let mut sz = 0;
      let _ = iq.get_message(i, None,&mut sz);
      //info!("get_message returned {:?}", hr); // It is the case when hr!=0 and it's ok
      // create buffer to receive message
      let arr: Vec<u8> = Vec::with_capacity(sz as usize); 
      let mut sz1=sz; 
      unsafe {
        // get_message expects Option<&mut D3D12_MESSAGE> as second parameter
        // it should be Option<&[u8]>, but I don't have annotation for that yet.
        let _ = iq.get_message(i, Some(mem::transmute(arr.as_ptr())), &mut sz1); 
        assert_eq!(sz, sz1); // just to be sure. hr is Err(1) on success.
        // Reinterpret first chunk of arr as D3D12_MESSAGE, and byte-copy it into msg
        let msg: D3D12_MESSAGE = mem::transmute_copy(&(*arr.as_ptr()));
        // msg contains pointer into memory occupied by arr. arr should be borrowed now, but it is unsafe code.
        let cdescr = ::std::ffi::CStr::from_ptr(msg.pDescription as *const i8);
        let descr = String::from_utf8_lossy(cdescr.to_bytes()).to_owned();
        match msg.Severity {
          D3D12_MESSAGE_SEVERITY_CORRUPTION |
          D3D12_MESSAGE_SEVERITY_ERROR =>
            {error!("{:}", descr)},
          D3D12_MESSAGE_SEVERITY_WARNING =>
            {warn!("{:}", descr)},
          _ =>
            {debug!("{:}", descr)},
        }
      }
    };
    iq.clear_stored_messages();
  }
}

