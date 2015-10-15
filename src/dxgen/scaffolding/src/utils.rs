use std::ffi::{OsString};
use std::os::windows::ffi::OsStringExt;
use std::os::windows::ffi::OsStrExt;

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

pub fn str_to_vec_u16(s : &str) -> Vec<u16> {
  let osstr = OsString::from(s);
  osstr.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
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
use core::*;

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

use std::ptr;
use std::slice;
use create_device::*;
use structwrappers::*;
use kernel32::*;
use std::sync::atomic::Ordering;

pub unsafe fn upload_into_buffer<T>(buf: &D3D12Resource, data: &[T]) {
  let sz = mem::size_of_val(data);
//  debug!("Map buffer");
  let mut p_buf: *mut u8 = ptr::null_mut();
  buf.map(0, None, Some(&mut p_buf)).unwrap();
  let buf_slice = slice::from_raw_parts_mut(p_buf, sz);
  buf_slice.clone_from_slice(slice::from_raw_parts(data.as_ptr() as *const u8, sz)); 
//  debug!("Unmap buffer");
  buf.unmap(0, None);
}

pub fn upload_into_texture(core: &DXCore, tex: &D3D12Resource, w: usize, h: usize, data: &[u32]) -> HResult<()> {
  debug!("upload_into_texture");
  debug!("get_desc tex.iptr(): 0x{:x}, vtbl: 0x{:x}, vtbl[0]: 0x{:x}", tex.iptr() as usize, unsafe{((*tex.iptr()).lpVtbl) as usize}, unsafe{(*(*tex.iptr()).lpVtbl).QueryInterface as usize});
  let desc = tex.get_desc();
  debug!("get_device");
  let dev = &core.dev;
  let mut num_rows = [0];
  let mut row_size_bytes = [0];
  let mut total_size_bytes = 0;
  let mut psfp: [D3D12_PLACED_SUBRESOURCE_FOOTPRINT; 1] = [unsafe{ ::std::mem::uninitialized() }];
  debug!("get_copyable_footprints");
  // Microsoft sample application uses HeapAlloc for footprint and other arguments
  // TODO: investigate
  dev.get_copyable_footprints(&desc, 0, 0, Some(&mut psfp), Some(&mut num_rows), Some(&mut row_size_bytes), Some(&mut total_size_bytes));
  let rows = num_rows[0] as usize;
  let row_len = (row_size_bytes[0]/4) as usize;
  let row_pitch = (psfp[0].Footprint.RowPitch/4) as usize;
  let total_len = (total_size_bytes/4) as usize;
  debug!("Placed subres. footprint:{:?}", psfp[0]);
  debug!("Rows:{}, Row len:{}, Row pitch:{}  Total len:{}", rows, row_len, row_pitch, total_len);

  debug!("create_committed_resource");
  let res_buf=try!(dev.create_committed_resource(&heap_properties_upload(), D3D12_HEAP_FLAG_NONE, &resource_desc_buffer(total_size_bytes), D3D12_RESOURCE_STATE_GENERIC_READ, None));
  res_buf.set_name("Temporary texture buffer".into()).unwrap();

  let mut temp_buf = Vec::with_capacity(total_len);
  unsafe {
    temp_buf.set_len(total_len);
  };
  for y in 0..rows {
    &temp_buf[y*row_pitch..y*row_pitch+w].clone_from_slice(&data[y*w..y*w+w]);
  }

  unsafe {
    upload_into_buffer(&res_buf, &temp_buf[..]);
  };

  let dest = texture_copy_location_index(&tex, 0);
  let src = texture_copy_location_footprint(&res_buf, &psfp[0]);

  debug!("create_command_allocator");
  let callocator = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COPY));
  debug!("create_command_list");
  let clist: D3D12GraphicsCommandList = try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COPY, &callocator, None));
  clist.copy_texture_region(&dest, 0, 0, 0, &src, None); 
  clist.resource_barrier(&mut [*ResourceBarrier::transition(&tex, D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON)]);

  debug!("Command list Close");
  try!(clist.close());

  debug!("Command queue execute");
  core.copy_queue.execute_command_lists(&[&clist]);
  debug!("fence");
  let fence=try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));
  debug!("fence_event");
  let fence_event = unsafe{ CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  if fence_event == ptr::null_mut() {
    panic!("Cannot create event");
  }

  let fv=core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;
  debug!("signal");
  try!(core.copy_queue.signal(&fence, fv));
  //try!(core.copy_queue.wait(&fence, fv)); // !!! D3D12CommandQueue::wait causes GPU to wait for signal
  if fence.get_completed_value() < fv {
    debug!("set_event_on_completion");
    try!(fence.set_event_on_completion(fv, fence_event));
    debug!("wait");
    unsafe { WaitForSingleObject(fence_event, INFINITE) };
  }
  Ok(())
}

pub fn create_depth_stencil(w: u64, h: u32, ds_format: DXGI_FORMAT, dev: &D3D12Device, 
                        dsd_heap: &D3D12DescriptorHeap, heap_offset: u32) -> HResult<D3D12Resource> {
  let offset = 
    if heap_offset == 0 { 0 } 
    else {
      dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_DSV)*heap_offset
    };
  let ds_desc = resource_desc_tex2d_nomip(w, h, ds_format, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);
  debug!("Depth stencil resource");
  let ds_res=try!(dev.create_committed_resource(&heap_properties_default(), D3D12_HEAP_FLAG_NONE, &ds_desc, 
                                          D3D12_RESOURCE_STATE_DEPTH_WRITE, Some(&depth_stencil_clear_value_depth_f32())));

  let dsv_desc = depth_stencil_view_desc_tex2d_default(ds_format);
  let mut handle = dsd_heap.get_cpu_descriptor_handle_for_heap_start();
  handle.ptr += offset as SIZE_T;
  dev.create_depth_stencil_view(Some(&ds_res), Some(&dsv_desc), handle);

  Ok(ds_res)
}

pub fn wait_for_graphics_queue(core: &DXCore, fence: &D3D12Fence, fence_event: &HANDLE) {
  // TODO: Look for overflow behaviour of atomics
  let fence_value = core.fence_value.fetch_add(1, Ordering::Relaxed) as u64;

  core.graphics_queue.signal(fence, fence_value).unwrap();
  if fence.get_completed_value() < fence_value {
    match fence.set_event_on_completion(fence_value, *fence_event) {
      Ok(_) => (),
      Err(hr) => {
        dump_info_queue(core.info_queue.as_ref());
        panic!("set_event_on_completion error: 0x{:x}",hr);
      },
    }
    unsafe { WaitForSingleObject(*fence_event, INFINITE) };
  }
}

