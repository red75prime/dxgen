use std::ffi::OsString;
use std::os::windows::ffi::OsStringExt;
use std::os::windows::ffi::OsStrExt;

pub fn wchar_array_to_string_lossy(ws: &[u16]) -> String {
    match ws.iter().position(|c| *c == 0) {
        Some(p) => OsString::from_wide(&ws[0..p]).to_string_lossy().into_owned(),
        None => OsString::from_wide(ws).to_string_lossy().into_owned(),
    }
}

pub fn str_to_vec_u16(s: &str) -> Vec<u16> {
    let osstr = OsString::from(s);
    osstr.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
}

use cgmath::*;

pub fn matrix4_to_4x4(m: &Matrix4<f32>) -> [[f32; 4]; 4] {
    [[m.x.x, m.y.x, m.z.x, m.w.x],
     [m.x.y, m.y.y, m.z.y, m.w.y],
     [m.x.z, m.y.z, m.z.z, m.w.z],
     [m.x.w, m.y.w, m.z.w, m.w.w]]
}

pub fn rotshift3_to_4x4(m: &Basis3<f32>, v: &Vector3<f32>) -> [[f32; 4]; 4] {
    let m = m.as_ref();
    [[m.x.x, m.y.x, m.z.x, v.x],
     [m.x.y, m.y.y, m.z.y, v.y],
     [m.x.z, m.y.z, m.z.z, v.z],
     [0., 0., 0., 1.]]
}

pub fn rot3_to_3x3(m: &Basis3<f32>) -> [[f32; 3]; 3] {
    let m = m.as_ref();
    [[m.x.x, m.y.x, m.z.x],
     [m.x.y, m.y.y, m.z.y],
     [m.x.z, m.y.z, m.z.z]]
}


pub fn v3(x: f32, y: f32, z: f32) -> Vector3<f32> {
    Vector3::new(x, y, z)
}

pub fn p3(x: f32, y: f32, z: f32) -> Point3<f32> {
    Point3::new(x, y, z)
}

use dx_safe::*;
use winapi::*;
use core::*;
use std::mem;
use std::ptr;
use std::slice;
use dx_safe::structwrappers::*;
use kernel32::*;

pub fn get_required_intermediate_size(res: &D3D12Resource) -> HResult<u64> {
    let desc = res.get_desc();
    let dev: D3D12Device = try!(res.get_device());
    let mut required_size = 0;
    // First zero is index of first subresource, second zero is BaseOffset: u64
    // TODO: revise this binding. NumSubresources in meaningful even if pLayouts is None.
    dev.get_copyable_footprints(&desc, 0, 0, None, None, None, Some(&mut required_size));
    Ok(required_size)
}

pub fn upload_into_buffer<T>(buf: &D3D12Resource, data: &[T]) -> HResult<()> {
    let buf_sz = try!(get_required_intermediate_size(buf)) as usize;
    let sz = mem::size_of_val(data);
    assert!(buf_sz < sz);
    let mut p_buf: *mut u8 = ptr::null_mut();
    unsafe {
        try!(buf.map(0, None, Some(&mut p_buf))) ;
        let buf_slice = slice::from_raw_parts_mut(p_buf, sz);
        buf_slice.clone_from_slice(slice::from_raw_parts(data.as_ptr() as *const u8, sz));
    };
    buf.unmap(0, None);
    Ok(())
}

pub fn upload_into_texture(core: &DXCore,
                           tex: &D3D12Resource,
                           w: u32,
                           _: u32,
                           data: &[u32])
                           -> HResult<()> {
    let w = w as usize; 
    trace!("upload_into_texture");
    trace!("get_desc tex.iptr(): 0x{:x}, vtbl: 0x{:x}, vtbl[0]: 0x{:x}",
           tex.iptr() as usize,
           unsafe { ((*tex.iptr()).lpVtbl) as usize },
           unsafe { (*(*tex.iptr()).lpVtbl).QueryInterface as usize });
    let desc = tex.get_desc();
    trace!("get_device");
    let dev = &core.dev;
    let mut num_rows = [0];
    let mut row_size_bytes = [0];
    let mut total_size_bytes = 0;
    let mut psfp: [D3D12_PLACED_SUBRESOURCE_FOOTPRINT; 1] = [unsafe {
                                                                 ::std::mem::uninitialized()
                                                             }];
    trace!("get_copyable_footprints");
    // Microsoft sample application uses HeapAlloc for footprint and other arguments
    // TODO: investigate
    dev.get_copyable_footprints(&desc,
                                0,
                                0,
                                Some(&mut psfp),
                                Some(&mut num_rows),
                                Some(&mut row_size_bytes),
                                Some(&mut total_size_bytes));
    let rows = num_rows[0] as usize;
    let row_len = (row_size_bytes[0] / 4) as usize;
    let row_pitch = (psfp[0].Footprint.RowPitch / 4) as usize;
    let total_len = (total_size_bytes / 4) as usize;
    debug!("Placed subres. footprint:{:?}", psfp[0]);
    debug!("Rows:{}, Row len:{}, Row pitch:{}  Total len:{}",
           rows,
           row_len,
           row_pitch,
           total_len);

    trace!("create_committed_resource");
    let res_buf = try!(dev.create_committed_resource(&heap_properties_upload(),
                                                     D3D12_HEAP_FLAG_NONE,
                                                     &resource_desc_buffer(total_size_bytes),
                                                     D3D12_RESOURCE_STATE_GENERIC_READ,
                                                     None));
    res_buf.set_name("Temporary texture buffer".into()).unwrap();

    let mut temp_buf = Vec::with_capacity(total_len);
    unsafe {
        temp_buf.set_len(total_len);
    };
    for y in 0..rows {
        &temp_buf[y * row_pitch..y * row_pitch + w].clone_from_slice(&data[y * w .. (y+1) * w]);
    }

    try!(upload_into_buffer(&res_buf, &temp_buf[..]));

    let dest = texture_copy_location_index(&tex, 0);
    let src = texture_copy_location_footprint(&res_buf, &psfp[0]);

    trace!("create_command_allocator");
    let callocator = try!(dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COPY));
    trace!("create_command_list");
    let clist: D3D12GraphicsCommandList =
        try!(dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_COPY, &callocator, None));
    clist.resource_barrier(&mut [*ResourceBarrier::transition(&tex,
                                                              D3D12_RESOURCE_STATE_COMMON,
                                                              D3D12_RESOURCE_STATE_COPY_DEST)]);
    clist.copy_texture_region(&dest, 0, 0, 0, &src, None);
    clist.resource_barrier(&mut [*ResourceBarrier::transition(&tex,
                                                              D3D12_RESOURCE_STATE_COPY_DEST,
                                                              D3D12_RESOURCE_STATE_COMMON)]);

    trace!("Command list Close");
    try!(clist.close());

    trace!("Command queue execute");
    core.copy_queue.execute_command_lists(&[&clist]);
    trace!("fence");
    let fence = try!(dev.create_fence(0, D3D12_FENCE_FLAG_NONE));
    trace!("fence_event");
    let fence_event = create_event();

    let fv = core.next_fence_value();
    trace!("wait for copy queue");
    try!(wait_for_queue(&core.copy_queue, fv, &fence, &fence_event));
    Ok(())
}

pub fn create_depth_stencil(dev: &D3D12Device,
                            w: u64,
                            h: u32,
                            ds_format: DXGI_FORMAT,
                            dheap_handle: D3D12_CPU_DESCRIPTOR_HANDLE)
                            -> HResult<D3D12Resource> {
    let ds_desc = resource_desc_tex2d_nomip(w,
                                            h,
                                            ds_format,
                                            D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL);
    debug!("Depth stencil resource");
    let ds_res = try!(dev.create_committed_resource(&heap_properties_default(),
                                                    D3D12_HEAP_FLAG_NONE,
                                                    &ds_desc,
                                                    D3D12_RESOURCE_STATE_DEPTH_WRITE,
                                                    Some(&depth_stencil_clear_value_depth_f32(1.0)))
                         .map_err(|hr| {
                             error!("create_commited_resource failed with 0x{:x}", hr);
                             hr
                         }));

    let dsv_desc = depth_stencil_view_desc_tex2d_default(ds_format);
    debug!("Depth stencil view");
    dev.create_depth_stencil_view(Some(&ds_res), Some(&dsv_desc), dheap_handle);

    Ok(ds_res)
}

pub fn wait_for_queue(queue: &D3D12CommandQueue,
                      fence_value: u64,
                      fence: &D3D12Fence,
                      fence_event: &Event)
                      -> HResult<()> {
    try!(queue.signal(fence, fence_value));
    if fence.get_completed_value() < fence_value {
        try!(fence.set_event_on_completion(fence_value, fence_event.handle()));
        wait_for_single_object(fence_event, INFINITE);
    }
    Ok(())
}

pub fn wait_for_graphics_queue(core: &DXCore, fence: &D3D12Fence, fence_event: &Event) {
    // TODO: Look for overflow behaviour of atomics
    let fence_value = core.next_fence_value();

    match wait_for_queue(&core.graphics_queue, fence_value, fence, fence_event) {
        Ok(_) => (),
        Err(hr) => {
            core.dump_info_queue();
            panic!("set_event_on_completion error: 0x{:x}", hr);
        }
    }
}

pub fn wait_for_compute_queue(core: &DXCore, fence: &D3D12Fence, fence_event: &Event) {
    // TODO: Look for overflow behaviour of atomics
    let fence_value = core.next_fence_value();

    match wait_for_queue(&core.compute_queue, fence_value, fence, fence_event) {
        Ok(_) => (),
        Err(hr) => {
            core.dump_info_queue();
            panic!("set_event_on_completion error: 0x{:x}", hr);
        }
    }
}

pub fn wait_for_copy_queue(core: &DXCore, fence: &D3D12Fence, fence_event: &Event) {
    // TODO: Look for overflow behaviour of atomics
    let fence_value = core.next_fence_value();

    match wait_for_queue(&core.copy_queue, fence_value, fence, fence_event) {
        Ok(_) => (),
        Err(hr) => {
            core.dump_info_queue();
            panic!("set_event_on_completion error: 0x{:x}", hr);
        }
    }
}

pub fn wait_for_single_object(event: &Event, ms: u32) -> u32 {
    unsafe { WaitForSingleObject(event.handle(), ms) }
}

use user32::*;

pub fn set_capture(hwnd: HWND) -> HWND {
    unsafe { SetCapture(hwnd) }
}

pub fn release_capture() -> BOOL {
    unsafe { ReleaseCapture() }
}

// Fence holds temporary resources until it's waited upon.
pub struct Fence {
    dxfence: D3D12Fence,
    event: Event,
    temp_resources: Vec<D3D12Resource>,
    first_signal: bool,
    fence_value: Option<u64>,
}

impl Fence {
    pub fn new(fence: D3D12Fence) -> Fence {
        Fence {
            dxfence: fence,
            event: create_event(),
            temp_resources: vec![],
            first_signal: true,
            fence_value: None,
        }
    }

    // Fence takes ownership of res
    pub fn hold(&mut self, res: D3D12Resource) -> &mut Self {
        self.temp_resources.push(res);
        self
    }

    // Marks the point in command queue as safe for releasing temporary resources
    pub fn signal(&mut self, queue: &D3D12CommandQueue, fence_value: u64) -> HResult<()> {
        if !self.first_signal {
            warn!("Two Fence::signal's without Fence::wait_for_gpu() or Fence::wait() in between");
        }
        self.first_signal = false;
        self.fence_value = Some(fence_value);
        queue.signal(&self.dxfence, fence_value).map(|_| ())
    }

    pub fn wait(&mut self, queue: &D3D12CommandQueue) -> HResult<()> {
        if let Some(fence_value) = self.fence_value {
            return queue.wait(&self.dxfence, fence_value).map(|_|());
        }
        warn!("Fence::wait() without Fence::signal()");
        Ok(())
    }

    pub fn wait_for_gpu(&mut self) -> HResult<()> {
        match self.fence_value {
            Some(fence_value) => {
                try!(self.dxfence.set_event_on_completion(fence_value, self.event.handle()));
                wait_for_single_object(&self.event, INFINITE);
                self.first_signal = true;
                self.temp_resources.truncate(0);
            }
            None => {
                // signal wasn't called. Nothing to wait for
            }
        }
        Ok(())
    }
}

pub mod d2d {
    use winapi::*;
    
    pub fn rectf(left: f32, top: f32, right: f32, bottom: f32) -> D2D1_RECT_F {
    D2D1_RECT_F {
        left: left,
        right: right,
        top: top,
        bottom: bottom,
    }
    }

    pub fn color3(r: f32, g: f32, b: f32) -> D3DCOLORVALUE {
    D3DCOLORVALUE { r: r, g: g, b: b, a: 1.0 }
    }    
}
