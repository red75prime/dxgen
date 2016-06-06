// This file is autogenerated

use utils::*;

use dxgi_safe::*;
use dxgi1_2_safe::*;
use dxgi1_3_safe::*;
pub trait TDXGIAdapter3: TDXGIAdapter2 {
  //  Method RegisterHardwareContentProtectionTeardownStatusEvent
  
  #[allow(non_snake_case)]
  fn register_hardware_content_protection_teardown_status_event(&self, hEvent: HANDLE) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIAdapter3)).RegisterHardwareContentProtectionTeardownStatusEvent(hEvent, &mut lv1 as *mut _ as *mut _) };
    hr2ret(_hr,lv1)
  }
  
  //  Method UnregisterHardwareContentProtectionTeardownStatus
  
  #[allow(non_snake_case)]
  fn unregister_hardware_content_protection_teardown_status(&self, cookie: DWORD) -> () {
  
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIAdapter3)).UnregisterHardwareContentProtectionTeardownStatus(cookie) };
    ()
  }
  
  //  Method QueryVideoMemoryInfo
  
  #[allow(non_snake_case)]
  fn query_video_memory_info(&self, node_index: UINT, memory_segment_group: DXGI_MEMORY_SEGMENT_GROUP) -> HResult<DXGI_QUERY_VIDEO_MEMORY_INFO> {
    let mut lv1: DXGI_QUERY_VIDEO_MEMORY_INFO = unsafe {mem::uninitialized::<_>()};
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIAdapter3)).QueryVideoMemoryInfo(node_index, memory_segment_group, &mut lv1 as *mut _ as *mut _) };
    hr2ret(_hr,lv1)
  }
  
  //  Method SetVideoMemoryReservation
  
  #[allow(non_snake_case)]
  fn set_video_memory_reservation(&self, node_index: UINT, memory_segment_group: DXGI_MEMORY_SEGMENT_GROUP, reservation: UINT64) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIAdapter3)).SetVideoMemoryReservation(node_index, memory_segment_group, reservation) };
    hr2ret(_hr,_hr)
  }
  
  //  Method RegisterVideoMemoryBudgetChangeNotificationEvent
  
  #[allow(non_snake_case)]
  fn register_video_memory_budget_change_notification_event(&self, hEvent: HANDLE) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIAdapter3)).RegisterVideoMemoryBudgetChangeNotificationEvent(hEvent, &mut lv1 as *mut _ as *mut _) };
    hr2ret(_hr,lv1)
  }
  
  //  Method UnregisterVideoMemoryBudgetChangeNotification
  
  #[allow(non_snake_case)]
  fn unregister_video_memory_budget_change_notification(&self, cookie: DWORD) -> () {
  
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIAdapter3)).UnregisterVideoMemoryBudgetChangeNotification(cookie) };
    ()
  }
  
  
}

impl TUnknown for DXGIAdapter3 {
  fn new(ptr: *mut IUnknown) -> Self {
    DXGIAdapter3(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for DXGIAdapter3 {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for DXGIAdapter3 {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TDXGIObject for DXGIAdapter3 {}
impl TDXGIAdapter for DXGIAdapter3 {}
impl TDXGIAdapter1 for DXGIAdapter3 {}
impl TDXGIAdapter2 for DXGIAdapter3 {}
impl TDXGIAdapter3 for DXGIAdapter3 {}

pub struct DXGIAdapter3(*mut IDXGIAdapter3);

impl HasIID for DXGIAdapter3 {
  fn iid() -> REFGUID { &IID_IDXGIAdapter3 }
}

pub trait TDXGIFactory4: TDXGIFactory3 {
  //  Method EnumAdapterByLuid
  
  #[allow(non_snake_case)]
  fn enum_adapter_by_luid<T: TUnknown+HasIID>(&self, adapter_luid: LUID) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIFactory4)).EnumAdapterByLuid(adapter_luid, T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(_hr,T::new(lv1))
  }
  
  //  Method EnumWarpAdapter
  
  #[allow(non_snake_case)]
  fn enum_warp_adapter<T: TUnknown+HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIFactory4)).EnumWarpAdapter(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(_hr,T::new(lv1))
  }
  
  
}

impl TUnknown for DXGIFactory4 {
  fn new(ptr: *mut IUnknown) -> Self {
    DXGIFactory4(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for DXGIFactory4 {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for DXGIFactory4 {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TDXGIObject for DXGIFactory4 {}
impl TDXGIFactory for DXGIFactory4 {}
impl TDXGIFactory1 for DXGIFactory4 {}
impl TDXGIFactory2 for DXGIFactory4 {}
impl TDXGIFactory3 for DXGIFactory4 {}
impl TDXGIFactory4 for DXGIFactory4 {}

pub struct DXGIFactory4(*mut IDXGIFactory4);
unsafe impl Send for DXGIFactory4 {}

impl HasIID for DXGIFactory4 {
  fn iid() -> REFGUID { &IID_IDXGIFactory4 }
}

pub trait TDXGIOutput4: TDXGIOutput3 {
  //  Method CheckOverlayColorSpaceSupport
  
  #[allow(non_snake_case)]
  fn check_overlay_color_space_support<T: TUnknown>(&self, format: DXGI_FORMAT, color_space: DXGI_COLOR_SPACE_TYPE, concerned_device: &T) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let _hr=unsafe { (*(self.iptr() as *mut IDXGIOutput4)).CheckOverlayColorSpaceSupport(format, color_space, concerned_device.iptr() as *mut _ as *mut _ , &mut lv1 as *mut _ as *mut _) };
    hr2ret(_hr,lv1)
  }
  
  
}

impl TUnknown for DXGIOutput4 {
  fn new(ptr: *mut IUnknown) -> Self {
    DXGIOutput4(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for DXGIOutput4 {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for DXGIOutput4 {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TDXGIObject for DXGIOutput4 {}
impl TDXGIOutput for DXGIOutput4 {}
impl TDXGIOutput1 for DXGIOutput4 {}
impl TDXGIOutput2 for DXGIOutput4 {}
impl TDXGIOutput3 for DXGIOutput4 {}
impl TDXGIOutput4 for DXGIOutput4 {}

pub struct DXGIOutput4(*mut IDXGIOutput4);

impl HasIID for DXGIOutput4 {
  fn iid() -> REFGUID { &IID_IDXGIOutput4 }
}

pub trait TDXGISwapChain3: TDXGISwapChain2 {
  //  Method GetCurrentBackBufferIndex
  
  #[allow(non_snake_case)]
  fn get_current_back_buffer_index(&self) -> UINT {
  
    let _hr=unsafe { (*(self.iptr() as *mut IDXGISwapChain3)).GetCurrentBackBufferIndex() };
    _hr
  }
  
  //  Method CheckColorSpaceSupport
  
  #[allow(non_snake_case)]
  fn check_color_space_support(&self, color_space: DXGI_COLOR_SPACE_TYPE) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let _hr=unsafe { (*(self.iptr() as *mut IDXGISwapChain3)).CheckColorSpaceSupport(color_space, &mut lv1 as *mut _ as *mut _) };
    hr2ret(_hr,lv1)
  }
  
  //  Method SetColorSpace1
  
  #[allow(non_snake_case)]
  fn set_color_space1(&self, color_space: DXGI_COLOR_SPACE_TYPE) -> HResult<HRESULT> {
  
    let _hr=unsafe { (*(self.iptr() as *mut IDXGISwapChain3)).SetColorSpace1(color_space) };
    hr2ret(_hr,_hr)
  }
  
  //  Method ResizeBuffers1
  
  #[allow(non_snake_case)]
  fn resize_buffers1<T: TUnknown>(&self, width: UINT, height: UINT, format: DXGI_FORMAT, swap_chain_flags: UINT, creation_node_mask: &[UINT], present_queue: &[&T]) -> HResult<HRESULT> {
    let mut lv1: Vec<*mut IUnknown> = present_queue.iter().map(|o|o.iptr()).collect();
    let _hr=unsafe { (*(self.iptr() as *mut IDXGISwapChain3)).ResizeBuffers1( same_length(&[Some(creation_node_mask.len()),Some(present_queue.len())]).expect("Arrays must have equal sizes") as UINT, width, height, format, swap_chain_flags, slice_as_ptr(creation_node_mask), lv1.as_mut_ptr() as *mut *mut _ as *mut *mut _) };
    hr2ret(_hr,_hr)
  }
  
  
}

impl TUnknown for DXGISwapChain3 {
  fn new(ptr: *mut IUnknown) -> Self {
    DXGISwapChain3(ptr as *mut _)
  }
  fn iptr(&self) -> *mut IUnknown {
    self.0 as *mut _
  }
}
impl Drop for DXGISwapChain3 {
  fn drop(&mut self) { drop_unknown(self) }
}
impl Clone for DXGISwapChain3 {
  fn clone(&self) -> Self { clone_unknown(self) }
}
impl TDXGIObject for DXGISwapChain3 {}
impl TDXGIDeviceSubObject for DXGISwapChain3 {}
impl TDXGISwapChain for DXGISwapChain3 {}
impl TDXGISwapChain1 for DXGISwapChain3 {}
impl TDXGISwapChain2 for DXGISwapChain3 {}
impl TDXGISwapChain3 for DXGISwapChain3 {}

pub struct DXGISwapChain3(*mut IDXGISwapChain3);

impl HasIID for DXGISwapChain3 {
  fn iid() -> REFGUID { &IID_IDXGISwapChain3 }
}

