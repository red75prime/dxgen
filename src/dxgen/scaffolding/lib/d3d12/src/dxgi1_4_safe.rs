use dxgi_safe::*;
use dxgi1_2_safe::*;
use dxgi1_3_safe::*;
// This file is autogenerated

use utils::*;


pub struct DXGIAdapter3(*mut IDXGIAdapter3);

impl HasIID for DXGIAdapter3 {
  fn iid() -> REFGUID { &IID_IDXGIAdapter3 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIAdapter3(pp_vtbl as *mut _ as *mut IDXGIAdapter3) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIAdapter3 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIAdapter3 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIAdapter3 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method EnumOutputs
  
  pub fn enum_outputs(&self, output: UINT) -> HResult<DXGIOutput> {
    let mut lv1: *mut IDXGIOutput = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter)).EnumOutputs(output, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIOutput::new(lv1 as *mut _))
  }
  
  //  Method GetDesc
  
  pub fn get_desc(&self) -> HResult<DXGI_ADAPTER_DESC> {
    let mut lv1: DXGI_ADAPTER_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter)).GetDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method CheckInterfaceSupport
  
  pub fn check_interface_support(&self, interface_name: &GUID) -> HResult<LARGE_INTEGER> {
    let mut lv1: LARGE_INTEGER = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter)).CheckInterfaceSupport(interface_name, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetDesc1
  
  pub fn get_desc1(&self) -> HResult<DXGI_ADAPTER_DESC1> {
    let mut lv1: DXGI_ADAPTER_DESC1 = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).GetDesc1(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetDesc2
  
  pub fn get_desc2(&self) -> HResult<DXGI_ADAPTER_DESC2> {
    let mut lv1: DXGI_ADAPTER_DESC2 = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter2)).GetDesc2(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method RegisterHardwareContentProtectionTeardownStatusEvent
  
  pub fn register_hardware_content_protection_teardown_status_event(&self, hEvent: HANDLE) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter3)).RegisterHardwareContentProtectionTeardownStatusEvent(hEvent, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method UnregisterHardwareContentProtectionTeardownStatus
  
  pub fn unregister_hardware_content_protection_teardown_status(&self, cookie: DWORD) -> () {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter3)).UnregisterHardwareContentProtectionTeardownStatus(cookie) };
    ()
  }
  
  //  Method QueryVideoMemoryInfo
  
  pub fn query_video_memory_info(&self, node_index: UINT, memory_segment_group: DXGI_MEMORY_SEGMENT_GROUP) -> HResult<DXGI_QUERY_VIDEO_MEMORY_INFO> {
    let mut lv1: DXGI_QUERY_VIDEO_MEMORY_INFO = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter3)).QueryVideoMemoryInfo(node_index, memory_segment_group, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetVideoMemoryReservation
  
  pub fn set_video_memory_reservation(&self, node_index: UINT, memory_segment_group: DXGI_MEMORY_SEGMENT_GROUP, reservation: UINT64) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter3)).SetVideoMemoryReservation(node_index, memory_segment_group, reservation) };
    hr2ret(hr,hr)
  }
  
  //  Method RegisterVideoMemoryBudgetChangeNotificationEvent
  
  pub fn register_video_memory_budget_change_notification_event(&self, hEvent: HANDLE) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter3)).RegisterVideoMemoryBudgetChangeNotificationEvent(hEvent, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method UnregisterVideoMemoryBudgetChangeNotification
  
  pub fn unregister_video_memory_budget_change_notification(&self, cookie: DWORD) -> () {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter3)).UnregisterVideoMemoryBudgetChangeNotification(cookie) };
    ()
  }
  
  
}

pub struct DXGIFactory4(*mut IDXGIFactory4);

impl HasIID for DXGIFactory4 {
  fn iid() -> REFGUID { &IID_IDXGIFactory4 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIFactory4(pp_vtbl as *mut _ as *mut IDXGIFactory4) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIFactory4 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIFactory4 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIFactory4 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method EnumAdapters
  
  pub fn enum_adapters(&self, adapter: UINT) -> HResult<DXGIAdapter> {
    let mut lv1: *mut IDXGIAdapter = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).EnumAdapters(adapter, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIAdapter::new(lv1 as *mut _))
  }
  
  //  Method MakeWindowAssociation
  
  pub fn make_window_association(&self, window_handle: HWND, flags: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).MakeWindowAssociation(window_handle, flags) };
    hr2ret(hr,hr)
  }
  
  //  Method GetWindowAssociation
  
  pub fn get_window_association(&self) -> HResult<HWND> {
    let mut lv1: HWND = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).GetWindowAssociation(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method CreateSwapChain
  
  pub fn create_swap_chain<T: HasIID>(&self, device: &T, desc: &mut DXGI_SWAP_CHAIN_DESC) -> HResult<DXGISwapChain> {
    let mut lv1: *mut IDXGISwapChain = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).CreateSwapChain(device.iptr() as *mut _ as *mut _ , desc, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGISwapChain::new(lv1 as *mut _))
  }
  
  //  Method CreateSoftwareAdapter
  
  pub fn create_software_adapter(&self, module: HMODULE) -> HResult<DXGIAdapter> {
    let mut lv1: *mut IDXGIAdapter = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).CreateSoftwareAdapter(module, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIAdapter::new(lv1 as *mut _))
  }
  
  //  Method EnumAdapters1
  
  pub fn enum_adapters1(&self, adapter: UINT) -> HResult<DXGIAdapter1> {
    let mut lv1: *mut IDXGIAdapter1 = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).EnumAdapters1(adapter, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIAdapter1::new(lv1 as *mut _))
  }
  
  //  Method IsCurrent
  
  pub fn is_current(&self) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).IsCurrent() };
    hr
  }
  
  //  Method IsWindowedStereoEnabled
  
  pub fn is_windowed_stereo_enabled(&self) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).IsWindowedStereoEnabled() };
    hr
  }
  
  //  Method CreateSwapChainForHwnd
  
  pub fn create_swap_chain_for_hwnd<T: HasIID>(&self, device: &T, hWnd: HWND, desc: &DXGI_SWAP_CHAIN_DESC1, fullscreen_desc: Option<&DXGI_SWAP_CHAIN_FULLSCREEN_DESC>, restrict_to_output: Option<&DXGIOutput>) -> HResult<DXGISwapChain1> {
    let mut lv1: *mut IDXGISwapChain1 = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).CreateSwapChainForHwnd(device.iptr() as *mut _ as *mut _ , hWnd, desc, fullscreen_desc.as_ref().map(|p|*p as *const _ as *const _).unwrap_or(ptr::null()), restrict_to_output.map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGISwapChain1::new(lv1 as *mut _))
  }
  
  //  Method CreateSwapChainForCoreWindow
  
  pub fn create_swap_chain_for_core_window<T: HasIID, T1: HasIID>(&self, device: &T, window: &T1, desc: &DXGI_SWAP_CHAIN_DESC1, restrict_to_output: Option<&DXGIOutput>) -> HResult<DXGISwapChain1> {
    let mut lv1: *mut IDXGISwapChain1 = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).CreateSwapChainForCoreWindow(device.iptr() as *mut _ as *mut _ , window.iptr() as *mut _ as *mut _ , desc, restrict_to_output.map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGISwapChain1::new(lv1 as *mut _))
  }
  
  //  Method GetSharedResourceAdapterLuid
  
  pub fn get_shared_resource_adapter_luid(&self, hResource: HANDLE) -> HResult<LUID> {
    let mut lv1: LUID = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).GetSharedResourceAdapterLuid(hResource, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method RegisterStereoStatusWindow
  
  pub fn register_stereo_status_window(&self, window_handle: HWND, wMsg: UINT) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).RegisterStereoStatusWindow(window_handle, wMsg, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method RegisterStereoStatusEvent
  
  pub fn register_stereo_status_event(&self, hEvent: HANDLE) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).RegisterStereoStatusEvent(hEvent, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method UnregisterStereoStatus
  
  pub fn unregister_stereo_status(&self, cookie: DWORD) -> () {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).UnregisterStereoStatus(cookie) };
    ()
  }
  
  //  Method RegisterOcclusionStatusWindow
  
  pub fn register_occlusion_status_window(&self, window_handle: HWND, wMsg: UINT) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).RegisterOcclusionStatusWindow(window_handle, wMsg, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method RegisterOcclusionStatusEvent
  
  pub fn register_occlusion_status_event(&self, hEvent: HANDLE) -> HResult<DWORD> {
    let mut lv1: DWORD = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).RegisterOcclusionStatusEvent(hEvent, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method UnregisterOcclusionStatus
  
  pub fn unregister_occlusion_status(&self, cookie: DWORD) -> () {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).UnregisterOcclusionStatus(cookie) };
    ()
  }
  
  //  Method CreateSwapChainForComposition
  
  pub fn create_swap_chain_for_composition<T: HasIID>(&self, device: &T, desc: &DXGI_SWAP_CHAIN_DESC1, restrict_to_output: Option<&DXGIOutput>) -> HResult<DXGISwapChain1> {
    let mut lv1: *mut IDXGISwapChain1 = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory2)).CreateSwapChainForComposition(device.iptr() as *mut _ as *mut _ , desc, restrict_to_output.map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGISwapChain1::new(lv1 as *mut _))
  }
  
  //  Method GetCreationFlags
  
  pub fn get_creation_flags(&self) -> UINT {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory3)).GetCreationFlags() };
    hr
  }
  
  //  Method EnumAdapterByLuid
  
  pub fn enum_adapter_by_luid<T: HasIID>(&self, adapter_luid: LUID) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory4)).EnumAdapterByLuid(adapter_luid, T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method EnumWarpAdapter
  
  pub fn enum_warp_adapter<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory4)).EnumWarpAdapter(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  
}

pub struct DXGIOutput4(*mut IDXGIOutput4);

impl HasIID for DXGIOutput4 {
  fn iid() -> REFGUID { &IID_IDXGIOutput4 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIOutput4(pp_vtbl as *mut _ as *mut IDXGIOutput4) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIOutput4 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIOutput4 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIOutput4 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method GetDesc
  
  pub fn get_desc(&self) -> HResult<DXGI_OUTPUT_DESC> {
    let mut lv1: DXGI_OUTPUT_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetDisplayModeList
  
  pub fn get_display_mode_list(&self, enum_format: DXGI_FORMAT, flags: UINT, desc: Option<&mut [DXGI_MODE_DESC]>) -> HResult<UINT> {
    let mut lv1: UINT = desc.as_ref().map(|v|v.len()).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetDisplayModeList(enum_format, flags, &mut lv1, opt_arr_as_mut_ptr(&desc) as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method FindClosestMatchingMode
  
  pub fn find_closest_matching_mode(&self, mode_to_match: &DXGI_MODE_DESC, concerned_device: Option<&Unknown>) -> HResult<DXGI_MODE_DESC> {
    let mut lv1: DXGI_MODE_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).FindClosestMatchingMode(mode_to_match, &mut lv1 as *mut _ as *mut _, concerned_device.map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method WaitForVBlank
  
  pub fn wait_for_v_blank(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).WaitForVBlank() };
    hr2ret(hr,hr)
  }
  
  //  Method TakeOwnership
  
  pub fn take_ownership<T: HasIID>(&self, device: &T, exclusive: BOOL) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).TakeOwnership(device.iptr() as *mut _ as *mut _ , exclusive) };
    hr2ret(hr,hr)
  }
  
  //  Method ReleaseOwnership
  
  pub fn release_ownership(&self) -> () {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).ReleaseOwnership() };
    ()
  }
  
  //  Method GetGammaControlCapabilities
  
  pub fn get_gamma_control_capabilities(&self) -> HResult<DXGI_GAMMA_CONTROL_CAPABILITIES> {
    let mut lv1: DXGI_GAMMA_CONTROL_CAPABILITIES = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetGammaControlCapabilities(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetGammaControl
  
  pub fn set_gamma_control(&self, array: &DXGI_GAMMA_CONTROL) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).SetGammaControl(array) };
    hr2ret(hr,hr)
  }
  
  //  Method GetGammaControl
  
  pub fn get_gamma_control(&self) -> HResult<DXGI_GAMMA_CONTROL> {
    let mut lv1: DXGI_GAMMA_CONTROL = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetGammaControl(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetDisplaySurface
  
  pub fn set_display_surface<T: HasIID>(&self, scanout_surface: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).SetDisplaySurface(scanout_surface.iptr() as *mut _ as *mut _ ) };
    hr2ret(hr,hr)
  }
  
  //  Method GetDisplaySurfaceData
  
  pub fn get_display_surface_data<T: HasIID>(&self, destination: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetDisplaySurfaceData(destination.iptr() as *mut _ as *mut _ ) };
    hr2ret(hr,hr)
  }
  
  //  Method GetFrameStatistics
  
  pub fn get_frame_statistics(&self) -> HResult<DXGI_FRAME_STATISTICS> {
    let mut lv1: DXGI_FRAME_STATISTICS = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetFrameStatistics(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetDisplayModeList1
  
  pub fn get_display_mode_list1(&self, enum_format: DXGI_FORMAT, flags: UINT, desc: Option<&mut [DXGI_MODE_DESC1]>) -> HResult<UINT> {
    let mut lv1: UINT = desc.as_ref().map(|v|v.len()).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput1)).GetDisplayModeList1(enum_format, flags, &mut lv1, opt_arr_as_mut_ptr(&desc) as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method FindClosestMatchingMode1
  
  pub fn find_closest_matching_mode1(&self, mode_to_match: &DXGI_MODE_DESC1, closest_match: &mut DXGI_MODE_DESC1, concerned_device: Option<&Unknown>) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput1)).FindClosestMatchingMode1(mode_to_match, closest_match, concerned_device.map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetDisplaySurfaceData1
  
  pub fn get_display_surface_data1<T: HasIID>(&self, destination: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput1)).GetDisplaySurfaceData1(destination.iptr() as *mut _ as *mut _ ) };
    hr2ret(hr,hr)
  }
  
  //  Method DuplicateOutput
  
  pub fn duplicate_output<T: HasIID>(&self, device: &T) -> HResult<DXGIOutputDuplication> {
    let mut lv1: *mut IDXGIOutputDuplication = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput1)).DuplicateOutput(device.iptr() as *mut _ as *mut _ , &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIOutputDuplication::new(lv1 as *mut _))
  }
  
  //  Method SupportsOverlays
  
  pub fn supports_overlays(&self) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput2)).SupportsOverlays() };
    hr
  }
  
  //  Method CheckOverlaySupport
  
  pub fn check_overlay_support<T: HasIID>(&self, enum_format: DXGI_FORMAT, concerned_device: &T) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput3)).CheckOverlaySupport(enum_format, concerned_device.iptr() as *mut _ as *mut _ , &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method CheckOverlayColorSpaceSupport
  
  pub fn check_overlay_color_space_support<T: HasIID>(&self, format: DXGI_FORMAT, color_space: DXGI_COLOR_SPACE_TYPE, concerned_device: &T) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput4)).CheckOverlayColorSpaceSupport(format, color_space, concerned_device.iptr() as *mut _ as *mut _ , &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  
}

pub struct DXGISwapChain3(*mut IDXGISwapChain3);

impl HasIID for DXGISwapChain3 {
  fn iid() -> REFGUID { &IID_IDXGISwapChain3 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGISwapChain3(pp_vtbl as *mut _ as *mut IDXGISwapChain3) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGISwapChain3 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGISwapChain3 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGISwapChain3 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIObject)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method Present
  
  pub fn present(&self, sync_interval: UINT, flags: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).Present(sync_interval, flags) };
    hr2ret(hr,hr)
  }
  
  //  Method GetBuffer
  
  pub fn get_buffer<T: HasIID>(&self, buffer: UINT) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetBuffer(buffer, T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method SetFullscreenState
  
  pub fn set_fullscreen_state(&self, fullscreen: BOOL, target: Option<&DXGIOutput>) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).SetFullscreenState(fullscreen, target.map(|i|i.iptr()).unwrap_or(ptr::null_mut()) as *mut _ as *mut _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetFullscreenState
  
  pub fn get_fullscreen_state(&self, fullscreen: Option<&mut BOOL>) -> HResult<DXGIOutput> {
    let mut lv1: *mut IDXGIOutput = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetFullscreenState(opt_as_mut_ptr(&fullscreen), &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIOutput::new(lv1 as *mut _))
  }
  
  //  Method GetDesc
  
  pub fn get_desc(&self) -> HResult<DXGI_SWAP_CHAIN_DESC> {
    let mut lv1: DXGI_SWAP_CHAIN_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method ResizeBuffers
  
  pub fn resize_buffers(&self, buffer_count: UINT, width: UINT, height: UINT, new_format: DXGI_FORMAT, swap_chain_flags: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).ResizeBuffers(buffer_count, width, height, new_format, swap_chain_flags) };
    hr2ret(hr,hr)
  }
  
  //  Method ResizeTarget
  
  pub fn resize_target(&self, new_target_parameters: &DXGI_MODE_DESC) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).ResizeTarget(new_target_parameters) };
    hr2ret(hr,hr)
  }
  
  //  Method GetContainingOutput
  
  pub fn get_containing_output(&self) -> HResult<DXGIOutput> {
    let mut lv1: *mut IDXGIOutput = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetContainingOutput(&mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIOutput::new(lv1 as *mut _))
  }
  
  //  Method GetFrameStatistics
  
  pub fn get_frame_statistics(&self) -> HResult<DXGI_FRAME_STATISTICS> {
    let mut lv1: DXGI_FRAME_STATISTICS = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetFrameStatistics(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetLastPresentCount
  
  pub fn get_last_present_count(&self) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetLastPresentCount(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetDesc1
  
  pub fn get_desc1(&self) -> HResult<DXGI_SWAP_CHAIN_DESC1> {
    let mut lv1: DXGI_SWAP_CHAIN_DESC1 = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetDesc1(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetFullscreenDesc
  
  pub fn get_fullscreen_desc(&self) -> HResult<DXGI_SWAP_CHAIN_FULLSCREEN_DESC> {
    let mut lv1: DXGI_SWAP_CHAIN_FULLSCREEN_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetFullscreenDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetHwnd
  
  pub fn get_hwnd(&self) -> HResult<HWND> {
    let mut lv1: HWND = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetHwnd(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetCoreWindow
  
  pub fn get_core_window<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetCoreWindow(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method Present1
  
  pub fn present1(&self, sync_interval: UINT, present_flags: UINT, present_parameters: &DXGI_PRESENT_PARAMETERS) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).Present1(sync_interval, present_flags, present_parameters) };
    hr2ret(hr,hr)
  }
  
  //  Method IsTemporaryMonoSupported
  
  pub fn is_temporary_mono_supported(&self) -> BOOL {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).IsTemporaryMonoSupported() };
    hr
  }
  
  //  Method GetRestrictToOutput
  
  pub fn get_restrict_to_output(&self) -> HResult<DXGIOutput> {
    let mut lv1: *mut IDXGIOutput = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetRestrictToOutput(&mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIOutput::new(lv1 as *mut _))
  }
  
  //  Method SetBackgroundColor
  
  pub fn set_background_color(&self, color: &DXGI_RGBA) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).SetBackgroundColor(color) };
    hr2ret(hr,hr)
  }
  
  //  Method GetBackgroundColor
  
  pub fn get_background_color(&self) -> HResult<DXGI_RGBA> {
    let mut lv1: DXGI_RGBA = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetBackgroundColor(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetRotation
  
  pub fn set_rotation(&self, rotation: DXGI_MODE_ROTATION) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).SetRotation(rotation) };
    hr2ret(hr,hr)
  }
  
  //  Method GetRotation
  
  pub fn get_rotation(&self) -> HResult<DXGI_MODE_ROTATION> {
    let mut lv1: DXGI_MODE_ROTATION = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain1)).GetRotation(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetSourceSize
  
  pub fn set_source_size(&self, width: UINT, height: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).SetSourceSize(width, height) };
    hr2ret(hr,hr)
  }
  
  //  Method GetSourceSize
  
  pub fn get_source_size(&self) -> HResult<DSize> {
    let mut lv1: DSize = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).GetSourceSize(&mut (lv1.width) as *mut _ as *mut _, &mut (lv1.height) as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetMaximumFrameLatency
  
  pub fn set_maximum_frame_latency(&self, max_latency: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).SetMaximumFrameLatency(max_latency) };
    hr2ret(hr,hr)
  }
  
  //  Method GetMaximumFrameLatency
  
  pub fn get_maximum_frame_latency(&self) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).GetMaximumFrameLatency(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetFrameLatencyWaitableObject
  
  pub fn get_frame_latency_waitable_object(&self) -> HANDLE {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).GetFrameLatencyWaitableObject() };
    hr
  }
  
  //  Method SetMatrixTransform
  
  pub fn set_matrix_transform(&self, matrix: &DXGI_MATRIX_3X2_F) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).SetMatrixTransform(matrix) };
    hr2ret(hr,hr)
  }
  
  //  Method GetMatrixTransform
  
  pub fn get_matrix_transform(&self) -> HResult<DXGI_MATRIX_3X2_F> {
    let mut lv1: DXGI_MATRIX_3X2_F = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain2)).GetMatrixTransform(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetCurrentBackBufferIndex
  
  pub fn get_current_back_buffer_index(&self) -> UINT {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain3)).GetCurrentBackBufferIndex() };
    hr
  }
  
  //  Method CheckColorSpaceSupport
  
  pub fn check_color_space_support(&self, color_space: DXGI_COLOR_SPACE_TYPE) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain3)).CheckColorSpaceSupport(color_space, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetColorSpace1
  
  pub fn set_color_space1(&self, color_space: DXGI_COLOR_SPACE_TYPE) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain3)).SetColorSpace1(color_space) };
    hr2ret(hr,hr)
  }
  
  //  Method ResizeBuffers1
  
  pub fn resize_buffers1<T: HasIID>(&self, width: UINT, height: UINT, format: DXGI_FORMAT, swap_chain_flags: UINT, creation_node_mask: &[UINT], present_queue: &[&T]) -> HResult<HRESULT> {
    let mut lv1: Vec<*mut IUnknown> = present_queue.iter().map(|o|o.iptr()).collect();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain3)).ResizeBuffers1( same_length(&[Some(creation_node_mask.len()),Some(present_queue.len())]).expect("Arrays must have equal sizes") as UINT, width, height, format, swap_chain_flags, slice_as_ptr(creation_node_mask), lv1.as_mut_ptr() as *mut *mut _ as *mut *mut _) };
    hr2ret(hr,hr)
  }
  
  
}

