use d3dcommon_safe::*;
use d3d12sdklayers_safe::*;
use d3d12_safe::*;
// This file is autogenerated

use utils::*;


pub struct DXGIAdapter1(*mut IDXGIAdapter1);
unsafe impl Send for DXGIAdapter1 {}

impl HasIID for DXGIAdapter1 {
  fn iid() -> REFGUID { &IID_IDXGIAdapter1 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIAdapter1(pp_vtbl as *mut _ as *mut IDXGIAdapter1) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIAdapter1 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIAdapter1 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIAdapter1 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method EnumOutputs
  
  pub fn enum_outputs(&self, output: UINT) -> HResult<DXGIOutput> {
    let mut lv1: *mut IDXGIOutput = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).EnumOutputs(output, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIOutput::new(lv1 as *mut _))
  }
  
  //  Method GetDesc
  
  pub fn get_desc(&self) -> HResult<DXGI_ADAPTER_DESC> {
    let mut lv1: DXGI_ADAPTER_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).GetDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method CheckInterfaceSupport
  
  pub fn check_interface_support(&self, interface_name: &GUID) -> HResult<LARGE_INTEGER> {
    let mut lv1: LARGE_INTEGER = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).CheckInterfaceSupport(interface_name, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetDesc1
  
  pub fn get_desc1(&self) -> HResult<DXGI_ADAPTER_DESC1> {
    let mut lv1: DXGI_ADAPTER_DESC1 = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter1)).GetDesc1(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  
}

pub struct DXGIAdapter(*mut IDXGIAdapter);

impl HasIID for DXGIAdapter {
  fn iid() -> REFGUID { &IID_IDXGIAdapter }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIAdapter(pp_vtbl as *mut _ as *mut IDXGIAdapter) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIAdapter {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIAdapter {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIAdapter {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIAdapter)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
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
  
  
}

pub struct DXGIDevice1(*mut IDXGIDevice1);

impl HasIID for DXGIDevice1 {
  fn iid() -> REFGUID { &IID_IDXGIDevice1 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIDevice1(pp_vtbl as *mut _ as *mut IDXGIDevice1) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIDevice1 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIDevice1 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIDevice1 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method GetAdapter
  
  pub fn get_adapter(&self) -> HResult<DXGIAdapter> {
    let mut lv1: *mut IDXGIAdapter = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).GetAdapter(&mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIAdapter::new(lv1 as *mut _))
  }
  
  //  Method QueryResourceResidency
  
  pub fn query_resource_residency<T: HasIID>(&self, resources: &[&T], residency_status: &mut [DXGI_RESIDENCY]) -> HResult<HRESULT> {
    let mut lv1: Vec<*mut IUnknown> = resources.iter().map(|o|o.iptr()).collect();
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).QueryResourceResidency(lv1.as_mut_ptr() as *mut *mut _ as *mut *mut _, slice_as_mut_ptr(residency_status),  same_length(&[Some(resources.len()),Some(residency_status.len())]).expect("Arrays must have equal sizes") as UINT) };
    hr2ret(hr,hr)
  }
  
  //  Method SetGPUThreadPriority
  
  pub fn set_gpu_thread_priority(&self, priority: INT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).SetGPUThreadPriority(priority) };
    hr2ret(hr,hr)
  }
  
  //  Method GetGPUThreadPriority
  
  pub fn get_gpu_thread_priority(&self) -> HResult<INT> {
    let mut lv1: INT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).GetGPUThreadPriority(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetMaximumFrameLatency
  
  pub fn set_maximum_frame_latency(&self, max_latency: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).SetMaximumFrameLatency(max_latency) };
    hr2ret(hr,hr)
  }
  
  //  Method GetMaximumFrameLatency
  
  pub fn get_maximum_frame_latency(&self) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice1)).GetMaximumFrameLatency(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  
}

pub struct DXGIDeviceSubObject(*mut IDXGIDeviceSubObject);

impl HasIID for DXGIDeviceSubObject {
  fn iid() -> REFGUID { &IID_IDXGIDeviceSubObject }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIDeviceSubObject(pp_vtbl as *mut _ as *mut IDXGIDeviceSubObject) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIDeviceSubObject {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIDeviceSubObject {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIDeviceSubObject {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIDeviceSubObject)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIDeviceSubObject)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIDeviceSubObject)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  
}

pub struct DXGIDevice(*mut IDXGIDevice);

impl HasIID for DXGIDevice {
  fn iid() -> REFGUID { &IID_IDXGIDevice }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIDevice(pp_vtbl as *mut _ as *mut IDXGIDevice) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIDevice {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIDevice {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIDevice {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method GetAdapter
  
  pub fn get_adapter(&self) -> HResult<DXGIAdapter> {
    let mut lv1: *mut IDXGIAdapter = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).GetAdapter(&mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIAdapter::new(lv1 as *mut _))
  }
  
  //  Method QueryResourceResidency
  
  pub fn query_resource_residency<T: HasIID>(&self, resources: &[&T], residency_status: &mut [DXGI_RESIDENCY]) -> HResult<HRESULT> {
    let mut lv1: Vec<*mut IUnknown> = resources.iter().map(|o|o.iptr()).collect();
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).QueryResourceResidency(lv1.as_mut_ptr() as *mut *mut _ as *mut *mut _, slice_as_mut_ptr(residency_status),  same_length(&[Some(resources.len()),Some(residency_status.len())]).expect("Arrays must have equal sizes") as UINT) };
    hr2ret(hr,hr)
  }
  
  //  Method SetGPUThreadPriority
  
  pub fn set_gpu_thread_priority(&self, priority: INT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).SetGPUThreadPriority(priority) };
    hr2ret(hr,hr)
  }
  
  //  Method GetGPUThreadPriority
  
  pub fn get_gpu_thread_priority(&self) -> HResult<INT> {
    let mut lv1: INT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIDevice)).GetGPUThreadPriority(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  
}

pub struct DXGIFactory1(*mut IDXGIFactory1);

impl HasIID for DXGIFactory1 {
  fn iid() -> REFGUID { &IID_IDXGIFactory1 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIFactory1(pp_vtbl as *mut _ as *mut IDXGIFactory1) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIFactory1 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIFactory1 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIFactory1 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method EnumAdapters
  
  pub fn enum_adapters(&self, adapter: UINT) -> HResult<DXGIAdapter> {
    let mut lv1: *mut IDXGIAdapter = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).EnumAdapters(adapter, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGIAdapter::new(lv1 as *mut _))
  }
  
  //  Method MakeWindowAssociation
  
  pub fn make_window_association(&self, window_handle: HWND, flags: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).MakeWindowAssociation(window_handle, flags) };
    hr2ret(hr,hr)
  }
  
  //  Method GetWindowAssociation
  
  pub fn get_window_association(&self) -> HResult<HWND> {
    let mut lv1: HWND = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).GetWindowAssociation(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method CreateSwapChain
  
  pub fn create_swap_chain<T: HasIID>(&self, device: &T, desc: &mut DXGI_SWAP_CHAIN_DESC) -> HResult<DXGISwapChain> {
    let mut lv1: *mut IDXGISwapChain = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).CreateSwapChain(device.iptr() as *mut _ as *mut _ , desc, &mut lv1 as *mut *mut _) };
    hr2ret(hr,DXGISwapChain::new(lv1 as *mut _))
  }
  
  //  Method CreateSoftwareAdapter
  
  pub fn create_software_adapter(&self, module: HMODULE) -> HResult<DXGIAdapter> {
    let mut lv1: *mut IDXGIAdapter = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory1)).CreateSoftwareAdapter(module, &mut lv1 as *mut *mut _) };
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
  
  
}

pub struct DXGIFactory(*mut IDXGIFactory);

impl HasIID for DXGIFactory {
  fn iid() -> REFGUID { &IID_IDXGIFactory }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIFactory(pp_vtbl as *mut _ as *mut IDXGIFactory) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIFactory {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIFactory {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIFactory {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIFactory)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
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
  
  
}

pub struct DXGIKeyedMutex(*mut IDXGIKeyedMutex);

impl HasIID for DXGIKeyedMutex {
  fn iid() -> REFGUID { &IID_IDXGIKeyedMutex }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIKeyedMutex(pp_vtbl as *mut _ as *mut IDXGIKeyedMutex) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIKeyedMutex {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIKeyedMutex {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIKeyedMutex {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIKeyedMutex)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIKeyedMutex)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIKeyedMutex)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method AcquireSync
  
  pub fn acquire_sync(&self, key: UINT64, milliseconds: DWORD) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIKeyedMutex)).AcquireSync(key, milliseconds) };
    hr2ret(hr,hr)
  }
  
  //  Method ReleaseSync
  
  pub fn release_sync(&self, key: UINT64) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIKeyedMutex)).ReleaseSync(key) };
    hr2ret(hr,hr)
  }
  
  
}

pub struct DXGIObject(*mut IDXGIObject);

impl HasIID for DXGIObject {
  fn iid() -> REFGUID { &IID_IDXGIObject }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIObject(pp_vtbl as *mut _ as *mut IDXGIObject) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIObject {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIObject {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIObject {
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
  
  
}

pub struct DXGIOutput(*mut IDXGIOutput);

impl HasIID for DXGIOutput {
  fn iid() -> REFGUID { &IID_IDXGIOutput }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIOutput(pp_vtbl as *mut _ as *mut IDXGIOutput) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIOutput {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIOutput {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIOutput {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIOutput)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
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
  
  
}

pub struct DXGIResource(*mut IDXGIResource);

impl HasIID for DXGIResource {
  fn iid() -> REFGUID { &IID_IDXGIResource }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGIResource(pp_vtbl as *mut _ as *mut IDXGIResource) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGIResource {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGIResource {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGIResource {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method GetSharedHandle
  
  pub fn get_shared_handle(&self) -> HResult<HANDLE> {
    let mut lv1: HANDLE = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).GetSharedHandle(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetUsage
  
  pub fn get_usage(&self) -> HResult<DXGI_USAGE> {
    let mut lv1: DXGI_USAGE = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).GetUsage(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method SetEvictionPriority
  
  pub fn set_eviction_priority(&self, eviction_priority: UINT) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).SetEvictionPriority(eviction_priority) };
    hr2ret(hr,hr)
  }
  
  //  Method GetEvictionPriority
  
  pub fn get_eviction_priority(&self) -> HResult<UINT> {
    let mut lv1: UINT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGIResource)).GetEvictionPriority(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  
}

pub struct DXGISurface1(*mut IDXGISurface1);

impl HasIID for DXGISurface1 {
  fn iid() -> REFGUID { &IID_IDXGISurface1 }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGISurface1(pp_vtbl as *mut _ as *mut IDXGISurface1) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGISurface1 {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGISurface1 {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGISurface1 {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method GetDesc
  
  pub fn get_desc(&self) -> HResult<DXGI_SURFACE_DESC> {
    let mut lv1: DXGI_SURFACE_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).GetDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method Map
  
  pub fn map(&self, map_flags: UINT) -> HResult<DXGI_MAPPED_RECT> {
    let mut lv1: DXGI_MAPPED_RECT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).Map(&mut lv1 as *mut _ as *mut _, map_flags) };
    hr2ret(hr,lv1)
  }
  
  //  Method Unmap
  
  pub fn unmap(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).Unmap() };
    hr2ret(hr,hr)
  }
  
  //  Method GetDC
  
  pub fn get_d_c(&self, discard: BOOL) -> HResult<HDC> {
    let mut lv1: HDC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).GetDC(discard, &mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method ReleaseDC
  
  pub fn release_d_c(&self, dirty_rect: Option<&mut RECT>) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISurface1)).ReleaseDC(opt_as_mut_ptr(&dirty_rect)) };
    hr2ret(hr,hr)
  }
  
  
}

pub struct DXGISurface(*mut IDXGISurface);

impl HasIID for DXGISurface {
  fn iid() -> REFGUID { &IID_IDXGISurface }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGISurface(pp_vtbl as *mut _ as *mut IDXGISurface) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGISurface {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGISurface {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGISurface {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISurface)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGISurface)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISurface)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
    hr2ret(hr,T::new(lv1))
  }
  
  //  Method GetDesc
  
  pub fn get_desc(&self) -> HResult<DXGI_SURFACE_DESC> {
    let mut lv1: DXGI_SURFACE_DESC = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISurface)).GetDesc(&mut lv1 as *mut _ as *mut _) };
    hr2ret(hr,lv1)
  }
  
  //  Method Map
  
  pub fn map(&self, map_flags: UINT) -> HResult<DXGI_MAPPED_RECT> {
    let mut lv1: DXGI_MAPPED_RECT = unsafe {mem::uninitialized::<_>()};
    let hr=unsafe { (*(self.0 as *mut IDXGISurface)).Map(&mut lv1 as *mut _ as *mut _, map_flags) };
    hr2ret(hr,lv1)
  }
  
  //  Method Unmap
  
  pub fn unmap(&self) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISurface)).Unmap() };
    hr2ret(hr,hr)
  }
  
  
}

pub struct DXGISwapChain(*mut IDXGISwapChain);

impl HasIID for DXGISwapChain {
  fn iid() -> REFGUID { &IID_IDXGISwapChain }
  fn new(pp_vtbl : *mut IUnknown) -> Self { DXGISwapChain(pp_vtbl as *mut _ as *mut IDXGISwapChain) }
  fn iptr(&self) -> *mut IUnknown { self.0 as *mut _ as  *mut IUnknown}
}

impl Drop for DXGISwapChain {
  fn drop(&mut self) {
    release_com_ptr(self)
  }
}

impl Clone for DXGISwapChain {
  fn clone(&self) -> Self {
    clone_com_ptr(self)
  }
}

impl DXGISwapChain {
  //  Method SetPrivateData
  
  pub fn set_private_data<T>(&self, name: &GUID, data: &T) -> HResult<HRESULT> {
  
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).SetPrivateData(name, mem::size_of_val(data) as UINT, data as *const _ as *const _) };
    hr2ret(hr,hr)
  }
  
  //  Method GetPrivateData
  
  pub fn get_private_data<T>(&self, name: &GUID, data: Option<&mut T>) -> HResult<UINT> {
    let mut lv1: UINT = data.as_ref().map(|v|mem::size_of_val(*v)).unwrap_or(0) as UINT;
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetPrivateData(name, &mut lv1, data.map(|v|v as *const _ as *mut _).unwrap_or(ptr::null_mut())) };
    hr2ret(hr,lv1)
  }
  
  //  Method GetParent
  
  pub fn get_parent<T: HasIID>(&self) -> HResult<T> {
    let mut lv1: *mut IUnknown = ptr::null_mut();
    let hr=unsafe { (*(self.0 as *mut IDXGISwapChain)).GetParent(T::iid(), &mut lv1 as *mut *mut _ as *mut *mut c_void) };
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
  
  
}

