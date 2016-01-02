use create_device::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use winapi::*;
use dx_safe::*;
use structwrappers::*;
use std::ptr;
use kernel32;

pub trait Handle: Sized {
  fn handle(&self) -> HANDLE;
}

#[derive(Debug)]
pub struct Event(HANDLE);

unsafe impl Sync for Event {}
unsafe impl Send for Event {}

impl Handle for Event {
  fn handle(&self) -> HANDLE {
    self.0
  }
}

impl Drop for Event {
  fn drop(&mut self) {
    let handle = self.handle();
    if handle != ptr::null_mut() {
      unsafe {
        kernel32::CloseHandle(handle);
      }
    }
  }
}

pub fn create_event() -> Event {
  let event_handle = unsafe{ kernel32::CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  if event_handle == ptr::null_mut() {
    panic!("Cannot create event.");
  }
  Event(event_handle)
}

pub struct DXCore {
  pub dev: D3D12Device,
  pub graphics_queue: D3D12CommandQueue,
  pub compute_queue: D3D12CommandQueue,
  pub copy_queue: D3D12CommandQueue,
  pub dxgi_factory: DXGIFactory4,
  pub info_queue: Option<D3D12InfoQueue>,
  pub fence_value: Arc<AtomicUsize>,
}

impl DXCore {
  // Warning! 32-bit build will convert u32 to u64
  pub fn next_fence_value(&self) -> u64 {
    self.fence_value.fetch_add(1, Ordering::Relaxed) as u64
  }
}

pub fn create_core(adapter: Option<&DXGIAdapter1>, feature_level: D3D_FEATURE_LEVEL, enable_debug: bool) -> Result<DXCore, String> {
  if enable_debug {
    try!(get_debug_interface()
      .map_err(|hr|format!("get_debug_interface failed with 0x{:x}", hr))
    ).enable_debug_layer();
  };
  let dev = try!(d3d12_create_device(adapter, feature_level)
        .map_err(|hr|format!("d3d12_create_device failed with 0x{:x}", hr)));

  let info_queue = 
    if enable_debug {
      let info_queue: D3D12InfoQueue = 
        try!(dev.query_interface()
            .map_err(|hr|format!("dev.query_interface::<D3D12InfoQueue> failed with 0x{:x}", hr)));
      Some(info_queue)
    } else {
      None
    };

  let factory: DXGIFactory4 = try!(create_dxgi_factory2()
        .map_err(|hr| format!("create_dxgi_factory2 failed with 0x{:x}", hr)));

  let gqd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
    Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
    Priority: 0,
    NodeMask: 0,
  };

  let gr_q = try!(dev.create_command_queue(&gqd)
        .map_err(|hr|format!("create_command_queue for graphics queue failed with 0x{:x}", hr)));
  gr_q.set_name("Graphics command queue".into()).unwrap();
  
  let cqd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_COMPUTE, .. gqd
  };
  let cm_q = try!(dev.create_command_queue(&cqd)
        .map_err(|hr|format!("create_command_queue for compute queue failed with 0x{:x}", hr)));
  cm_q.set_name("Compute command queue".into()).unwrap();

  let pqd = D3D12_COMMAND_QUEUE_DESC {
    Type: D3D12_COMMAND_LIST_TYPE_COPY, .. gqd
  };

  let cp_q = try!(dev.create_command_queue(&pqd)
        .map_err(|hr|format!("create_command_queue for copy queue failed with 0x{:x}", hr)));
  cp_q.set_name("Copy command queue".into()).unwrap();

  Ok(DXCore {
    dev: dev,
    graphics_queue: gr_q,
    compute_queue: cm_q,
    copy_queue: cp_q,
    dxgi_factory: factory,
    info_queue: info_queue,
    fence_value: Arc::new(AtomicUsize::new(1)), // not zero, as D3D12Fence::get_completed_value() returns zero before fence is reached.
  })
}

pub struct DXSwapChain {
  pub swap_chain: DXGISwapChain3,
  pub render_targets: Vec<D3D12Resource>,
  pub rtv_heap: DescriptorHeap,
  pub rtv_format: DXGI_FORMAT,
  pub frame_count: u32,
}

pub fn create_swap_chain(core: &DXCore, desc: &DXGI_SWAP_CHAIN_DESC1, format: DXGI_FORMAT, hwnd: HWND, 
                         fullscreen_desc: Option<&DXGI_SWAP_CHAIN_FULLSCREEN_DESC>, 
                         restrict_to_output: Option<&DXGIOutput>) -> Result<DXSwapChain, String> {
  let swap_chain: DXGISwapChain3 = 
    match core.dxgi_factory.create_swap_chain_for_hwnd(&core.graphics_queue, hwnd, desc, fullscreen_desc, restrict_to_output) {
      Err(hr) => return Err(format!("create_swap_chain failed with 0x{:x}",hr)),
      Ok(i) => try!(i.query_interface()
                    .map_err(|hr|format!("query_interface::<DXGISwapChain3> failed with 0x{:x}",hr))),
    };
  let frame_count = desc.BufferCount;

  let rtvheap = try!(DescriptorHeap::new(&core.dev, frame_count, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, false, 0)
                    .map_err(|hr|format!("create_descriptor_heap failed with 0x{:x}",hr)));
  let mut render_targets=vec![];

  for i in 0..frame_count {
    let buf = try!(swap_chain.get_buffer::<D3D12Resource>(i as u32)
                   .map_err(|hr|format!("swap_chain.get failed with 0x{:x}",hr)));
    core.dev.create_render_target_view(Some(&buf), Some(&render_target_view_desc_tex2d_default(format)), rtvheap.cpu_handle(i));
    render_targets.push(buf);
  }
  Ok(DXSwapChain {
    swap_chain: swap_chain,
    render_targets: render_targets,
    rtv_heap: rtvheap,
    rtv_format: format,
    frame_count: frame_count,
  })
}

pub fn drop_render_targets(sc: &mut DXSwapChain) {
  sc.render_targets.truncate(0);
}

pub fn reaquire_render_targets(core: &DXCore, sc: &mut DXSwapChain) -> HResult<()> {
  sc.render_targets.truncate(0);
  //info!("Old format: {:?}", sc.rtv_format);
  //sc.rtv_format = sc.swap_chain.get_desc().unwrap().BufferDesc.Format;
  //info!("New format: {:?}", sc.rtv_format);
  for i in 0 .. sc.frame_count {
    let buf=try!(sc.swap_chain.get_buffer::<D3D12Resource>(i as u32));
    core.dev.create_render_target_view(Some(&buf), Some(&render_target_view_desc_tex2d_default(sc.rtv_format)), sc.rtv_heap.cpu_handle(i));
    sc.render_targets.push(buf);
  }
  Ok(())
}

