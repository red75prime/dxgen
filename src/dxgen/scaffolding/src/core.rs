use create_device::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use winapi::*;
use d3d12_safe::*;
use structwrappers::*;

pub struct DXCore {
  pub dev: D3D12Device,
  pub graphics_queue: D3D12CommandQueue,
  pub compute_queue: D3D12CommandQueue,
  pub copy_queue: D3D12CommandQueue,
  pub dxgi_factory: DXGIFactory4,
  pub info_queue: Option<D3D12InfoQueue>,
  pub fence_value: Arc<AtomicUsize>,
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
  
  let cqd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_COMPUTE, .. gqd
  };
  let cm_q = try!(dev.create_command_queue(&cqd)
        .map_err(|hr|format!("create_command_queue for compute queue failed with 0x{:x}", hr)));

  let pqd = D3D12_COMMAND_QUEUE_DESC {
    Type: D3D12_COMMAND_LIST_TYPE_COPY, .. gqd
  };

  let cp_q = try!(dev.create_command_queue(&pqd)
        .map_err(|hr|format!("create_command_queue for copy queue failed with 0x{:x}", hr)));

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
  pub rtv_heap: D3D12DescriptorHeap,
  pub rtv_dsize: SIZE_T, // render target view descriptor size  
  pub rtv_format: DXGI_FORMAT,
  pub frame_count: u32,
}

pub fn create_swap_chain(core: &DXCore, desc: &DXGI_SWAP_CHAIN_DESC1, format: DXGI_FORMAT, hwnd: HWND, fullscreen_desc: Option<&DXGI_SWAP_CHAIN_FULLSCREEN_DESC>, restrict_to_output: Option<&DXGIOutput>) -> Result<DXSwapChain, String> {
  let swap_chain: DXGISwapChain3 = 
    match core.dxgi_factory.create_swap_chain_for_hwnd(&core.graphics_queue, hwnd, desc, fullscreen_desc, restrict_to_output) {
      Err(hr) => return Err(format!("create_swap_chain failed with 0x{:x}",hr)),
      Ok(i) => try!(i.query_interface()
                    .map_err(|hr|format!("query_interface::<DXGISwapChain3> failed with 0x{:x}",hr))),
    };
  let frame_count = desc.BufferCount;

  let rtvhd=D3D12_DESCRIPTOR_HEAP_DESC {
    NumDescriptors: frame_count,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let rtvheap = try!(core.dev.create_descriptor_heap(&rtvhd)
                    .map_err(|hr|format!("create_descriptor_heap failed with 0x{:x}",hr)));
  let rtvdsize=core.dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_RTV) as SIZE_T;
  let mut cdh=rtvheap.get_cpu_descriptor_handle_for_heap_start();
  let mut render_targets=vec![];

  for i in 0..frame_count {
    let buf = try!(swap_chain.get_buffer::<D3D12Resource>(i as u32)
                   .map_err(|hr|format!("swap_chain.get failed with 0x{:x}",hr)));
    core.dev.create_render_target_view(Some(&buf), Some(&render_target_view_desc_tex2d_default(format)), cdh);
    cdh.ptr += rtvdsize;
    render_targets.push(buf);
  }
  Ok(DXSwapChain {
    swap_chain: swap_chain,
    render_targets: render_targets,
    rtv_heap: rtvheap,
    rtv_dsize: rtvdsize,
    rtv_format: format,
    frame_count: frame_count,
  })
}

pub fn drop_render_targets(sc: &mut DXSwapChain) {
  sc.render_targets.truncate(0);
}

pub fn reaquire_render_targets(core: &DXCore, sc: &mut DXSwapChain) -> HResult<()> {
  let mut cdh = sc.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  sc.render_targets.truncate(0);
  for i in 0 .. sc.frame_count {
    let buf=try!(sc.swap_chain.get_buffer::<D3D12Resource>(i as u32));
    core.dev.create_render_target_view(Some(&buf), Some(&render_target_view_desc_tex2d_default(sc.rtv_format)), cdh);
    cdh.ptr += sc.rtv_dsize;
    sc.render_targets.push(buf);
  }
  Ok(())
}

