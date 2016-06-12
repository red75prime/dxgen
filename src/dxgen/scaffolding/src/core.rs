use create_device::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use winapi::*;
use dxsafe::*;
use dxsafe::structwrappers::*;
use std::ptr;
use kernel32;
use std::mem;

pub trait DumpOnError {
    fn dump(self, core: &DXCore) -> Self;
    fn dump_iq(self, core: &D3D12InfoQueue) -> Self;
}

impl<T> DumpOnError for HResult<T> {
    fn dump(self, core: &DXCore) -> Self {
        match self {
            Ok(_) => (),
            Err(_) => {
                core.dump_info_queue();
                ()
            },
        };
        self
    }

    fn dump_iq(self, iq: &D3D12InfoQueue) -> Self {
        match self {
            Ok(_) => (),
            Err(_) => {
                dump_info_queue(iq);
                ()
            },
        };
        self
    }
}

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
    let event_handle = unsafe { kernel32::CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
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

    pub fn dump_info_queue(&self) {
        if let Some(ref iq) = self.info_queue {
            dump_info_queue(iq);
        }
    }

    pub fn dump_info_queue_tagged(&self, tag: &str) {
        if let Some(ref iq) = self.info_queue {
            if iq.get_num_stored_messages_allowed_by_retrieval_filter() != 0 {
                info!("{}", tag);
            };
            dump_info_queue(iq);
        }
    }
}

pub fn dump_info_queue(iq: &D3D12InfoQueue) {
    let mnum = iq.get_num_stored_messages_allowed_by_retrieval_filter();
    // println!("Number of debug messages is {}", mnum);
    for i in 0..mnum {
        let mut sz = 0;
        let _ = iq.get_message(i, None, &mut sz);
        // info!("get_message returned {:?}", hr); // It is the case when hr!=0 and it's ok
        // create buffer to receive message
        let arr: Vec<u8> = Vec::with_capacity(sz as usize);
        let mut sz1 = sz;
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
                D3D12_MESSAGE_SEVERITY_ERROR => { 
                    error!("{:}", descr);
                },
                D3D12_MESSAGE_SEVERITY_WARNING => warn!("{:}", descr),
                _ => debug!("{:}", descr),
            }
        }
    }
    iq.clear_stored_messages();
}

pub fn create_core(dxgi_factory: &DXGIFactory4, adapter: Option<&DXGIAdapter1>,
                   feature_level: D3D_FEATURE_LEVEL,
                   enable_debug: bool)
                   -> Result<DXCore, HRESULT> {
    if enable_debug {
        trace!("get_debug_interface");
        try!(d3d12_get_debug_interface())
            .enable_debug_layer();
    };


    let dev = try!(d3d12_create_device(adapter, feature_level));

    let info_queue = if enable_debug {
        trace!("get D3D12InfoQueue");
        Some(try!(dev.query_interface::<D3D12InfoQueue>()))
    } else {
        None
    };

    let factory = dxgi_factory.clone();

    let gqd = D3D12_COMMAND_QUEUE_DESC {
        Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
        Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
        Priority: 0,
        NodeMask: 0,
    };

    let gr_q = try!(dev.create_command_queue(&gqd));
    try!(gr_q.set_name("Graphics command queue".into()));

    let cqd = D3D12_COMMAND_QUEUE_DESC { Type: D3D12_COMMAND_LIST_TYPE_COMPUTE, ..gqd };
    let cm_q = try!(dev.create_command_queue(&cqd));
    try!(cm_q.set_name("Compute command queue".into()));

    let pqd = D3D12_COMMAND_QUEUE_DESC { Type: D3D12_COMMAND_LIST_TYPE_COPY, ..gqd };

    let cp_q = try!(dev.create_command_queue(&pqd));
    try!(cp_q.set_name("Copy command queue".into()));

    Ok(DXCore {
        dev: dev,
        graphics_queue: gr_q,
        compute_queue: cm_q,
        copy_queue: cp_q,
        dxgi_factory: factory,
        info_queue: info_queue,
        fence_value: Arc::new(AtomicUsize::new(1)), /* not zero, as D3D12Fence::get_completed_value() returns zero before fence is reached. */
    })
}

pub struct DXSwapChain {
    pub swap_chain: DXGISwapChain3,
    pub render_targets: Vec<D3D12Resource>,
    pub rtv_heap: DescriptorHeap,
    pub rtv_format: DXGI_FORMAT,
    pub frame_count: u32,
}

impl DXSwapChain {
    pub fn drop_render_targets(&mut self) {
        self.render_targets.truncate(0);
    }

    pub fn reaquire_render_targets(&mut self, dev: &D3D12Device) -> HResult<()> {
        self.render_targets.truncate(0);
        // info!("Old format: {:?}", sc.rtv_format);
        // sc.rtv_format = sc.swap_chain.get_desc().unwrap().BufferDesc.Format;
        // info!("New format: {:?}", sc.rtv_format);
        for i in 0..self.frame_count {
            let buf = try!(self.swap_chain.get_buffer::<D3D12Resource>(i as u32));
            dev.create_render_target_view(Some(&buf), Some(&render_target_view_desc_tex2d_default(self.rtv_format)), self.rtv_heap.cpu_handle(i));
            self.render_targets.push(buf);
        }
        Ok(())
    }

    pub fn rtv_cpu_handle(&self, idx: u32) -> D3D12_CPU_DESCRIPTOR_HANDLE {
        assert!(idx < self.frame_count);
        self.rtv_heap.cpu_handle(idx)
    }

    pub fn render_target(&self, idx: u32) -> &D3D12Resource {
        &self.render_targets[idx as usize]
    }

    pub fn resize(&mut self,
                  dev: &D3D12Device,
                  w: u32,
                  h: u32,
                  format: DXGI_FORMAT)
                  -> HResult<()> {
        // For buffer resize to succeed there should be no outstanding references to back buffers
        self.drop_render_targets();
        // Resize back buffers to new size.
        // Number of back buffers and swapchain flags remain unchanged.
        try!(self.swap_chain.resize_buffers(0, w, h, format, 0));
        self.rtv_format = format;
        try!(self.reaquire_render_targets(dev));
        Ok(())
    }
}

pub fn create_swap_chain(core: &DXCore,
                         desc: &DXGI_SWAP_CHAIN_DESC1,
                         format: DXGI_FORMAT,
                         hwnd: HWND,
                         fullscreen_desc: Option<&DXGI_SWAP_CHAIN_FULLSCREEN_DESC>,
                         restrict_to_output: Option<&DXGIOutput>)
                         -> HResult<DXSwapChain> {
    trace!("Create swap chain");
    let swap_chain: DXGISwapChain3 = 
        try!(try!(core.dxgi_factory.create_swap_chain_for_hwnd(&core.graphics_queue,
                                                                hwnd,
                                                                desc,
                                                                fullscreen_desc,
                                                                restrict_to_output))
                .query_interface());
    let frame_count = desc.BufferCount;
    
    trace!("Create DescriptorHeap");
    let rtvheap = try!(DescriptorHeap::new(&core.dev,
                                           frame_count,
                                           D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
                                           false,
                                           0));
    let mut render_targets = vec![];

    for i in 0..frame_count {
        trace!("swap_chain.get_buffer({})", i);
        let buf = try!(swap_chain.get_buffer::<D3D12Resource>(i as u32));
        trace!("Create render target view");
        core.dev.create_render_target_view(Some(&buf),
                                           Some(&render_target_view_desc_tex2d_default(format)),
                                           rtvheap.cpu_handle(i));
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
