use create_device::*;
use dxsafe::*;
use dxsafe::structwrappers::*;
use failure::{Error, ResultExt};
use error_conversion::{HRFail, ResultExtHr};
use kernel32;
use std::mem;
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use utils;
use winapi::*;

static FENCE_VALUE: AtomicUsize = AtomicUsize::new(1);

#[cfg(target_pointer_width = "64")]
pub fn next_fence_value() -> u64 {
    FENCE_VALUE.fetch_add(1, Ordering::SeqCst) as u64
}

#[cfg(target_pointer_width = "32")]
pub fn next_fence_value() -> u64 {
    unimplemented!()
}

pub fn wait_for(
    queue: &D3D12CommandQueue,
    fence_value: u64,
    fence: &D3D12Fence,
    fence_event: &Event,
) -> HResult<()> {
    try!(queue.signal(fence, fence_value));
    if fence.get_completed_value() < fence_value {
        try!(fence.set_event_on_completion(fence_value, fence_event.handle()));
        wait_for_single_object(fence_event, INFINITE);
    }
    Ok(())
}

pub fn wait(queue: &D3D12CommandQueue, fence: &D3D12Fence, event: &Event) -> HResult<()> {
    wait_for(queue, next_fence_value(), fence, event)
}

pub trait DumpOnError {
    type Error;
    fn dump<M: AsRef<str>>(self, core: &DXCore, msg: M) -> Self;
    fn dump_iq(self, core: &D3D12InfoQueue) -> Self;
    fn msg<M: AsRef<str>>(self, msg: M) -> Self
    where
        Self::Error: ::std::fmt::Display;
}

impl<T, E> DumpOnError for Result<T, E> {
    type Error = E;

    fn dump<M: AsRef<str>>(self, core: &DXCore, msg: M) -> Self {
        if self.is_err() {
            core.dump_info_queue_tagged(msg.as_ref());
        };
        self
    }

    fn dump_iq(self, iq: &D3D12InfoQueue) -> Self {
        if self.is_err() {
            dump_info_queue(iq);
        };
        self
    }

    fn msg<M: AsRef<str>>(self, msg: M) -> Self
    where
        Self::Error: ::std::fmt::Display,
    {
        match self {
            Err(ref err) => error!("{}, err: {}", msg.as_ref(), err),
            _ => (),
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
        if !handle.is_null() {
            unsafe {
                kernel32::CloseHandle(handle);
            }
        }
    }
}

pub fn create_event() -> Event {
    let event_handle = unsafe { kernel32::CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
    if event_handle.is_null() {
        panic!("Cannot create event.");
    }
    Event(event_handle)
}

pub fn wait_for_single_object(event: &Event, ms: u32) -> u32 {
    unsafe { kernel32::WaitForSingleObject(event.handle(), ms) }
}

pub struct DXCore {
    pub dev: D3D12Device,
    pub graphics_queue: D3D12CommandQueue,
    pub compute_queue: D3D12CommandQueue,
    pub copy_queue: D3D12CommandQueue,
    pub dxgi_factory: DXGIFactory4,
    pub info_queue: Option<D3D12InfoQueue>,
}

impl DXCore {
    pub fn dump_info_queue(&self) {
        if let Some(ref iq) = self.info_queue {
            dump_info_queue(iq);
        }
    }

    pub fn dump_info_queue_tagged(&self, tag: &str) {
        if let Some(ref iq) = self.info_queue {
            if iq.get_num_stored_messages_allowed_by_retrieval_filter() != 0 {
                debug!("Dump Infoqueue at '{}'", tag);
            };
            dump_info_queue(iq);
        }
    }
}

pub fn dump_info_queue(iq: &D3D12InfoQueue) {
    let mnum = iq.get_num_stored_messages_allowed_by_retrieval_filter();
    // println!("Number of debug messages is {}", mnum);
    if mnum != 0 {
        debug!("Start of infoqueue dump");
    };
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
            let _ = iq.get_message(
                i,
                Some(&mut *(arr.as_ptr() as *mut D3D12_MESSAGE)),
                &mut sz1,
            );
            assert_eq!(sz, sz1); // just to be sure. hr is Err(1) on success.
                                 // Reinterpret first chunk of arr as D3D12_MESSAGE, and byte-copy it into msg
            let msg: D3D12_MESSAGE = mem::transmute_copy(&(*arr.as_ptr()));
            // msg contains pointer into memory occupied by arr. arr should be borrowed now, but it is unsafe code.
            let cdescr = ::std::ffi::CStr::from_ptr(msg.pDescription as *const i8);
            let descr = String::from_utf8_lossy(cdescr.to_bytes()).to_owned();
            match msg.Severity {
                D3D12_MESSAGE_SEVERITY_CORRUPTION | D3D12_MESSAGE_SEVERITY_ERROR => {
                    error!("{:}", descr);
                }
                D3D12_MESSAGE_SEVERITY_WARNING => warn!("{:}", descr),
                _ => debug!("{:}", descr),
            }
        }
    }
    iq.clear_stored_messages();
    if mnum != 0 {
        debug!("End of infoqueue dump");
    };
}

#[derive(Debug)]
pub enum InitFailure {
    UnsupportedFeatureLevel(D3D_FEATURE_LEVEL),
}

pub trait GetInitFailure {
    fn get_init_failure(&self) -> Option<&InitFailure>;
    fn is_unsupported_feature_level(&self) -> bool;
}

impl GetInitFailure for Error {
    fn get_init_failure(&self) -> Option<&InitFailure> {
        use error_conversion::ErrorExt;
        use failure::Context;

        self.downcast_ref_causes::<Context<InitFailure>>()
            .map(Context::get_context)
    }

    fn is_unsupported_feature_level(&self) -> bool {
        self.get_init_failure()
            .map(|i| i.is_unsupported_feature_level())
            .unwrap_or(false)
    }
}

impl ::std::fmt::Display for InitFailure {
    fn fmt(&self, w: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        match *self {
            InitFailure::UnsupportedFeatureLevel(fl) => write!(
                w,
                "Unsupported feature level {}",
                utils::feature_level_desc(fl)
            ),
        }
    }
}

impl InitFailure {
    pub fn is_unsupported_feature_level(&self) -> bool {
        match *self {
            InitFailure::UnsupportedFeatureLevel(_) => true,
        }
    }
    pub fn new_ufl(fl: D3D_FEATURE_LEVEL) -> Self {
        InitFailure::UnsupportedFeatureLevel(fl)
    }
}

pub fn create_core(
    dxgi_factory: &DXGIFactory4,
    adapter: Option<&DXGIAdapter1>,
    feature_level: D3D_FEATURE_LEVEL,
    enable_debug: bool,
) -> Result<DXCore, Error> {
    if enable_debug {
        trace!("get_debug_interface");
        d3d12_get_debug_interface()
            .into_error_context("Cannot get debug interface")?
            .enable_debug_layer();
    };

    trace!("d3d12_create_device");
    let dev = match d3d12_create_device(adapter, feature_level) {
        res @ Err(DXGI_ERROR_UNSUPPORTED) => {
            res.into_error_context(InitFailure::new_ufl(feature_level))?
        }
        res => res.into_error_context("Cannot create D3D12Device")?,
    };

    let info_queue = if enable_debug {
        trace!("get D3D12InfoQueue");
        let iq = dev.query_interface::<D3D12InfoQueue>()
            .into_error_context("Cannot get D3D12InfoQueue from device")?;
        iq.set_break_on_severity(D3D12_MESSAGE_SEVERITY_CORRUPTION, 0)
            .into_error_context("Cannot set break on severity for D3D12InfoQueue")?;
        Some(iq)
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

    let gr_q = dev.create_command_queue(&gqd)
        .into_error_context("Cannot create graphics command queue")?;
    gr_q.set_name("Graphics command queue")
        .into_error_context("Cannot set name for graphics command queue")?;

    let cqd = D3D12_COMMAND_QUEUE_DESC {
        Type: D3D12_COMMAND_LIST_TYPE_COMPUTE,
        Priority: 0,
        Flags: D3D12_COMMAND_QUEUE_FLAG_DISABLE_GPU_TIMEOUT,
        ..gqd
    };
    let cm_q = dev.create_command_queue(&cqd)
        .into_error_context("Cannot create compute command queue")?;
    cm_q.set_name("Compute command queue")
        .into_error_context("Cannot set compute command queue name")?;
    let pqd = D3D12_COMMAND_QUEUE_DESC {
        Type: D3D12_COMMAND_LIST_TYPE_COPY,
        ..gqd
    };

    let cp_q = dev.create_command_queue(&pqd)
        .into_error_context("Cannot create copy command queue")?;
    cp_q.set_name("Copy command queue")
        .into_error_context("Cannot set copy command queue name")?;

    Ok(DXCore {
        dev: dev,
        graphics_queue: gr_q,
        compute_queue: cm_q,
        copy_queue: cp_q,
        dxgi_factory: factory,
        info_queue: info_queue,
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

    pub fn reaquire_render_targets(&mut self, dev: &D3D12Device) -> Result<(), Error> {
        self.render_targets.truncate(0);
        // info!("Old format: {:?}", sc.rtv_format);
        // sc.rtv_format = sc.swap_chain.get_desc().unwrap().BufferDesc.Format;
        // info!("New format: {:?}", sc.rtv_format);
        for i in 0..self.frame_count {
            let buf = self.swap_chain
                .get_buffer::<D3D12Resource>(i as u32)
                .into_error_with_context(|_| format!("Cannot get swap chain buffer #{}", i))?;
            let rtvdesc = render_target_view_desc_tex2d_default(self.rtv_format);
            dev.create_render_target_view(Some(&buf), Some(&rtvdesc), self.rtv_heap.cpu_handle(i));
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

    pub fn resize(
        &mut self,
        dev: &D3D12Device,
        w: u32,
        h: u32,
        format: DXGI_FORMAT,
    ) -> Result<(), Error> {
        // For buffer resize to succeed there should be no outstanding references to back buffers
        self.drop_render_targets();
        // Resize back buffers to new size.
        // Number of back buffers and swapchain flags remain unchanged.
        self.swap_chain
            .resize_buffers(0, w, h, format, 0)
            .into_error_context("Cannot resize back buffers")?;
        self.rtv_format = format;
        self.reaquire_render_targets(dev)
            .context("Cannot reaquire render targets")?;
        Ok(())
    }
}

pub fn create_swap_chain(
    core: &DXCore,
    desc: &DXGI_SWAP_CHAIN_DESC1,
    format: DXGI_FORMAT,
    hwnd: HWND,
    fullscreen_desc: Option<&DXGI_SWAP_CHAIN_FULLSCREEN_DESC>,
    restrict_to_output: Option<&DXGIOutput>,
) -> Result<DXSwapChain, Error> {
    trace!("Create swap chain");
    let swap_chain: DXGISwapChain3 = core.dxgi_factory
        .create_swap_chain_for_hwnd(
            &core.graphics_queue,
            hwnd,
            desc,
            fullscreen_desc,
            restrict_to_output,
        )
        .into_error_context("Cannot create swap chain for hwnd")?
        .query_interface()
        .into_error_context("Cannot get IDXGISwapChain3 interface")?;
    let frame_count = desc.BufferCount;

    trace!("Create DescriptorHeap");
    let rtvheap = DescriptorHeap::new(
        &core.dev,
        frame_count,
        D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
        false,
        0,
    ).into_error_context("Cannot create descriptor heap")?;
    let mut render_targets = vec![];

    for i in 0..frame_count {
        trace!("swap_chain.get_buffer({})", i);
        let buf = swap_chain
            .get_buffer::<D3D12Resource>(i as u32)
            .into_error_with_context(|_| format!("Cannot get swap chain buffer #{}", i))?;
        trace!("Create render target view");
        core.dev.create_render_target_view(
            Some(&buf),
            Some(&render_target_view_desc_tex2d_default(format)),
            rtvheap.cpu_handle(i),
        );
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
