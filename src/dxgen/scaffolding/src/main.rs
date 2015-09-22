#![feature(optin_builtin_traits)]

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate libc;
extern crate winapi;
extern crate d3d12_safe;
//extern crate dxguid_sys;
extern crate dxgi_sys;
extern crate kernel32;
//extern crate d3d12_sys;
extern crate rand;

#[macro_use] mod macros;
mod create_device;
mod window;

use winapi::*;
use d3d12_safe::*;
use create_device::*;
use kernel32::{CreateEventW, WaitForSingleObject};
use std::ptr;
use std::fmt;
use std::mem;
use std::cell::RefCell;
use std::rc::Rc;
use window::*;

#[link(name="d3dcompiler")]
extern {}

const FRAME_COUNT : u32 = 2;

fn shader_4component_mapping(a: u32, b: u32, c: u32, d: u32) -> UINT {
  (0x1000 | (a & 7) | ((b & 7)*8) | ((c & 7)*8*8) | ((d & 7)*8*8*8)) as UINT
}

fn dump_info_queue(iq: &D3D12InfoQueue) {
  let iq=iq;
  let mnum=iq.get_num_stored_messages_allowed_by_retrieval_filter();
  //println!("Number of debug messages is {}", mnum);
  for i in 0..mnum {
    let mut sz = 0;
    let _ = iq.get_message(i, None,&mut sz);
    //info!("get_message returned {:?}", hr); // It is the case when hr!=0 and it's ok
    // create buffer to receive message
    let mut arr: Vec<u8> = Vec::with_capacity(sz as usize); 
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
      warn!("{:}", descr);
    }
  };
  iq.clear_stored_messages();
}

fn main() {
  env_logger::init().unwrap();

  {
    let factory: DXGIFactory4 = create_dxgi_factory1().expect("Cannot create DXGIFactory1. No can do.");
    let mut i=0;
    while let Ok(adapter)=factory.enum_adapters1(i) {
      println!("Adapter {} dedicated video memory: {}MiB", i, adapter.get_desc1().unwrap().DedicatedVideoMemory/1024/1024);
      i+=1;
    }
  }

  let wnd=create_window("D3D12 Hello, rusty world!", 512, 256);
  let data=
    match create_appdata(&wnd) {
      Ok(appdata) => {
        Rc::new(RefCell::new(appdata))
      },
      Err(mut iq) => {
        dump_info_queue(&mut iq);
        return ();
      },
    };
  
  let rdata=data.clone();
  wnd.set_resize_fn(Box::new(move |w,h,c|{
    on_resize(&mut *rdata.borrow_mut(), w, h, c);
  }));

  let mut x=0;
  let mut y=0;
  for mmsg in wnd.poll_events() {
    if let Some(msg) = mmsg {
      //println!("{} ", msg.message);
      match msg.message {
        WM_MOUSEMOVE => {
          x=GET_X_LPARAM(msg.lParam);
          y=GET_Y_LPARAM(msg.lParam);
        },
        _ => {},
      } 
    } else {
      on_render(&mut *data.borrow_mut(), x, y);
      //std::thread::sleep_ms(1);
    }
  }
  // copy info queue
  let iq=data.borrow().iq.clone();
  // release resources
  wnd.set_resize_fn(Box::new(|_,_,_|{}));
  wait_for_prev_frame(&mut *data.borrow_mut());
  drop(data);
  // maybe debug layer has something to say
  dump_info_queue(&iq);  
}

fn on_render(data: &mut AppData, x: i32, y: i32) {
  use std::f64; 

  wait_for_prev_frame(data);
  //let tick=data.tick;
  data.tick += 0.01;
  let aspect=data.viewport.Height/data.viewport.Width;
  
  let tick=f64::atan2(x as f64, y as f64);

  let (s,c)=f64::sin_cos(tick); // f32 is too small for time counting
  let (s,c)=(s as f32, c as f32);
  let vtc=vec![
    Vertex {pos: [(0.5*s+0.0*c)*aspect, (0.5*(-c)+0.0*s), 0.0, 0.0],color: [1.0,0.0,0.0,1.0],  texc0: [1.0, 1.0, 0.0, 0.0],},
    Vertex {pos: [(-0.5*s-0.5*c)*aspect, (-0.5*(-c)-0.5*s), 0.0, 0.0],color: [0.0,0.0,1.0,1.0], texc0: [0.0, 1.0, 0.0, 0.0],},
    Vertex {pos: [(-0.5*s+0.5*c)*aspect, (-0.5*(-c)+0.5*s), 0.0, 0.0],color: [0.0,1.0,0.0,1.0], texc0: [1.0, 0.0, 0.0, 0.0],},
    Vertex {pos: [(-1.0*s+0.0*c)*aspect, (-1.0*(-c)+0.0*s), 0.0, 0.0],color: [0.0,0.5,0.5,1.0], texc0: [0.0, 0.0, 0.0, 0.0],},
  ];

  unsafe {
    let mut p_buf: *mut Vertex = ptr::null_mut();
    data.vertex_buffer.map(0, None, Some(&mut p_buf)).unwrap();
    let buf_slice=std::slice::from_raw_parts_mut(p_buf, vtc.len());
    for (place, data) in buf_slice.iter_mut().zip(vtc.iter()) {
      *place = *data
    }
    data.vertex_buffer.unmap(0, None);
  }

  populate_command_list(data);
  data.command_queue.execute_command_lists(&[&data.command_list]);
  match data.swap_chain.present(0, 0) {
    Err(hr) => {
      dump_info_queue(&data.iq);
      panic!("Present failed with 0x{:x}", hr);
    },
    _ => (),
  }
  dump_info_queue(&data.iq);
}

struct AppData {
  viewport: D3D12_VIEWPORT,
  scissor_rect: D3D12_RECT,
  swap_chain: DXGISwapChain3,
  device: D3D12Device,
  render_targets: Vec<D3D12Resource>,
  command_allocator: D3D12CommandAllocator,
  command_queue: D3D12CommandQueue,
  root_signature: D3D12RootSignature,
  rtv_heap: D3D12DescriptorHeap,
  srv_heap: D3D12DescriptorHeap,
  sam_heap: D3D12DescriptorHeap,
  pipeline_state: D3D12PipelineState,
  command_list: D3D12GraphicsCommandList,
  rtv_descriptor_size: SIZE_T,
  vertex_buffer: D3D12Resource,
  vertex_buffer_view: D3D12_VERTEX_BUFFER_VIEW,
  tex_resource: D3D12Resource,
  tex_view: D3D12_SHADER_RESOURCE_VIEW_DESC,
  frame_index: UINT,
  fence_event: HANDLE,
  fence: D3D12Fence,
  fence_value: UINT64,
  tick: f64,
  iq: D3D12InfoQueue,
}

impl fmt::Debug for AppData {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f,"struct AppData")
  }
}

#[derive(Clone,Copy,Debug)]
struct Vertex {
  pos: [f32;4],
  color: [f32;4],
  texc0: [f32;4],
}

fn wait_for_prev_frame(data: &mut AppData) {
  let fence_value=data.fence_value;
  data.command_queue.signal(&mut data.fence, fence_value).unwrap();
  data.fence_value += 1;
  if data.fence.get_completed_value() < fence_value {
    match data.fence.set_event_on_completion(fence_value, data.fence_event) {
      Ok(_) => (),
      Err(hr) => {
        dump_info_queue(&mut data.iq);
        panic!("set_event_on_completion error: 0x{:x}",hr);
      },
    }
    unsafe { WaitForSingleObject(data.fence_event, INFINITE) };
  }
  data.frame_index = data.swap_chain.get_current_back_buffer_index();
}

fn populate_command_list(data: &mut AppData) {
  let d_info=false;
  if d_info {debug!("Command allocator reset")};
  data.command_allocator.reset().unwrap();
  if d_info {debug!("Command list reset")};
  data.command_list.reset(&data.command_allocator, Some(&data.pipeline_state)).unwrap();

  if d_info {debug!("Set graphics root signature")};
  data.command_list.set_graphics_root_signature(&data.root_signature);

  if d_info {debug!("Set descriptor heaps")};
  data.command_list.set_descriptor_heaps(&[&data.srv_heap, &data.sam_heap]);
  
  let gpu_dh=data.srv_heap.get_gpu_descriptor_handle_for_heap_start();
  if d_info {debug!("Set graphics root descriptor table 0")};
  data.command_list.set_graphics_root_descriptor_table(0, gpu_dh);
  if d_info {debug!("Set graphics root descriptor table 1")};
  data.command_list.set_graphics_root_descriptor_table(1, data.sam_heap.get_gpu_descriptor_handle_for_heap_start());
  if d_info {debug!("Set viepowrts")};
  data.command_list.rs_set_viewports(&mut [data.viewport]);
  if d_info {debug!("Set scissor rects")};
  data.command_list.rs_set_scissor_rects(&mut [data.scissor_rect]);

//  data.command_list.set_graphics_root_shader_resource_view(0, data.tex_resource.get_gpu_virtual_address());

  let rb=
    D3D12_RESOURCE_BARRIER {
      Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
      Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
      u: D3D12_RESOURCE_TRANSITION_BARRIER {
        pResource: data.render_targets[data.frame_index as usize].iptr() as *mut _,
        StateBefore: D3D12_RESOURCE_STATE_PRESENT,
        StateAfter: D3D12_RESOURCE_STATE_RENDER_TARGET,
        Subresource: D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES, 
      },
    };
  data.command_list.resource_barrier(&mut [rb]);
  let mut rtvh=data.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  rtvh.ptr += (data.frame_index as SIZE_T)*data.rtv_descriptor_size;
  data.command_list.om_set_render_targets(1, &rtvh, None);

  let mut clear_color = [0.01, 0.01, 0.05, 1.0];
  data.command_list.clear_render_target_view(rtvh, &mut clear_color, &mut []);
  data.command_list.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
  data.command_list.ia_set_vertex_buffers(0, Some(&mut [data.vertex_buffer_view]));
  data.command_list.draw_instanced(4, 1, 0, 0);
  
  let rb1=
    D3D12_RESOURCE_BARRIER {
      Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
      Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
      u: D3D12_RESOURCE_TRANSITION_BARRIER {
        pResource: data.render_targets[data.frame_index as usize].iptr() as *mut _,
        StateBefore: D3D12_RESOURCE_STATE_RENDER_TARGET,
        StateAfter: D3D12_RESOURCE_STATE_PRESENT,
        Subresource: D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES, 
      },
    };
  data.command_list.resource_barrier(&mut [rb1]);

  data.command_list.close().unwrap();
}

fn on_resize(data: &mut AppData, w: u32, h: u32, c: u32) {
   wait_for_prev_frame(data);
   drop_render_targets(data);
   let res=data.swap_chain.resize_buffers(0, w, h, DXGI_FORMAT_UNKNOWN, 0);
   println!("Resize to {},{}. Result:{:?}",w,h,res);
   match res {
     Err(hr) => {
       dump_info_queue(&data.iq);
       error!("resize_buffers returned 0x{:x}", hr);
     },
     _ => (),
   };
   reaquire_render_targets(data).unwrap();
   data.viewport=D3D12_VIEWPORT {
     TopLeftX: 0.,
     TopLeftY: 0.,
     MinDepth: 0.,
     Width: w as f32,
     Height: h as f32,
     MaxDepth: 1.0,
   };
   data.scissor_rect=D3D12_RECT {
     right: w as i32,
     bottom: h as i32,
     left: 0,
     top: 0,
   };
  on_render(data, 1, 1);
}

fn drop_render_targets(data: &mut AppData) {
  data.render_targets.truncate(0);
}

fn reaquire_render_targets(data: &mut AppData) -> HResult<()> {
  wait_for_prev_frame(data);
  let mut cdh=data.rtv_heap.get_cpu_descriptor_handle_for_heap_start();
  data.render_targets.truncate(0);
  for i in 0..FRAME_COUNT {
    let buf=try!(data.swap_chain.get_buffer::<D3D12Resource>(i as u32));
    data.device.create_render_target_view(Some(&buf), None, cdh);
    cdh.ptr += data.rtv_descriptor_size;
    data.render_targets.push(buf);
  }
  Ok(())
}

fn create_appdata(wnd: &Window) -> Result<AppData,D3D12InfoQueue> {
  let w=1024.;
  let h=1024.;
  let hwnd=wnd.get_hwnd();
  debug!("HWND");
  let viewport=D3D12_VIEWPORT {
    TopLeftX: 0.,
    TopLeftY: 0.,
    MinDepth: 0.,
    Width: w,
    Height: h,
    MaxDepth: 1.0,
  };
  let sci_rect=D3D12_RECT {
    right: w as i32,
    bottom: h as i32,
    left: 0,
    top: 0,
  };

  debug!("Debug");
  let debug=get_debug_interface().unwrap();
  debug.enable_debug_layer();

  debug!("Factory");
  let factory: DXGIFactory4 = create_dxgi_factory1().unwrap();
  debug!("Device");
  let dev = 
    match d3d12_create_device(None, D3D_FEATURE_LEVEL_11_0) {
      Ok(dev) => dev,
      Err(_) => {
        warn!("Fallback to warp adapter");
        debug!("Warp");
        let warp: DXGIAdapter = factory.enum_warp_adapter().unwrap();
        d3d12_create_device(Some(&warp), D3D_FEATURE_LEVEL_11_0).expect("Cannot create warp device")
      },
    };
  debug!("LUID");
  let luid=dev.get_adapter_luid();
  println!("Adapter LUID {:?}", luid);

  debug!("Info queue");
  let info_queue: D3D12InfoQueue = dev.query_interface().unwrap();

  let mut opts: D3D12_FEATURE_DATA_D3D12_OPTIONS = unsafe{ mem::uninitialized::<_>() };
  debug!("Check feature support: options");
  dev.check_feature_support_options(&mut opts).unwrap();
  info!("{:#?}",opts);

  let fl_array=[D3D_FEATURE_LEVEL_9_1, D3D_FEATURE_LEVEL_9_2, D3D_FEATURE_LEVEL_9_3, 
                D3D_FEATURE_LEVEL_10_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_11_0, 
                D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_12_0,
                D3D_FEATURE_LEVEL_12_1, ];
  let mut feat_levels=D3D12_FEATURE_DATA_FEATURE_LEVELS {
      NumFeatureLevels: fl_array.len() as UINT,
      pFeatureLevelsRequested: fl_array.as_ptr(),
      MaxSupportedFeatureLevel: D3D_FEATURE_LEVEL_9_1,
    };
  debug!("Check feature support: levels");
  dev.check_feature_support_feature_levels(&mut feat_levels).unwrap();
  info!("Max supported feature level: {:?}", feat_levels.MaxSupportedFeatureLevel);

  let qd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
    Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
    Priority: 0,
    NodeMask: 0,
  };
  debug!("Command queue");
  let cqueue=dev.create_command_queue(&qd).unwrap();
  let mut scd=DXGI_SWAP_CHAIN_DESC{
    BufferCount: FRAME_COUNT,
    BufferDesc: DXGI_MODE_DESC{
        Width: w as UINT,
        Height: h as UINT,
        RefreshRate: DXGI_RATIONAL {Numerator:0, Denominator: 0},
        Format: DXGI_FORMAT_R8G8B8A8_UNORM,
        ScanlineOrdering: DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED,
        Scaling: DXGI_MODE_SCALING_UNSPECIFIED,
      },
    SampleDesc: DXGI_SAMPLE_DESC {Count:1, Quality: 0,},
    BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
    SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
    OutputWindow: hwnd,
    Windowed: 1,
    Flags: 0,
  };
  debug!("Swap chain");
  let swap_chain: DXGISwapChain3 = 
    match factory.create_swap_chain(&cqueue, &mut scd) {
      Err(hr) => {
        println!("create_swap_chain failed with 0x{:x}",hr);
        return Err(info_queue.clone());
      },
      Ok(i) => 
        i.query_interface().unwrap(),
    };
//  info!("Maximum frame latency: {}", swap_chain.get_maximum_frame_latency().unwrap());
//  debug!("Swap chain: get_maximum_frame_latency");
  //println!("{:?}", &scd);
  let frame_index=swap_chain.get_current_back_buffer_index();
  println!("Frame index: {}", &frame_index);
  let rtvhd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: FRAME_COUNT,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  debug!("RTV descriptor heap");
  let rtvheap=dev.create_descriptor_heap(&rtvhd).unwrap(); 
  let rtvdsize=dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_RTV) as SIZE_T;
  let mut cdh=rtvheap.get_cpu_descriptor_handle_for_heap_start();
  let mut render_targets=vec![];
  for i in 0..FRAME_COUNT {
    let buf=swap_chain.get_buffer::<D3D12Resource>(i as u32).unwrap();
    dev.create_render_target_view(Some(&buf), None, cdh);
    cdh.ptr += rtvdsize;
    render_targets.push(buf);
  }

  let srv_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("SRV descriptor heap");
  let srv_heap=dev.create_descriptor_heap(&srv_hd).unwrap();

  let sam_hd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: 1,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
    NodeMask: 0,
  };
  debug!("Sampler descriptor heap");
  let sam_heap=dev.create_descriptor_heap(&sam_hd).unwrap();

  let sampler_desc = D3D12_SAMPLER_DESC {
    Filter: D3D12_FILTER_MIN_MAG_MIP_LINEAR,
    AddressU: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
    AddressV: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
    AddressW: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
    MinLOD: 0.0,
    MaxLOD: D3D12_FLOAT32_MAX,
    MipLODBias: 0.0,
    MaxAnisotropy: 16,
    ComparisonFunc: D3D12_COMPARISON_FUNC_ALWAYS,
    BorderColor: [1.0,1.0,1.0,1.0],
  };
  debug!("Sampler");
  dev.create_sampler(&sampler_desc, sam_heap.get_cpu_descriptor_handle_for_heap_start());


  debug!("Command allocator");
  let callocator=dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).unwrap();

  let static_samplers = vec![
    D3D12_STATIC_SAMPLER_DESC {
      Filter: D3D12_FILTER_ANISOTROPIC,
      AddressU: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
      AddressV: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
      AddressW: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
      MipLODBias: 0.0,
      MaxAnisotropy: 16,
      ComparisonFunc: D3D12_COMPARISON_FUNC_LESS_EQUAL,
      BorderColor: D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE,
      MinLOD: 0.0,
      MaxLOD: D3D12_FLOAT32_MAX,
      ShaderRegister: 0,
      RegisterSpace: 0,
      ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
    },
  ];

  let desc_ranges = vec![
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND,
    },
    D3D12_DESCRIPTOR_RANGE {
      RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER,
      NumDescriptors: 1,
      BaseShaderRegister: 0,
      RegisterSpace: 0,
      OffsetInDescriptorsFromTableStart: D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND,
    },
  ];

  let mut rs_parms = vec![
    D3D12_ROOT_PARAMETER {
      ParameterType: D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
      u: D3D12_ROOT_DESCRIPTOR_TABLE {
        NumDescriptorRanges: 1,
        pDescriptorRanges: desc_ranges[0..0].as_ptr(),
      },
      ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
    },    
    D3D12_ROOT_PARAMETER {
      ParameterType: D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
      u: D3D12_ROOT_DESCRIPTOR_TABLE {
        NumDescriptorRanges: 1,
        pDescriptorRanges: desc_ranges[1..1].as_ptr(),
      },
      ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
    },    
  ];
//  unsafe{
//    *rs_parms[0].DescriptorTable_mut() = D3D12_ROOT_DESCRIPTOR_TABLE {
//      NumDescriptorRanges: 1,
//      pDescriptorRanges: desc_ranges[0..0].as_ptr(),
//    };
//    *rs_parms[1].DescriptorTable_mut() = D3D12_ROOT_DESCRIPTOR_TABLE {
//      NumDescriptorRanges: 1,
//      pDescriptorRanges: desc_ranges[1..1].as_ptr(),
//    };
//  };
  debug!("Size of D3D12_ROOT_PARAMETER:{}", ::std::mem::size_of_val(&rs_parms[0]));

  let rsd=D3D12_ROOT_SIGNATURE_DESC{
    NumParameters: rs_parms.len() as UINT,
    pParameters: rs_parms[..].as_ptr(),
    NumStaticSamplers: 0,
    pStaticSamplers: ptr::null(),
    Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
  };

  debug!("Serialize root signature");
  let blob=try!(d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1)
        .map_err(|hr|{
          error!("HRESULT=0x{:x}",hr);
          info_queue.clone()}));

  debug!("Root signature");
  let root_sign=
    unsafe{
      let blob_slice:&[u8]=::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8, blob.get_buffer_size() as usize);
      let root_sign=dev.create_root_signature(0, blob_slice).unwrap();
      root_sign
    };
  let compile_flags=0;
  debug!("Vertex shader");
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  debug!("Pixel shader");
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();

  let w_pos=str_to_cstring("POSITION");
  let w_color=str_to_cstring("COLOR");
  let w_texc0=str_to_cstring("TEXCOORD");
  let input_elts_desc=[
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_pos.as_ptr(), 
      SemanticIndex: 0,
      Format: DXGI_FORMAT_R32G32B32_FLOAT, 
      InputSlot: 0, 
      AlignedByteOffset: offset_of!(Vertex, pos) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_color.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32B32A32_FLOAT, 
      InputSlot: 0, 
      AlignedByteOffset: offset_of!(Vertex, color) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_texc0.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32B32A32_FLOAT, 
      InputSlot: 0,
      AlignedByteOffset: offset_of!(Vertex, texc0) as UINT, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
  ];
  let def_stencil_op_desc=D3D12_DEPTH_STENCILOP_DESC{
      StencilFunc: D3D12_COMPARISON_FUNC_ALWAYS,
      StencilDepthFailOp: D3D12_STENCIL_OP_KEEP,
      StencilPassOp: D3D12_STENCIL_OP_KEEP,
      StencilFailOp: D3D12_STENCIL_OP_KEEP,
    };
  let blend_desc_def=
    D3D12_RENDER_TARGET_BLEND_DESC{
      BlendEnable: 0,
      LogicOpEnable: 0,
      SrcBlend: D3D12_BLEND_ONE,
      DestBlend: D3D12_BLEND_ZERO,
      BlendOp: D3D12_BLEND_OP_ADD,
      SrcBlendAlpha: D3D12_BLEND_ONE,
      DestBlendAlpha: D3D12_BLEND_ZERO,
      BlendOpAlpha: D3D12_BLEND_OP_ADD,
      LogicOp: D3D12_LOGIC_OP_NOOP,
      RenderTargetWriteMask: 15,
    };

  let pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
    pRootSignature: root_sign.iptr() as *mut _,
    VS: D3D12_SHADER_BYTECODE{pShaderBytecode: vshader.as_ptr() as *const _ ,BytecodeLength: vshader.len() as SIZE_T, },
    PS: D3D12_SHADER_BYTECODE{pShaderBytecode: pshader.as_ptr() as *const _ ,BytecodeLength: pshader.len() as SIZE_T, },
    DS: D3D12_SHADER_BYTECODE{pShaderBytecode: ptr::null_mut() ,BytecodeLength: 0, },
    HS: D3D12_SHADER_BYTECODE{pShaderBytecode: ptr::null_mut() ,BytecodeLength: 0, },
    GS: D3D12_SHADER_BYTECODE{pShaderBytecode: ptr::null_mut() ,BytecodeLength: 0, },
    StreamOutput: D3D12_STREAM_OUTPUT_DESC {pSODeclaration: ptr::null(), NumEntries: 0, pBufferStrides: ptr::null(), NumStrides: 0, RasterizedStream: 0,},
    BlendState: D3D12_BLEND_DESC {AlphaToCoverageEnable: 0, IndependentBlendEnable: 0, 
        RenderTarget: [
          D3D12_RENDER_TARGET_BLEND_DESC{
            BlendEnable: 0,
            LogicOpEnable: 0,
            SrcBlend: D3D12_BLEND_ONE,
            DestBlend: D3D12_BLEND_ZERO,
            BlendOp: D3D12_BLEND_OP_ADD,
            SrcBlendAlpha: D3D12_BLEND_ONE,
            DestBlendAlpha: D3D12_BLEND_ZERO,
            BlendOpAlpha: D3D12_BLEND_OP_ADD,
            LogicOp: D3D12_LOGIC_OP_NOOP,
            RenderTargetWriteMask: 15,
          },
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
          blend_desc_def,
         ],},
    SampleMask: 0xffffffff,
    RasterizerState : D3D12_RASTERIZER_DESC{
      FillMode: D3D12_FILL_MODE_SOLID,
      CullMode: D3D12_CULL_MODE_BACK,
      FrontCounterClockwise: 0,
      DepthBias: 0,
      SlopeScaledDepthBias: 0.0,
      DepthBiasClamp: 0.0,
      DepthClipEnable: 1,
      MultisampleEnable: 0,
      AntialiasedLineEnable: 0,
      ForcedSampleCount: 0,
      ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF,
    },
    DepthStencilState : D3D12_DEPTH_STENCIL_DESC{
      DepthEnable: 0,
      DepthWriteMask: D3D12_DEPTH_WRITE_MASK_ALL,
      DepthFunc: D3D12_COMPARISON_FUNC_LESS,
      StencilEnable: 0,
      StencilReadMask: 0xff,
      StencilWriteMask: 0xff,
      FrontFace: def_stencil_op_desc,
      BackFace: def_stencil_op_desc,
      },
    InputLayout : D3D12_INPUT_LAYOUT_DESC{
      pInputElementDescs: input_elts_desc.as_ptr(),
      NumElements: input_elts_desc.len() as u32,
      },
    IBStripCutValue : D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED,
    PrimitiveTopologyType : D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
    NumRenderTargets : 1,
    RTVFormats : [DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, DXGI_FORMAT_UNKNOWN, ],
    DSVFormat : DXGI_FORMAT_UNKNOWN, 
    SampleDesc : DXGI_SAMPLE_DESC{Count:1, Quality: 0,},
    NodeMask : 0,
    CachedPSO : D3D12_CACHED_PIPELINE_STATE{pCachedBlob: ptr::null(), CachedBlobSizeInBytes: 0,},
    Flags : D3D12_PIPELINE_STATE_FLAG_NONE,         
  };
  debug!("Graphics pipeline state");
  let gps=match dev.create_graphics_pipeline_state(&pso_desc) {
      Ok(gps) => gps,
      Err(_) => {return Err(info_queue);},
    };
  debug!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  debug!("Command list");
  let command_list : D3D12GraphicsCommandList = dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, Some(&gps)).unwrap();
  debug!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  debug!("Command list Close");
  command_list.close().unwrap();
  debug!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  
  let vtc=vec![
    Vertex {pos: [0.0, 0.5, 0.0, 0.0], color: [1.0,0.0,0.0,1.0], texc0: [1.0, 1.0, 0.0, 0.0],},
    Vertex {pos: [0.5, -0.5, 0.0, 0.0], color: [0.0,1.0,0.0,1.0], texc0: [1.0, 0.0, 0.0, 0.0],},
    Vertex {pos: [-0.5, -0.5, 0.0, 0.0], color: [0.0,0.0,1.0,1.0], texc0: [0.0, 0.0, 0.0, 0.0],},
    Vertex {pos: [0.0, -1.5, 0.0, 0.0], color: [0.0,0.0,1.0,1.0], texc0: [0.0, 1.0, 0.0, 0.0],},
  ];

  let vbuf_size = mem::size_of_val(&vtc[..]) as UINT64;

  let heap_prop=
    D3D12_HEAP_PROPERTIES {
      Type: D3D12_HEAP_TYPE_UPLOAD,
      CPUPageProperty: D3D12_CPU_PAGE_PROPERTY_UNKNOWN,
      MemoryPoolPreference: D3D12_MEMORY_POOL_UNKNOWN,
      CreationNodeMask: 0,
      VisibleNodeMask: 0,
  };
//  let heap_prop=dev.get_custom_heap_properties(0, D3D12_HEAP_TYPE_UPLOAD);

//  debug!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
//  debug!("Heap properties: {:#?}", &heap_prop);
  
  let res_desc =
    D3D12_RESOURCE_DESC {
      Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
      Alignment:  0,
      Width: vbuf_size,
      Height: 1,
      DepthOrArraySize: 1,
      MipLevels: 1,
      Format: DXGI_FORMAT_UNKNOWN,
      SampleDesc: DXGI_SAMPLE_DESC {Count:1, Quality: 0,},
      Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
      Flags: D3D12_RESOURCE_FLAG_NONE,
  };
  info!("Resource desc: {:#?}", &res_desc);


  debug!("Vertex buffer");
  let vbuf=try!(dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &res_desc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|info_queue.clone()));
  unsafe {
    debug!("Map vertex buffer");
    let mut p_buf: *mut Vertex = ptr::null_mut();
    vbuf.map(0, None, Some(&mut p_buf)).unwrap();
    let buf_slice=std::slice::from_raw_parts_mut(p_buf, vtc.len());
    for (place, data) in buf_slice.iter_mut().zip(vtc.iter()) {
      *place = *data
    }
    debug!("Unmap vertex buffer");
    vbuf.unmap(0, None);
  }
  
  let vbview = 
    D3D12_VERTEX_BUFFER_VIEW {
      BufferLocation: vbuf.get_gpu_virtual_address(),
      StrideInBytes: mem::size_of::<Vertex>() as u32,
      SizeInBytes: vbuf_size as u32,
  };

  let tex_w=256usize;
  let tex_h=256usize;

  let tex_desc =
    D3D12_RESOURCE_DESC {
      Dimension: D3D12_RESOURCE_DIMENSION_TEXTURE2D,
      Alignment:  0,
      Width: tex_w as u64,
      Height: tex_h as u32,
      DepthOrArraySize: 1,
      MipLevels: 1,
      Format: DXGI_FORMAT_R8G8B8A8_UNORM,
      SampleDesc: DXGI_SAMPLE_DESC {Count:1, Quality: 0,},
      Layout: D3D12_TEXTURE_LAYOUT_UNKNOWN,
      Flags: D3D12_RESOURCE_FLAG_NONE,
  };
  info!("Texture desc: {:#?}", &res_desc);

  debug!("Texture resource");
  let tex_resource=try!(dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &tex_desc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|info_queue.clone()));
  unsafe {
    debug!("Map texture");
    if let Err(hr)=tex_resource.map::<()>(0, None, None) {
      error!("Error 0x{}",hr);
      return Err(info_queue.clone());
    }
    let mut texdata: Vec<u32> = vec![0xff00ffff; tex_w*tex_h as usize];
//    let mut c = 0u32;
    for v in &mut texdata[..] {
      *v=rand::random();
//      c += 2;
    }
    debug!("Write to subresource");
    tex_resource.write_to_subresource(0, None, &texdata[..], (4*tex_w) as UINT, (4*tex_w*tex_h) as UINT).unwrap(); // Yes, "magic" constant 
    
    debug!("Unmap texture");
    tex_resource.unmap(0, None);
  }

  let mut tex_view = D3D12_SHADER_RESOURCE_VIEW_DESC {
    Format: DXGI_FORMAT_R8G8B8A8_UNORM,
    ViewDimension: D3D12_SRV_DIMENSION_TEXTURE2D,
    Shader4ComponentMapping: shader_4component_mapping(0,1,2,3),
    u: unsafe{ mem::uninitialized() },
  };
  unsafe {
    *tex_view.Texture2D_mut() = D3D12_TEX2D_SRV {
      MostDetailedMip: 0,
      MipLevels: 1,
      PlaneSlice: 0,
      ResourceMinLODClamp: 0.0,
    };
  };

  let mut srv_desc = D3D12_SHADER_RESOURCE_VIEW_DESC {
    Format: DXGI_FORMAT_R8G8B8A8_UNORM,
    ViewDimension: D3D12_SRV_DIMENSION_TEXTURE2D,
    Shader4ComponentMapping: shader_4component_mapping(0, 1, 2, 3),
    u: unsafe{ ::std::mem::uninitialized() },
  };
  unsafe {
    *srv_desc.Texture2D_mut() = D3D12_TEX2D_SRV {
      MostDetailedMip: 0,
      MipLevels: 1,
      PlaneSlice: 0,
      ResourceMinLODClamp: 0.0,
    };
  };

  //let srv_dsize=dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV) as SIZE_T;
  let srv_dh=srv_heap.get_cpu_descriptor_handle_for_heap_start();
  debug!("Create shader resource view");
  dev.create_shader_resource_view(Some(&tex_resource), Some(&srv_desc), srv_dh);

  debug!("fence");
  let fence=dev.create_fence(0, D3D12_FENCE_FLAG_NONE).unwrap();

  let fence_val=1u64;
  let fence_event = unsafe{ CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  debug!("fence_event");
  if fence_event == ptr::null_mut() {
    panic!("Cannot create event");
  }
  
  let mut ret=
    AppData {
      viewport: viewport,
      scissor_rect: sci_rect,
      swap_chain: swap_chain,
      device: dev,
      render_targets: render_targets,
      command_allocator: callocator,
      command_queue: cqueue,
      root_signature: root_sign,
      rtv_heap: rtvheap,
      srv_heap: srv_heap,
      sam_heap: sam_heap,
      pipeline_state: gps,
      command_list: command_list,
      rtv_descriptor_size: rtvdsize,
      vertex_buffer: vbuf,
      vertex_buffer_view: vbview,
      tex_resource: tex_resource,
      tex_view: tex_view,
      frame_index: frame_index,
      fence_event: fence_event,
      fence: fence,
      fence_value: fence_val,
      tick: 0.0,
      iq: info_queue,
    };
  debug!("wait_for_prev_frame");
  wait_for_prev_frame(&mut ret);
  Ok(ret)
}

