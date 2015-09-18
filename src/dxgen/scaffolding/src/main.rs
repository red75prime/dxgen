#![feature(result_expect)]

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate libc;
extern crate winapi;
extern crate d3d12_safe;
extern crate dxguid_sys;
extern crate dxgi_sys;
extern crate kernel32;
//extern crate d3d12_sys;

mod macros;
mod create_device;

use winapi::*;
use d3d12_safe::*;
use create_device::*;
use kernel32::{CreateEventW, WaitForSingleObject};
use std::ptr;
use std::fmt;
use std::mem;
use std::cell::RefCell;
use std::rc::Rc;

#[link(name="d3dcompiler")]
extern {}

const FRAME_COUNT : u32 = 2;

fn dump_info_queue(iq: &mut D3D12InfoQueue) {
  let mut iq=iq;
  let mnum=iq.get_num_stored_messages_allowed_by_retrieval_filter();
  println!("Number of debug messages is {}", mnum);
  for i in 0..mnum {
    let mut sz=0;
    let hr=iq.get_message(i,None,&mut sz);
    info!("get_message returned {:?}", hr); // It is the case when hr!=0 and it's ok
    // create buffer to receive message
    let mut arr: Vec<u8> = Vec::with_capacity(sz as usize); 
    let mut sz1=sz; 
    unsafe {
      // get_message expects Option<&mut D3D12_MESSAGE> as second parameter
      // it should be Option<&[u8]>, but I don't have annotation for that yet.
      let hr=iq.get_message(i,Some(mem::transmute(arr.as_ptr())),&mut sz1); 
      assert_eq!(sz,sz1); // just to be sure. hr is Err(1) on success.
      // Reinterpret first chunk of arr as D3D12_MESSAGE, and byte-copy it into msg
      let msg:D3D12_MESSAGE=unsafe {mem::transmute_copy(&(*arr.as_ptr()))};
      // msg contains pointer into memory occupied by arr. arr should be borrowed now, but it is unsafe code.
      let cdescr=::std::ffi::CStr::from_ptr(msg.pDescription as *const i8);
      let descr=String::from_utf8_lossy(cdescr.to_bytes()).to_owned();
      println!("{:}",descr);
    }
  };
  iq.clear_stored_messages();
}

fn main() {
  env_logger::init().unwrap();

  {
    let mut factory: DXGIFactory1 = create_dxgi_factory1().expect("Cannot create DXGIFactory1. No can do.");
    let mut i=0;
    while let Ok(adapter)=factory.enum_adapters(i) {
  //    println!("Adapter {}: {:?}", i, adapter.get_desc());
      i+=1;
    }
  }

  let wnd=create_window("Hello, rusty world!", 512, 256);
  let appdata=
    match create_appdata(wnd) {
      Ok(appdata) => {
        appdata
      },
      Err(mut iq) => {
        dump_info_queue(&mut iq);
        return ();
      },
    };
  let packed_ad=Rc::new(RefCell::new(appdata));
  {
    let rdata=packed_ad.clone();
    let cl=move |w,h,c| {
      let mut data=rdata.borrow_mut();
      drop_render_targets(&mut *data);
      let res=data.swap_chain.resize_buffers(0, w as u32, h as u32, DXGI_FORMAT_UNKNOWN, 0);
      println!("Resize to {},{}. Result:{:?}",w,h,res);
      match res {
        Err(_) => dump_info_queue(&mut data.iq),
        _ => (),
      };
      reaquire_render_targets(&mut *data);
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

    };
    set_resize_fn(wnd,Some(Box::new(cl)));
  }
  {
    let rdata=packed_ad.clone();
    let cl=move||{
      on_render(&mut *(rdata.borrow_mut()));
    };
    set_render_fn(wnd,Some(Box::new(cl)));
  }
  message_loop(wnd);

}

struct AppData {
  viewport : D3D12_VIEWPORT,
  scissor_rect : D3D12_RECT,
  swap_chain : DXGISwapChain3,
  device : D3D12Device,
  render_targets : Vec<D3D12Resource>,
  command_allocator : D3D12CommandAllocator,
  command_queue : D3D12CommandQueue,
  root_signature : D3D12RootSignature,
  rtv_heap : D3D12DescriptorHeap,
  pipeline_state : D3D12PipelineState,
  command_list : D3D12GraphicsCommandList,
  rtv_descriptor_size : UINT,
  vertex_buffer : D3D12Resource,
  vertex_buffer_view : D3D12_VERTEX_BUFFER_VIEW,
  frame_index : UINT,
  fence_event : HANDLE,
  fence : D3D12Fence,
  fence_value : UINT64,
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
  wait_for_prev_frame(data);
  Ok(())
}

fn create_appdata(hwnd: HWnd) -> Result<AppData,D3D12InfoQueue> {
  let w=1024.;
  let h=1024.;
  let hwnd=get_hwnd(hwnd).unwrap();
  info!("HWND");
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

  let mut debug=get_debug_interface().unwrap();
  info!("Debug");
  debug.enable_debug_layer();

  let mut factory: DXGIFactory4 = create_dxgi_factory1().unwrap();
  info!("Factory");
  let mut dev=d3d12_create_device(None, D3D_FEATURE_LEVEL_11_0).unwrap();
  info!("Device");

  let mut info_queue: D3D12InfoQueue = dev.query_interface().unwrap();
  info!("Info queue");

  let mut opts: D3D12_FEATURE_DATA_D3D12_OPTIONS = unsafe{ mem::uninitialized::<_>() };
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
  dev.check_feature_support_feature_levels(&mut feat_levels).unwrap();
  info!("Max supported feature level: {:?}", feat_levels.MaxSupportedFeatureLevel);

  let qd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
    Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
    Priority: 0,
    NodeMask: 0,
  };
  let mut cqueue=dev.create_command_queue(&qd).unwrap();
  info!("Command queue");
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
  let mut swap_chain: DXGISwapChain3 = factory.create_swap_chain(&cqueue, &mut scd).unwrap().query_interface().unwrap();
  info!("Swap chain");
  //println!("{:?}", &scd);
  let frame_index=swap_chain.get_current_back_buffer_index();
  println!("Frame index: {}", &frame_index);
  let rtvhd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: FRAME_COUNT,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let mut rtvheap=dev.create_descriptor_heap(&rtvhd).unwrap(); 
  info!("Descriptor heap");
  let rtvdsize=dev.get_descriptor_handle_increment_size(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
  let mut cdh=rtvheap.get_cpu_descriptor_handle_for_heap_start();
  let mut render_targets=vec![];
  for i in 0..FRAME_COUNT {
    let buf=swap_chain.get_buffer::<D3D12Resource>(i as u32).unwrap();
    dev.create_render_target_view(Some(&buf), None, cdh);
    cdh.ptr += rtvdsize;
    render_targets.push(buf);
  }
  let mut callocator=dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).unwrap();
  info!("Command allocator");
  let rsd=D3D12_ROOT_SIGNATURE_DESC{
    NumParameters: 0,
    pParameters: ptr::null(),
    NumStaticSamplers: 0,
    pStaticSamplers: ptr::null(),
    Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
  };

  let mut blob=d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1).unwrap();

  let mut root_sign=
    unsafe{
      let blob_slice:&[u8]=::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8, blob.get_buffer_size() as usize);
      let mut root_sign=dev.create_root_signature(0, blob_slice).unwrap();
      root_sign
    };
  info!("Root signature");
  let compile_flags=0;
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  info!("Vertex shader");
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();
  info!("Pixel shader");
  let w_pos=str_to_cstring("POSITION");
  let w_color=str_to_cstring("COLOR");
  let input_elts_desc=[
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_pos.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32B32_FLOAT, 
      InputSlot: 0, 
      AlignedByteOffset: 0, 
      InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 
      InstanceDataStepRate: 0,
    },
    D3D12_INPUT_ELEMENT_DESC{
      SemanticName: w_color.as_ptr(), 
      SemanticIndex: 0, 
      Format: DXGI_FORMAT_R32G32B32A32_FLOAT, 
      InputSlot: 0, 
      AlignedByteOffset: 16, 
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

  let pso_desc= D3D12_GRAPHICS_PIPELINE_STATE_DESC {
    pRootSignature: root_sign.iptr() as *mut _,
    VS: D3D12_SHADER_BYTECODE{pShaderBytecode: vshader.as_ptr() as *const _ ,BytecodeLength: vshader.len() as u32, },
    PS: D3D12_SHADER_BYTECODE{pShaderBytecode: pshader.as_ptr() as *const _ ,BytecodeLength: pshader.len() as u32, },
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
  info!("Here be dragons");
  let mut gps=match dev.create_graphics_pipeline_state(&pso_desc) {
      Ok(gps) => gps,
      Err(_) => {return Err(info_queue);},
    };
  info!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  info!("Graphics pipeline state");
  let mut command_list : D3D12GraphicsCommandList = dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, Some(&gps)).unwrap();
  info!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  info!("Command list");
  command_list.close().unwrap();
  info!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  info!("Command list Close");
  
  let vtc=vec![
    Vertex {pos: [0.0, 0.25, 0.0, 0.0],color: [1.0,0.0,0.0,1.0],},
    Vertex {pos: [0.25, -0.25, 0.0, 0.0],color: [0.0,1.0,0.0,1.0],},
    Vertex {pos: [-0.25, -0.25, 0.0, 0.0],color: [0.0,0.0,1.0,1.0],},
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

  info!("dev.iptr: 0x{:x}, lpVtbl: 0x{:x}", dev.iptr() as usize, unsafe{ (*dev.iptr()).lpVtbl as usize});
  info!("Heap properties: {:#?}", &heap_prop);
  
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


  let mut vbuf=try!(dev.create_committed_resource(&heap_prop, D3D12_HEAP_FLAG_NONE, &res_desc, D3D12_RESOURCE_STATE_GENERIC_READ, None).map_err(|_|info_queue.clone()));
  info!("Vertex buffer");
  unsafe {
    let buf_slice=std::slice::from_raw_parts_mut(vbuf.map(0, None).unwrap() as *mut Vertex, vtc.len());
    info!("Map vertex buffer");
    for (place, data) in buf_slice.iter_mut().zip(vtc.iter()) {
      *place = *data
    }
    vbuf.unmap(0, None);
    info!("Unmap vertex buffer");
  }
  
  let vbview = 
    D3D12_VERTEX_BUFFER_VIEW {
      BufferLocation: vbuf.get_gpu_virtual_address(),
      StrideInBytes: mem::size_of::<Vertex>() as u32,
      SizeInBytes: vbuf_size as u32,
  };

  let fence=dev.create_fence(0, D3D12_FENCE_FLAG_NONE).unwrap();
  info!("fence");

  let fence_val=1u64;
  let fence_event = unsafe{ CreateEventW(ptr::null_mut(), 0, 0, ptr::null_mut()) };
  if fence_event == ptr::null_mut() {
    panic!("Cannot create event");
  }
  info!("fence_event");
  
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
      pipeline_state: gps,
      command_list: command_list,
      rtv_descriptor_size: rtvdsize,
      vertex_buffer: vbuf,
      vertex_buffer_view: vbview,
      frame_index: frame_index,
      fence_event: fence_event,
      fence: fence,
      fence_value: fence_val,
      tick: 0.0,
      iq: info_queue,
    };
  wait_for_prev_frame(&mut ret);
  info!("wait_for_prev_frame");
  Ok(ret)
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
  data.command_allocator.reset().unwrap();
  data.command_list.reset(&data.command_allocator, Some(&data.pipeline_state)).unwrap();
  data.command_list.set_graphics_root_signature(&data.root_signature);
  data.command_list.rs_set_viewports(&mut [data.viewport]);
  data.command_list.rs_set_scissor_rects(&mut [data.scissor_rect]);
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
  rtvh.ptr += data.frame_index*data.rtv_descriptor_size;
  data.command_list.om_set_render_targets(1, &rtvh, None);
  
  let mut clear_color = [0.0, 0.2, 0.4, 1.0];
  data.command_list.clear_render_target_view(rtvh, &mut clear_color, &mut []);
  data.command_list.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
  data.command_list.ia_set_vertex_buffers(0, Some(&mut [data.vertex_buffer_view]));
  data.command_list.draw_instanced(3, 1, 0, 0);
  
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

use std::f64;

fn on_render(data: &mut AppData) {
  let tick=data.tick;
  data.tick += 0.01;
  let (s,c)=f64::sin_cos(tick);
  let (s,c)=(s as f32, c as f32);
  let vtc=vec![
    Vertex {pos: [0.25*s+0.0*c, 0.25*(-c)+0.0*s, 0.0, 0.0],color: [1.0,0.0,0.0,1.0],},
    Vertex {pos: [-0.25*s-0.25*c, -0.25*(-c)-0.25*s, 0.0, 0.0],color: [0.0,0.0,1.0,1.0],},
    Vertex {pos: [-0.25*s+0.25*c, -0.25*(-c)+0.25*s, 0.0, 0.0],color: [0.0,1.0,0.0,1.0],},
  ];

  unsafe {
    let buf_slice=std::slice::from_raw_parts_mut(data.vertex_buffer.map(0, None).unwrap() as *mut Vertex, vtc.len());
    for (place, data) in buf_slice.iter_mut().zip(vtc.iter()) {
      *place = *data
    }
    data.vertex_buffer.unmap(0, None);
  }

  populate_command_list(data);
  data.command_queue.execute_command_lists(&[&data.command_list]);
  match data.swap_chain.present(1,0) {
    Err(hr) => {
      dump_info_queue(&mut data.iq);
      panic!("Present failed with 0x{:x}", hr);
    },
    _ => (),
  }
  wait_for_prev_frame(data);
}
