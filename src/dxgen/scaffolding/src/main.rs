#![feature(result_expect)]

#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;
extern crate libc;
extern crate d3d12_sys;
extern crate d3d12;

mod macros;
mod create_device;

use d3d12_sys::*;
use d3d12::*;
use create_device::*;
use libc::HANDLE;
use std::ptr;
use std::fmt;
use std::mem;

#[link(name="d3d12")]
#[link(name="d3dcompiler")]
#[link(name="dxguid")]
#[link(name="dxgi")]
extern {}

const FRAME_COUNT : u32 = 2;

fn main() {
  env_logger::init().unwrap();

  let factory=create_dxgi_factory1().expect("Cannot create DXGIFactory1. No can do.");
  let mut i=0;
  while let Ok(adapter)=factory.enum_adapters(i) {
    println!("Adapter {}: {:?}", i, adapter.get_desc());
    i+=1;
  }

  let wnd=create_window("Hello, world!", 512, 256);
  match create_appdata(wnd) {
    Ok(appdata) => {
    },
    Err(iq) => {
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
    },
  };

  set_resize_fn(wnd,Some(Box::new(|w,h,c|{println!("Resize to {},{}",w,h);})));
  message_loop();

  //let dev=match d3d12_create_device(D3D_FEATURE_LEVEL_11_1) {
  //  Ok(dev) => dev,
  //  Err(hr) => {
  //    println!("Cannot create device. Error {:X}", hr);
  //    return;
  //  },
  //};
  //let mut opts=D3D12_FEATURE_DATA_D3D12_OPTIONS{ ..Default::default() };
  //dev.check_feature_support_options(&mut opts).unwrap();
  //println!("{:#?}",opts);

  //let luid=dev.get_adapter_luid();
  //println!("{:?}", luid);

  //let ca=match dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT) {
  //  Ok(ca) => ca,
  //  Err(hr) => {
  //   println!("Command allocator creation error {:X}",hr);
  //   return;
  //  },
  //};
  //println!("{:?}", ca.iptr());

  //let cqd=D3D12_COMMAND_QUEUE_DESC { Type : D3D12_COMMAND_LIST_TYPE_DIRECT, ..Default::default() };
  //let cq=match dev.create_command_queue(&cqd) {
  //  Ok(cq) => cq,
  //  Err(hr) => {
  //   println!("Command queue creation error {:X}",hr);
  //   return;
  //  },
  //};
  //println!("{:?}", cq.iptr());
 
  //if let Ok(factory)=create_dxgi_factory1() {
  //  if let Ok(adapter0)=factory.enum_adapters1(0) {
  //    println!("{:?}",adapter0.get_desc().unwrap());
  //    if let Ok(output0)=adapter0.enum_outputs(0) {
  //      let desc=output0.get_desc().unwrap();
  //      println!("{:?}",desc);
  //      for fnum in 0..130 {
  //        let format=DXGI_FORMAT(fnum);
  //        match output0.get_display_mode_list(format, 0, None) {
  //          Ok(num_modes) => {
  //            if num_modes!=0 {
  //              println!("Num modes: {} for {:?}", num_modes, format);
  //              let mut modes=vec![DXGI_MODE_DESC::default(); num_modes as usize];
  //              output0.get_display_mode_list(format, 0, Some(&mut modes[..]));
  //              for mode in modes {
  //                println!("{:?}", mode);
  //              }
  //            }
  //          },
  //          Err(hr) => {
  //            println!("Error 0x{:x}", hr);
  //          },
  //        }
  //      }
  //    }
  //  }
  //}
}

struct AppData {
  viewport : D3D12_VIEWPORT,
  scissor_rect : D3D12_RECT,
  swap_chain : DXGISwapChain,
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
}

impl fmt::Debug for AppData {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f,"struct AppData")
  }
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

  let debug=get_debug_interface().unwrap();
  info!("Debug");
  debug.enable_debug_layer();

  let factory=create_dxgi_factory1().unwrap();
  info!("Factory");
  let dev=d3d12_create_device(None, D3D_FEATURE_LEVEL_12_0).unwrap();
  info!("Device");

  let mut opts=D3D12_FEATURE_DATA_D3D12_OPTIONS{ ..Default::default() };
  dev.check_feature_support_options(&mut opts).unwrap();
  info!("{:#?}",opts);

  let fl_array=[D3D_FEATURE_LEVEL_9_1, D3D_FEATURE_LEVEL_9_2, D3D_FEATURE_LEVEL_9_3, 
                D3D_FEATURE_LEVEL_10_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_11_0, 
                D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_12_0,
                D3D_FEATURE_LEVEL_12_1, ];
  let mut feat_levels=D3D12_FEATURE_DATA_FEATURE_LEVELS {
      NumFeatureLevels: fl_array.len() as UINT,
      pFeatureLevelsRequested: fl_array.as_ptr(),
      MaxSupportedFeatureLevel: D3D_FEATURE_LEVEL(0),
    };
  dev.check_feature_support_feature_levels(&mut feat_levels).unwrap();
  info!("Max supported feature level: {:?}", feat_levels.MaxSupportedFeatureLevel);

  let info_queue: D3D12InfoQueue = dev.query_interface().unwrap();
  info!("Info queue");

  let qd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
    Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
    Priority: 0,
    NodeMask: 0,
  };
  let cqueue=dev.create_command_queue(&qd).unwrap();
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
    BufferUsage: 32,//DXGI_USAGE_RENDER_TARGET_OUTPUT,
    SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
    OutputWindow: hwnd,
    Windowed: 1,
    Flags: 0,
  };
  let swap_chain=factory.create_swap_chain(&cqueue, &mut scd).unwrap();
  info!("Swap chain");
  //println!("{:?}", &scd);
  //let frameindex=swap_chain.get_current_back_buffer_index();
  //println!("Frame index: {:?}", &frameindex);
  let rtvhd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: FRAME_COUNT,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let rtvheap=dev.create_descriptor_heap(&rtvhd).unwrap(); 
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
  let callocator=dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT).unwrap();
  info!("Command allocator");
  let rsd=D3D12_ROOT_SIGNATURE_DESC{
    NumParameters: 0,
    pParameters: ptr::null(),
    NumStaticSamplers: 0,
    pStaticSamplers: ptr::null(),
    Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
  };
  let blob=d3d12_serialize_root_signature(&rsd, D3D_ROOT_SIGNATURE_VERSION_1).unwrap();
  let root_sign=
    unsafe{
      let blob_slice:&[u8]=::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8, blob.get_buffer_size() as usize);
      let root_sign=dev.create_root_signature(0, blob_slice).unwrap();
      ::std::mem::forget(blob_slice);
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
      AlignedByteOffset: 12, 
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
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
          D3D12_RENDER_TARGET_BLEND_DESC::default(),
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
  let gps=match dev.create_graphics_pipeline_state(&pso_desc) {
      Ok(gps) => gps,
      Err(_) => {return Err(info_queue);},
    };
  info!("Graphics pipeline state");

  let command_list : D3D12GraphicsCommandList = dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &callocator, Some(&gps)).unwrap();
  info!("Command list");
  command_list.close().unwrap();
  info!("Command list Close");
  Err(info_queue)
}
