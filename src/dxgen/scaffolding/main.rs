#[macro_use]
extern crate lazy_static;

extern crate libc;

mod macros;
mod iid;
mod d3d12_sys;
mod d3d12;
mod create_device;

use d3d12_sys::*;
use d3d12::*;
use create_device::*;
use iid::HasIID;
use libc::HANDLE;
use std::ptr;


#[link(name="d3d12")]
#[link(name="d3dcompiler")]
#[link(name="dxguid")]
#[link(name="dxgi")]
extern {}

const FRAME_COUNT : u32 = 2;

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

fn create_appdata(hwnd: HWnd) -> HResult<AppData> {
  let w=1024.;
  let h=1024.;
  let hwnd=get_hwnd(hwnd).unwrap();
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
  let factory=create_dxgi_factory1().unwrap();
  let dev=d3d12_create_device(D3D_FEATURE_LEVEL_11_1).unwrap();
  let qd=D3D12_COMMAND_QUEUE_DESC{
    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
    Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
    Priority: 0,
    NodeMask: 0,
  };
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
    BufferUsage: 32,//DXGI_USAGE_RENDER_TARGET_OUTPUT,
    SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
    OutputWindow: hwnd,
    Windowed: 1,
    Flags: 0,
  };
  let swap_chain=factory.create_swap_chain(&cqueue, &mut scd).unwrap();
  println!("{:?}", &scd);
  //let frameindex=swap_chain.get_current_back_buffer_index();
  //println!("Frame index: {:?}", &frameindex);
  let rtvhd=D3D12_DESCRIPTOR_HEAP_DESC{
    NumDescriptors: FRAME_COUNT,
    Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
    Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
    NodeMask: 0,
  };
  let rtvheap=dev.create_descriptor_heap(&rtvhd).unwrap(); 
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
  let compile_flags=0;
  let vshader=d3d_compile_from_file("shaders.hlsl","VSMain","vs_5_0", compile_flags).unwrap();
  let pshader=d3d_compile_from_file("shaders.hlsl","PSMain","ps_5_0", compile_flags).unwrap();
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
  let pso_desc= D3D12_GRAPHICS_PIPELINE_STATE_DESC {
    pRootSignature: root_sign.0,
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

  };
  Err(1)
}

fn main() {
  let dev=match d3d12_create_device(D3D_FEATURE_LEVEL_11_1) {
    Ok(dev) => dev,
    Err(hr) => {
      println!("Cannot create device. Error {:X}", hr);
      return;
    },
  };
  let mut opts=D3D12_FEATURE_DATA_D3D12_OPTIONS{ ..Default::default() };
  dev.check_feature_support_options(&mut opts).unwrap();
  println!("{:#?}",opts);

  let luid=dev.get_adapter_luid();
  println!("{:?}", luid);

  let ca=match dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT) {
    Ok(ca) => ca,
    Err(hr) => {
     println!("Command allocator creation error {:X}",hr);
     return;
    },
  };
  println!("{:?}", ca.iptr());

  let cqd=D3D12_COMMAND_QUEUE_DESC { Type : D3D12_COMMAND_LIST_TYPE_DIRECT, ..Default::default() };
  let cq=match dev.create_command_queue(&cqd) {
    Ok(cq) => cq,
    Err(hr) => {
     println!("Command queue creation error {:X}",hr);
     return;
    },
  };
  println!("{:?}", cq.iptr());
 
  let wnd=create_window("Hello, world!", 512, 256);
  let mut appdata=create_appdata(wnd);

  if let Ok(factory)=create_dxgi_factory1() {
    if let Ok(adapter0)=factory.enum_adapters1(0) {
      println!("{:?}",adapter0.get_desc().unwrap());
      if let Ok(output0)=adapter0.enum_outputs(0) {
        let desc=output0.get_desc().unwrap();
        println!("{:?}",desc);
        for fnum in 0..130 {
          let format=DXGI_FORMAT(fnum);
          match output0.get_display_mode_list(format, 0, None) {
            Ok(num_modes) => {
              if num_modes!=0 {
                println!("Num modes: {} for {:?}", num_modes, format);
                let mut modes=vec![DXGI_MODE_DESC::default(); num_modes as usize];
                output0.get_display_mode_list(format, 0, Some(&mut modes[..]));
                for mode in modes {
                  println!("{:?}", mode);
                }
              }
            },
            Err(hr) => {
              println!("Error 0x{:x}", hr);
            },
          }
        }
      }
    }
  }
  set_resize_fn(wnd,Some(Box::new(|w,h,c|{println!("Resize to {},{}",w,h);})));
  message_loop();
}

