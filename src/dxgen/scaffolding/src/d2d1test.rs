use winapi::*;
use dx_safe::*;
use dx_safe::structwrappers::*;
use window::{self, Window};
use create_device as create;
use utils;
use utils::d2d::*;
use std::ptr;
use std::mem;
use core;
use core::DumpOnError;

dx_vertex!( Vertex {
  (POSITION, 0, DXGI_FORMAT_R32G32B32_FLOAT) pos: [f32;3],
  (COLOR   , 0, DXGI_FORMAT_R32G32B32A32_FLOAT) color: [f32;4],
});


pub fn main() -> HResult<()> {
  let w = 520;
  let h = 300;
  let wnd = window::create_window("d2d1 test", w, h);
  
  trace!("enable_debug_layer");
  try!(create::get_debug_interface()).enable_debug_layer();
  trace!("dxgi_factory");
  let dxgi_factory = try!(create::create_dxgi_factory2::<DXGIFactory4>(true));
  trace!("adapter");
  let adapter = try!(get_hardware_adapter(&dxgi_factory));
  trace!("d3d12dev");
  let d3d12dev = try!(create::d3d12_create_device(Some(&adapter), D3D_FEATURE_LEVEL_11_0));
  trace!("infoqueue");
  let iq = try!(d3d12dev.query_interface::<D3D12InfoQueue>());
  
  let cqd = D3D12_COMMAND_QUEUE_DESC {
      Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
      Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
      Priority: 0,
      NodeMask: 0,
  };
  trace!("cqueue");
  let cqueue = try!(d3d12dev.create_command_queue(&cqd));
  
  let frame_count = 2;
  let sc_desc = DXGI_SWAP_CHAIN_DESC1 {
      Width: w as u32,
      Height: h as u32,
      Format: DXGI_FORMAT_R8G8B8A8_UNORM,
      Stereo: 0,
      SampleDesc: sample_desc_default(),
      BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
      BufferCount: frame_count,
      Scaling: DXGI_SCALING_NONE,
      SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
      AlphaMode: DXGI_ALPHA_MODE_UNSPECIFIED,
      Flags: 0, // DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH.0,
  };
  trace!("swap_chain");
  let swap_chain = try!(dxgi_factory.create_swap_chain_for_hwnd(&cqueue, wnd.get_hwnd(), &sc_desc, None, None));
  
  try!(dxgi_factory.make_window_association(wnd.get_hwnd(), 0));
  
  let swap_chain = try!(swap_chain.query_interface::<DXGISwapChain3>());
  let flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT | D3D11_CREATE_DEVICE_DEBUG;
  let (d3d11dev, d3d11devctx) = try!(create::d3d11on12_create_device(&d3d12dev, flags, &cqueue));
  let d3d11on12dev = try!(d3d11dev.query_interface::<D3D11On12Device>());
  
  let d2dfactory = try!(create::create_d2d1_factory_single_threaded::<D2D1Factory3>());
  let dxgidev = try!(d3d11on12dev.query_interface::<DXGIDevice>());
  let d2ddev = try!(d2dfactory.create_device3(&dxgidev));
  let d2ddevctx = try!(d2ddev.create_device_context2(D2D1_DEVICE_CONTEXT_OPTIONS_NONE));
  let dwfactory = try!(create::create_dwrite_factory_shared());
  
  let (dpi_x, dpi_y) = d2dfactory.get_desktop_dpi();
  let bmp_props = D2D1_BITMAP_PROPERTIES1 {
      pixelFormat: D2D1_PIXEL_FORMAT {
        format: DXGI_FORMAT_UNKNOWN,
        alphaMode: D2D1_ALPHA_MODE_PREMULTIPLIED,
      },
      dpiX: dpi_x,
      dpiY: dpi_y,
      bitmapOptions: D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
      colorContext: ptr::null_mut(), 
  };
  let dheap = try!(DescriptorHeap::new(&d3d12dev, frame_count, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, false, 0));

  let mut frame_resources = vec![];
  
  let d3d11_res_flags = D3D11_RESOURCE_FLAGS {
      BindFlags: D3D11_BIND_RENDER_TARGET.0,
      MiscFlags: 0,
      CPUAccessFlags: 0,
      StructureByteStride: 0,
  };
  
  for i in 0..frame_count {
    let rt = try!(swap_chain.get_buffer(i));
    d3d12dev.create_render_target_view(Some(&rt), None, dheap.cpu_handle(i));
    
    let wrt = try!(d3d11on12dev.create_wrapped_resource::<_, D3D11Resource>(&rt, &d3d11_res_flags, D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT));
    let surface = try!(wrt.query_interface::<DXGISurface>());
    let d2drt = try!(d2ddevctx.create_bitmap_from_dxgi_surface(&surface, &bmp_props));
    let calloc = try!(d3d12dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
    
    frame_resources.push((rt,wrt,d2drt,calloc));
  }

  let root_sign_desc = root_signature_desc_empty(D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT); 
  let signature = try!(create::d3d12_serialize_root_signature(&root_sign_desc, D3D_ROOT_SIGNATURE_VERSION_1));
  let root_sign = try!(d3d12dev.create_root_signature(0, create::blob_as_slice(&signature)));
  
  let mut vs_bytecode = vec![];
  let mut ps_bytecode = vec![];
  trace!("parse shaders");
  match create::compile_shaders("d2d1test.hlsl", &mut[("VSMain", "vs_5_0", &mut vs_bytecode), ("PSMain", "ps_5_0", &mut ps_bytecode)], 0) {
    Ok(_) => {},
    Err(err) => {
      error!("{}", err);
      return Err(E_FAIL);
    }
  };
  
  let input_elts_desc = Vertex::generate(0);
  
  let mut pso_desc = graphics_pipeline_state_desc_default(); 
  pso_desc.pRootSignature = root_sign.iptr() as *mut _;
  pso_desc.DepthStencilState.DepthEnable = 0;
  pso_desc.DepthStencilState.StencilEnable = 0;
  pso_desc.NumRenderTargets = 1;
  pso_desc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
  pso_desc.VS = ShaderBytecode::from_vec(&vs_bytecode).get(); 
  pso_desc.PS = ShaderBytecode::from_vec(&ps_bytecode).get(); 
  pso_desc.InputLayout = 
        D3D12_INPUT_LAYOUT_DESC {
            pInputElementDescs: input_elts_desc.as_ptr(),
            NumElements: input_elts_desc.len() as u32,
        };

  trace!("pso");
  let pso = try!(d3d12dev.create_graphics_pipeline_state(&pso_desc).dump_iq(&iq));
  
  trace!("clist");
  let clist: D3D12GraphicsCommandList = try!(d3d12dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &frame_resources[0].3, Some(&pso)));

  let text_brush = try!(d2ddevctx.create_solid_color_brush(&color3(0., 0., 0.), None));
  let text_format = try!(dwfactory.create_text_format(
      "Verdana".into(), None, DWRITE_FONT_WEIGHT_NORMAL, 
      DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL, 
      50., "en-us".into()));

  try!(text_format.set_text_alignment(DWRITE_TEXT_ALIGNMENT_CENTER));
  try!(text_format.set_paragraph_alignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER));
  
  let mut aspect = (w as f32)/(h as f32);

  let triangle_vertices = vec![
    Vertex{pos: [0., 0.25*aspect, 0.], color: [1., 0., 0., 0.]},
    Vertex{pos: [0.25, -0.25*aspect, 0.], color: [0., 1., 0., 0.]},
    Vertex{pos: [-0.25, -0.25*aspect, 0.], color: [0., 0., 1., 0.]},
  ];

  let vb_size = mem::size_of_val(&triangle_vertices[..]) as u64;
  trace!("size of buffer: {}", vb_size);
  
  trace!("vbuf_up");
  let vbuf_up = try!(d3d12dev.create_committed_resource(
                          &heap_properties_upload(), 
                          D3D12_HEAP_FLAG_NONE, 
                          &resource_desc_buffer(vb_size),
                          D3D12_RESOURCE_STATE_GENERIC_READ,
                          None));
  trace!("upload");
  try!(utils::upload_into_buffer(&vbuf_up, &triangle_vertices[..]));

  let vb_view = D3D12_VERTEX_BUFFER_VIEW {
    BufferLocation: vbuf_up.get_gpu_virtual_address(),
    StrideInBytes: mem::size_of::<Vertex>() as u32,
    SizeInBytes: vb_size as u32,
  };

  try!(clist.close());
  
  let event = core::create_event();
  
  let mut fence_val = 1;
  
  let fence = try!(d3d12dev.create_fence(fence_val, D3D12_FENCE_FLAG_NONE));

  let frame_index = swap_chain.get_current_back_buffer_index();
  
  let viewport = D3D12_VIEWPORT {
      TopLeftX: 0.,
      TopLeftY: 0.,
      MinDepth: 0.,
      Width: w as f32,
      Height: h as f32,
      MaxDepth: 1.0,
  };
  let sci_rect = D3D12_RECT {
      right: w as i32,
      bottom: h as i32,
      left: 0,
      top: 0,
  };

  let hello_vec = ::utils::str_to_vec_u16("DWrite and D2D1\nBarebone functionality\n你好，世界");                                
  for msg in wnd.events() { // blocking window event iterator
    match msg.message {
      WM_PAINT => {
        let fi = swap_chain.get_current_back_buffer_index();
        let (ref mut rt, ref mut wrt, ref mut d2drt, ref mut calloc) = frame_resources[fi as usize]; 
        render(calloc, &clist, &pso, &root_sign, &viewport, &sci_rect, rt, dheap.cpu_handle(fi), 
                &vb_view, &cqueue, d2drt, &hello_vec[..], &d3d11on12dev, wrt, &d2ddevctx, 
                &text_format, &text_brush, &d3d11devctx);
        fence_val += 1;
        try!(utils::wait_for_queue(&cqueue, fence_val, &fence, &event));
        try!(swap_chain.present(1,0));
        validate_window(&wnd);
      },
      WM_SIZE => {
          // Normally this message goes to wndproc, in window.rs I repost it into message queue to prevent reentrancy problems
          unsafe{ ::user32::InvalidateRect(wnd.get_hwnd(), ::std::ptr::null(), 0) };
      },
      _ => {
        // do nothing
      },
    }
  };
  Ok(())
}

// Nothing to see here. Please, move along 
fn render(calloc: &D3D12CommandAllocator, clist: &D3D12GraphicsCommandList,
          pso: &D3D12PipelineState, root_sign: &D3D12RootSignature,
          viewport: &D3D12_VIEWPORT, sci_rect: &D3D12_RECT,
          rt: &D3D12Resource, cpu_rt_handle: D3D12_CPU_DESCRIPTOR_HANDLE,
          vb_view: &D3D12_VERTEX_BUFFER_VIEW, cqueue: &D3D12CommandQueue,
          d2drt: &D2D1Bitmap1, text: &[u16], d3d11on12dev: &D3D11On12Device,
          wrt: &D3D11Resource, d2ddevctx: &D2D1DeviceContext2,
          text_format: &DWriteTextFormat, text_brush: &D2D1SolidColorBrush,
          d3d11devctx: &D3D11DeviceContext,
          ) -> HResult<()> {
  try!(calloc.reset());
  try!(clist.reset(calloc, Some(pso)));
  clist.set_graphics_root_signature(root_sign);
  clist.rs_set_viewports(&[*viewport]);
  clist.rs_set_scissor_rects(&[*sci_rect]);
  
  clist.resource_barrier(&[*ResourceBarrier::transition(rt, D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET)]);
  clist.om_set_render_targets_arr(&[cpu_rt_handle], None);
  clist.clear_render_target_view(cpu_rt_handle, &[0., 0.2, 0.4, 1.], &[]);
  clist.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
  clist.ia_set_vertex_buffers(0, Some(&[*vb_view]));
  clist.draw_instanced(3, 1, 0, 0);
  
  try!(clist.close());
  
  cqueue.execute_command_lists(&[clist]);
  
  let rtsize = d2drt.get_size();
  
  let text_rect = rectf(0., 0., rtsize.width, rtsize.height);
  
  d3d11on12dev.acquire_wrapped_resources(&[wrt]);
  
  d2ddevctx.set_target(d2drt);
  d2ddevctx.begin_draw();
  d2ddevctx.draw_text(text, text_format, &text_rect, text_brush, D2D1_DRAW_TEXT_OPTIONS_NONE, DWRITE_MEASURING_MODE_NATURAL);
  try!(d2ddevctx.end_draw(None, None));
  
  d3d11on12dev.release_wrapped_resources(&[wrt]);
  
  d3d11devctx.flush();
  
  Ok(())
}

fn validate_window(wnd: &Window) {
  unsafe{ ::user32::ValidateRect(wnd.get_hwnd(), ::std::ptr::null()) };  
}

fn rt_properties_default() -> D2D1_RENDER_TARGET_PROPERTIES {
  D2D1_RENDER_TARGET_PROPERTIES {
    _type: D2D1_RENDER_TARGET_TYPE_DEFAULT,
    pixelFormat: D2D1_PIXEL_FORMAT {
      format: DXGI_FORMAT_R8G8B8A8_UNORM,
      alphaMode: D2D1_ALPHA_MODE_IGNORE,
    },
    dpiX: 0.0,
    dpiY: 0.0,
    usage: D2D1_RENDER_TARGET_USAGE_NONE,
    minLevel: D2D1_FEATURE_LEVEL_DEFAULT,
  }
}

fn hwnd_rt_properties_default(hwnd: HWND, w: u32, h: u32) -> D2D1_HWND_RENDER_TARGET_PROPERTIES {
  D2D1_HWND_RENDER_TARGET_PROPERTIES {
    hwnd: hwnd,
    pixelSize: D2D1_SIZE_U {
      width: w,
      height: h,
    },
    presentOptions: D2D1_PRESENT_OPTIONS_NONE,
  }
}

fn get_hardware_adapter(dxgi_factory: &DXGIFactory4) -> HResult<DXGIAdapter1> {
  for adapter in dxgi_factory {
    let adapter1 = try!(adapter.query_interface::<DXGIAdapter1>());
    let desc = try!(adapter1.get_desc1());
    trace!("Testing {}", utils::wchar_array_to_string_lossy(&desc.Description));
    if desc.Flags & DXGI_ADAPTER_FLAG_SOFTWARE.0 != 0 {
      // skip this software one
      trace!("  software device")
    } else {
      match create::d3d12_test_create_device(Some(&adapter1), D3D_FEATURE_LEVEL_11_0) {
        Ok(()) => {
          trace!("  success!");
          return Ok(adapter1);
        },
        Err(err) => trace!("  incapable of d3d12: 0x{:x}", err),
      }
    }
  }
  error!("No D3D12 capable hardware adapters found");
  Err(E_FAIL)
}
