use winapi::*;
use dx_safe::*;
use window::{self, Window};
use create_device;

pub fn main() {
  let wnd = window::create_window("d2d1 test", 200, 120);
  let factory = create_device::create_d2d1_factory_single_threaded().expect("Cannot create D2D1Factory");
  let rtprop = rt_properties_default();
  let (ww, wh) = wnd.size();
  let rthwndprop = hwnd_rt_properties_default(wnd.get_hwnd(), ww, wh); 
  let rt = factory.create_hwnd_render_target(&rtprop, &rthwndprop).expect("Cannot create D2D1HWNDRenderTarget");
  let bbrush = rt.create_solid_color_brush(&color3(0., 0., 0.), None).unwrap();
  let dgbrush = rt.create_solid_color_brush(&color3(0., 0.2, 0.), None).unwrap();
  let dwfactory = create_device::create_dwrite_factory_shared().expect("Cannot create DWriteFactory");
  let text_format = dwfactory.create_text_format("Lucida Sans Unicode".into(), None, DWRITE_FONT_WEIGHT_NORMAL,
                                DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL, 16., "en-GB".into()).expect("Cannot create DWriteTextFormat");
  let hello_vec = ::utils::str_to_vec_u16("DWrite and D2D1\nBarebone functionality\n你好，世界");                                
  for msg in wnd.events() { // blocking window event iterator
    match msg.message {
      WM_PAINT => {
        rt.begin_draw();
        rt.clear(&D3DCOLORVALUE{r: 0.5, g: 0.9, b: 0.5, a: 1.0});
        rt.fill_rectangle(&rectf(5., 5., 30., 40.), &dgbrush);
        rt.draw_text(&hello_vec[..], &text_format, &rectf(35., 5., 300., 100.), &bbrush, D2D1_DRAW_TEXT_OPTIONS_NONE, DWRITE_MEASURING_MODE_NATURAL);
        rt.end_draw(None, None).unwrap();
        validate_window(&wnd);
      },
      WM_SIZE => {
          // Normally this message goes to wndproc, in window.rs I repost it into message queue to prevent reentrancy problems
          let w = LOWORD(msg.lParam as u32) as u32;
          let h = HIWORD(msg.lParam as u32) as u32;
          rt.resize(&D2D1_SIZE_U{width: w, height: h}).unwrap();
          unsafe{ ::user32::InvalidateRect(wnd.get_hwnd(), ::std::ptr::null(), 0) };
      },
      _ => {
        // do nothing
      },
    }
  }
}

fn validate_window(wnd: &Window) {
  unsafe{ ::user32::ValidateRect(wnd.get_hwnd(), ::std::ptr::null()) };  
}

fn rectf(left: f32, top: f32, right: f32, bottom: f32) -> D2D1_RECT_F {
  D2D1_RECT_F {
    left: left,
    right: right,
    top: top,
    bottom: bottom,
  }
}

fn color3(r: f32, g: f32, b: f32) -> D3DCOLORVALUE {
  D3DCOLORVALUE { r: r, g: g, b: b, a: 1.0 }
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

