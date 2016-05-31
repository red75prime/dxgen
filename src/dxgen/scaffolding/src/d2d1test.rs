use winapi::*;
use dx_safe::*;
use window;
use create_device;

pub fn main() {
  let wnd = window::create_window("d2d1 test", 200, 120);
  let factory = create_device::create_d2d1_factory_single_threaded().expect("Cannot create D2D1Factory");
  let rtprop = D2D1_RENDER_TARGET_PROPERTIES {
    _type: D2D1_RENDER_TARGET_TYPE_DEFAULT,
    pixelFormat: D2D1_PIXEL_FORMAT {
      format: DXGI_FORMAT_R8G8B8A8_UNORM,
      alphaMode: D2D1_ALPHA_MODE_IGNORE,
    },
    dpiX: 0.0,
    dpiY: 0.0,
    usage: D2D1_RENDER_TARGET_USAGE_NONE,
    minLevel: D2D1_FEATURE_LEVEL_DEFAULT,
  };
  let rthwndprop = D2D1_HWND_RENDER_TARGET_PROPERTIES {
    hwnd: wnd.get_hwnd(),
    pixelSize: D2D1_SIZE_U {
      width: 200,
      height: 120,
    },
    presentOptions: D2D1_PRESENT_OPTIONS_NONE,
  };
  let rt = factory.create_hwnd_render_target(&rtprop, &rthwndprop).expect("Cannot create D2D1HWNDRenderTarget");
  let bbrush = rt.create_solid_color_brush(&D3DCOLORVALUE{r: 0., g: 0., b: 0., a: 1.0}, None).unwrap();
  let dwfactory = create_device::create_dwrite_factory_shared();
  for msg in wnd.events() { // blocking window event iterator
    match msg.message {
      WM_PAINT => {
        rt.begin_draw();
        rt.clear(&D3DCOLORVALUE{r: 0.5, g: 0.9, b: 0.5, a: 1.0});
        rt.fill_rectangle(&D2D1_RECT_F{left: 5., top: 5., right: 30., bottom: 40.}, &bbrush);
        rt.end_draw(None, None).unwrap();
        unsafe{ ::user32::ValidateRect(wnd.get_hwnd(), ::std::ptr::null()) };
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
