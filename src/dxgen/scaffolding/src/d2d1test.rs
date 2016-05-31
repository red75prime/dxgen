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
  for mmsg in wnd.poll_events() {
    if let Some(msg) = mmsg {
      if msg.message == WM_PAINT {
        rt.begin_draw();
        rt.clear(&D3DCOLORVALUE{r: 0.5, g: 0.9, b: 0.5, a: 1.0});
        rt.end_draw(None, None).unwrap();
      }
    } else {
      ::std::thread::yield_now();
    }
  }
}
