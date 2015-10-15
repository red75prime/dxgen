use winapi::d3d12::*;
use d3d12_safe::*;

struct AppData {
  gps: D3D12PipelineState,
}

pub fn on_init(core: &DXCore, sc: &DXSwapChain) -> HResult<AppData> {
  AppData {
    gps: 
  }
}

pub fn on_render(core: &DXCore, sc: &DXSwapChain, dt: &mut AppData) -> HResult<()> {
}
