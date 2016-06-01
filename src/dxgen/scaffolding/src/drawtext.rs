use winapi::*;
use dx_safe::*;

pub struct DrawText {
    d3d11:     D3D11Device,
    d3d11on12: D3D11On12Device,
    device_2d: D2D1Device,
    dwfactory: DWriteFactory,
}

pub struct DrawTextResources {
    
}
