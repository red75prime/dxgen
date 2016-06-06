#[macro_use(DEFINE_GUID)] extern crate winapi;  
extern crate dxguid;

mod utils;
mod iid;
pub mod dwrite_safe;
pub mod d3d11_safe;
pub mod d3d12_safe;
pub mod d3d12sdklayers_safe;
pub mod d3dcommon_safe;
pub mod dxgi_safe;
pub mod dxgi1_2_safe;
pub mod dxgi1_3_safe;
pub mod dxgi1_4_safe;
pub mod structwrappers;
//pub mod d3d12shader_safe;
pub mod d2d1_safe;
pub mod d2d1_1_safe;
pub mod d2d1_2_safe;
pub mod d2d1_3_safe;
pub mod d3d11on12_safe;
pub mod helpers;

pub use dwrite_safe::*;
pub use iid::*;
pub use d3d11_safe::*;
pub use d3d12_safe::*;
pub use d3d12sdklayers_safe::*;
pub use d3dcommon_safe::*;
pub use dxgi_safe::*;
pub use dxgi1_2_safe::*;
pub use dxgi1_3_safe::*;
pub use dxgi1_4_safe::*;
//pub use d3d12shader_safe::*;
pub use d2d1_safe::*;
pub use d2d1_1_safe::*;
pub use d2d1_2_safe::*;
pub use d2d1_3_safe::*;
pub use dxguid::*;
pub use d3d11on12_safe::*;

// some tweaks
pub type D3DBlob = D3D10Blob;
