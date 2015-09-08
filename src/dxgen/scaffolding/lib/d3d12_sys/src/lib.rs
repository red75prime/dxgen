extern crate libc;

mod iid;
mod d3d12_sys;

pub use iid::*;
pub use d3d12_sys::*;

#[link(name="d3d12")]
#[link(name="dxguid")]
#[link(name="dxgi")]
extern {}
