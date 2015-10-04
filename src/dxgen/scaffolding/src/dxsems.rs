// While there's no support for const fn's in Rust, let's use what we have

// vertex shader input
pub static BINORMAL: &'static str = "BINORMAL\x00";
pub static BLENDINDICES: &'static str = "BLENDINDICES\x00";
pub static BLENDWEIGHT: &'static str = "BLENDWEIGHT\x00";
pub static COLOR: &'static str = "COLOR\x00";
pub static NORMAL: &'static str = "NORMAL\x00";
pub static POSITION: &'static str = "POSITION\x00";
pub static POSITIONT: &'static str = "POSITIONT\x00";
pub static PSIZE: &'static str = "PSIZE\x00";
pub static TANGENT: &'static str = "TANGENT\x00";
pub static TEXCOORD: &'static str = "TEXCOORD\x00";

use winapi::D3D12_INPUT_ELEMENT_DESC;

pub trait VertexFormat  {
  fn generate(input_slot: u32) -> Vec<D3D12_INPUT_ELEMENT_DESC>;
}
