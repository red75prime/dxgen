use structwrappers::*;
use window::*;
use rand::Rng;
use rand;
use cgmath;
use crossbeam;
use create_device::*;
use std::cmp::{min, max};

use dxsems::VertexFormat;

dx_vertex!( Vertex {
  (POSITION, 0, DXGI_FORMAT_R32G32_FLOAT) pos: [f32;2],
});
