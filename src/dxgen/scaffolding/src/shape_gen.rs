use winapi::{D3D_PRIMITIVE_TOPOLOGY, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST};
use cgmath::*;
use utils::v3;

pub trait GenVertex {
  fn new_vertex(p: &Vector3<f32>) -> Self;
  fn set_uv(self, u: f32, v: f32) -> Self;
  fn set_normal(self, n: &Vector3<f32>) -> Self;
}

pub fn cube<V: GenVertex>(sz: f32) -> (D3D_PRIMITIVE_TOPOLOGY, Vec<V>) {
  let mut ret = Vec::with_capacity(36);
  let facevecs = vec![ // normal, up vectors for faces
    (v3(0.,0.,-1.), v3(0.,1.,0.)),
    (v3(1.,0.,0.), v3(0.,1.,0.)),
    (v3(0.,0.,1.), v3(0.,1.,0.)),
    (v3(-1.,0.,0.), v3(0.,1.,0.)),
    (v3(0.,1.,0.), v3(0.,0.,1.)),
    (v3(0.,-1.,0.), v3(0.,0.,1.)),
  ];
  for (n, up) in facevecs.into_iter() {
    let rt = Vector3::cross(&up, &n);
    let p1 = Vector::mul_s(&(n - up - rt), sz);
    let p2 = Vector::mul_s(&(n + up - rt), sz);
    let p3 = Vector::mul_s(&(n + up + rt), sz);
    let p4 = Vector::mul_s(&(n - up + rt), sz);
    ret.push(V::new_vertex(&p1).set_normal(&n).set_uv(0., 0.));
    ret.push(V::new_vertex(&p2).set_normal(&n).set_uv(0., 1.));
    ret.push(V::new_vertex(&p3).set_normal(&n).set_uv(1., 1.));

    ret.push(V::new_vertex(&p3).set_normal(&n).set_uv(1., 1.));
    ret.push(V::new_vertex(&p4).set_normal(&n).set_uv(1., 0.));
    ret.push(V::new_vertex(&p1).set_normal(&n).set_uv(0., 0.));
  }
  (D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST, ret)
}

pub fn cube_indexed<V: GenVertex>(sz: f32) -> (Vec<V>,Vec<u32>) {
  let mut ret = Vec::with_capacity(24);
  let mut idx = Vec::with_capacity(36);
  let facevecs = vec![ // normal, up vectors for faces
    (v3(0.,0.,-1.), v3(0.,1.,0.)),
    (v3(1.,0.,0.), v3(0.,1.,0.)),
    (v3(0.,0.,1.), v3(0.,1.,0.)),
    (v3(-1.,0.,0.), v3(0.,1.,0.)),
    (v3(0.,1.,0.), v3(0.,0.,1.)),
    (v3(0.,-1.,0.), v3(0.,0.,1.)),
  ];
  for (n, up) in facevecs.into_iter() {
    let rt = Vector3::cross(&up, &n);
    let p1 = Vector::mul_s(&(n - up - rt), sz);
    let p2 = Vector::mul_s(&(n + up - rt), sz);
    let p3 = Vector::mul_s(&(n + up + rt), sz);
    let p4 = Vector::mul_s(&(n - up + rt), sz);
    let base = ret.len() as u32;
    ret.push(V::new_vertex(&p1).set_normal(&n).set_uv(0., 0.));
    ret.push(V::new_vertex(&p2).set_normal(&n).set_uv(0., 1.));
    ret.push(V::new_vertex(&p3).set_normal(&n).set_uv(1., 1.));
    ret.push(V::new_vertex(&p4).set_normal(&n).set_uv(1., 0.));

    idx.extend([base+0, base+1, base+2, base+2, base+3, base+0].into_iter());
  }
  (ret, idx)
}
