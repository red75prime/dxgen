module custom_impls

let id2d1geometrygroup_getsourcegeometries = """
#[allow(non_snake_case)]
pub fn get_source_geometries(&self) -> Vec<D2D1Geometry> {
  let cnt = self.get_source_geometry_count() as usize;
  let mut lv1: Vec<*mut ID2D1Geometry> = vec![ptr::null_mut(); cnt];
  let _hr = unsafe { (*(self.0 as *mut ID2D1GeometryGroup)).GetSourceGeometries(lv1[..].as_mut_ptr(), cnt as u32) };
  lv1.iter().map(|&ptr|D2D1Geometry::new(ptr as *mut _)).collect()
}  
"""
