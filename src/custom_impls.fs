module custom_impls

let id2d1geometrygroup_getsourcegeometries = """
#[allow(non_snake_case)]
fn get_source_geometries(&self) -> Vec<D2D1Geometry> {
  let cnt = self.get_source_geometry_count() as usize;
  let mut lv1: Vec<*mut ID2D1Geometry> = vec![ptr::null_mut(); cnt];
  let _hr = unsafe { (*(self.iptr() as *mut ID2D1GeometryGroup)).GetSourceGeometries(lv1[..].as_mut_ptr(), cnt as u32) };
  lv1.iter().map(|&ptr|D2D1Geometry::new(ptr as *mut _)).collect()
}  
"""

let id3d12checkfeaturesupport = """
#[allow(non_snake_case)]
fn check_feature_support_options(&self) -> HResult<D3D12_FEATURE_DATA_D3D12_OPTIONS> {
    let mut lv1: D3D12_FEATURE_DATA_D3D12_OPTIONS = unsafe{ mem::uninitialized() };
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_D3D12_OPTIONS, &mut lv1 as *mut _ as *mut c_void, mem::size_of_val(&lv1) as UINT) };
    hr2ret(_hr,lv1)
}

#[allow(non_snake_case)]
fn check_feature_support_arch(&self, node_index: UINT) -> HResult<D3D12_FEATURE_DATA_ARCHITECTURE> {
    let mut lv1: D3D12_FEATURE_DATA_ARCHITECTURE = unsafe{ mem::uninitialized() };
    lv1.NodeIndex = node_index;
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_ARCHITECTURE, &mut lv1 as *mut _ as *mut _, mem::size_of_val(&lv1) as UINT) };
    hr2ret(_hr,lv1)
}

#[allow(non_snake_case)]
fn check_feature_support_feature_levels(&self, levels: &[D3D_FEATURE_LEVEL]) -> HResult<D3D_FEATURE_LEVEL> {
    let mut lv1 = D3D12_FEATURE_DATA_FEATURE_LEVELS {
        NumFeatureLevels: levels.len() as UINT,
        pFeatureLevelsRequested: levels.as_ptr(),
        MaxSupportedFeatureLevel: unsafe{ mem::uninitialized() }, // indicate out member
    };
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_FEATURE_LEVELS, &mut lv1 as *mut _ as *mut _, mem::size_of_val(&lv1) as UINT) };
    hr2ret(_hr,lv1.MaxSupportedFeatureLevel)
}
  
#[allow(non_snake_case)]
fn check_feature_support_format_support(&self, format: DXGI_FORMAT) -> HResult<D3D12_FEATURE_DATA_FORMAT_SUPPORT> {
    let mut lv1 = D3D12_FEATURE_DATA_FORMAT_SUPPORT {
        Format: format,
        Support1: unsafe{ mem::uninitialized() }, // indicate out member
        Support2: unsafe{ mem::uninitialized() }, // indicate out member
    };
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_FORMAT_SUPPORT, &mut lv1 as *mut _ as *mut _, mem::size_of_val(&lv1) as UINT) };
    hr2ret(_hr,lv1)
}
  
#[allow(non_snake_case)]
fn check_feature_support_multisample_quality_levels(&self, feature_support_data: &mut D3D12_FEATURE_DATA_MULTISAMPLE_QUALITY_LEVELS) -> HResult<HRESULT> {
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_MULTISAMPLE_QUALITY_LEVELS, feature_support_data as *mut _ as *mut _, mem::size_of_val(feature_support_data) as UINT) };
    hr2ret(_hr,_hr)
}
  
#[allow(non_snake_case)]
fn check_feature_support_format_info(&self, feature_support_data: &mut D3D12_FEATURE_DATA_FORMAT_INFO) -> HResult<HRESULT> {
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_FORMAT_INFO, feature_support_data as *mut _ as *mut _, mem::size_of_val(feature_support_data) as UINT) };
    hr2ret(_hr,_hr)
}
  
#[allow(non_snake_case)]
fn check_feature_support_virtual_address(&self) -> HResult<D3D12_FEATURE_DATA_GPU_VIRTUAL_ADDRESS_SUPPORT> {
    let mut lv1: D3D12_FEATURE_DATA_GPU_VIRTUAL_ADDRESS_SUPPORT = unsafe{ mem::uninitialized() };
    let _hr=unsafe { (*(self.iptr() as *mut ID3D12Device)).CheckFeatureSupport(D3D12_FEATURE_GPU_VIRTUAL_ADDRESS_SUPPORT, &mut lv1 as *mut _ as *mut c_void, mem::size_of_val(&lv1) as UINT) };
    hr2ret(_hr,lv1)
}
"""
