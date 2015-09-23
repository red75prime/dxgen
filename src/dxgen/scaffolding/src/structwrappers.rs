use winapi::*;
use d3d12_safe::*;

pub struct ResourceDescBuilder {
  alignment: u64,  
}

pub fn resource_desc_buffer(size_in_bytes: u64) -> D3D12_RESOURCE_DESC {
  D3D12_RESOURCE_DESC {
    Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
    Alignment:  0,
    Width: size_in_bytes,
    Height: 1,
    DepthOrArraySize: 1,
    MipLevels: 1,
    Format: DXGI_FORMAT_UNKNOWN,
    SampleDesc: DXGI_SAMPLE_DESC {Count:1, Quality: 0,},
    Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
    Flags: D3D12_RESOURCE_FLAG_NONE,
  }
}

pub fn heap_properties_upload() -> D3D12_HEAP_PROPERTIES {
  D3D12_HEAP_PROPERTIES {
    Type: D3D12_HEAP_TYPE_UPLOAD,
    CPUPageProperty: D3D12_CPU_PAGE_PROPERTY_UNKNOWN,
    MemoryPoolPreference: D3D12_MEMORY_POOL_UNKNOWN,
    CreationNodeMask: 0,
    VisibleNodeMask: 0,
  }
}

pub fn heap_properties_default() -> D3D12_HEAP_PROPERTIES {
  D3D12_HEAP_PROPERTIES {
    Type: D3D12_HEAP_TYPE_DEFAULT,
    CPUPageProperty: D3D12_CPU_PAGE_PROPERTY_UNKNOWN,
    MemoryPoolPreference: D3D12_MEMORY_POOL_UNKNOWN,
    CreationNodeMask: 0,
    VisibleNodeMask: 0,
  }
}

// TODO: I need some kind of wrapper around structures with pointers to control lifetime 
pub fn resource_barrier_transition(res: &D3D12Resource, state_before: D3D12_RESOURCE_STATES, 
                                    state_after: D3D12_RESOURCE_STATES) -> D3D12_RESOURCE_BARRIER {
  let mut ret=
    D3D12_RESOURCE_BARRIER {
      Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
      Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
      u: unsafe{ ::std::mem::uninitialized() },
    };
  unsafe {
    *ret.Transition_mut() =
      D3D12_RESOURCE_TRANSITION_BARRIER {
         pResource: res.iptr() as *mut _,
         StateBefore: state_before,
         StateAfter: state_after,
         Subresource: D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES, 
       };
  };
  ret
}

pub fn texture_copy_location_footprint(res: &D3D12Resource, footprint: &D3D12_PLACED_SUBRESOURCE_FOOTPRINT) -> D3D12_TEXTURE_COPY_LOCATION {
  unsafe {
    let mut ret = D3D12_TEXTURE_COPY_LOCATION {
      pResource: res.iptr() as *mut _,
      Type: D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT,
      u: ::std::mem::uninitialized(),
    };
    *ret.PlacedFootprint_mut() = footprint.clone();
    ret
  }
}

pub fn texture_copy_location_index(res: &D3D12Resource, subresource_index: u32) -> D3D12_TEXTURE_COPY_LOCATION {
  unsafe {
    let mut ret = D3D12_TEXTURE_COPY_LOCATION {
      pResource: res.iptr() as *mut _,
      Type: D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX,
      u: ::std::mem::uninitialized(),
    };
    *ret.SubresourceIndex_mut() = subresource_index;
    ret
  }
}
