use winapi::*;
use d3d12_safe::*;
use std::marker::PhantomData;
use std::ops::Deref;

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

pub enum ResourceBarrier<'a,'b> {
  Transition { data: D3D12_RESOURCE_BARRIER, phantom: PhantomData<&'a D3D12Resource>, },
  Aliasing { data: D3D12_RESOURCE_BARRIER, phantom1: PhantomData<&'a D3D12Resource>, phantom2: PhantomData<&'b D3D12Resource>, },
  Uav { data: D3D12_RESOURCE_BARRIER, phantom: PhantomData<&'a D3D12Resource>, },
}

impl<'e, 'f> ResourceBarrier<'e,'f> {
  pub fn transition<'a>(
    resource: &'a D3D12Resource,
    before: D3D12_RESOURCE_STATES, after: D3D12_RESOURCE_STATES
  ) -> ResourceBarrier<'a,'a> {
    let mut ret=
      D3D12_RESOURCE_BARRIER {
        Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
        Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
        u: unsafe{ ::std::mem::uninitialized() },
      };
    unsafe {
      *ret.Transition_mut() =
        D3D12_RESOURCE_TRANSITION_BARRIER {
           pResource: resource.iptr() as *mut _,
           StateBefore: before,
           StateAfter: after,
           Subresource: D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES, 
         };
    };
    ResourceBarrier::Transition {
      data: ret,
      phantom: PhantomData::<&'a _>,
    }
  }

  pub fn transition_subresource<'a>(
    resource: &'a D3D12Resource, subresource: UINT,
    before: D3D12_RESOURCE_STATES, after: D3D12_RESOURCE_STATES
  ) -> ResourceBarrier<'a,'a> {
    let mut ret=
      D3D12_RESOURCE_BARRIER {
        Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
        Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
        u: unsafe{ ::std::mem::uninitialized() },
      };
    unsafe {
      *ret.Transition_mut() =
        D3D12_RESOURCE_TRANSITION_BARRIER {
           pResource: resource.iptr() as *mut _,
           StateBefore: before,
           StateAfter: after,
           Subresource: subresource, 
         };
    };
    ResourceBarrier::Transition {
      data: ret,
      phantom: PhantomData::<&'a _>,
    }
  }

  pub fn aliasing<'a,'b>(
    before: Option<&'a D3D12Resource>, after: &'b D3D12Resource
  ) -> ResourceBarrier<'a,'b> {
    let mut ret=
      D3D12_RESOURCE_BARRIER {
        Type: D3D12_RESOURCE_BARRIER_TYPE_ALIASING,
        Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
        u: unsafe{ ::std::mem::uninitialized() },
      };
    unsafe {
      *ret.Aliasing_mut() =
        D3D12_RESOURCE_ALIASING_BARRIER {
           pResourceBefore: before.map(|r|r.iptr()).unwrap_or(::std::ptr::null_mut()) as *mut _,
           pResourceAfter: after.iptr() as *mut _,
         };
    };
    ResourceBarrier::Aliasing {
      data: ret,
      phantom1: PhantomData::<&'a _>,
      phantom2: PhantomData::<&'b _>,
    }
  }

  pub fn uav<'a>(
    resource: &'a D3D12Resource
  ) -> ResourceBarrier<'a,'a> {
    let mut ret=
      D3D12_RESOURCE_BARRIER {
        Type: D3D12_RESOURCE_BARRIER_TYPE_UAV,
        Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
        u: unsafe{ ::std::mem::uninitialized() },
      };
    unsafe {
      *ret.UAV_mut() =
        D3D12_RESOURCE_UAV_BARRIER {
           pResource: resource.iptr() as *mut _,
         };
    };
    ResourceBarrier::Uav {
      data: ret,
      phantom: PhantomData::<&'a _>,
    }
  }

  pub fn flags(self, flags: D3D12_RESOURCE_BARRIER_FLAGS) -> ResourceBarrier<'e,'f> {
    match self {
      ResourceBarrier::Transition {mut data, phantom: p} => { 
        data.Flags = flags;
        ResourceBarrier::Transition {data: data, phantom: p}
      },
      ResourceBarrier::Aliasing {mut data, phantom1: p1, phantom2: p2} => { 
          data.Flags = flags;
          ResourceBarrier::Aliasing {data: data, phantom1: p1, phantom2: p2 }
      },
      ResourceBarrier::Uav {mut data, phantom: p} => { 
        data.Flags = flags;
        ResourceBarrier::Uav {data: data, phantom: p}
      },
    }
  }
}

impl<'e, 'f> Deref for ResourceBarrier<'e, 'f> {
  type Target = D3D12_RESOURCE_BARRIER;

  fn deref<'a>(&'a self) -> &'a Self::Target {
    match *self {
      ResourceBarrier::Transition {ref data, ..} => { data },
      ResourceBarrier::Aliasing {ref data, ..} => { data },
      ResourceBarrier::Uav {ref data, ..} => { data },
    }
  }  
}

pub enum RootParameter<'a> {
  DescriptorTable { data: D3D12_ROOT_PARAMETER, ranges: PhantomData<&'a [D3D12_DESCRIPTOR_RANGE]>, },
  Other { data: D3D12_ROOT_PARAMETER, },
}

impl<'e> Deref for RootParameter<'e> {
  type Target = D3D12_ROOT_PARAMETER;

  fn deref<'a>(&'a self) -> &'a Self::Target {
    match *self {
      RootParameter::DescriptorTable {ref data, ..} => { data },
      RootParameter::Other {ref data, ..} => { data },
    }
  }
}

impl<'a> RootParameter<'a> {
  pub fn descriptor_table<'b>(ranges: &'b [D3D12_DESCRIPTOR_RANGE], visibility: D3D12_SHADER_VISIBILITY) -> RootParameter<'b> {
    let mut ret = D3D12_ROOT_PARAMETER {
      ParameterType: D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
      u: unsafe { ::std::mem::uninitialized() },
      ShaderVisibility: visibility,
    };
    unsafe{
      *ret.DescriptorTable_mut() = D3D12_ROOT_DESCRIPTOR_TABLE {
        NumDescriptorRanges: ranges.len() as UINT,
        pDescriptorRanges: ranges.as_ptr(),
      };
    };
    RootParameter::DescriptorTable {
      data: ret,
      ranges: PhantomData::<&'b _>,
    }
  }

  pub fn constants<'b>(shader_register: u32, register_space: u32, num_32bit_values: u32, visibility: D3D12_SHADER_VISIBILITY) -> RootParameter<'b> {
    let mut ret = D3D12_ROOT_PARAMETER {
      ParameterType: D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS,
      u: unsafe { ::std::mem::uninitialized() },
      ShaderVisibility: visibility,
    };
    unsafe{
      *ret.Constants_mut() = D3D12_ROOT_CONSTANTS {
        ShaderRegister: shader_register,
        RegisterSpace: register_space,
        Num32BitValues: num_32bit_values,
      };
    };
    RootParameter::Other {
      data: ret,
    }
  }
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
