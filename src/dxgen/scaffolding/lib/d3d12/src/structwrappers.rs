use winapi::*;
use iid::{HResult, HasIID, TUnknown};
use d3d12_safe::*;
use std::marker::PhantomData;
use std::ops::Deref;
use std::mem;
use std::ptr;

pub fn shader_4component_mapping(a: u32, b: u32, c: u32, d: u32) -> UINT {
    (0x1000 | (a & 7) | ((b & 7) * 8) | ((c & 7) * 8 * 8) | ((d & 7) * 8 * 8 * 8)) as UINT
}

// I should move that into dx_safe
#[derive(Clone,Copy)]
pub struct Luid(pub LUID);

impl PartialEq<Luid> for Luid {
    fn eq(&self, other: &Luid) -> bool {
        self.0.LowPart == other.0.LowPart && self.0.HighPart == other.0.HighPart
    }
}

pub fn root_signature_desc_empty(flags: D3D12_ROOT_SIGNATURE_FLAGS) -> D3D12_ROOT_SIGNATURE_DESC {
    D3D12_ROOT_SIGNATURE_DESC {
        NumParameters: 0,
        pParameters: ptr::null(),
        NumStaticSamplers: 0,
        pStaticSamplers: ptr::null(),
        Flags: flags,
    }
}

pub fn resource_desc_buffer(size_in_bytes: u64) -> D3D12_RESOURCE_DESC {
    D3D12_RESOURCE_DESC {
        Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
        Alignment: 0,
        Width: size_in_bytes,
        Height: 1,
        DepthOrArraySize: 1,
        MipLevels: 1,
        Format: DXGI_FORMAT_UNKNOWN,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
        Flags: D3D12_RESOURCE_FLAG_NONE,
    }
}

pub fn resource_desc_const_buffer(size_in_bytes: u64) -> D3D12_RESOURCE_DESC {
    D3D12_RESOURCE_DESC {
        Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
        Alignment: 65536,
        Width: size_in_bytes,
        Height: 1,
        DepthOrArraySize: 1,
        MipLevels: 1,
        Format: DXGI_FORMAT_UNKNOWN,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
        Flags: D3D12_RESOURCE_FLAG_NONE,
    }
}

pub fn resource_desc_buffer_uav(size_in_bytes: u64) -> D3D12_RESOURCE_DESC {
    D3D12_RESOURCE_DESC {
        Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
        Alignment: 0,
        Width: size_in_bytes,
        Height: 1,
        DepthOrArraySize: 1,
        MipLevels: 1,
        Format: DXGI_FORMAT_UNKNOWN,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
        Flags: D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS,
    }
}

pub fn resource_desc_tex2d_nomip(width: u64,
                                 height: u32,
                                 format: DXGI_FORMAT,
                                 flags: D3D12_RESOURCE_FLAGS)
                                 -> D3D12_RESOURCE_DESC {
    D3D12_RESOURCE_DESC {
        Dimension: D3D12_RESOURCE_DIMENSION_TEXTURE2D,
        Alignment: 0,
        Width: width,
        Height: height,
        DepthOrArraySize: 1,
        MipLevels: 1,
        Format: format,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Layout: D3D12_TEXTURE_LAYOUT_UNKNOWN,
        Flags: flags,
    }
}

pub fn resource_desc_tex2darray_nomip(width: u64,
                                 height: u32,
				 array_size: u16,
                                 format: DXGI_FORMAT,
                                 flags: D3D12_RESOURCE_FLAGS)
                                 -> D3D12_RESOURCE_DESC {
    D3D12_RESOURCE_DESC {
        Dimension: D3D12_RESOURCE_DIMENSION_TEXTURE2D,
        Alignment: 0,
        Width: width,
        Height: height,
        DepthOrArraySize: array_size,
        MipLevels: 1,
        Format: format,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Layout: D3D12_TEXTURE_LAYOUT_UNKNOWN,
        Flags: flags,
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

pub fn heap_properties_readback() -> D3D12_HEAP_PROPERTIES {
    D3D12_HEAP_PROPERTIES {
        Type: D3D12_HEAP_TYPE_READBACK,
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

pub enum ResourceBarrier<'a, 'b> {
    Transition {
        data: D3D12_RESOURCE_BARRIER,
        phantom: PhantomData<&'a D3D12Resource>,
    },
    Aliasing {
        data: D3D12_RESOURCE_BARRIER,
        phantom1: PhantomData<&'a D3D12Resource>,
        phantom2: PhantomData<&'b D3D12Resource>,
    },
    Uav {
        data: D3D12_RESOURCE_BARRIER,
        phantom: PhantomData<&'a D3D12Resource>,
    },
}

impl<'e, 'f> ResourceBarrier<'e, 'f> {
    pub fn transition<'a>(resource: &'a D3D12Resource,
                          before: D3D12_RESOURCE_STATES,
                          after: D3D12_RESOURCE_STATES)
                          -> ResourceBarrier<'a, 'a> {
        let mut ret = D3D12_RESOURCE_BARRIER {
            Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
            Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
            u: unsafe { ::std::mem::uninitialized() },
        };
        unsafe {
            *ret.Transition_mut() = D3D12_RESOURCE_TRANSITION_BARRIER {
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

    pub fn transition_subresource<'a>(resource: &'a D3D12Resource,
                                      subresource: UINT,
                                      before: D3D12_RESOURCE_STATES,
                                      after: D3D12_RESOURCE_STATES)
                                      -> ResourceBarrier<'a, 'a> {
        let mut ret = D3D12_RESOURCE_BARRIER {
            Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
            Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
            u: unsafe { ::std::mem::uninitialized() },
        };
        unsafe {
            *ret.Transition_mut() = D3D12_RESOURCE_TRANSITION_BARRIER {
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

    pub fn aliasing<'a, 'b>(before: Option<&'a D3D12Resource>,
                            after: &'b D3D12Resource)
                            -> ResourceBarrier<'a, 'b> {
        let mut ret = D3D12_RESOURCE_BARRIER {
            Type: D3D12_RESOURCE_BARRIER_TYPE_ALIASING,
            Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
            u: unsafe { ::std::mem::uninitialized() },
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

    pub fn uav<'a>(resource: &'a D3D12Resource) -> ResourceBarrier<'a, 'a> {
        let mut ret = D3D12_RESOURCE_BARRIER {
            Type: D3D12_RESOURCE_BARRIER_TYPE_UAV,
            Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
            u: unsafe { ::std::mem::uninitialized() },
        };
        unsafe {
            *ret.UAV_mut() = D3D12_RESOURCE_UAV_BARRIER { pResource: resource.iptr() as *mut _ };
        };
        ResourceBarrier::Uav {
            data: ret,
            phantom: PhantomData::<&'a _>,
        }
    }

    pub fn flags(self, flags: D3D12_RESOURCE_BARRIER_FLAGS) -> ResourceBarrier<'e, 'f> {
        match self {
            ResourceBarrier::Transition {mut data, phantom: p} => {
                data.Flags = flags;
                ResourceBarrier::Transition {
                    data: data,
                    phantom: p,
                }
            }
            ResourceBarrier::Aliasing {mut data, phantom1: p1, phantom2: p2} => {
                data.Flags = flags;
                ResourceBarrier::Aliasing {
                    data: data,
                    phantom1: p1,
                    phantom2: p2,
                }
            }
            ResourceBarrier::Uav {mut data, phantom: p} => {
                data.Flags = flags;
                ResourceBarrier::Uav {
                    data: data,
                    phantom: p,
                }
            }
        }
    }
}

impl<'e, 'f> Deref for ResourceBarrier<'e, 'f> {
    type Target = D3D12_RESOURCE_BARRIER;

    fn deref<'a>(&'a self) -> &'a Self::Target {
        match *self {
            ResourceBarrier::Transition {ref data, ..} => data,
            ResourceBarrier::Aliasing {ref data, ..} => data,
            ResourceBarrier::Uav {ref data, ..} => data,
        }
    }
}

pub enum RootParameter<'a> {
    DescriptorTable {
        data: D3D12_ROOT_PARAMETER,
        ranges: PhantomData<&'a [D3D12_DESCRIPTOR_RANGE]>,
    },
    Other {
        data: D3D12_ROOT_PARAMETER,
    },
}

impl<'e> Deref for RootParameter<'e> {
    type Target = D3D12_ROOT_PARAMETER;

    fn deref<'a>(&'a self) -> &'a Self::Target {
        match *self {
            RootParameter::DescriptorTable {ref data, ..} => data,
            RootParameter::Other {ref data, ..} => data,
        }
    }
}

impl<'a> RootParameter<'a> {
    pub fn descriptor_table<'b>(ranges: &'b [D3D12_DESCRIPTOR_RANGE],
                                visibility: D3D12_SHADER_VISIBILITY)
                                -> RootParameter<'b> {
        let mut ret = D3D12_ROOT_PARAMETER {
            ParameterType: D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            u: unsafe { ::std::mem::uninitialized() },
            ShaderVisibility: visibility,
        };
        unsafe {
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

    pub fn constants<'b>(shader_register: u32,
                         register_space: u32,
                         num_32bit_values: u32,
                         visibility: D3D12_SHADER_VISIBILITY)
                         -> RootParameter<'b> {
        let mut ret = D3D12_ROOT_PARAMETER {
            ParameterType: D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS,
            u: unsafe { ::std::mem::uninitialized() },
            ShaderVisibility: visibility,
        };
        unsafe {
            *ret.Constants_mut() = D3D12_ROOT_CONSTANTS {
                ShaderRegister: shader_register,
                RegisterSpace: register_space,
                Num32BitValues: num_32bit_values,
            };
        };
        RootParameter::Other { data: ret }
    }

    fn other<'b>(shader_register: u32,
                 register_space: u32,
                 visibility: D3D12_SHADER_VISIBILITY,
                 rptype: D3D12_ROOT_PARAMETER_TYPE)
                 -> RootParameter<'b> {
        let mut ret = D3D12_ROOT_PARAMETER {
            ParameterType: rptype,
            u: unsafe { ::std::mem::uninitialized() },
            ShaderVisibility: visibility,
        };
        unsafe {
            *ret.Descriptor_mut() = D3D12_ROOT_DESCRIPTOR {
                ShaderRegister: shader_register,
                RegisterSpace: register_space,
            };
        };
        RootParameter::Other { data: ret }
    }

    pub fn cbv<'b>(shader_register: u32,
                   register_space: u32,
                   visibility: D3D12_SHADER_VISIBILITY)
                   -> RootParameter<'b> {
        Self::other(shader_register,
                    register_space,
                    visibility,
                    D3D12_ROOT_PARAMETER_TYPE_CBV)
    }

    pub fn srv<'b>(shader_register: u32,
                   register_space: u32,
                   visibility: D3D12_SHADER_VISIBILITY)
                   -> RootParameter<'b> {
        Self::other(shader_register,
                    register_space,
                    visibility,
                    D3D12_ROOT_PARAMETER_TYPE_SRV)
    }

    pub fn uav<'b>(shader_register: u32,
                   register_space: u32,
                   visibility: D3D12_SHADER_VISIBILITY)
                   -> RootParameter<'b> {
        Self::other(shader_register,
                    register_space,
                    visibility,
                    D3D12_ROOT_PARAMETER_TYPE_UAV)
    }
}

pub fn texture_copy_location_footprint(res: &D3D12Resource,
                                       footprint: &D3D12_PLACED_SUBRESOURCE_FOOTPRINT)
                                       -> D3D12_TEXTURE_COPY_LOCATION {
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

pub fn texture_copy_location_index(res: &D3D12Resource,
                                   subresource_index: u32)
                                   -> D3D12_TEXTURE_COPY_LOCATION {
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

pub fn srv_buffer(cnt: u32, struct_stride: u32)
                                  -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    let mut ret = D3D12_SHADER_RESOURCE_VIEW_DESC {
        Format: DXGI_FORMAT_UNKNOWN,
        ViewDimension: D3D12_SRV_DIMENSION_BUFFER,
        Shader4ComponentMapping: shader_4component_mapping(0, 1, 2, 3),
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Buffer_mut() = D3D12_BUFFER_SRV {
            FirstElement: 0,
            NumElements: cnt,
            StructureByteStride: struct_stride,
            Flags: D3D12_BUFFER_SRV_FLAG_NONE,
        };
    };
    ret
}

pub fn srv_tex2d(format: DXGI_FORMAT,
                                  four_comp_map: UINT,
                                  most_det_mip: UINT,
                                  mip_levels: UINT,
                                  plane_slice: UINT,
                                  res_min_lod_clamp: f32)
                                  -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    let mut ret = D3D12_SHADER_RESOURCE_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_SRV_DIMENSION_TEXTURE2D,
        Shader4ComponentMapping: four_comp_map,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Texture2D_mut() = D3D12_TEX2D_SRV {
            MostDetailedMip: most_det_mip,
            MipLevels: mip_levels,
            PlaneSlice: plane_slice,
            ResourceMinLODClamp: res_min_lod_clamp,
        };
    };
    ret
}

pub fn srv_tex2d_default(format: DXGI_FORMAT) -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    srv_tex2d(format, shader_4component_mapping(0, 1, 2, 3), 0, 1, 0, 0.0)
}

pub fn shader_resource_view_tex2d_default_mip(format: DXGI_FORMAT,
                                              mip_levels: UINT)
                                              -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    srv_tex2d(format,
                               shader_4component_mapping(0, 1, 2, 3),
                               0,
                               mip_levels,
                               0,
                               0.0)
}

pub fn srv_tex2d_default_slice_mip(format: DXGI_FORMAT,
                                   mip_slice: UINT,
                                   mip_levels: UINT)
                                   -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    srv_tex2d(format,
                               shader_4component_mapping(0, 1, 2, 3),
                               mip_slice,
                               mip_levels,
                               0,
                               0.0)
}

pub fn srv_tex2darray(format: DXGI_FORMAT,
                                  four_comp_map: UINT,
                                  most_det_mip: UINT,
                                  mip_levels: UINT,
                                  first_array_slice: UINT,
                                  array_size: UINT,
                                  plane_slice: UINT,
                                  res_min_lod_clamp: f32)
                                  -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    let mut ret = D3D12_SHADER_RESOURCE_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_SRV_DIMENSION_TEXTURE2DARRAY,
        Shader4ComponentMapping: four_comp_map,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Texture2DArray_mut() = D3D12_TEX2D_ARRAY_SRV {
            MostDetailedMip: most_det_mip,
            MipLevels: mip_levels,
            FirstArraySlice: first_array_slice,
            ArraySize: array_size,
            PlaneSlice: plane_slice,
            ResourceMinLODClamp: res_min_lod_clamp,
        };
    };
    ret
}

pub fn srv_tex2darray_default(format : DXGI_FORMAT, array_size: u32) -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    srv_tex2darray(format, shader_4component_mapping(0, 1, 2, 3), 0, 1, 0, array_size, 0, 0.0)
}

pub fn srv_texcube(format: DXGI_FORMAT,
                                  four_comp_map: UINT,
                                  most_det_mip: UINT,
                                  mip_levels: UINT,
                                  res_min_lod_clamp: f32)
                                  -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    let mut ret = D3D12_SHADER_RESOURCE_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_SRV_DIMENSION_TEXTURECUBE,
        Shader4ComponentMapping: four_comp_map,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.TextureCube_mut() = D3D12_TEXCUBE_SRV {
            MostDetailedMip: most_det_mip,
            MipLevels: mip_levels,
            ResourceMinLODClamp: res_min_lod_clamp,
        };
    };
    ret
}

pub fn srv_texcube_default(format: DXGI_FORMAT) -> D3D12_SHADER_RESOURCE_VIEW_DESC {
    srv_texcube(format, shader_4component_mapping(0, 1, 2, 3), 0, 1, 0.0)  
}

pub fn uav_tex2d_desc(format: DXGI_FORMAT,
                      mip_slice: UINT,
                      plane_slice: UINT)
                      -> D3D12_UNORDERED_ACCESS_VIEW_DESC {
    let mut ret = D3D12_UNORDERED_ACCESS_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_UAV_DIMENSION_TEXTURE2D,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Texture2D_mut() = D3D12_TEX2D_UAV {
            MipSlice: mip_slice,
            PlaneSlice: plane_slice,
        };
    };
    ret
}

pub fn uav_buffer_desc(count: u32, size: u32)
                      -> D3D12_UNORDERED_ACCESS_VIEW_DESC {
    let mut ret = D3D12_UNORDERED_ACCESS_VIEW_DESC {
        Format: DXGI_FORMAT_UNKNOWN,
        ViewDimension: D3D12_UAV_DIMENSION_BUFFER,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Buffer_mut() = D3D12_BUFFER_UAV {
            FirstElement: 0,
            NumElements: count,
            StructureByteStride: size,
            CounterOffsetInBytes: 0,
            Flags: D3D12_BUFFER_UAV_FLAG_NONE,
        };
    };
    ret
}

pub fn cbv_desc(location: D3D12_GPU_VIRTUAL_ADDRESS, size: u32) -> D3D12_CONSTANT_BUFFER_VIEW_DESC {
    D3D12_CONSTANT_BUFFER_VIEW_DESC {
        BufferLocation: location,
        SizeInBytes: (size+255)/256*256,
    }
}

pub fn uav_raw_buffer_desc(size: u32)
                      -> D3D12_UNORDERED_ACCESS_VIEW_DESC {
    let mut ret = D3D12_UNORDERED_ACCESS_VIEW_DESC {
        Format: DXGI_FORMAT_R32_TYPELESS,
        ViewDimension: D3D12_UAV_DIMENSION_BUFFER,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Buffer_mut() = D3D12_BUFFER_UAV {
            FirstElement: 0,
            NumElements: size,
            StructureByteStride: 1,
            CounterOffsetInBytes: 0,
            Flags: D3D12_BUFFER_UAV_FLAG_RAW,
        };
    };
    ret
}

pub fn depth_stencil_view_desc_tex2d_default(format: DXGI_FORMAT) -> D3D12_DEPTH_STENCIL_VIEW_DESC {
    let mut ret = D3D12_DEPTH_STENCIL_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_DSV_DIMENSION_TEXTURE2D,
        Flags: D3D12_DSV_FLAG_NONE,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Texture2D_mut() = D3D12_TEX2D_DSV { MipSlice: 0 };
    };
    ret
}

pub fn depth_stencil_view_desc_tex2darray_default(format: DXGI_FORMAT, size: u32) -> D3D12_DEPTH_STENCIL_VIEW_DESC {
    let mut ret = D3D12_DEPTH_STENCIL_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_DSV_DIMENSION_TEXTURE2DARRAY,
        Flags: D3D12_DSV_FLAG_NONE,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Texture2DArray_mut() = D3D12_TEX2D_ARRAY_DSV { MipSlice: 0, FirstArraySlice: 0, ArraySize: size };
    };
    ret
}

pub fn render_target_view_desc_tex2d_default(format: DXGI_FORMAT) -> D3D12_RENDER_TARGET_VIEW_DESC {
    let mut ret = D3D12_RENDER_TARGET_VIEW_DESC {
        Format: format,
        ViewDimension: D3D12_RTV_DIMENSION_TEXTURE2D,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Texture2D_mut() = D3D12_TEX2D_RTV {
            MipSlice: 0,
            PlaneSlice: 0,
        };
    };
    ret
}

pub fn depth_stencil_clear_value_depth_f32(clear_depth: f32) -> D3D12_CLEAR_VALUE {
    let mut ret = D3D12_CLEAR_VALUE {
        Format: DXGI_FORMAT_D32_FLOAT,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.DepthStencil_mut() = D3D12_DEPTH_STENCIL_VALUE {
            Depth: clear_depth,
            Stencil: 0,
        };
    };
    ret
}

pub fn rt_rgba_f32_clear_value(color: [f32; 4]) -> D3D12_CLEAR_VALUE {
    let mut ret = D3D12_CLEAR_VALUE {
        Format: DXGI_FORMAT_R32G32B32A32_FLOAT,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Color_mut() = color;
    };
    ret
}

pub fn rt_rgba_clear_value(format: DXGI_FORMAT, color: [f32; 4]) -> D3D12_CLEAR_VALUE {
    let mut ret = D3D12_CLEAR_VALUE {
        Format: format,
        u: unsafe { mem::uninitialized() },
    };
    unsafe {
        *ret.Color_mut() = color;
    };
    ret
}


pub fn depth_stencilop_desc_default() -> D3D12_DEPTH_STENCILOP_DESC {
    D3D12_DEPTH_STENCILOP_DESC {
        StencilFunc: D3D12_COMPARISON_FUNC_ALWAYS,
        StencilDepthFailOp: D3D12_STENCIL_OP_KEEP,
        StencilPassOp: D3D12_STENCIL_OP_KEEP,
        StencilFailOp: D3D12_STENCIL_OP_KEEP,
    }
}

pub fn render_target_blend_desc_default() -> D3D12_RENDER_TARGET_BLEND_DESC {
    D3D12_RENDER_TARGET_BLEND_DESC {
        BlendEnable: 0,
        LogicOpEnable: 0,
        SrcBlend: D3D12_BLEND_ONE,
        DestBlend: D3D12_BLEND_ZERO,
        BlendOp: D3D12_BLEND_OP_ADD,
        SrcBlendAlpha: D3D12_BLEND_ONE,
        DestBlendAlpha: D3D12_BLEND_ZERO,
        BlendOpAlpha: D3D12_BLEND_OP_ADD,
        LogicOp: D3D12_LOGIC_OP_NOOP,
        RenderTargetWriteMask: D3D12_COLOR_WRITE_ENABLE_ALL.0 as u8,
    }
}

pub fn rasterizer_desc_default() -> D3D12_RASTERIZER_DESC {
    D3D12_RASTERIZER_DESC {
        FillMode: D3D12_FILL_MODE_SOLID,
        CullMode: D3D12_CULL_MODE_BACK,
        FrontCounterClockwise: 0,
        DepthBias: D3D12_DEFAULT_DEPTH_BIAS as i32,
        SlopeScaledDepthBias: D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS,
        DepthBiasClamp: D3D12_DEFAULT_DEPTH_BIAS_CLAMP,
        DepthClipEnable: 1,
        MultisampleEnable: 0,
        AntialiasedLineEnable: 0,
        ForcedSampleCount: 0,
        ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF,
    }
}

pub fn graphics_pipeline_state_desc_default() -> D3D12_GRAPHICS_PIPELINE_STATE_DESC {
    let blend_desc_def = render_target_blend_desc_default();
    let stencilop_desc_def = depth_stencilop_desc_default();

    D3D12_GRAPHICS_PIPELINE_STATE_DESC {
        pRootSignature: ptr::null_mut(),
        VS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: ptr::null(),
            BytecodeLength: 0,
        },
        PS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: ptr::null(),
            BytecodeLength: 0,
        },
        DS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: ptr::null(),
            BytecodeLength: 0,
        },
        HS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: ptr::null(),
            BytecodeLength: 0,
        },
        GS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: ptr::null(),
            BytecodeLength: 0,
        },
        StreamOutput: D3D12_STREAM_OUTPUT_DESC {
            pSODeclaration: ptr::null(),
            NumEntries: 0,
            pBufferStrides: ptr::null(),
            NumStrides: 0,
            RasterizedStream: 0,
        },
        BlendState: D3D12_BLEND_DESC {
            AlphaToCoverageEnable: 0,
            IndependentBlendEnable: 0,
            RenderTarget: [blend_desc_def,
                           blend_desc_def,
                           blend_desc_def,
                           blend_desc_def,
                           blend_desc_def,
                           blend_desc_def,
                           blend_desc_def,
                           blend_desc_def],
        },
        SampleMask: 0xffffffff,
        RasterizerState: rasterizer_desc_default(),
        DepthStencilState: D3D12_DEPTH_STENCIL_DESC {
            DepthEnable: 1,
            DepthWriteMask: D3D12_DEPTH_WRITE_MASK_ALL,
            DepthFunc: D3D12_COMPARISON_FUNC_LESS,
            StencilEnable: 0,
            StencilReadMask: D3D12_DEFAULT_STENCIL_READ_MASK as u8,
            StencilWriteMask: D3D12_DEFAULT_STENCIL_WRITE_MASK as u8,
            FrontFace: stencilop_desc_def,
            BackFace: stencilop_desc_def,
        },
        InputLayout: D3D12_INPUT_LAYOUT_DESC {
            pInputElementDescs: ptr::null(),
            NumElements: 0,
        },
        IBStripCutValue: D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED,
        PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
        NumRenderTargets: 1,
        RTVFormats: [DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN,
                     DXGI_FORMAT_UNKNOWN],
        DSVFormat: DXGI_FORMAT_D32_FLOAT,
        SampleDesc: sample_desc_default(),
        NodeMask: 0,
        CachedPSO: D3D12_CACHED_PIPELINE_STATE {
            pCachedBlob: ptr::null(),
            CachedBlobSizeInBytes: 0,
        },
        Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
    }
}

pub fn compute_pipeline_state_desc_default() -> D3D12_COMPUTE_PIPELINE_STATE_DESC {
    D3D12_COMPUTE_PIPELINE_STATE_DESC {
        pRootSignature: ptr::null_mut(),
        CS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: ptr::null(),
            BytecodeLength: 0,
        },
        NodeMask: 0,
        CachedPSO: D3D12_CACHED_PIPELINE_STATE {
            pCachedBlob: ptr::null(),
            CachedBlobSizeInBytes: 0,
        },
        Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
    }
}

pub fn sample_desc_default() -> DXGI_SAMPLE_DESC {
    DXGI_SAMPLE_DESC {
        Count: 1,
        Quality: 0,
    }
}

pub fn static_sampler_anisotropic_default() -> D3D12_STATIC_SAMPLER_DESC {
    D3D12_STATIC_SAMPLER_DESC {
        Filter: D3D12_FILTER_ANISOTROPIC,
        AddressU: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
        AddressV: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
        AddressW: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
        MipLODBias: 0.0,
        MaxAnisotropy: 16,
        ComparisonFunc: D3D12_COMPARISON_FUNC_LESS_EQUAL,
        BorderColor: D3D12_STATIC_BORDER_COLOR_OPAQUE_WHITE,
        MinLOD: 0.0,
        MaxLOD: D3D12_FLOAT32_MAX,
        ShaderRegister: 0,
        RegisterSpace: 0,
        ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
    }
}

pub struct DescriptorHeap {
    dheap: D3D12DescriptorHeap,
    heap_size: u32,
    handle_size: SIZE_T,
}

impl DescriptorHeap {
    pub fn new(dev: &D3D12Device,
               size: u32,
               ty: D3D12_DESCRIPTOR_HEAP_TYPE,
               shader_visible: bool,
               node_mask: u32)
               -> HResult<DescriptorHeap> {
        let dsd_hd = D3D12_DESCRIPTOR_HEAP_DESC {
            NumDescriptors: size as u32,
            Type: ty,
            Flags: if shader_visible {
                D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE
            } else {
                D3D12_DESCRIPTOR_HEAP_FLAG_NONE
            },
            NodeMask: node_mask,
        };
        let dheap = try!(dev.create_descriptor_heap(&dsd_hd));
        Ok(DescriptorHeap {
            dheap: dheap,
            heap_size: size,
            handle_size: dev.get_descriptor_handle_increment_size(ty) as SIZE_T,
        })
    }

    pub fn cpu_handle(&self, i: u32) -> D3D12_CPU_DESCRIPTOR_HANDLE {
        assert!(i < self.heap_size);
        let mut handle = self.dheap.get_cpu_descriptor_handle_for_heap_start();
        handle.ptr += (i as SIZE_T) * self.handle_size;
        handle
    }

    pub fn gpu_handle(&self, i: u32) -> D3D12_GPU_DESCRIPTOR_HANDLE {
        assert!(i < self.heap_size);
        let mut handle = self.dheap.get_gpu_descriptor_handle_for_heap_start();
        handle.ptr += (i as SIZE_T) * self.handle_size;
        handle
    }

    pub fn get(&self) -> &D3D12DescriptorHeap {
        &self.dheap
    }
}

pub struct ShaderBytecode<'a> {
    inner: D3D12_SHADER_BYTECODE,
    ph: PhantomData<&'a u32>,
}

impl<'g> ShaderBytecode<'g> {
    pub fn empty() -> ShaderBytecode<'static> {
        ShaderBytecode {
            inner: D3D12_SHADER_BYTECODE {
                pShaderBytecode: ptr::null(),
                BytecodeLength: 0,
            },
            ph: PhantomData::<&'static _>,
        }
    }

    pub fn from_vec<'a>(v: &'a Vec<u8>) -> ShaderBytecode<'a> {
        ShaderBytecode {
            inner: D3D12_SHADER_BYTECODE {
                pShaderBytecode: v.as_ptr() as *const _,
                BytecodeLength: v.len() as SIZE_T,
            },
            ph: PhantomData::<&'a _>,
        }
    }

    pub fn from_slice<'a>(v: &'a [u8]) -> ShaderBytecode<'a> {
        ShaderBytecode {
            inner: D3D12_SHADER_BYTECODE {
                pShaderBytecode: v.as_ptr() as *const _,
                BytecodeLength: v.len() as SIZE_T,
            },
            ph: PhantomData::<&'a _>,
        }
    }

    pub fn get(self) -> D3D12_SHADER_BYTECODE {
        self.inner
    }
}

pub fn create_default_cpso(dev: &D3D12Device, 
                            root_signature: &D3D12RootSignature, 
                            shader_bytecode: Vec<u8>) -> HResult<D3D12PipelineState> {
    let cpsd = D3D12_COMPUTE_PIPELINE_STATE_DESC {
        pRootSignature: root_signature.iptr() as *mut _,
        CS: ShaderBytecode::from_vec(&shader_bytecode).get(),
        .. compute_pipeline_state_desc_default()
    };
    dev.create_compute_pipeline_state(&cpsd)
}
