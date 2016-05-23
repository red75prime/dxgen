use winapi::*;
use cgmath::Vector3;
use core::DXCore;
use create_device;
use utils;
use dx_safe::*;
use dx_safe::structwrappers::*;
use dxsems::VertexFormat;
use obj;
use obj::SimplePolygon;
use std::path::Path;
use std::io::{self, BufReader};
use std::io::prelude::*;
use std::fs::File;

pub struct LightSource {
    pso: D3D12PipelineState,
    root_sig: D3D12RootSignature,
    luid: Luid,
}

impl LightSource {
    pub fn new(dev: &D3D12Device, rt_format: DXGI_FORMAT) -> HResult<LightSource> {
        let mut vshader_bc = vec![];
        let mut rsig_bc = vec![];
        trace!("Compiling 'light.hlsl'...");
        match create_device::compile_shaders("light.hlsl",&mut[
                    ("VSMain", "vs_5_0", &mut vshader_bc),
                    ("RSD", "rootsig_1_0", &mut rsig_bc),
                ], D3DCOMPILE_OPTIMIZATION_LEVEL3) {
            Err(err) => {
                error!("Error compiling 'light.hlsl': {}", err);
                return Err(E_FAIL);
            },
            Ok(_) => {},
        };
        trace!("Done");

        trace!("Root signature creation");
        let root_sig = try!(dev.create_root_signature(0, &rsig_bc[..]));

        // pso_desc contains pointers to local data, so I mustn't pass it around, but I can. Unsafe.
        let mut pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
            pRootSignature: root_sig.iptr() as *mut _,
            VS: ShaderBytecode::from_vec(&vshader_bc).get(),
            RasterizerState: D3D12_RASTERIZER_DESC {
                ..rasterizer_desc_default()
            },
            InputLayout: D3D12_INPUT_LAYOUT_DESC {
                pInputElementDescs: ::std::ptr::null(),
                NumElements: 0,
            },
            PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
            NumRenderTargets: 1, 
            DSVFormat: DXGI_FORMAT_D32_FLOAT,
            Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
            // Take other fields from default gpsd
            ..graphics_pipeline_state_desc_default()
        };
        pso_desc.RTVFormats[0] = rt_format;

        trace!("Graphics pipeline state creation");
        let pso = try!(dev.create_graphics_pipeline_state(&pso_desc));
        Ok(LightSource {
            pso: pso,
            root_sig: root_sig,
            luid: Luid(dev.get_adapter_luid()),
        })
    }
}

pub struct LightSourceResources {
    crd_buffer: D3D12Resource, // vertex coordinates
    nrm_buffer: D3D12Resource, // vertex normals
    tcr_buffer: D3D12Resource, // vertex texture coordinates
    idx_buffer: D3D12Resource, // indices into buffers
    ins_buffer: D3D12Resource, // instance data
}

impl LightSourceResources {
    pub fn new(core: &DXCore) -> LightSourceResources {
        panic!("Not implemented");
    }
}

type V3 = Vector3<f32>;

#[derive(Debug)]
#[repr(C)]
struct ShaderIndices {
    crd: u32,
    nrm: u32,
    tex: u32,
}

fn sphere<P: AsRef<Path>>(path: P) -> (Vec<V3>, Vec<V3>, Vec<ShaderIndices>) {
    let scene = obj::load::<SimplePolygon>(path.as_ref()).expect("3d model load failed");
    if let Some(obj) = scene.object_iter().next() {
        if let Some(group) = obj.group_iter().next() {
          let mut indices = vec![];
          for p in group.indices.iter() {
            for i in p {
              match i {
                &(v, Some(t), Some(n)) => indices.push(ShaderIndices{crd: v as u32, nrm: n as u32, tex: t as u32}),
                _ => panic!("Missing normal or texture coord"),
              };
            };
          };
          let positions = scene.position().iter().map(|&c| utils::v3(c[0],c[1],c[2])).collect();
          let normals = scene.normal().iter().map(|&n| utils::v3(n[0],n[1],n[2])).collect();
          return (positions, normals, indices);
        };
    };
    panic!("No objects in 'sphere.obj'");
}

#[test]
fn sphere_test() {
  let path = ::std::env::current_exe().unwrap().parent().unwrap().join("../../src/sphere.obj");
  let (pos,_,_) = sphere(&path);
  assert_eq!(pos.len(), 362);
}
