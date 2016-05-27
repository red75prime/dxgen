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
        let mut pshader_bc = vec![];
        let mut rsig_bc = vec![];
        trace!("Compiling 'light.hlsl'...");
        match create_device::compile_shaders("light.hlsl",&mut[
                    ("VSMain", "vs_5_0", &mut vshader_bc),
                    ("PSMain", "ps_5_0", &mut pshader_bc),
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
            PS: ShaderBytecode::from_vec(&pshader_bc).get(),
            RasterizerState: D3D12_RASTERIZER_DESC {
                ..rasterizer_desc_default()
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
//    tcr_buffer: D3D12Resource, // vertex texture coordinates
    idx_buffer: D3D12Resource, // indices into buffers
    idx_cnt: usize,
}

impl LightSourceResources {
    pub fn new(dev: &D3D12Device) -> HResult<LightSourceResources> {
        trace!("LightSourceResources new");
        trace!("Load 'sphere.obj'");
        let (crd, nrm, idx) = sphere("sphere.obj");
        
        // TODO: I need typed D3D12Resource
        let buf_size = ::std::mem::size_of::<[f32;3]>() * crd.len();
        trace!("Create coordinates buffer");
        let crd_buffer = try!(dev.create_committed_resource(
                                  &heap_properties_upload(),
                                  D3D12_HEAP_FLAG_NONE,
                                  &resource_desc_buffer(buf_size as u64),
                                  D3D12_RESOURCE_STATE_GENERIC_READ, // Must be GENERIC_READ for upload heap
                                  None));
        try!(crd_buffer.set_name("Light source coordinates buffer".into()));
        try!(utils::upload_into_buffer(&crd_buffer, &crd[..]));

        let buf_size = ::std::mem::size_of::<[f32;3]>() * nrm.len();
        trace!("Create normals buffer");
        let nrm_buffer = try!(dev.create_committed_resource(
                                  &heap_properties_upload(),
                                  D3D12_HEAP_FLAG_NONE,
                                  &resource_desc_buffer(buf_size as u64),
                                  D3D12_RESOURCE_STATE_GENERIC_READ, // Must be GENERIC_READ for upload heap
                                  None));
        try!(nrm_buffer.set_name("Light source normals buffer".into()));
        try!(utils::upload_into_buffer(&nrm_buffer, &nrm[..]));
        
        let idx_cnt = idx.len();
        let buf_size = ::std::mem::size_of::<ShaderIndices>() * idx_cnt;
        trace!("Create indices buffer");
        let idx_buffer = try!(dev.create_committed_resource(
                                  &heap_properties_upload(),
                                  D3D12_HEAP_FLAG_NONE,
                                  &resource_desc_buffer(buf_size as u64),
                                  D3D12_RESOURCE_STATE_GENERIC_READ, // Must be GENERIC_READ for upload heap
                                  None));
        try!(idx_buffer.set_name("Light source index buffer".into()));
        try!(utils::upload_into_buffer(&idx_buffer, &idx[..]));

        Ok(LightSourceResources {
          crd_buffer: crd_buffer,
          nrm_buffer: nrm_buffer,
          idx_buffer: idx_buffer,
          idx_cnt: idx_cnt,
        })
    }

    // Precondition: render targets, viewports, scissor rects should be set for glist
    pub fn populate_command_list(&self, glist: &D3D12GraphicsCommandList, light: &LightSource, cbuf: &D3D12Resource) {
        glist.set_pipeline_state(&light.pso);
        glist.set_graphics_root_signature(&light.root_sig);
        glist.ia_set_primitive_topology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        glist.ia_set_vertex_buffers(0, None);
        glist.ia_set_index_buffer(None);
        glist.set_graphics_root_constant_buffer_view(0, cbuf.get_gpu_virtual_address());
        glist.set_graphics_root_shader_resource_view(1, self.idx_buffer.get_gpu_virtual_address());
        glist.set_graphics_root_shader_resource_view(2, self.crd_buffer.get_gpu_virtual_address());
        glist.set_graphics_root_shader_resource_view(3, self.nrm_buffer.get_gpu_virtual_address());

        glist.draw_instanced(self.idx_cnt as u32, 1, 0, 0);
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
  let (pos,nrm,idx) = sphere(&path);
  assert_eq!(pos.len(), 362);
  assert_eq!(nrm.len(), 362);
  assert_eq!(idx.len(), 2160);
}
