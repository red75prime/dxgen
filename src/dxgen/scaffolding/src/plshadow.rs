use winapi::*;
use core;
use create_device;
use utils;
use dx_safe::*;
use dx_safe::structwrappers::*;
use std::ptr;
use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::str;
use dxsems::VertexFormat;
use std::marker::PhantomData;

// Point Light Shadow
// The name isn't quite correct. This module just fills depth cubemap.
pub struct PLShadow<T: VertexFormat> {
    pso: D3D12PipelineState,
    root_sig: D3D12RootSignature,
    dheap: DescriptorHeap,
    luid: Luid,
    phd: PhantomData<T>,
}

impl<T: VertexFormat> PLShadow<T> {
    pub fn new(dev: &D3D12Device) -> HResult<PLShadow<T>> {
        let dheap = DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0)
                        .expect("Cannot create descriptor heap");

        let mut f = File::open("plshadow.hlsl").expect("Cannot open 'plshadow.hlsl' shader file.");
        let mut content = vec![];
        f.read_to_end(&mut content).expect("Cannot read 'plshadow.hlsl' shader file.");
        let shader = str::from_utf8(&content[..])
                         .expect("'plshadow.hlsl' file content is not a valid UTF-8");

        trace!("Vertex shader compilation");
        let vshader_bc = create_device::d3d_compile_from_str(shader, "plshadow.hlsl::VSMain", "VSMain", "vs_5_0", 0)
                             .map_err(|err| error!("Vertex shader compilation error: {}", err))
                             .unwrap();
        trace!("Geometry shader compilation");
        let gshader_bc = create_device::d3d_compile_from_str(shader, "plshadow.hlsl::GSMain", "GSMain", "gs_5_0", 0)
                             .map_err(|err| error!("Geometry shader compilation error: {}", err))
                             .unwrap();
        trace!("Pixel shader compilation");
        let pshader_bc = create_device::d3d_compile_from_str(shader, "plshadow.hlsl::PSMain", "PSMain", "ps_5_0", 0)
                             .map_err(|err| error!("Pixel shader compilation error: {}", err))
                             .unwrap();
        trace!("Root signature compilation");
        let rsig_bc = create_device::d3d_compile_from_str(shader, "plshadow.hlsl::RSD", "RSD", "rootsig_1_0", 0)
                          .map_err(|err| error!("Root signature compilation error: {}", err))
                          .unwrap();

        trace!("Root signature creation");
        let root_sig = try!(dev.create_root_signature(0, &rsig_bc[..]));

        let input_elts_desc = T::generate(0);

        // pso_desc contains pointers to local data, so I mustn't pass it around, but I can. Unsafe.
        let pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
            pRootSignature: root_sig.iptr() as *mut _,
            VS: ShaderBytecode::from_vec(&vshader_bc).get(),
            GS: ShaderBytecode::from_vec(&gshader_bc).get(),
            PS: ShaderBytecode::from_vec(&pshader_bc).get(),
            RasterizerState: D3D12_RASTERIZER_DESC {
                CullMode: D3D12_CULL_MODE_NONE,
                ..rasterizer_desc_default()
            },
            InputLayout: D3D12_INPUT_LAYOUT_DESC {
                pInputElementDescs: input_elts_desc.as_ptr(),
                NumElements: input_elts_desc.len() as u32,
            },
            PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
            NumRenderTargets: 0, // Pixel shader writes depth buffer only
            DSVFormat: DXGI_FORMAT_D32_FLOAT,
            Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
            // Take other fields from default gpsd
            ..graphics_pipeline_state_desc_default()
        };
        //pso_desc.RTVFormats[0] = DXGI_FORMAT_D32_FLOAT;

        trace!("Graphics pipeline state creation");
        let pso = try!(dev.create_graphics_pipeline_state(&pso_desc));
        Ok(PLShadow::<T> {
            pso: pso,
            root_sig: root_sig,
            dheap: dheap,
            luid: Luid(dev.get_adapter_luid()),
            phd: PhantomData,
        })
    }

    pub fn fill_cubemap(core: &core::DXCore) -> HResult<()> {
        Ok(())
    }
}
