use winapi::*;
use create_device;
use dxsafe::*;
use dxsafe::structwrappers::*;
use dxsems::VertexFormat;
use std::marker::PhantomData;

// Point Light Shadow
// The name isn't quite correct. This module just fills depth cubemap.
pub struct PLShadow<T: VertexFormat> {
    pub pso: D3D12PipelineState,
    pub root_sig: D3D12RootSignature,
    _luid: Luid,
    _phd: PhantomData<T>,
}

impl<T: VertexFormat> PLShadow<T> {
    pub fn new(dev: &D3D12Device) -> HResult<PLShadow<T>> {
        let mut vshader_bc = vec![];
        let mut gshader_bc = vec![];
        let mut pshader_bc = vec![];
        let mut rsig_bc = vec![];
        trace!("Compiling 'plshadow.hlsl'...");
        match create_device::compile_shaders("plshadow.hlsl",&mut[
                    ("VSMain", "vs_5_0", &mut vshader_bc),
                    ("GSMain", "gs_5_0", &mut gshader_bc),
                    ("PSMain", "ps_5_0", &mut pshader_bc),
                    ("RSD", "rootsig_1_0", &mut rsig_bc),
                ], D3DCOMPILE_OPTIMIZATION_LEVEL3) {
            Err(err) => {
                error!("Error compiling 'plshadow.hlsl': {}", err);
                return Err(E_FAIL);
            },
            Ok(_) => {},
        };
        trace!("Done");

        trace!("Root signature creation");
        let root_sig = try!(dev.create_root_signature(0, &rsig_bc[..]));
        try!(root_sig.set_name("plshadow RSD"));

        let input_elts_desc = T::generate(0);

        // pso_desc contains pointers to local data, so I mustn't pass it around, but I can. Unsafe.
        let pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
            pRootSignature: root_sig.iptr() as *mut _,
            VS: ShaderBytecode::from_vec(&vshader_bc).get(),
            GS: ShaderBytecode::from_vec(&gshader_bc).get(),
            PS: ShaderBytecode::from_vec(&pshader_bc).get(),
            RasterizerState: D3D12_RASTERIZER_DESC {
                CullMode: D3D12_CULL_MODE_NONE,
                DepthBias: 550,
                SlopeScaledDepthBias: 1.5, 
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
            _luid: Luid(dev.get_adapter_luid()),
            _phd: PhantomData,
        })
    }

}
