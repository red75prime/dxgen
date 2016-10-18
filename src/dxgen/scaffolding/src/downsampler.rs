use winapi::*;
use core::*;
use create_device::*;
use utils::*;
use dxsafe::*;
use dxsafe::structwrappers::*;
use std::ptr;
use std::io::prelude::*;
use std::fs::File;
use std::str;

pub struct Downsampler {
    cpso: D3D12PipelineState,
    root_sig: D3D12RootSignature,
    dheap: DescriptorHeap,
    // luid of adapter cpso was generated on. generate_mips asserts that it is the same adapter as passed in DXCore
    luid: Luid,
}

impl Downsampler {
    pub fn new(dev: &D3D12Device) -> Downsampler {
        let dheap = DescriptorHeap::new(&dev, 2, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, true, 0)
                        .expect("Cannot create descriptor heap");

        let mut f = File::open("downsampler.hlsl").expect("Cannot open tonemapper shader file.");
        let mut content = vec![];
        f.read_to_end(&mut content).expect("Cannot read tonemapper shader file.");
        let shader = str::from_utf8(&content[..])
                         .expect("Shader file content is not a valid UTF-8");

        let (shader_bytecode, root_sig_bytecode) = compile_shader_and_root_signature(shader.into(), "downsampler.hlsl",
                                                                                     "CSMain",
                                                                                     "RSD", 0)
                                                       .expect("Tonemapper horizontal pass \
                                                                shader compile error");

        trace!("Root signature");
        let root_sig = dev.create_root_signature(0, &root_sig_bytecode[..])
                          .expect("Cannot create root signature");

        // It doesn't work.
        // let crs = d3d_get_root_signature_from_str(SHADER, dev).expect("Cannot extract root signature");
        let cpsd = D3D12_COMPUTE_PIPELINE_STATE_DESC {
            pRootSignature: root_sig.iptr() as *mut _,
            CS: ShaderBytecode::from_vec(&shader_bytecode).get(),
            NodeMask: 0,
            CachedPSO: D3D12_CACHED_PIPELINE_STATE {
                pCachedBlob: ptr::null(),
                CachedBlobSizeInBytes: 0,
            },
            Flags: D3D12_PIPELINE_STATE_FLAG_NONE,
        };
        trace!("create_compute_pipeline_state");
        let cpso = dev.create_compute_pipeline_state(&cpsd)
                      .expect("Cannot create compute pipeline state");
        Downsampler {
            cpso: cpso,
            root_sig: root_sig,
            luid: Luid(dev.get_adapter_luid()),
            dheap: dheap,
        }
    }

    pub fn generate_mips(&self, tex: &D3D12Resource, core: &DXCore) -> HResult<()> {
        trace!("generate_mips");
        trace!("Assert same adapter");
        assert!(&self.luid == &Luid(core.dev.get_adapter_luid()));

        trace!("rdesc");
        let rdesc = tex.get_desc();
        let (mut cw, mut ch) = ((rdesc.Width / 2) as u32, rdesc.Height / 2);

        // Create intermediate texture to hold computed mip-map levels
        // I had no success with generating mip-maps inplace.
        let tex_desc = D3D12_RESOURCE_DESC {
            MipLevels: 1,
            // D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS is required for generating mip-maps (at least it required for my method of generating mip-maps)
            ..resource_desc_tex2d_nomip(cw as u64,
                                        ch as u32,
                                        rdesc.Format,
                                        D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS)
        };
        trace!("Create intermediate texture");
        let im_tex = try!(core.dev.create_committed_resource(&heap_properties_default(),
                                                             D3D12_HEAP_FLAG_NONE,
                                                             &tex_desc,
                                                             D3D12_RESOURCE_STATE_COMMON,
                                                             None));
        try!(im_tex.set_name("generate_mips intermediate texture"));

        trace!("uav_desc");
        let uav_desc = uav_tex2d_desc(rdesc.Format, 0, 0);
        core.dev.create_unordered_access_view(Some(&im_tex),
                                              None,
                                              Some(&uav_desc),
                                              self.dheap.cpu_handle(1));

        // callocator and clist are destroyed on function exit.
        // It is not very efficient, but it's ok for now.
        trace!("callocator");
        let callocator = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_COMPUTE));
        // DX12 API developers possibly had an idea of type-level separation of graphics and compute command lists, but
        // they (wisely?) decided to keep interface count low and do runtime validation of commands.
        // So, compute and graphics command lists are represented as D3D12GraphicsCommandList.
        trace!("clist");
        let clist: D3D12GraphicsCommandList =
            try!(core.dev.create_command_list(0,
                                              D3D12_COMMAND_LIST_TYPE_COMPUTE,
                                              &callocator,
                                              Some(&self.cpso)));
        trace!("clist.close()");
        try!(clist.close());

        let fence = try!(core.dev.create_fence(0, D3D12_FENCE_FLAG_NONE));
        let fence_event = create_event();

        for i in 0..((rdesc.MipLevels - 1) as u32) {
            trace!("calloc.reset()");
            try!(callocator.reset());
            trace!("clist.reset()");
            try!(clist.reset(&callocator, None));
            // Create descriptors in descriptor heap.
            // Their layout should match root signature specified in compute shader below.
            // Descriptor for source mip-map slice
            trace!("srv_desc");
            let srv_desc = srv_tex2d_default_slice_mip(rdesc.Format, i, 1);
            core.dev.create_shader_resource_view(Some(&tex), Some(&srv_desc), self.dheap.cpu_handle(0));

            trace!("clist.set_pipeline_state");
            clist.set_pipeline_state(&self.cpso);
            clist.set_compute_root_signature(&self.root_sig);
            trace!("clist.set_descriptor_heaps");
            clist.set_descriptor_heaps(&[self.dheap.get()]);
            trace!("clist.set_compute_root_descriptor_table(0, ...");
            clist.set_compute_root_descriptor_table(0, self.dheap.gpu_handle(0));

            trace!("clist.resource_barrier");
            clist.resource_barrier(&[
              // Transition source mip-map slice (resource state promotion doesn't apply. texture isn't marked as Simultaneous-Access)
              *ResourceBarrier::transition_subresource(&tex, i,
                D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE),
              // Transition intermediate texture (resource state promotion doesn't apply. texture isn't marked as Simultaneous-Access)
              *ResourceBarrier::transition(&im_tex, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_UNORDERED_ACCESS),
            ]);
            trace!("dispatch");
            clist.dispatch(cw, ch, 1);
            trace!("resource barrier");
            clist.resource_barrier(&[
              // Transition source mip-map slice
              *ResourceBarrier::transition_subresource(&tex, i,
                D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_COMMON),
              // Transition intermediate texture
              *ResourceBarrier::transition(&im_tex, 
                D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE),
              // Transition target slice
              *ResourceBarrier::transition_subresource(&tex, i+1,
                D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_COPY_DEST),
            ]);

            let source_copy_loc = texture_copy_location_index(&im_tex, 0);
            let dest_copy_loc = texture_copy_location_index(&tex, i + 1);

            let src_box = D3D12_BOX {
                left: 0,
                top: 0,
                front: 0,
                right: cw,
                bottom: ch,
                back: 1,
            };
            trace!("copy_texture_region");
            clist.copy_texture_region(&dest_copy_loc, 0, 0, 0, &source_copy_loc, Some(&src_box));
            trace!("resource_barrier");
            clist.resource_barrier(&[
              // Transition intermediate texture
              *ResourceBarrier::transition(&im_tex, 
                D3D12_RESOURCE_STATE_COPY_SOURCE, D3D12_RESOURCE_STATE_COMMON),
              // Transition target slice
              *ResourceBarrier::transition_subresource(&tex, i+1,
                D3D12_RESOURCE_STATE_COPY_DEST, D3D12_RESOURCE_STATE_COMMON),
            ]);

            // Don't forget to close command list before executing.
            trace!("clist.close()");
            try!(clist.close());

            trace!("execute_command_lists");
            core.compute_queue.execute_command_lists(&[&clist]);

            let fence_value = core.next_fence_value();
            try!(wait_for_queue(&core.compute_queue, fence_value, &fence, &fence_event));

            cw /= 2;
            ch /= 2;
        }

        Ok(())
    }
}
