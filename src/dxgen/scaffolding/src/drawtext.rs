use winapi::*;
use dx_safe::*;
use core::DXCore;
use create_device as create;
use std::ptr;
use dx_safe::structwrappers::*;
use utils::d2d::*;

pub struct DrawText {
    dev11: D3D11Device,
    devcontext11: D3D11DeviceContext,
    dev11on12: D3D11On12Device,
    devd2d: D2D1Device2,
    factoryd2d: D2D1Factory3,
    devctxd2d: D2D1DeviceContext2,
    factorydw: DWriteFactory,
}

impl DrawText {
    pub fn new(core: &DXCore) -> HResult<DrawText> {
        trace!("Call d3d11on12_create_device");
        let (dev11, devcontext11) = try!(create::d3d11on12_create_device(
                    &core.dev, 
                    D3D11_CREATE_DEVICE_BGRA_SUPPORT | D3D11_CREATE_DEVICE_DEBUG, 
                    &core.graphics_queue));
        trace!("Call dev11.query_interface::<D3D11On12Device>()");
        let dev11on12 = try!(dev11.query_interface::<D3D11On12Device>());
        trace!("Call create_d2d1_factory_single_threaded");
        let factoryd2d = try!(create::create_d2d1_factory_single_threaded::<D2D1Factory3>());
        trace!("Call dev11on12.query_interface::<DXGIDevice>");
        let devdxgi = try!(dev11on12.query_interface::<DXGIDevice>());
        core.dump_info_queue();
        trace!("Call factoryd2d.create_device3");
        let devd2d = try!(factoryd2d.create_device3(&devdxgi));
        trace!("Call devd2d.create_device_context2");
        let devctxd2d = try!(devd2d.create_device_context2(D2D1_DEVICE_CONTEXT_OPTIONS_NONE));
        trace!("Call create_dwrite_factory_shared");
        let factorydw = try!(create::create_dwrite_factory_shared());
        Ok(DrawText {
            dev11: dev11,
            devcontext11: devcontext11,
            dev11on12: dev11on12,
            devd2d: devd2d,
            factoryd2d: factoryd2d,
            devctxd2d: devctxd2d,
            factorydw: factorydw,
        })
    }
}

pub struct DrawTextResources {
    d11context: D3D11DeviceContext,
    d11back: D3D11Resource,
    d2drt: D2D1Bitmap1,
    glist: D3D12GraphicsCommandList,
    calloc: D3D12CommandAllocator,
    tbrush: D2D1SolidColorBrush,
    tformat: DWriteTextFormat,
    
}

impl Drop for DrawTextResources {
    fn drop(&mut self) {
        self.d11context.clear_state();
    }
}

impl DrawTextResources {
    pub fn new(core: &DXCore, dt: &DrawText, rt: &D3D12Resource, format: DXGI_FORMAT) -> HResult<DrawTextResources> {
        let d3d11_res_flags = D3D11_RESOURCE_FLAGS {
            BindFlags: D3D11_BIND_RENDER_TARGET.0,
            MiscFlags: 0,
            CPUAccessFlags: 0,
            StructureByteStride: 0,
        };
        let d11back: D3D11Resource = try!(dt.dev11on12.create_wrapped_resource(
            rt, &d3d11_res_flags, 
            D3D12_RESOURCE_STATE_RENDER_TARGET,
            D3D12_RESOURCE_STATE_PRESENT
        ));

        let (dpi_x, dpi_y) = dt.factoryd2d.get_desktop_dpi();
        info!("Desktop DPI: {}, {}", dpi_x, dpi_y);
        let bmp_props = D2D1_BITMAP_PROPERTIES1 {
            pixelFormat: D2D1_PIXEL_FORMAT {
              format: DXGI_FORMAT_UNKNOWN,
              alphaMode: D2D1_ALPHA_MODE_PREMULTIPLIED,
            },
            dpiX: dpi_x,
            dpiY: dpi_y,
            bitmapOptions: D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
            colorContext: ptr::null_mut(), 
        };
        let surf = try!(d11back.query_interface::<DXGISurface>()); 
        let d2drt = try!(dt.devctxd2d.create_bitmap_from_dxgi_surface(
            &surf,
            &bmp_props        
        ));


        let calloc = try!(core.dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT));
        let glist: D3D12GraphicsCommandList =
            try!(core.dev.create_command_list(0, D3D12_COMMAND_LIST_TYPE_DIRECT, &calloc, None));
        try!(glist.close());
        
        let tbrush = try!(dt.devctxd2d.create_solid_color_brush(&color3(0.9, 0.9, 1.), None));
        let tformat = try!(dt.factorydw.create_text_format("Lucida Sans Unicode".into(), None, DWRITE_FONT_WEIGHT_NORMAL,
                                DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL, 16., "en-GB".into()));
        
        Ok(DrawTextResources {
            d11context: dt.devcontext11.clone(),
            d11back: d11back,
            d2drt: d2drt,
            calloc: calloc,
            glist: glist,
            tbrush: tbrush,
            tformat: tformat,
        })
    }
    
    pub fn render(&self, core: &DXCore, dt: &DrawText, rt: &D3D12Resource) -> HResult<()> {
        //return Ok(());
        trace!("DrawTextResources.render");
        trace!("calloc.reset()");
        try!(self.calloc.reset());
        let glist = &self.glist;
        trace!("glist.reset");
        try!(glist.reset(&self.calloc, None));
        glist.resource_barrier(&[*ResourceBarrier::transition(rt, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_RENDER_TARGET)]);
        trace!("glist.close");
        try!(glist.close());
        trace!("execute");
        core.graphics_queue.execute_command_lists(&[glist]);
        
        core.dump_info_queue();
        trace!("acquire");
        dt.dev11on12.acquire_wrapped_resources(&[&self.d11back]);
        trace!("set_target");
        dt.devctxd2d.set_target(Some(&try!(self.d2drt.query_interface())));
        trace!("begin_draw");
        dt.devctxd2d.begin_draw();
        
        let hello_vec = ::utils::str_to_vec_u16("DWrite and D3D12\nIt works at last\n你好，世界");
        trace!("draw_text");
        dt.devctxd2d.draw_text(&hello_vec[..], &self.tformat, &rectf(35., 5., 300., 100.), &self.tbrush, D2D1_DRAW_TEXT_OPTIONS_NONE, DWRITE_MEASURING_MODE_NATURAL);
        
        trace!("end_draw");
        try!(dt.devctxd2d.end_draw(None, None));
        trace!("set_target");
        dt.devctxd2d.set_target(None);
        trace!("release_wrapped_resources");
        dt.dev11on12.release_wrapped_resources(&[&self.d11back]);
        trace!("flush");
        dt.devcontext11.flush();                                        
        
        Ok(())
    }
}
