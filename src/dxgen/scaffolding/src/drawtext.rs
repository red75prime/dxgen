use core::DXCore;
use create_device as create;
use dxsafe::*;
use dxsafe::structwrappers::*;
use std::panic;
use std::ptr;
use utils;
use utils::d2d::*;
use winapi::*;

pub struct DrawText {
    _dev11: D3D11Device,
    devcontext11: D3D11DeviceContext,
    dev11on12: D3D11On12Device,
    _devd2d: D2D1Device2,
    factoryd2d: D2D1Factory3,
    devctxd2d: D2D1DeviceContext2,
    factorydw: DWriteFactory,
}

impl DrawText {
    pub fn new(core: &DXCore, debug: bool) -> HResult<DrawText> {
        trace!("Call d3d11on12_create_device");
        let debug_flag = if debug {D3D11_CREATE_DEVICE_DEBUG} else {D3D11_CREATE_DEVICE_FLAG(0)};
        let (dev11, devcontext11) = try!(create::d3d11on12_create_device(
                    &core.dev, 
                    D3D11_CREATE_DEVICE_BGRA_SUPPORT | debug_flag, 
                    &core.graphics_queue));
        trace!("Call dev11.query_interface::<D3D11On12Device>()");
        let dev11on12 = try!(dev11.query_interface::<D3D11On12Device>());
        trace!("Call create_d2d1_factory_single_threaded");
        let factoryd2d = try!(create::create_d2d1_factory_single_threaded::<D2D1Factory3>());
        trace!("Call dev11on12.query_interface::<DXGIDevice>");
        let devdxgi = try!(dev11on12.query_interface::<DXGIDevice>());
        core.dump_info_queue_tagged("drawtext1");
        trace!("Call factoryd2d.create_device3");
        let devd2d = try!(factoryd2d.create_device3(&devdxgi));
        trace!("Call devd2d.create_device_context2");
        let devctxd2d = try!(devd2d.create_device_context2(D2D1_DEVICE_CONTEXT_OPTIONS_NONE));
        trace!("Call create_dwrite_factory_shared");
        let factorydw = try!(create::create_dwrite_factory_shared());
        Ok(DrawText {
            _dev11: dev11,
            devcontext11: devcontext11,
            dev11on12: dev11on12,
            _devd2d: devd2d,
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
    bbrush: D2D1SolidColorBrush,
    gbrush: D2D1LinearGradientBrush,
    tformat: DWriteTextFormat,
    render_trace: bool,
}

impl Drop for DrawTextResources {
    fn drop(&mut self) {
        self.d11context.clear_state();
    }
}

impl DrawTextResources {
    pub fn new(core: &DXCore, dt: &DrawText, rt: &D3D12Resource, _format: DXGI_FORMAT, render_trace: bool) -> HResult<DrawTextResources> {
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
        let bbrush = try!(dt.devctxd2d.create_solid_color_brush(&color3(0.0, 0.0, 0.), None));

        let gradient_stops = [D2D1_GRADIENT_STOP {color: color3(0.2, 1., 0.2), position: 0.},
                             D2D1_GRADIENT_STOP {color: color3(1., 0.2, 0.2), position: 0.5},
                             D2D1_GRADIENT_STOP {color: color3(0.2, 0.2, 1.), position: 1.},];
        let gsc = try!(dt.devctxd2d.create_gradient_stop_collection(&gradient_stops[..], D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP));
        let gbrush = try!(dt.devctxd2d.create_linear_gradient_brush(
                        &D2D1_LINEAR_GRADIENT_BRUSH_PROPERTIES{startPoint: point2f(0., 20.), endPoint: point2f(0., 45.)}, 
                        None, &gsc));

        let tformat = try!(dt.factorydw.create_text_format("Lucida Sans Unicode", 
                                None, DWRITE_FONT_WEIGHT_BOLD, DWRITE_FONT_STYLE_NORMAL, 
                                DWRITE_FONT_STRETCH_NORMAL, 20., "en-GB"));
        
        Ok(DrawTextResources {
            d11context: dt.devcontext11.clone(),
            d11back: d11back,
            d2drt: d2drt,
            calloc: calloc,
            glist: glist,
            tbrush: tbrush,
            bbrush: bbrush,
            gbrush: gbrush,
            tformat: tformat,
            render_trace: render_trace,
        })
    }
    
    pub fn render(&self, core: &DXCore, dt: &DrawText, rt: &D3D12Resource, fps: f32) -> HResult<()> {
        let da = cfg!(debug_assertions) || self.render_trace;
        if da { trace!("DrawTextResources.render"); }
        core.dump_info_queue_tagged("DrawText. Before calloc.reset()");
        if da { trace!("calloc.reset()"); }
        try!(self.calloc.reset());
        let glist = &self.glist;
        if da { trace!("glist.reset"); }
        try!(glist.reset(&self.calloc, None));
        glist.resource_barrier(&[*ResourceBarrier::transition(rt, D3D12_RESOURCE_STATE_COMMON, D3D12_RESOURCE_STATE_RENDER_TARGET)]);
        if da { trace!("glist.close"); }
        try!(glist.close());
        if da { trace!("execute"); }
        core.graphics_queue.execute_command_lists(&[glist]);
        
        core.dump_info_queue_tagged("DrawText. After execute_command_lists (barrier)");
        if da { trace!("acquire"); }
        dt.dev11on12.acquire_wrapped_resources(&[&self.d11back]);
        if da { trace!("set_target"); }
        dt.devctxd2d.set_target(Some(&try!(self.d2drt.query_interface())));
        if da { trace!("begin_draw"); }
        dt.devctxd2d.begin_draw();
        
        let text = format!("FPS: {}\nDWrite and D3D12\n早人间", fps);
        let hello_vec = utils::str_to_vec_u16(&text);

        if da { trace!("create_text_layout"); }
        let tla = try!(dt.factorydw.create_text_layout(&hello_vec[..], &self.tformat, 265., 95.));
        try!(tla.set_font_size(40., text_range(0, 4)));

        if da { trace!("draw_text"); }
        let d = 1.0f32;
        for dx in [-d, 0., d].into_iter() {
            for dy in [-d, 0., d].into_iter() {
                dt.devctxd2d.draw_text_layout(point2f(35.+dx, 5.+dy), &tla, &self.bbrush, D2D1_DRAW_TEXT_OPTIONS_NONE);
            };
        };
        try!(tla.set_drawing_effect(&self.gbrush, text_range(0, 4)));
        dt.devctxd2d.draw_text_layout(point2f(35., 5.), &tla, &self.tbrush, D2D1_DRAW_TEXT_OPTIONS_NONE);
        dt.devctxd2d.draw_rectangle(&rectf(35.-0.5, 5.-0.5, 35.+265.+0.5, 5.+95.+0.5), &self.tbrush, 1., None);
        //dt.devctxd2d.draw_text(&hello_vec[..], &self.tformat, &rectf(35., 5., 300., 100.), &self.tbrush, D2D1_DRAW_TEXT_OPTIONS_NONE, DWRITE_MEASURING_MODE_NATURAL);
        
        if da { trace!("end_draw"); }
        try!(dt.devctxd2d.end_draw(None, None));
        if da { trace!("set_target"); }
        dt.devctxd2d.set_target(None);
        if da { trace!("release_wrapped_resources"); }
        dt.dev11on12.release_wrapped_resources(&[&self.d11back]);
        if da { trace!("flush"); }
        dt.devcontext11.flush();                                        
        core.dump_info_queue_tagged("DrawText. End");
        
        Ok(())
    }
}
