extern crate image;

use core::DXCore;
use create_device as cdev;
use downsampler::Downsampler;
use dxsafe::*;
use dxsafe::structwrappers::*;
use self::image::hdr;
use std::fs;
use std::io;
use utils;
use winapi::*;

pub struct Skybox {
    root_sig: D3D12RootSignature,
    pso: D3D12PipelineState,
    skytex: D3D12Resource,
    dheap: DescriptorHeap,
}

impl Skybox {
    pub fn new(core: &DXCore, downsampler: &Downsampler) -> HResult<Skybox> {
        let f = try!(fs::File::open("assets/skybox.hdr").or(fs::File::open("skybox.hdr"))
                .map_err(|_|{ error!("'skybox.hdr' is not found"); ERROR_FILE_NOT_FOUND as HRESULT }));
        let reader = io::BufReader::new(f);
        let decoder = try!(hdr::HDRDecoder::new(reader)
                .map_err(|_|{ error!("Cannot parse 'skybox.hdr'"); ERROR_INVALID_DATA as HRESULT }));
        let meta = decoder.metadata();
        let data = try!(decoder.read_image_native()
                .map_err(|err|{ error!("Cannot read 'skybox.hdr': {}", err); ERROR_INVALID_DATA as HRESULT }));
        
        unimplemented!()
    }
}
