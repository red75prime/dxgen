#![feature(libc)]

extern crate libc;

mod macros;
mod iid;
mod d3d12_sys;
mod d3d12;
mod create_device;

use d3d12_sys::*;
use create_device::*;
use iid::HasIID;

#[link(name="d3d12")]
#[link(name="dxguid")]
#[link(name="dxgi")]
extern {}

fn main() {
  let dev=match d3d12_create_device(D3D_FEATURE_LEVEL_11_1) {
    Ok(dev) => dev,
    Err(hr) => {
      println!("Cannot create device. Error {:X}", hr);
      return;
    },
  };
  let mut opts=D3D12_FEATURE_DATA_D3D12_OPTIONS{ ..Default::default() };
  dev.check_feature_support_options(&mut opts).unwrap();
  println!("{:#?}",opts);

  let luid=dev.get_adapter_luid();
  println!("{:?}", luid);

  let ca=match dev.create_command_allocator(D3D12_COMMAND_LIST_TYPE_DIRECT) {
    Ok(ca) => ca,
    Err(hr) => {
     println!("Command allocator creation error {:X}",hr);
     return;
    },
  };
  println!("{:?}", ca.iptr());

  let cqd=D3D12_COMMAND_QUEUE_DESC { Type : D3D12_COMMAND_LIST_TYPE_DIRECT, ..Default::default() };
  let cq=match dev.create_command_queue(&cqd) {
    Ok(cq) => cq,
    Err(hr) => {
     println!("Command queue creation error {:X}",hr);
     return;
    },
  };
  println!("{:?}", cq.iptr());
 
  if let Ok(factory)=create_dxgi_factory1() {
    if let Ok(adapter0)=factory.enum_adapters1(0) {
      if let Ok(output0)=adapter0.enum_outputs(0) {
        for fnum in 0..130 {
          let format=DXGI_FORMAT(fnum);
          let num_modes=output0.get_display_mode_list(format, 0, None).unwrap();
          if num_modes!=0 {
            println!("Num modes: {} for {:?}", num_modes, format);
            let mut modes=vec![DXGI_MODE_DESC::default(); num_modes as usize];
            output0.get_display_mode_list(format, 0, Some(&mut modes[..]));
            for mode in modes {
              println!("{:?}", mode);
            }
          }
        }
      }
    }
  }
}

