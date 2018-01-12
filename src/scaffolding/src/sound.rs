#![cfg(feature = "openal")]

extern crate alto;

use self::alto::{Alto, Mono, Source};
use std::sync::Arc;
use std::error::Error;

pub fn init() {
  match initr() {
    Err(err) => {
      println!("Sound init error {}", err);
    }
    _ => (),
  }
}

pub fn initr() -> Result<(), Box<Error>> {
  let alto = Alto::load_default()?;
  println!("Init sound");
  let device = alto.open(None)?;
  let ctxt = device.new_context(None)?;
  let b_len = 2048;
  let snd: Vec<i16> = 
    (0..b_len)
      .map(|i: usize| {
          let fi = (i as f32)/(b_len as f32);
          ((f32::sin(fi*200.*2.*3.14159265)*1000.) + (f32::sin(fi*210.*2.*3.14159265)*2000.)) as i16
        })
      .collect();
  let buf = Arc::new(ctxt.new_buffer::<Mono<i16>, _>(snd, 22010)?);

  ctxt.set_position([0., 0., 0.])?;
  ctxt.set_orientation(([0., 0., 1.], [0., 1., 0.]))?;

  let mut src = ctxt.new_static_source()?;
  src.set_buffer(buf)?;
  src.set_gain(1.)?;
  src.set_pitch(1.)?;
  src.set_position([0., 0., 0.])?;
  src.set_velocity([0., 0., 0.])?;
  src.set_direction([0., 0., 1.])?;
  src.set_looping(true);
  src.play();
  ::std::thread::sleep(::std::time::Duration::from_secs(1));
  Ok(())
}
