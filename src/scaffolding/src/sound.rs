#![cfg(feature = "openal")]

extern crate openal;
use self::openal::al;
use self::openal::alc;

fn error(msg: &str) -> bool {
    if let Some(err) = al::get_error() {
      println!("{}: {:?}", msg, err);
      true
    } else {
      false
    }
}

pub fn init() {
  if let Some(device) = alc::Device::open(None) {
    println!("Init sound");
    if let Some(ctxt) = device.create_context(&[]) {
      if ! ctxt.make_current() {
        println!("Cannot make current");
        return;
      }
      let buf = al::Buffer::gen();
      if error("Cannot gen buffer") { return; }
      let b_len = 2048;
      let snd: Vec<i16> = 
        (0..b_len)
          .map(|i: usize| {
              let fi = (i as f32)/(b_len as f32);
              ((f32::sin(fi*200.*2.*3.14159265)*1000.) + (f32::sin(fi*210.*2.*3.14159265)*2000.)) as i16
            })
          .collect();
      unsafe {
        buf.buffer_data(al::Format::Mono16, &snd, 22010);
      };
      if error("Cannot set buffer data") { return; }

      al::listener::set_position([0., 0., 0.]);
      al::listener::set_orientation([0., 0., 1.], [0., 1., 0.]);
      if error("Cannot set listener orientation") { return; }

      let src = al::Source::gen();
      if error("Cannot gen source") { return; }
      src.set_gain(1.);
      src.set_pitch(1.);
      src.set_position([0., 0., 0.]);
      src.set_velocity([0., 0., 0.]);
      src.set_direction([0., 0., 1.]);
      if error("Cannot set source parameters") { return; }
      src.set_looping(true);
      if error("Cannot set source looping") { return; }
      src.queue_buffer(&buf);
      if error("Cannot queue buffer") { return; }
      src.play();
      if error("Cannot play source") { return; }
      
      ::std::thread::sleep(::std::time::Duration::from_secs(1));
    }
  } else {
    println!("Init sound failure");
  }
}
