#![cfg(feature = "openal")]

extern crate openal;
use self::openal::al;
use self::openal::alc;

pub fn init() {
  if let Some(device) = alc::Device::open(None) {
    println!("Init sound");
    if let Some(ctxt) = device.create_context(&[]) {
      ctxt.make_current();
      let buf = al::Buffer::gen();
      let b_len = 2048;
      let snd: Vec<i16> = 
        (0..b_len)
          .map(|i: usize| {
              let fi = (i as f32)/(b_len as f32);
              (f32::sin(fi*300.*2.*3.14159265)*6000.) as i16
            })
          .collect();
      unsafe {
        buf.buffer_data(al::Format::Mono16, &snd, 22010);
      };

      let src = al::Source::gen();
      src.set_buffer(buf);
      src.set_looping(true);
      src.play();
      //::std::thread::sleep(::std::time::Duration::from_secs(10));
    }
  } else {
    println!("Init sound failure");
  }
}
