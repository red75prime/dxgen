#![cfg(feature = "openal")]

extern crate openal;
use self::openal::al;
use self::openal::alc;

pub fn init() {
  println!("Init sound");
  let device = alc::Device::open(None).expect("Could not open device");
}
