#![cfg(feature = "openal")]

extern crate openal;
use self::openal::al;
use self::openal::alc;

pub fn init() {
  if let Some(device) = alc::Device::open(None) {
    println!("Init sound");
  } else {
    println!("Init sound failure");
  }
}
