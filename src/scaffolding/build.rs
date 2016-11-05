#[cfg(all(windows, feature = "openal", target_env="msvc", target_pointer_width="32"))]
fn main() {
  println!("cargo:rustc-link-search=native=C:\\Program Files (x86)\\OpenAL 1.1 SDK\\libs\\Win32");
  println!("cargo:rustc-link-search=native=C:\\Program Files\\OpenAL 1.1 SDK\\libs\\Win32");
}

#[cfg(all(windows, feature = "openal", target_env="msvc", target_pointer_width="64"))]
fn main() {
  println!("cargo:rustc-link-search=native=C:\\Program Files (x86)\\OpenAL 1.1 SDK\\libs\\Win64");
  println!("cargo:rustc-link-search=native=C:\\Program Files\\OpenAL 1.1 SDK\\libs\\Win64");
}

#[cfg(not(feature = "openal"))]
fn main() {
//  println!("cargo:rustc-link-search=native=C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.10166.0\\um\\x86");
}
