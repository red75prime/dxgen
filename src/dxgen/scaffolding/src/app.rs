// use core::*;
// use winapi::*;
// pub trait PEnv {
// fn resize(&self, w: u32, h: u32, fullscreen: bool) -> Result<(),()>;
// fn quit(&self);
// }
//
// pub trait App {
// fn new<Env: PEnv>(env: &Env) -> Result<Self, ()>;
// fn on_render(&mut self, env: &Env) -> Result<(),()>;
// fn on_message(&mut self, env: &Env, msg: &MSG) -> Result<(),()>;
//
// fn core(&self) -> &DXCore;
// fn sc(&self) -> &DXSwapChain;
//
// fn wait_for_command_queue(&self, queue: &D3D12CommandQueue) -> Result<(),()>;
//
// fn on_resize(&mut self, w: u32, h: u32) -> Result<(),()> {
// }
// }
//
