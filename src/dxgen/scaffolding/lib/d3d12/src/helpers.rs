use winapi::*;
use super::*; 
use std::ptr;
use std::mem;

pub struct DXGIAdapterIterator<'a> {
    factory: &'a DXGIFactory4,
    cur_num: u32,
}

impl<'a> Iterator for DXGIAdapterIterator<'a> {
    type Item = DXGIAdapter1;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.factory.enum_adapters1(self.cur_num) {
            Ok(adapter) => {
                self.cur_num += 1;
                Some(adapter)
            },
            Err(err) if err == DXGI_ERROR_NOT_FOUND => {
                None
            },
            Err(err) => {
                panic!("Unexpected error while enumerating graphics adapters: 0x{:x}", err)
            }
        }
    }
}

impl<'a> IntoIterator for &'a DXGIFactory4 {
    type Item = DXGIAdapter1;
    type IntoIter = DXGIAdapterIterator<'a>;
    
    fn into_iter(self) -> Self::IntoIter {
        DXGIAdapterIterator {
            factory: self,
            cur_num: 0,
        }
    }
}
