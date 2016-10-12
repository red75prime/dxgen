use winapi::*;
use kernel32::*;

pub struct SehGuard {
    token: PVOID,
}

unsafe extern "system"
fn continuator(ex_info: *mut EXCEPTION_POINTERS) -> LONG {
    let exr = &*(*ex_info).ExceptionRecord;
    // GPU-Based validation produces continuable exceptions with code 0x87d
    // ignore them 
    if exr.ExceptionFlags == 0 && exr.ExceptionCode == 0x87d { 
        -1 //EXCEPTION_CONTINUE_EXECUTION
    } else {
        0 //EXCEPTION_CONTINUE_SEARCH
    }
}

impl SehGuard {
    pub fn new() -> SehGuard {
        let token = unsafe { AddVectoredExceptionHandler(0, Some(continuator)) };
        if token.is_null() {
            panic!("Cannot set SEH guard".to_string());
        };
        SehGuard {
            token: token,
        }
    }
}

impl Drop for SehGuard {
    fn drop(&mut self) {
        let success = unsafe{ RemoveVectoredExceptionHandler(self.token) };
        if success == 0 {
            panic!("Cannot remove SEH guard".to_string());
        } 
    }
}

#[test]
fn test() {
    let _guard = SehGuard::new();
    unsafe{ RaiseException(0x87d, 0, 0, ::std::ptr::null()) };
}
