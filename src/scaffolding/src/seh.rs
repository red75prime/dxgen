use kernel32::*;
use winapi::*;
use std::sync::RwLock;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CtrlCHandlerStatus {
    NotInstalled,
    Installed(bool), // Installed(true) if ctrl-c was pressed
}

use self::CtrlCHandlerStatus::{NotInstalled, Installed};

lazy_static!(
    static ref HANDLER_STATUS: RwLock<CtrlCHandlerStatus> = RwLock::new(NotInstalled);
);

pub fn install_ctrl_c_handler() {
    let mut w = HANDLER_STATUS.write().expect("Poisoned");
    if *w == NotInstalled {
        let res = unsafe { SetConsoleCtrlHandler(Some(native_ctrl_c_handler), TRUE) };
        if res == 0 {
            panic!("Cannot install console ctrl handler".to_string());
        }
        *w = Installed(false);
    }
}

pub fn ctrl_c_is_triggered() -> bool {
    // unwrap is fine. Bail out 
    let r = HANDLER_STATUS.read().expect("Poisoned");
    match *r {
        NotInstalled => false,
        Installed(triggered) => triggered,
    }
}

pub fn uninstall_ctrl_c_handler() {
    if let Ok(mut w) = HANDLER_STATUS.write() {
        let installed = match *w {
            Installed(_) => true,
            NotInstalled => false,
        }; 
            
        if installed  {
            let res = unsafe { SetConsoleCtrlHandler(Some(native_ctrl_c_handler), FALSE) };
            if res == 0 {
                panic!("Cannot uninstall console ctrl handler".to_string());
            }
            *w = NotInstalled;
        }
    }
}

unsafe extern "system"
fn native_ctrl_c_handler(ctrl_type: DWORD) -> BOOL {
    ctrl_c_handler(ctrl_type)
}

fn ctrl_c_handler(ctrl_type: DWORD) -> BOOL {
    if ctrl_type == CTRL_C_EVENT {
        let mut w = HANDLER_STATUS.write().expect("Poisoned");
        if *w != NotInstalled {
            *w = Installed(true);
        };
        TRUE
    } else {
        FALSE
    }
}
