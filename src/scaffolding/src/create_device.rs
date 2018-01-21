//use d3d12_sys::*;
use winapi::*;
use d2d1_sys::*;
use d3dcompiler_sys::*;
use dwrite_sys::*;
use dxgi_sys::*;
use dxsafe::*;
use std::error;
use std::ffi::OsString;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::Read;
use std::os::windows::ffi::OsStrExt;
use std::path::Path;
use std::ptr;

pub fn d3d11on12_create_device(
    dev12: &D3D12Device,
    flags: D3D11_CREATE_DEVICE_FLAG,
    cqueue: &D3D12CommandQueue,
) -> HResult<(D3D11Device, D3D11DeviceContext)> {
    let mut dev11: *mut IUnknown = ptr::null_mut();
    let mut devcontext11: *mut IUnknown = ptr::null_mut();
    let mut cqptr = cqueue.iptr();
    let hr = unsafe {
        D3D11On12CreateDevice(
            dev12.iptr() as *mut _,
            flags.0,                                  // D3D11_CREATE_DEVICE_FLAG
            ptr::null(),                              // feature levels array (optional)
            0,                                        // length of above array
            &mut cqptr as *mut *mut _ as *mut *mut _, // queues array
            1,                                        // length of above array
            0,                                        // Node mask
            &mut dev11 as *mut *mut _ as *mut *mut _,
            &mut devcontext11 as *mut *mut _ as *mut *mut _,
            ptr::null_mut(), // out feature level (optional)
        )
    };
    if SUCCEEDED(hr) {
        Ok((
            D3D11Device::new(dev11),
            D3D11DeviceContext::new(devcontext11),
        ))
    } else {
        Err(hr)
    }
}

pub fn d3d12_create_device(
    adapter: Option<&DXGIAdapter1>,
    min_feat_level: D3D_FEATURE_LEVEL,
) -> HResult<D3D12Device> {
    let mut p_dev: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        D3D12CreateDevice(
            adapter.map(|p| p.iptr()).unwrap_or(ptr::null_mut()),
            min_feat_level,
            &IID_ID3D12Device,
            &mut p_dev as *mut *mut _ as *mut *mut c_void,
        )
    };
    if SUCCEEDED(hr) {
        Ok(D3D12Device::new(p_dev))
    } else {
        Err(hr)
    }
}

#[allow(unused)]
pub fn d3d12_test_create_device(
    adapter: Option<&DXGIAdapter1>,
    min_feat_level: D3D_FEATURE_LEVEL,
) -> HResult<()> {
    let hr = unsafe {
        D3D12CreateDevice(
            adapter.map(|p| p.iptr()).unwrap_or(ptr::null_mut()),
            min_feat_level,
            &IID_ID3D12Device,
            ptr::null_mut(),
        )
    };
    if SUCCEEDED(hr) {
        Ok(())
    } else {
        Err(hr)
    }
}

pub fn create_dxgi_factory2<T: HasIID + TUnknown>(debug: bool) -> HResult<T> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        CreateDXGIFactory2(
            if debug { DXGI_CREATE_FACTORY_DEBUG } else { 0 },
            T::iid(),
            &mut p_fac as *mut *mut _ as *mut *mut _,
        )
    };
    if SUCCEEDED(hr) {
        Ok(T::new(p_fac))
    } else {
        Err(hr)
    }
}

#[allow(unused)]
pub fn create_dxgi_factory1<T: HasIID + TUnknown>() -> HResult<T> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let hr = unsafe { CreateDXGIFactory1(T::iid(), &mut p_fac as *mut *mut _ as *mut *mut _) };
    if SUCCEEDED(hr) {
        Ok(T::new(p_fac))
    } else {
        Err(hr)
    }
}

pub fn create_d2d1_factory_single_threaded<T: HasIID + TUnknown>() -> HResult<T> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let opts = D2D1_FACTORY_OPTIONS {
        debugLevel: D2D1_DEBUG_LEVEL_INFORMATION,
    };
    let hr = unsafe {
        D2D1CreateFactory(
            D2D1_FACTORY_TYPE_SINGLE_THREADED,
            T::iid(),
            &opts as *const _,
            &mut p_fac as *mut *mut _ as *mut *mut _,
        )
    };
    if SUCCEEDED(hr) {
        Ok(T::new(p_fac))
    } else {
        Err(hr)
    }
}

#[allow(unused)]
pub fn create_d2d1_factory_multi_threaded<T: HasIID + TUnknown>() -> HResult<T> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let opts = D2D1_FACTORY_OPTIONS {
        debugLevel: D2D1_DEBUG_LEVEL_INFORMATION,
    };
    let hr = unsafe {
        D2D1CreateFactory(
            D2D1_FACTORY_TYPE_MULTI_THREADED,
            T::iid(),
            &opts as *const _,
            &mut p_fac as *mut *mut _ as *mut *mut _,
        )
    };
    if SUCCEEDED(hr) {
        Ok(T::new(p_fac))
    } else {
        Err(hr)
    }
}

pub fn create_dwrite_factory_shared() -> HResult<DWriteFactory> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        DWriteCreateFactory(
            DWRITE_FACTORY_TYPE_SHARED,
            DWriteFactory::iid(),
            &mut p_fac as *mut *mut _ as *mut *mut _,
        )
    };
    if SUCCEEDED(hr) {
        Ok(DWriteFactory::new(p_fac))
    } else {
        Err(hr)
    }
}

pub fn d3d12_get_debug_interface() -> HResult<D3D12Debug> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        D3D12GetDebugInterface(&IID_ID3D12Debug, &mut p_fac as *mut *mut _ as *mut *mut _)
    };
    if SUCCEEDED(hr) {
        Ok(D3D12Debug::new(p_fac))
    } else {
        Err(hr)
    }
}

pub fn blob_to_string(blob: &D3D10Blob) -> String {
    if blob.get_buffer_size() == 0 {
        "".into()
    } else {
        let blob_slice: &[u8] = unsafe {
            ::std::slice::from_raw_parts(
                blob.get_buffer_pointer() as *mut u8,
                blob.get_buffer_size() as usize,
            )
        };
        let mut ret = vec![];
        ret.extend_from_slice(blob_slice);
        String::from_utf8_lossy(&ret[..]).into_owned()
    }
}

pub fn blob_as_slice(blob: &D3D10Blob) -> &[u8] {
    if blob.get_buffer_size() == 0 {
        &[]
    } else {
        unsafe {
            ::std::slice::from_raw_parts(
                blob.get_buffer_pointer() as *mut u8,
                blob.get_buffer_size() as usize,
            )
        }
    }
}

pub fn blob_to_vec(blob: &D3DBlob) -> Vec<u8> {
    blob_as_slice(blob).to_vec()
}

#[allow(unused)]
pub fn d3d12_serialize_root_signature(
    root_signature: &D3D12_ROOT_SIGNATURE_DESC,
    ver: D3D_ROOT_SIGNATURE_VERSION,
) -> HResult<D3D10Blob> {
    let mut p_blob: *mut IUnknown = ptr::null_mut();
    let mut p_err: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        D3D12SerializeRootSignature(
            root_signature,
            ver,
            &mut p_blob as *mut *mut _ as *mut *mut _,
            &mut p_err as *mut *mut _ as *mut *mut _,
        )
    };
    if SUCCEEDED(hr) {
        Ok(D3D10Blob::new(p_blob))
    } else {
        if !p_err.is_null() {
            error!("{}", blob_to_string(&D3D10Blob::new(p_err)));
        }
        Err(hr)
    }
}

pub fn d3d_compile_from_str(
    shader_text: &str,
    shader_name: &str,
    entry: &str,
    target: &str,
    flags: u32,
) -> Result<Vec<u8>, String> {
    let cstr_entry = str_to_cstring(entry);
    let cstr_target = str_to_cstring(target);
    let mut code_blob: *mut ID3DBlob = ptr::null_mut();
    let mut error_blob: *mut ID3DBlob = ptr::null_mut();
    let hr = unsafe {
        D3DCompile2(
            shader_text.as_ptr() as *const _,
            shader_text.len() as SIZE_T,
            str_to_cstring(shader_name).as_ptr(),
            ptr::null(),
            D3D_COMPILE_STANDARD_FILE_INCLUDE,
            cstr_entry.as_ptr(),
            cstr_target.as_ptr(),
            flags,
            0,
            0,
            ptr::null(),
            0,
            &mut code_blob,
            &mut error_blob,
        )
    };
    if SUCCEEDED(hr) {
        if !error_blob.is_null() {
            warn!("{}", blob_to_string(&D3DBlob::new(error_blob as *mut _)));
        };
        assert!(!code_blob.is_null());
        Ok(blob_to_vec(&D3DBlob::new(code_blob as *mut _)))
    } else {
        assert!(!error_blob.is_null());
        Err(blob_to_string(&D3DBlob::new(error_blob as *mut _)))
    }
}

#[allow(unused)]
pub fn d3d_get_root_signature_from_str(
    shader_text: &str,
    dev: &D3D12Device,
) -> Result<D3D12RootSignature, HRESULT> {
    let mut root_sign_blob: *mut ID3DBlob = ptr::null_mut();
    let hr = unsafe {
        D3DGetBlobPart(
            shader_text.as_ptr() as *mut _,
            shader_text.len() as SIZE_T,
            D3D_BLOB_ROOT_SIGNATURE,
            0,
            &mut root_sign_blob,
        )
    };
    if SUCCEEDED(hr) {
        dev.create_root_signature(0, blob_as_slice(&D3DBlob::new(root_sign_blob as *mut _)))
    } else {
        Err(hr)
    }
}

use std::ffi::CString;

pub fn str_to_cstring(s: &str) -> CString {
    CString::new(s).unwrap()
}

#[derive(Debug)]
pub enum ShaderCompileError {
    Io(io::Error),
    Utf8(::std::str::Utf8Error),
    Compile(String),
}

impl fmt::Display for ShaderCompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ShaderCompileError::Io(ref err) => write!(f, "IO error: {}", err),
            &ShaderCompileError::Utf8(ref err) => write!(f, "Utf8 error: {}", err),
            &ShaderCompileError::Compile(ref err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for ShaderCompileError {
    fn description(&self) -> &str {
        match self {
            &ShaderCompileError::Io(ref err) => err.description(),
            &ShaderCompileError::Utf8(ref err) => err.description(),
            &ShaderCompileError::Compile(ref err) => err.as_ref(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match self {
            &ShaderCompileError::Io(ref err) => Some(err),
            &ShaderCompileError::Utf8(ref err) => Some(err),
            &ShaderCompileError::Compile(ref _err) => None,
        }
    }
}

impl From<io::Error> for ShaderCompileError {
    fn from(err: io::Error) -> ShaderCompileError {
        ShaderCompileError::Io(err)
    }
}

impl From<::std::str::Utf8Error> for ShaderCompileError {
    fn from(err: ::std::str::Utf8Error) -> ShaderCompileError {
        ShaderCompileError::Utf8(err)
    }
}

pub fn compile_shaders(
    file_name: &str,
    func_names: &mut [(&'static str, &'static str, &mut Vec<u8>)],
    flags: u32,
) -> Result<(), ShaderCompileError> {
    let paths = ["./", "../../src/shaders/", "./shaders/"];
    let mut f = None;
    let mut fpath = Path::new(file_name).to_path_buf();
    for &path in &paths {
        fpath = Path::new(path).join(file_name);
        match File::open(&fpath) {
            Ok(file) => {
                f = Some(file);
                break;
            }
            Err(_) => (),
        };
    }
    let mut f = if let Some(f) = f {
        f
    } else {
        return Err(ShaderCompileError::Io(io::Error::new(
            io::ErrorKind::NotFound,
            format!("File not found: {}", file_name),
        )));
    };

    let mut content = vec![];
    try!(f.read_to_end(&mut content));
    let shader = try!(::std::str::from_utf8(&content[..]));

    let all_ok: Result<Vec<_>, _> = func_names
        .iter_mut()
        .map(|&mut (fname, target, ref mut bytecode)| {
            match d3d_compile_from_str(
                shader,
                fpath.to_string_lossy().as_ref(),
                fname,
                target,
                flags,
            ) {
                Ok(mut bc) => {
                    bytecode.truncate(0);
                    bytecode.append(&mut bc);
                    Ok(())
                }
                Err(err) => Err(ShaderCompileError::Compile(format!("{}: {}", fname, err))),
            }
        })
        .collect();
    all_ok?;
    Ok(())
}
