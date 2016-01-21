#![allow(dead_code)]

use winapi::*;
use dx_safe::*;
use std::ptr;

#[link(name="d3d12")]
extern "system" {
    pub fn D3D12CreateDevice(pAdapter: *mut IUnknown,
                             MinimumFeatureLevel: D3D_FEATURE_LEVEL,
                             riid: REFGUID,
                             ppDevice: *mut *mut c_void)
                             -> HRESULT;
    pub fn D3D12CreateRootSignatureDeserializer(pSrcData: LPCVOID,
                                                SrcDataSizeInBytes: SIZE_T,
                                                pRootSignatureDeserializerInterface: REFGUID,
                                                ppRootSignatureDeserializer: *mut *mut c_void)
                                                -> HRESULT;
    pub fn D3D12GetDebugInterface(riid: REFGUID, ppvDebug: *mut *mut c_void) -> HRESULT;
    pub fn D3D12SerializeRootSignature(pRootSignature: *const D3D12_ROOT_SIGNATURE_DESC,
                                       Version: D3D_ROOT_SIGNATURE_VERSION,
                                       ppBlob: *mut *mut ID3DBlob,
                                       ppErrorBlob: *mut *mut ID3DBlob)
                                       -> HRESULT;
}

#[link(name="dxgi")]
extern "system" {
    pub fn CreateDXGIFactory2(Flags: UINT, riid: REFGUID, ppFactory: *mut *mut c_void) -> HRESULT;
    pub fn DXGIGetDebugInterface1(Flags: UINT, riid: REFGUID, pDebug: *mut *mut c_void) -> HRESULT;
}


pub fn d3d12_create_device(adapter: Option<&DXGIAdapter1>,
                           min_feat_level: D3D_FEATURE_LEVEL)
                           -> HResult<D3D12Device> {
    let mut p_dev: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        D3D12CreateDevice(adapter.map(|p| p.iptr()).unwrap_or(ptr::null_mut()),
                          min_feat_level,
                          &IID_ID3D12Device,
                          &mut p_dev as *mut *mut _ as *mut *mut c_void)
    };
    if hr == 0 {
        Ok(D3D12Device::new(p_dev))
    } else {
        Err(hr)
    }
}

pub fn create_dxgi_factory2<T: HasIID>() -> HResult<T> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    if cfg!(debug_assertions) {
        debug!("cfg!(debug_assertions) is true");
    } else {
        debug!("cfg!(debug_assertions) is false");
    };
    let hr = unsafe {
        CreateDXGIFactory2(if cfg!(debug_assertions) {
                               DXGI_CREATE_FACTORY_DEBUG
                           } else {
                               0
                           },
                           T::iid(),
                           &mut p_fac as *mut *mut _ as *mut *mut _)
    };
    if hr == 0 {
        Ok(T::new(p_fac))
    } else {
        Err(hr)
    }
}

pub fn get_debug_interface() -> HResult<D3D12Debug> {
    let mut p_fac: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        D3D12GetDebugInterface(&IID_ID3D12Debug, &mut p_fac as *mut *mut _ as *mut *mut _)
    };
    if hr == 0 {
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
            ::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8,
                                         blob.get_buffer_size() as usize)
        };
        let mut ret = vec![];
        ret.extend(blob_slice);
        String::from_utf8_lossy(&ret[..]).into_owned()
    }
}

pub fn blob_as_slice<'a>(blob: &'a D3D10Blob) -> &'a [u8] {
    if blob.get_buffer_size() == 0 {
        &[]
    } else {
        unsafe {
            ::std::slice::from_raw_parts(blob.get_buffer_pointer() as *mut u8,
                                         blob.get_buffer_size() as usize)
        }
    }
}

pub fn blob_to_vec(blob: &D3DBlob) -> Vec<u8> {
    blob_as_slice(blob).to_vec()
}

pub fn d3d12_serialize_root_signature(root_signature: &D3D12_ROOT_SIGNATURE_DESC,
                                      ver: D3D_ROOT_SIGNATURE_VERSION)
                                      -> HResult<D3D10Blob> {
    let mut p_blob: *mut IUnknown = ptr::null_mut();
    let mut p_err: *mut IUnknown = ptr::null_mut();
    let hr = unsafe {
        D3D12SerializeRootSignature(root_signature,
                                    ver,
                                    &mut p_blob as *mut *mut _ as *mut *mut _,
                                    &mut p_err as *mut *mut _ as *mut *mut _)
    };
    if hr == 0 {
        Ok(D3D10Blob::new(p_blob))
    } else {
        if p_err != ptr::null_mut() {
            error!("{}", blob_to_string(&D3D10Blob::new(p_err)));
        }
        Err(hr)
    }
}

// TODO: generate winapi bindings
#[repr(C)]
pub struct D3D_BLOB_PART(UINT);
pub const D3D_BLOB_ROOT_SIGNATURE: D3D_BLOB_PART = D3D_BLOB_PART(11);
pub const D3DCOMPILE_OPTIMIZATION_LEVEL3: u32 = 1 << 15;
pub const D3DCOMPILE_OPTIMIZATION_LEVEL2: u32 = ((1 << 14) | (1 << 15));
pub const D3DCOMPILE_OPTIMIZATION_LEVEL1: u32 = 0;
pub const D3DCOMPILE_OPTIMIZATION_LEVEL0: u32 = 1 << 14;
pub const D3DCOMPILE_ENABLE_STRICTNESS: u32 = (1 << 11);

#[link(name="d3dcompiler")]
extern "system" {
    fn D3DCompileFromFile(file_name: LPCWSTR,
                          defines: *const D3D_SHADER_MACRO,
                          include: *mut ID3DInclude,
                          entry: LPCSTR,
                          target: LPCSTR,
                          flags1: UINT,
                          flags2: UINT,
                          code: *mut *mut ID3D10Blob,
                          errs: *mut *mut ID3D10Blob)
                          -> HRESULT;

    fn D3DCompile2(pSrcData: LPCVOID,
                   SrcDataSize: SIZE_T,
                   pSourceName: LPCSTR,
                   pDefines: *const D3D_SHADER_MACRO,
                   pInclude: *mut ID3DInclude,
                   pEntrypoint: LPCSTR,
                   pTarget: LPCSTR,
                   Flags1: UINT,
                   Flags2: UINT,
                   SecondaryDataFlags: UINT,
                   pSecondaryData: LPCVOID,
                   SecondaryDataSize: SIZE_T,
                   ppCode: *mut *mut ID3DBlob,
                   ppErrorMsgs: *mut *mut ID3DBlob)
                   -> HRESULT;
    fn D3DGetBlobPart(pSrcData: LPCVOID,
                      SrcDataSize: SIZE_T,
                      Part: D3D_BLOB_PART,
                      Flags: UINT,
                      ppBlob: *mut *mut ID3DBlob)
                      -> HRESULT;
    fn D3DReflect(pSrcData: LPCVOID,
                  SrcDataSize: SIZE_T,
                  pInterface: REFGUID,
                  ppReflector: *mut *mut c_void)
                  -> HRESULT;
}

pub fn d3d_compile_from_str(shader_text: &str, shader_name: &str,
                            entry: &str,
                            target: &str,
                            flags: u32)
                            -> Result<Vec<u8>, String> {
    let cstr_entry = str_to_cstring(entry);
    let cstr_target = str_to_cstring(target);
    let mut code_blob: *mut ID3DBlob = ptr::null_mut();
    let mut error_blob: *mut ID3DBlob = ptr::null_mut();
    let hr = unsafe {
        D3DCompile2(shader_text.as_ptr() as *const _,
                    shader_text.len() as SIZE_T,
                    str_to_cstring(shader_name).as_ptr(),
                    ptr::null(),
                    ptr::null_mut(),
                    cstr_entry.as_ptr(),
                    cstr_target.as_ptr(),
                    flags,
                    0,
                    0,
                    ptr::null(),
                    0,
                    &mut code_blob,
                    &mut error_blob)
    };
    if SUCCEEDED(hr) {
        if error_blob != ptr::null_mut() {
            warn!("{}", blob_to_string(&D3DBlob::new(error_blob as *mut _)));
        };
        assert!(code_blob != ptr::null_mut());
        Ok(blob_to_vec(&D3DBlob::new(code_blob as *mut _)))
    } else {
        assert!(error_blob != ptr::null_mut());
        Err(blob_to_string(&D3DBlob::new(error_blob as *mut _)))
    }
}

pub fn d3d_get_root_signature_from_str(shader_text: &str,
                                       dev: &D3D12Device)
                                       -> Result<D3D12RootSignature, HRESULT> {
    let mut root_sign_blob: *mut ID3DBlob = ptr::null_mut();
    let hr = unsafe {
        D3DGetBlobPart(shader_text.as_ptr() as *mut _,
                       shader_text.len() as SIZE_T,
                       D3D_BLOB_ROOT_SIGNATURE,
                       0,
                       &mut root_sign_blob)
    };
    if SUCCEEDED(hr) {
        dev.create_root_signature(0, blob_as_slice(&D3DBlob::new(root_sign_blob as *mut _)))
    } else {
        Err(hr)
    }
}

pub fn d3d_compile_from_file(file_name: &str,
                             entry: &str,
                             target: &str,
                             flags: UINT)
                             -> Result<Vec<u8>, String> {
    let w_file_name = str_to_vec_u16(file_name);
    let w_entry = str_to_cstring(entry);
    let w_target = str_to_cstring(target);
    let mut blob1: *mut ID3D10Blob = ptr::null_mut();
    let mut blob2: *mut ID3D10Blob = ptr::null_mut();
    let hr = unsafe {
        D3DCompileFromFile(w_file_name.as_ptr(),
                           ptr::null_mut(),
                           ptr::null_mut(),
                           w_entry.as_ptr(),
                           w_target.as_ptr(),
                           flags,
                           0,
                           &mut blob1,
                           &mut blob2)
    };
    if blob2 != ptr::null_mut() {
        let blob = D3D10Blob::new(blob2 as *mut _);
        Err(blob_to_string(&blob))
    } else {
        if hr == 0 {
            assert!(blob1 != ptr::null_mut());
            let blob = D3D10Blob::new(blob1 as *mut _);
            Ok(blob_to_vec(&blob))
        } else {
            Err("".into())
        }
    }
}


pub fn compile_shader_and_root_signature(text: &str, file_name: &str,
                                         fname: &str,
                                         rsname: &str,
                                         flags: u32)
                                         -> Result<(Vec<u8>, Vec<u8>), ()> {
    let shader_bytecode = match d3d_compile_from_str(text, file_name, fname, "cs_5_1", flags) {
        Ok(sbc) => sbc,
        Err(err) => {
            error!("Shader '{}' compile error: {}", fname, err);
            return Err(());
        }
    };
    let root_sig_bytecode = match d3d_compile_from_str(text, file_name, rsname, "rootsig_1_0", flags) {
        Ok(sbc) => sbc,
        Err(err) => {
            error!("Root signature '{}' compile error: {}", rsname, err);
            return Err(());
        }
    };
    Ok((shader_bytecode, root_sig_bytecode))
}


use std::ffi::OsString;
use std::os::windows::ffi::OsStrExt;

fn str_to_vec_u16(s: &str) -> Vec<u16> {
    let osstr = OsString::from(s);
    osstr.encode_wide().chain(Some(0).into_iter()).collect::<Vec<_>>()
}

use std::ffi::CString;

pub fn str_to_cstring(s: &str) -> CString {
    CString::new(s).unwrap()
}
