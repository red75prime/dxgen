module annotations

type InterfaceAnnotation=
  |IAAutogen
  |IAManual

// ----------------- Outdated description of safe rust code generation -----------------------------
// AThis : self.0
// ANone : depends on other annotations
// InOut : mutable borrow, C-type should be a pointer. e.g.
//    GetPrivateData(AThis ID3D* this, ANone REFGUID guid, InOut UINT* pDataSize) -> HRESULT =>  
//    fn get_private_data(&self, guid : REFGUID, data_size : &mut u32) -> HResult<()>

// InOutReturn : for Copy types only, in by value, out by function result e.g.
//    GetPrivateData(AThis ID3D* this, ANone REFGUID guid, InOutReturn UINT* pDataSize) -> HRESULT =>
//    fn get_private_data(&self, guid: REFGUID, u32 data_size) -> HResult<u32> {
//      let mut p1=data_size;
//      let hr=(...GetPrivateData)(..., guid, &mut p1);
//      hr2ret(hr,p1)
//    }

// OutReturn : same as InOutReturn, but without "in" part
//    GetPrivateData(AThis ID3D* this, ANone REFGUID guid, OutReturn UINT* pDataSize) -> HRESULT =>
//    fn get_private_data(&self, guid: REFGUID) -> HResult<u32> {
//      let mut p1=Default::<u32>::default();
//      let hr=(...GetPrivateData)(..., guid, &mut p1);
//      hr2ret(hr,p1)
//    }

// InIUnknown : makes function generic, transforms struct that has HasIID trait into *mut *mut IUnknownVtbl
//    SetPrivateInterface(AThis ID3D* this, ANone REFGUID guid, InIUnknown IUnknown* pInterface) =>
//    fn set_private_interface<T:HasIID>(&self, REFGUID guid, interface : T) -> () {
//      (...SetPrivateData)(..., guid, interface.expose_iptr());
//    }

// InOptional : for C-type T, *T => Option<&T>

// InOutOfSize s : C-type should be void*, makes a function generic on T, *mut c_void => &mut T, routes mem::size_of::<T>() to parameter s

// OutOfSize s : same as InOutOfSize s, Rust forbids uninitialized values anyway.

// InOfSize s : same as InOutOfSize s, but borrow is immutable

// OutReturnCombine(StructName, fieldName) : return value will be StructName or HResult(StructName), e.g.
//  GetClockCalibration(This : *mut ID3D12CommandQueue, OutReturnCombine(GPUCPUTimestamp,gpu) pGpuTimestamp : *mut UINT64, OutReturnCombine(GPUCPUTimestamp,cpu) pCpuTimestamp : *mut UINT64) -> HRESULT =>
//  struct GPUCPUTimestamp { gpu : u64, cpu: u64, }
//  fn get_clock_calibration(&self) -> HResult(GPUCPUTimestamp) {
//    let ret=GPUCPUTimestamp {gpu:0,cpu:0};
//    let hr=(...GetClockCalibration)(..., &mut ret.gpu as *mut UINT64, &mut ret.cpu as *mut UINT64);
//    hr2ret(hr, ret)
//  }
// Maybe I should just drop it. 

// OutReturnInterface riid : makes a function generic on T:HasIID, return type T or HResult<T>, routes HasIID::iid() to parameter riid

// OutReturnKnownInterface(riid, InterfaceName) : InterfaceName should implement HasIID, return type InterfaceName or HResult<InterfaceName>, routes InterfaceName::iid() to parameter riid

// InOptionalArrayOfSize s : *T => Option<&[T]>, routes param.map_or(0,|v|v.len()) to parameter s

// InArrayOfSize s : *T => &[T], routes param.len() to parameter s

// InByteArrayOfSize (s,n) : makes function generic over T, *c_void => &T, routes mem::size_of_val(s)/n to parameter s

// TypeSelector(buf, [(suffix, const, type)]) : annotated parameter select type that native method accepts in void* buf, buf parameter should be annotated InOutOfSize s
// suffix : rust function suffix for type
// const : const value for type
// type : accepted type
// 
// For each tuple (suffix, const, type) in list create method "native_method_<suffix>", route const to annotated parameter, route type to buf type
//


type ParamAnnotation=
  |AThis
  |ANone
  |InOut
  |InOutOptional
  |InOutReturn // 
  |InArrayOfSize of string // name of array lenght parameter
  |InOptionalArrayOfSize of string // name of array lenght parameter
  |OutArrayOfSize of string
  |OutOptionalArrayOfSize of string
  |InOutArrayOfSize of string 
  |InOutOfSize of string // Name of size parameter
  |OutOfSize of string // name of size parameter
  |OutOptionalOfSize of string // name of size parameter
  |OutPointer // pointer to a memory area of unknown size. ID3D12Resource::Map, ppData parameter
  |InOfSize of string // name of size parameter
  |OutReturn
  |OutReturnCombine of string*string // struct type, struct field
  |OutReturnInterface of string // parameter name of iid
  |OutReturnKnownInterface of string*string // parameter name of iid, interface type
  |InIUnknown
  |InByteArrayOfSize of string*uint32 // name of array lenght parameter
  |InOptional
  |InComPtr
  |InOptionalComPtr
  |OutOptional
  |TypeSelector of string*((string*string*string) list) // name of parameter that controls type of this parameter, list of triples (function suffix, selector value, type)
  |AConst of string // Don't use as annotation. For internal use.

let getReferencedParameters parameterAnnotation=
  match parameterAnnotation with
  |InOutOfSize p -> [p]
  |OutOfSize p -> [p]
  |OutOptionalOfSize p -> [p]
  |InOfSize p -> [p]
  |OutReturnInterface p -> [p]
  |OutReturnKnownInterface (p,_) -> [p]
  |InOptionalArrayOfSize p -> [p]
  |OutOptionalArrayOfSize p -> [p]
  |InArrayOfSize p -> [p]
  |OutArrayOfSize p -> [p]
  |InOutArrayOfSize p -> [p]
  |InByteArrayOfSize (p,_) -> [p]
  |TypeSelector (p,_) -> [p]
  |_ -> []

let getReturnDesc parmAnnot=
  match parmAnnot with
  |AThis -> []
  |ANone -> []
  |InOut -> []
  |InOutOptional -> []
  |InOutReturn -> [parmAnnot]
  |InOutArrayOfSize _ ->  []
  |OutArrayOfSize _ -> []
  |OutOptionalArrayOfSize _ -> []
  |InOutOfSize _ -> []
  |OutOfSize _ -> []
  |OutOptionalOfSize _ -> []
  |OutPointer -> []
  |InOfSize _ -> []
  |OutReturn -> [parmAnnot]
  |OutReturnCombine _ -> [parmAnnot]
  |OutReturnInterface _ -> [parmAnnot]
  |OutReturnKnownInterface _ -> [parmAnnot]
  |InIUnknown -> []
  |InOptionalArrayOfSize _ -> []
  |InArrayOfSize _ -> []
  |InByteArrayOfSize _ -> []
  |InOptional -> []
  |InComPtr -> []
  |InOptionalComPtr -> []
  |OutOptional -> []
  |TypeSelector _ -> []
  |AConst _ -> []

type MethodAnnotation=
  |MANone
  |MAIUnknown // Method is a part of IUnknown interface. 
  |MAUnsafe
  |MADontImplement

type EnumAnnotation=
  |EAFlags
  |EAEnum
