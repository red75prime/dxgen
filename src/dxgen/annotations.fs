module annotations

type InterfaceAnnotation=
  |IAAutogen
  |IAManual

type ParamAnnotation=
  |AThis
  |ANone
  |InOut
  |InOutOptional
  |InOutReturn // 
  |InOutArrayOfSize of string
  |OutArrayOfSize of string
  |OutOptionalArrayOfSize of string
  |InOutOfSize of string // Name of size parameter
  |OutOfSize of string // name of size parameter
  |OutPointer // pointer to a memory area of unknown size. ID3D12Resource::Map, ppData parameter
  |InOfSize of string // name of size parameter
  |OutReturn
  |OutReturnCombine of string*string // struct type, struct field
  |OutReturnInterface of string // parameter name of iid
  |OutReturnKnownInterface of string*string // parameter name of iid, interface type
  |InIUnknown
  |InOptionalArrayOfSize of string // name of array lenght parameter
  |InArrayOfSize of string // name of array lenght parameter
  |InByteArrayOfSize of string // name of array lenght parameter
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
  |InOfSize p -> [p]
  |OutReturnInterface p -> [p]
  |OutReturnKnownInterface (p,_) -> [p]
  |InOptionalArrayOfSize p -> [p]
  |OutOptionalArrayOfSize p -> [p]
  |InArrayOfSize p -> [p]
  |OutArrayOfSize p -> [p]
  |InOutArrayOfSize p -> [p]
  |InByteArrayOfSize p -> [p]
  |TypeSelector (p,_) -> [p]
  |_ -> []



type MethodAnnotation=
  |MANone
  |MAIUnknown // Method is a part of IUnknown interface. 
  |MAUnsafe
  |MADontImplement

type EnumAnnotation=
  |EAFlags
  |EAEnum
