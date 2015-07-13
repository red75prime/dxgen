module annotations

type InterfaceAnnotation=
  |IAAutogen
  |IAManual

type ParamAnnotation=
  |AThis
  |ANone
  |InOut
  |InOutReturn // 
  |InOutOfSize of string // Name of size parameter
  |OutOfSize of string // name of size parameter
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
  |TypeSelector of string*((string*string*string) list) // name of parameter that controls type of this parameter, list of triples (function suffix, selector value, type)
  |AConst of string // Ugly. Don't use as annotations. For internal use.

type MethodAnnotation=
  |MANone
  |MAIUnknown // Method is a part of IUnknown interface. 
  |MAUnsafe
  |MADontImplement

