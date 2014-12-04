//Define several Windows headers to hide them from the AST.
#define _BASETSD_H_
#define _INC_WINDOWS
#define _INC_SDKDDKVER
#define _WINRESRC_
#define _WINDOWS_
#define __RPC_H__
#define __RPCNDR_H__
#define _OLE2_H_
#define __oaidl_h__
#define __ocidl_h__
#define _INC_WINAPIFAMILY
#define HMONITOR_DECLARED

//Define several types that typically come from the above headers.
struct GUID;
struct IUnknown;

struct LUID;
struct RECT;

using HANDLE = void*;
using HWND = void*;
using HMONITOR = void*;

using BOOL = bool;
using BYTE = char;
using WCHAR = wchar_t;
using INT = int;
using UINT = unsigned int;
using SIZE_T = unsigned long;
using HRESULT = unsigned int;
using REFGUID = const GUID&;
using FLOAT = float;
using LARGE_INTEGER = long long;
#define REFIID __attribute__((annotate("In"))) REFGUID

//Override several macros to be more clean.
#define WINAPI __stdcall
#define STDMETHODCALLTYPE __stdcall
#define MIDL_INTERFACE(x) class          __attribute__((annotate("GUID(\"" ## x ##"\")")))

#define _In_                             __attribute__((annotate("In")))
#define _In_opt_                         __attribute__((annotate("In_Optional")))
#define _Out_                            __attribute__((annotate("Out")))
#define _Out_opt_                        __attribute__((annotate("Out_Optional")))
#define _Inout_                          __attribute__((annotate("In_Out")))
#define _In_reads_(x)					 __attribute__((annotate("In_Reads(\"" #x "\")")))
#define _In_reads_bytes_(x)              __attribute__((annotate("In_Reads_Bytes(\"" #x "\")")))
#define _Out_writes_(x)					 __attribute__((annotate("In_Writes(\"" #x "\")")))
#define _Out_writes_bytes_(x)            __attribute__((annotate("Out_Writes_Bytes(\"" #x "\")")))
#define _Out_writes_to_opt_(size, count) __attribute__((annotate("Out_Writes_To_Opt(\"" #size ", " #count "\")")))

#define DEFINE_GUID(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)