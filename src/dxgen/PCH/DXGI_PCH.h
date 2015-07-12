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
typedef struct _SECURITY_ATTRIBUTES SECURITY_ATTRIBUTES;
typedef struct _IUnknown IUnknown;

typedef void* HANDLE;
typedef void* HWND;
typedef void* HMONITOR;
typedef void* LPVOID;
typedef void* LONG_PTR;

typedef char BYTE;
typedef unsigned char UINT8;
typedef unsigned long int DWORD;
typedef unsigned short int WORD;
typedef short int SHORT;
typedef long int LONG;

//typedef bool BOOL;
typedef int BOOL;
//typedef wchar_t WCHAR;
typedef unsigned short WCHAR;
typedef int INT;
typedef unsigned int UINT;
typedef unsigned long SIZE_T;
typedef unsigned long ULONG;
typedef unsigned int HRESULT;
typedef void* HDC;
typedef void* HMODULE;
typedef const void* LPCVOID;
typedef float FLOAT;
typedef long long LARGE_INTEGER;
typedef unsigned long long UINT64;
typedef unsigned short UINT16;

typedef struct _GUID {
  DWORD Data1;
  WORD  Data2;
  WORD  Data3;
  BYTE  Data4[8];
} GUID;

typedef const GUID* REFGUID;

typedef GUID IID;

typedef struct _LUID {
  DWORD LowPart;
  LONG  HighPart;
} LUID;

typedef struct _RECT {
  LONG left;
  LONG top;
  LONG right;
  LONG bottom;
} RECT;

#define REFIID __attribute__((annotate("In"))) REFGUID

//Override several macros to be more clean.
#define WINAPI __stdcall
#define STDMETHODCALLTYPE __stdcall
#define MIDL_INTERFACE(x) class __attribute__((annotate("GUID(\"" ## x ##"\")")))
// #define interface ;\/\/

#define _In_                             __attribute__((annotate("In")))
#define _In_z_                             __attribute__((annotate("In_ZeroTerm")))
#define _In_opt_                         __attribute__((annotate("In_Optional")))
#define _Out_                            __attribute__((annotate("Out")))
#define _Out_opt_                        __attribute__((annotate("Out_Optional")))
#define _COM_Outptr_opt_    __attribute__((annotate("Out_Optional")))
#define _Inout_                          __attribute__((annotate("In_Out")))
#define _In_reads_(x)					 __attribute__((annotate("In_Reads(\"" #x "\")")))
#define _In_reads_bytes_(x)              __attribute__((annotate("In_Reads_Bytes(\"" #x "\")")))
#define _In_reads_bytes_opt_(x)              __attribute__((annotate("In_Reads_Bytes_Opt(\"" #x "\")")))
#define _Out_writes_(x)					 __attribute__((annotate("Out_Writes(\"" #x "\")")))
#define _Out_writes_opt_(x)					 __attribute__((annotate("Out_Writes_Opt(\"" #x "\")")))
#define _Out_writes_bytes_(x)            __attribute__((annotate("Out_Writes_Bytes(\"" #x "\")")))
#define _Out_writes_bytes_opt_(x)            __attribute__((annotate("Out_Writes_Bytes_Opt(\"" #x "\")")))
#define _Out_writes_to_opt_(size, count) __attribute__((annotate("Out_Writes_To_Opt(\"" #size ", " #count "\")")))
#define _COM_Outptr_opt_result_maybenull_ __attribute__((annotate("Out_Optional")))
#define _Field_size_bytes_full_(x) 
#define _Field_size_full_(x) 
#define _Field_size_(x) 
#define _Always_(x)
#define _Outptr_opt_result_bytebuffer_(x)
#define _In_range_(a,b)
#define _In_reads_opt_(x)
#define _Inout_updates_bytes_(x)
#define _Inout_opt_

#define DEFINE_GUID(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)

// C interfaces are better suited for the task. 

//#define CINTERFACE
#define interface struct
#define BEGIN_INTERFACE
#define END_INTERFACE
#define _COM_Outptr_

typedef const char * LPCSTR;
typedef const WCHAR * LPCWSTR;

#define CONST_VTBL
#define DECLARE_INTERFACE(iface)    typedef interface iface { \
                                    struct iface##Vtbl * lpVtbl; \
                                } iface; \
                                typedef struct iface##Vtbl iface##Vtbl; \
                                struct iface##Vtbl
#define DECLARE_INTERFACE_(iface, baseiface)    DECLARE_INTERFACE(iface)
#define DECLARE_INTERFACE_IID(iface, iid)               DECLARE_INTERFACE(iface)
#define DECLARE_INTERFACE_IID_(iface, baseiface, iid)   DECLARE_INTERFACE_(iface, baseiface)
#define PURE
#define EXTERN_C extern

typedef void* RPC_IF_HANDLE;

#define WINAPI_FAMILY_PARTITION(p) 1

#define STDMETHODCALLTYPE __stdcall
#define STDMETHOD(method)       HRESULT (STDMETHODCALLTYPE * method)
#define STDMETHOD_(type,method) type (STDMETHODCALLTYPE * method)
#define STDMETHODV(method)       HRESULT (STDMETHODVCALLTYPE * method)
#define STDMETHODV_(type,method) type (STDMETHODVCALLTYPE * method)

#define THIS_                   INTERFACE * This,
#define THIS                    INTERFACE * This

#define DEFINE_ENUM_FLAG_OPERATORS(ENUMTYPE)

typedef unsigned int DXGI_FORMAT;
