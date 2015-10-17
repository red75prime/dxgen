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

#define __unknwn_h__
#define __IUnknown_INTERFACE_DEFINED__
#define __AsyncIUnknown_INTERFACE_DEFINED__
#define __IClassFactory_INTERFACE_DEFINED__
#define __IWinTypesBase_INTERFACE_DEFINED__

// disable SAL
#define _USE_DECLSPECS_FOR_SAL 0
#define __RPC_string
#define __RPC_unique_pointer
#define __RPC__deref_out
#define __RPC__in
#define __RPC__deref_out_opt
#define __RPC_STUB
#define _SA_annotes3(a,b,c,d)
#define _GROUP_(a)

// Probably I'm curing the symptoms, but who cares
typedef unsigned char boolean; 
typedef unsigned short int WCHAR;
typedef WCHAR *LPWSTR;
typedef unsigned char byte;
typedef unsigned char UCHAR;

//Define several types that typically come from the above headers.
typedef struct _SECURITY_ATTRIBUTES SECURITY_ATTRIBUTES;

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
typedef unsigned __int16 WCHAR;
typedef int INT;
typedef unsigned int UINT;
typedef unsigned long SIZE_T;
typedef unsigned long ULONG;
typedef unsigned short USHORT;
//typedef unsigned int HRESULT;
typedef void* HDC;
typedef void* HMODULE;
typedef const void* LPCVOID;
typedef float FLOAT;
typedef long long LARGE_INTEGER;
typedef unsigned long long UINT64;
typedef unsigned __int16 UINT16;
typedef __int16 INT16;
typedef unsigned __int64 ULONGLONG;
typedef unsigned __int64 LONGLONG;
typedef unsigned __int32 UINT32;
typedef __int32 INT32;


typedef struct _LUID {
	DWORD LowPart;
	LONG  HighPart;
} LUID;

typedef struct tagPOINT
{
	LONG  x;
	LONG  y;
} POINT;

typedef struct tagSIZE
{
	LONG cx;
	LONG cy;
}   SIZE;

typedef struct _RECTL
{
	LONG left;
	LONG top;
	LONG right;
	LONG bottom;
}   RECTL;

typedef struct _BYTE_BLOB
{
	ULONG clSize;
	/* [size_is] */ byte abData[1];
}   BYTE_BLOB;

typedef struct _WORD_BLOB
{
	ULONG clSize;
	/* [size_is] */ unsigned short asData[1];
}   WORD_BLOB;

typedef struct _DWORD_BLOB
{
	ULONG clSize;
	/* [size_is] */ ULONG alData[1];
}   DWORD_BLOB;

typedef struct _FLAGGED_BYTE_BLOB
{
	ULONG fFlags;
	ULONG clSize;
	/* [size_is] */ byte abData[1];
}   FLAGGED_BYTE_BLOB;

typedef struct _FLAGGED_WORD_BLOB
{
	ULONG fFlags;
	ULONG clSize;
	/* [size_is] */ unsigned short asData[1];
}   FLAGGED_WORD_BLOB;

typedef WCHAR OLECHAR;

typedef OLECHAR *LPOLESTR;

//Override several macros to be more clean.
#define WINAPI __stdcall
#define STDMETHODCALLTYPE __stdcall
#define MIDL_INTERFACE(x) class __attribute__((annotate("GUID(\"" ## x ##"\")")))
// #define interface ;\/\/
#define DECLSPEC_UUID(x)

#define DEFINE_GUID(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)

#define CONST const
// C interfaces are better suited for the task. 

#define CINTERFACE
#define interface struct
#define BEGIN_INTERFACE
#define END_INTERFACE

typedef const char * LPCSTR;
typedef char * LPSTR;
typedef const WCHAR * LPCWSTR;

#define CONST_VTBL
#define DECLARE_INTERFACE(iface)    typedef interface iface { \
                                    struct iface##Vtbl * lpVtbl; \
                                } iface; \
                                typedef struct iface##Vtbl iface##Vtbl; \
                                struct iface##Vtbl
#define DECLARE_INTERFACE_(iface, baseiface)    DECLARE_INTERFACE(iface)
//#define DECLARE_INTERFACE_IID(iface, iid)               DECLARE_INTERFACE(iface)
//#define DECLARE_INTERFACE_IID_(iface, baseiface, iid)   DECLARE_INTERFACE_(iface, baseiface)
#define PURE
#define EXTERN_C extern

typedef void* RPC_IF_HANDLE;

#define WINAPI_FAMILY_PARTITION(p) 1

#ifdef _WIN64
#define STDMETHODCALLTYPE 
#else
#define STDMETHODCALLTYPE __stdcall
#endif

#define STDMETHOD(method)       HRESULT (STDMETHODCALLTYPE * method)
#define STDMETHOD_(type,method) type (STDMETHODCALLTYPE * method)
#define STDMETHODV(method)       HRESULT (STDMETHODVCALLTYPE * method)
#define STDMETHODV_(type,method) type (STDMETHODVCALLTYPE * method)

#define THIS_                   INTERFACE * This,
#define THIS                    INTERFACE * This

#define DEFINE_ENUM_FLAG_OPERATORS(ENUMTYPE)

//typedef unsigned int DXGI_FORMAT;

