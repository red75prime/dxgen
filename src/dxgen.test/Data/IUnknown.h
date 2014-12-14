#ifndef __IUNKNOWN__
#define __IUNKNOWN__

struct IID;
using HRESULT = int;

class __attribute__((annotate("GUID(\"00000000-0000-0000-C000-000000000046\")")))  IUnknown
{
public:
	virtual HRESULT QueryInterface(
		__attribute__((annotate("In"))) const IID& riid,
		__attirubte__((annotate("Out"))) void **ppbObject
		) = 0;
};

#endif