#ifndef __IUNKNOWN__
#define __IUNKNOWN__

struct IID;
using HRESULT = int;

class __attribute__((annotate("GUID(\"00000000-0000-0000-C000-000000000046\")"))) IUnknown
{
public:
	virtual HRESULT QueryInterface(
		__attribute__((annotate("In"))) const IID& riid,
		__attribute__((annotate("Out"))) void **ppvObject
		) = 0;
};


class __attribute__((annotate("GUID(\"32A3615B-3D98-48AA-A648-3EC4BD2E0440\")"))) ITest : IUnknown
{
};
#endif