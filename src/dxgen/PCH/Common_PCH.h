#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef GetMessage
#undef GetGlyphIndices
#undef DrawText
#define D3D11_NO_HELPERS
