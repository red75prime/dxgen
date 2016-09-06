#define TCRS "DescriptorTable(SRV(t0), UAV(u0))"

Texture2D<uint> rgbe8: register(t0);
RWTexture2D<float4> tex: register(u0);

[RootSignature(TCRS)]
[numthreads(1, 1, 1)]
void CSTexConvert(uint3 gid : SV_GroupId) {
	uint pix = rgbe8[gid.xy];
	uint3 col = uint3(pix & 255, (pix >> 8) & 255, (pix >> 16) & 255);
	uint e = (pix >> 24) & 255;
	float ex = exp2(e - 128 - 8);
	tex[gid] = float4(float3(ex, ex, ex)*col, 1);
}

#include "view_constants.h"

#define SBRS "CVB(b0), DescriptorTable(SRV(t0))"

struct PS_IN {
	float4 pos : SV_POSITION;
};

PS_IN VSMain(uint vtx: SV_VertexID) {
	PS_IN ret = { 0 };
	switch(vtx) {
		case 0:
			ret.pos = float4(-1, -1, 1, 1);
			break;
		case 1:
			ret.pos = float4(1, -1, 1, 1);
			break;
		case 2:
			ret.pos = float4(1, 1, 1, 1);
			break;
		case 3:
			ret.pos = float4(-1, 1, 1, 1);
			break;
	}
	return ret;
}

float4 PSMain(PS_IN dat) : SV_TARGET {
	
}
