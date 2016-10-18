#define TCRS "DescriptorTable(SRV(t0), UAV(u0))"

Texture2D<uint> rgbe8: register(t0);
RWTexture2D<float4> tex: register(u0);

[RootSignature(TCRS)]
[numthreads(1, 1, 1)]
void CSTexConvert(uint3 gid : SV_GroupId) {
	uint pix = rgbe8[gid.xy];
	uint3 col = uint3(pix & 255, (pix >> 8) & 255, (pix >> 16) & 255);
	uint e = (pix >> 24) & 255;
	float ex = exp2(e - 128. - 8.);
	tex[gid.xy] = float4(ex*col.r, ex*col.g, ex*col.b, 1);
}

#define SBRS "CBV(b0), DescriptorTable(SRV(t0)), StaticSampler(s0, addressU = TEXTURE_ADDRESS_CLAMP, addressV = TEXTURE_ADDRESS_CLAMP, borderColor=STATIC_BORDER_COLOR_TRANSPARENT_BLACK)"

struct PS_IN {
	float4 pos : SV_POSITION;
	float2 ncrd: TEXCOORD;
};

[RootSignature(SBRS)]
PS_IN VSMain(uint vtx: SV_VertexID) {
	PS_IN ret = { float4(0,0,0,0), float2(0,0) };
	switch(vtx) {
		case 0:
			ret.pos = float4(-1, -1, 1, 1);
			ret.ncrd = float2(-2, -1);
			break;
		case 1:
			ret.pos = float4(1, -1, 1, 1);
			ret.ncrd = float2(2, -1);
			break;
		case 2:
			ret.pos = float4(-1, 1, 1, 1);
			ret.ncrd = float2(-2, 1);
			break;
		case 3:
			ret.pos = float4(1, 1, 1, 1);
			ret.ncrd = float2(2, 1);
			break;
	}
	return ret;
}

Texture2D<float4> skytex: register(t0);
SamplerState default_sampler: register(s0);

#include "view_constants.hlsl"

[RootSignature(SBRS)]
[earlydepthstencil]
float4 PSMain(PS_IN dat) : SV_TARGET {
	float3 r = mul(viewproj, float4(dat.ncrd, 1, 0)).xyz;
	float2 p = float2(0.5 - atan2(r.x, r.z)/6.2832, 0.5 - atan2(r.y,sqrt(r.x*r.x+r.z*r.z))/3.1416);
	//return float4(p.x, p.y, 1, 1);
	return skytex.Sample(default_sampler, p);
}

