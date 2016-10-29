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


Texture2D<uint> rgbe8_2: register(t0);
RWTexture2D<uint> tex_shexp: register(u0); 

[RootSignature(TCRS)]
[numthreads(1, 1, 1)]
void CSTexConvertShexp(uint3 gid : SV_GroupId) {
	uint pix = rgbe8_2[gid.xy];
	uint3 col = uint3(pix & 255, (pix >> 8) & 255, (pix >> 16) & 255);
	int e = (int)((pix >> 24) & 255) - 128;
	uint e2 = clamp(e, -15, 16) + 15;
	col <<= 1;
	uint dither = (gid.x+gid.y)%2;
	col += uint3(dither, dither, dither);
	if (e < -15) {
		uint shft = clamp(-15-e, 0, 8);
		col >>= shft;
	};
	tex_shexp[gid.xy] = col.r | (col.g << 9) | (col.b << 18) | (e2 << 27);
}


#define SBRS "CBV(b0), DescriptorTable(SRV(t0)), StaticSampler(s0, addressU = TEXTURE_ADDRESS_CLAMP, addressV = TEXTURE_ADDRESS_CLAMP)"

struct PS_IN {
	float4 pos : SV_POSITION;
	float2 ncrd: TEXCOORD;
};

#include "view_constants.hlsl"

[RootSignature(SBRS)]
PS_IN VSMain(uint vtx: SV_VertexID) {
	PS_IN ret = { float4(0,0,0,0), float2(0,0) };
	switch(vtx) {
		case 0:
			ret.pos = float4(-1, -1, 1, 1);
			ret.ncrd = float2(tfov_xy.x, tfov_xy.y);
			break;
		case 1:
			ret.pos = float4(1, -1, 1, 1);
			ret.ncrd = float2(-tfov_xy.x, tfov_xy.y);
			break;
		case 2:
			ret.pos = float4(-1, 1, 1, 1);
			ret.ncrd = float2(tfov_xy.x, -tfov_xy.y);
			break;
		case 3:
			ret.pos = float4(1, 1, 1, 1);
			ret.ncrd = float2(-tfov_xy.x, -tfov_xy.y);
			break;
	}
	return ret;
}

Texture2D<float4> skytex: register(t0);
SamplerState default_sampler: register(s0);

[RootSignature(SBRS)]
[earlydepthstencil]
float4 PSMain(PS_IN dat) : SV_TARGET {
	float3 r = mul(view, float4(dat.ncrd, 1, 0)).xyz;
	//return float4(p.x, p.x, 0, 1);
	return sample_sphere(skytex, default_sampler, r);
}