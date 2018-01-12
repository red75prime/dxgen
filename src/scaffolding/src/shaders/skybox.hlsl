#define TCRS "DescriptorTable(SRV(t0), UAV(u0))"

Texture2D<uint> rgbe8: register(t0);
RWTexture2D<float4> tex: register(u0);

[RootSignature(TCRS)]
[numthreads(1, 1, 1)]
void CSTexConvert(uint3 gid : SV_GroupId) {
	uint pix = rgbe8[gid.xy];
	uint3 col = uint3(pix & 255, (pix >> 8) & 255, (pix >> 16) & 255);
	uint e = (pix >> 24) & 255;
	float4 org = float4(ldexp(col, e - 128. - 8.), 1);
	float delta = 100./256.;
	float4 orgmin = float4(mul(org.rgb, 1-delta), 1);
	float4 orgmax = float4(mul(org.rgb, 1+delta), 1);
	tex[gid.xy] = org;
}

#define Group 8
groupshared float3 tmp[Group+2][Group+2];

[RootSignature(TCRS)]
[numthreads(Group+2, Group+2, 1)]
void CSSmooth(uint3 gid: SV_GroupId, uint3 tid: SV_GroupThreadId) {
	uint2 c = mul(gid.xy, Group)+tid.xy+int2(-1,-1);
	uint pix = rgbe8[c];
	uint3 col = uint3(pix & 255, (pix >> 8) & 255, (pix >> 16) & 255);
	uint e = (pix >> 24) & 255;
	float3 org = ldexp(col, e - 128. - 8.);
	tmp[tid.x][tid.y] = tex[c].rgb;
	GroupMemoryBarrierWithGroupSync();
	if (tid.x == 0 || tid.y == 0 || tid.x == Group+1 || tid.y == Group+1) {
	} else {
		float delta = 10./256.;
		float3 orgmin = mul(org.rgb, 1-delta);
		float3 orgmax = max(mul(org.rgb, 1+delta), mul(float3(0.001, 0.001, 0.001), 1+delta));
		float3 sum = tmp[tid.x][tid.y];
		sum += tmp[tid.x-1][tid.y];
		sum += tmp[tid.x+1][tid.y];
		sum += tmp[tid.x][tid.y-1];
		sum += tmp[tid.x][tid.y+1];
		sum = mul(sum, 0.2);
        tex[c] = float4(max(orgmin, min(orgmax, sum)), 1);
	};
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


#define SBRS "CBV(b0), DescriptorTable(SRV(t0)), StaticSampler(s0, filter = FILTER_MIN_MAG_MIP_POINT, addressU = TEXTURE_ADDRESS_CLAMP, addressV = TEXTURE_ADDRESS_CLAMP)"

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
	float2 sc = sphere_coord(r);
	return skytex.SampleLevel(default_sampler, sc, 0);
	//return sample_sphere(skytex, default_sampler, r);
}
