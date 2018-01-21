// constants, instance data, indices, coordinated, normals, texture coords
#define RSD "RootFlags(0)," \
            "CBV(b0), SRV(t1), SRV(t2), SRV(t3)" 

#include "view_constants.hlsl"

struct Indices {
  uint crd;
  uint nrm;
  uint tex;
};

StructuredBuffer<Indices> indices : register(t1);
StructuredBuffer<float3> coords: register(t2);
StructuredBuffer<float3> normals: register(t3);
//StructuredBuffer<float2> tex_coord: register(t4); 

struct VS_OUTPUT
{
    float4 pos: SV_Position;
    float4 col: COLOR;
};

[RootSignature(RSD)]
VS_OUTPUT VSMain(uint vtx: SV_VertexID) {
  VS_OUTPUT ret;
  // Instancing isn't used for now. Number of instances is 1.
  ret.pos = float4(coords[indices[vtx].crd]/10 + light_pos, 1);
  ret.pos = mul(ret.pos, viewproj);
  float l = 1e30;
  ret.col = float4(l, l, l, 1);
  return ret;
}

[RootSignature(RSD)]
[earlydepthstencil]
float4 PSMain(VS_OUTPUT v) : SV_TARGET {
  return v.col;
}
