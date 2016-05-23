// constants, instance data, indices, coordinated, normals, texture coords
#define RSD "RootFlags(None)," \
            "CBV(b0), SRV(t0), SRV(t1), SRV(t2), SRV(t3), SRV(t4)" 

cbuffer cb0 : register(b0) {
  float4x4 view;
  float4x4 proj;
  float3 eye_pos;
  float3 light_pos;
}

struct InstanceData {
  float4x4 world;
  float3x3 n_world;
};

StructuredBuffer<InstanceData> instances : register(t0);

struct Indices {
  uint crd;
  uint nrm;
  uint tex;
};

StructuredBuffer<Indices> indices : register(t1);

StructuredBuffer<float3> coords: register(t2);

StructuredBuffer<float3> normals: register(t3);

StructuredBuffer<float2> tex_coord: register(t4); 

struct VS_OUTPUT
{
    float4 pos: SV_Position;
    float4 color: SV_Target;
};

[RootSignature(RSD)]
VS_OUTPUT VSMain(uint vtx: SV_VertexID, uint iidx : SV_InstanceID){
  VS_OUTPUT ret;
  ret.pos = mul(float4(coords[indices[vtx].crd], 1), instances[iidx].world);
  ret.pos = mul(ret.pos, view);
  ret.pos = mul(ret.pos, proj);

  ret.color = float4(200, 200, 200, 1);
  return ret;
}
