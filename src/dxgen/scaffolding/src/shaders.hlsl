#define RSD "RootFlags(ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT)," \
            "DescriptorTable(SRV(t0), SRV(t1), CBV(b0), SRV(t2), visibility=SHADER_VISIBILITY_ALL)," \
            "StaticSampler(s0)," \
            "StaticSampler(s1, filter=FILTER_MIN_MAG_MIP_POINT)"

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

StructuredBuffer <InstanceData> instances : register(t1);

struct VS_INPUT
{
    float3 vPosition : POSITION;
    float3 vColor : COLOR;
    float2 texc0 : TEXCOORD0;
    float3 norm : NORMAL;
};

struct VS_OUTPUT
{
    float4 vPosition : SV_Position;
    float4 vDiffuse : COLOR;
    float2 texc0 : TEXCOORD0;
    float3 norm : NORMAL;
    float3 w_pos : TEXCOORD1;
};

[RootSignature(RSD)]
VS_OUTPUT VSMain(VS_INPUT vtx, uint iidx : SV_InstanceID){
  VS_OUTPUT ret;
  ret.vPosition = mul(float4(vtx.vPosition, 1), instances[iidx].world);
  ret.w_pos = ret.vPosition.xyz;
  float z = ret.vPosition.z;
  ret.vPosition = mul(ret.vPosition, view);
  ret.vPosition = mul(ret.vPosition, proj);
  
  ret.vDiffuse.rgb = vtx.vColor;
  ret.vDiffuse.r += z;
  ret.vDiffuse.a = 1;
  ret.texc0 = vtx.texc0;
  ret.norm.xyz = mul(vtx.norm, instances[iidx].n_world);
  return ret;
}

Texture2D   testTex : register(t0);
Texture2DArray shadowMap : register(t2);
TextureCube shadowMapCube : register(t2);
SamplerState  testSamp : register(s0);
SamplerState  pointSamp : register(s1);

#include "shadow_constants.hlsl"

float sampleCubemap(Texture2DArray shadow, float3 v) {
  float3 va = abs(v);
  float3 cfc;
  if (va.x>=va.y && va.x>=va.z) {
      // x - max
      cfc = float3(v.y/va.x/2+0.5, v.z/va.x/2+0.5, v.x>0 ? 0 : 1);
  } else if (va.y>=va.x && va.y>=va.z) {
      // y - max
      cfc = float3(v.x/va.y/2+0.5, v.z/va.y/2+0.5, v.y>0 ? 2 : 3);
  } else {
      // z - max
      cfc = float3(v.x/va.z/2+0.5, v.y/va.z/2+0.5, v.z>0 ? 4 : 5);
  }
  float w = shadow.Sample(pointSamp, cfc).r;

  return w==1 ? 1000000000.0 : k2/(w-k1);
}

float mlen(float3 v) {
  float3 va = abs(v);
  if (va.x>=va.y && va.x>=va.z) {
      // x - max
      return va.x;
  } else if (va.y>=va.x && va.y>=va.z) {
      // y - max
      return va.y;
  } else {
      // z - max
      return va.z;
  }
}

[RootSignature(RSD)]
float4 PSMain(VS_OUTPUT pv) : SV_Target {
  float4 texel = testTex.Sample(testSamp, pv.texc0.xy);
  float3 eye_off = eye_pos - pv.w_pos;
  float3 eye_n = normalize(eye_off);
  float3 light_off = light_pos - pv.w_pos;
  //float shadowDist = sampleCubemap(shadowMap, -light_off);
  float shadowDist = k2/(shadowMapCube.Sample(pointSamp, -light_off).r-k1);
  float4 ambientTerm = texel*float4(0.01,0.01,0.15,1);
  if (shadowDist < mlen(light_off)) {
    return ambientTerm;
  }
  float light_invd = 10. / (1. + length(light_off)/10);
  float3 light_n = normalize(light_off);
  float3 light_r = reflect(-light_n, normalize(pv.norm));
  float sb=1.-clamp(1.-dot(light_r,eye_n),0,0.0001)/0.0001;
  float l=clamp(dot(light_n,pv.norm),0,1);
  return texel*l*0.6*light_invd+sb*float4(10,10,300,1)/(0.1+(length(light_off)+length(eye_off))/1000)+ambientTerm;
}
