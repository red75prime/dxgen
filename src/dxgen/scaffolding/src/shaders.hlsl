#define RSD "RootFlags(ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT)," \
            "SRV(t1)," \
            "CBV(b0)," \
            "DescriptorTable(SRV(t0), SRV(t2))," \
            "StaticSampler(s0)," \
            "StaticSampler(s1, filter=FILTER_MIN_MAG_MIP_POINT)," \
            "StaticSampler(s2, filter=FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT, ComparisonFunc = COMPARISON_LESS, visibility = SHADER_VISIBILITY_PIXEL)"

#include "view_constants.hlsl"

struct InstanceData {
  float4x4 world;
  float3x3 n_world;
  float3 color;
  uint blink;
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
    nointerpolation float4 vDiffuse : COLOR;
    float2 texc0 : TEXCOORD0;
    float blink: TEXCOORD2;
    nointerpolation float3 norm : NORMAL;
    float3 w_pos : TEXCOORD1;
};

[RootSignature(RSD)]
VS_OUTPUT VSMain(VS_INPUT vtx, uint iidx : SV_InstanceID){
  VS_OUTPUT ret;
  ret.vPosition = mul(float4(vtx.vPosition, 1), instances[iidx].world);
  ret.w_pos = ret.vPosition.xyz;
  float z = ret.vPosition.z;
  ret.vPosition = mul(ret.vPosition, viewproj);
  
  ret.vDiffuse = float4(instances[iidx].color, 1);
  ret.texc0 = vtx.texc0;
  ret.norm.xyz = mul(vtx.norm, instances[iidx].n_world);
  ret.blink = (float)instances[iidx].blink;
  return ret;
}

Texture2D   testTex : register(t0);
Texture2DArray shadowMap : register(t2);
TextureCube<float> shadowMapCube : register(t2);
SamplerState  testSamp : register(s0);
SamplerState  pointSamp : register(s1);
SamplerComparisonState shadowSamp: register(s2);

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
[earlydepthstencil]
float4 PSMain(VS_OUTPUT pv) : SV_Target {
  float4 texel = testTex.Sample(testSamp, pv.texc0.xy)*pv.vDiffuse;
  float3 eye_off = eye_pos - pv.w_pos;
  float3 eye_n = normalize(eye_off);
  float3 light_off = light_pos - pv.w_pos;
  //float shadowDist = sampleCubemap(shadowMap, -light_off);
  float shdist = k2/(mlen(light_off))+k1;
  float shadowCoverage = shadowMapCube.SampleCmp(shadowSamp, -light_off, shdist);
  float4 ambientTerm = float4(texel.rgb*(float3(0.01,0.01,0.02)+float3(0.05,0.05,0.05)/(1+length(light_off)/20)),1);
  if (pv.blink == 1) {
    ambientTerm = pv.vDiffuse; 
  };
  float light_invd = 10. / (1. + length(light_off)/10);
  float3 light_n = normalize(light_off);
  float3 light_r = reflect(-light_n, normalize(pv.norm));
  float sb=1.-clamp(1.-dot(light_r,eye_n),0,0.0001)/0.0001;
  float l=clamp(dot(light_n,pv.norm),0,1);
  return (texel*l*0.6*light_invd+sb*float4(10,10,300,1)/(0.1+(length(light_off)+length(eye_off))/1000))*shadowCoverage+ambientTerm;
}
