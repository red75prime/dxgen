cbuffer cb0 : register(b0) {
  float4x4 world;
  float4x4 view;
//  float4x4 mProj;
}

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
};

VS_OUTPUT VSMain(VS_INPUT vtx){
  VS_OUTPUT ret;
  ret.vPosition = mul(mul(float4(vtx.vPosition, 1), world), view);
  
  ret.vDiffuse.rgb = vtx.vColor;
  ret.vDiffuse.a = 1;
  ret.texc0 = vtx.texc0;
  ret.norm = vtx.norm;
  return ret;
}

Texture2D   testTex : register(t0);
SamplerState  testSamp : register(s0);

float4 PSMain(VS_OUTPUT pv) : SV_Target {
  float4 ret=pv.vDiffuse;
  float x=pv.vPosition.x-1000;
  float y=pv.vPosition.y-1000;
//  ret.r += sin(sqrt(x*x+y*y)/10)*0.5;
  ret *= testTex.Sample(testSamp, pv.texc0.xy);
  return ret;
}
