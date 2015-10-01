cbuffer cb0 : register(b0) {
  float4x4 model;
  float4x4 view;
  float4x4 proj;
  float4x4 n_model;
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
    float3 w_pos : TEXCOORD1;
};

VS_OUTPUT VSMain(VS_INPUT vtx){
  VS_OUTPUT ret;
  ret.vPosition =float4(vtx.vPosition, 1);
//  ret.vPosition = mul(float4(vtx.vPosition, 1), model);
  ret.w_pos = ret.vPosition.xyz;
  float z= ret.vPosition.z;
//  ret.vPosition = mul(ret.vPosition, view);
//  ret.vPosition = mul(ret.vPosition, proj);
  
  ret.vDiffuse.rgb = vtx.vColor;
  ret.vDiffuse.r += z;
  ret.vDiffuse.a = 1;
  ret.texc0 = vtx.texc0;
  ret.norm.xyz = mul(float4(vtx.norm,1), n_model).xyz;
  ret.vPosition.z = 0.5;
  return ret;
}

Texture2D   testTex : register(t0);
SamplerState  testSamp : register(s0);

float4 PSMain(VS_OUTPUT pv) : SV_Target {
  float4 texel=testTex.Sample(testSamp, pv.texc0.xy);

  float4 ret=pv.vDiffuse;
  float3 eye_off = float3(0.,0.,-3.) - pv.w_pos;
  float3 eye_n = normalize(eye_off);
  float3 light_off = float3(-1.5,0.,0.) - pv.w_pos;
  float3 light_n = normalize(light_off);
  float3 light_r = reflect(-light_n, pv.norm);
  float sb=clamp(dot(light_r,pv.norm),0,0.1)/0.1;
//  ret.r += sin(sqrt(x*x+y*y)/10)*0.5;
  float l=clamp(dot(light_n,pv.norm),0,1);
  float s=clamp(dot(light_r,eye_n),0,1);
  float s2=1.0001-s*s;
  float s3=1-clamp(s2,0,0.001)/0.001;
  ret = texel*(0.1+l)+float4(s3,s3,s3,1)*sb*0.5;
  ret = float4(1,1,1,1);
  return ret;
}
