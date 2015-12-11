cbuffer cb0 : register(b0) {
  float4x4 model;
  float4x4 view;
  float4x4 proj;
  float4x4 n_model;
  float3 eye_pos;
  float3 light_pos;
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
//  ret.vPosition =float4(vtx.vPosition, 1);
  ret.vPosition = mul(float4(vtx.vPosition, 1), model);
  ret.w_pos = ret.vPosition.xyz;
  float z= ret.vPosition.z;
  ret.vPosition = mul(ret.vPosition, view);
  ret.vPosition = mul(ret.vPosition, proj);
  
  ret.vDiffuse.rgb = vtx.vColor;
  ret.vDiffuse.r += z;
  ret.vDiffuse.a = 1;
  ret.texc0 = vtx.texc0;
  ret.norm.xyz = mul(float4(vtx.norm,1), n_model).xyz;
//  ret.vPosition.z = 0.01;
//  ret.vPosition.w = 1;
  return ret;
}

Texture2D   testTex : register(t0);
SamplerState  testSamp : register(s0);

float4 PSMain(VS_OUTPUT pv) : SV_Target {
  float4 texel=testTex.Sample(testSamp, pv.texc0.xy);
  float3 eye_off = eye_pos - pv.w_pos;
  float3 eye_n = normalize(eye_off);
  float3 light_off = light_pos - pv.w_pos;
  float light_invd = 10. / length(light_off/5.);
  float3 light_n = normalize(light_off);
  float3 light_r = reflect(-light_n, normalize(pv.norm));
  float sb=1.-clamp(1.-dot(light_r,eye_n),0,0.001)/0.001;
  float l=clamp(dot(light_n,pv.norm),0,1);
  return texel*l*0.6*light_invd+sb*float4(1,1,1,1)+texel*float4(0.01,0.01,0.15,1);
}

