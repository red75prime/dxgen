struct VS_INPUT
{
    float4 vPosition : POSITION;
    float4 vColor : COLOR;
    float4 texc0 : TEXCOORD0;
};

struct VS_OUTPUT
{
    float4 vPosition : SV_Position;
    float4 vDiffuse : COLOR;
    float4 texc0 : TEXCOORD0;
};

VS_OUTPUT VSMain(VS_INPUT vtx){
  VS_OUTPUT ret;
  ret.vPosition = vtx.vPosition;
  ret.vDiffuse = vtx.vColor;
  ret.texc0 = vtx.texc0;
  return ret;
}

Texture2D   testTex : register(t0);
SamplerState  testSamp : register(s0);

float4 PSMain(VS_OUTPUT pv) : SV_Target {
  float4 ret=pv.vDiffuse;
  float x=pv.vPosition.x-1000;
  float y=pv.vPosition.y-1000;
  ret.r += sin(sqrt(x*x+y*y)/10)*0.5;
  ret = testTex.Sample(testSamp, pv.texc0.xy);
  return ret;
}
