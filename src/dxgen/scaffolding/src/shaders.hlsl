
struct VS_INPUT
{
    float3 vPosition : POSITION;
    float4 vColor : COLOR;
};

struct VS_OUTPUT
{
    float4  vPosition : POSITION;
    float4  vDiffuse : COLOR;
};

VS_OUTPUT VSMain(VS_INPUT vtx){
  VS_OUTPUT ret;
  ret.vPosition.xyz=vtx.vPosition.xyz;
  ret.vPosition.w=0;
  ret.vDiffuse=vtx.vColor;
  return ret;
}

float4 PSMain(VS_OUTPUT pv) : SV_Target {
  return pv.vDiffuse;
}
