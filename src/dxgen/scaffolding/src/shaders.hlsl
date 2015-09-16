
struct VS_INPUT
{
    float4 vPosition : POSITION;
    float4 vColor : COLOR;
};

struct VS_OUTPUT
{
    float4  vPosition : SV_Position;
    float4  vDiffuse : COLOR;
};

VS_OUTPUT VSMain(VS_INPUT vtx){
  VS_OUTPUT ret;
  ret.vPosition=vtx.vPosition;
  ret.vDiffuse=vtx.vColor;
  return ret;
}

float4 PSMain(VS_OUTPUT pv) : SV_Target {
  return pv.vDiffuse;
}
