struct VS_OUTPUT
{
    float4 pos: SV_Position;
    float4 color: COLOR;
};

 VS_OUTPUT VSMain(float3 pos: POSITION, float4 color: COLOR) {
    VS_OUTPUT ret;
    ret.pos = float4(pos, 1);
    ret.color = color;
    return ret;
 }
 
float4 PSMain(VS_OUTPUT input) : SV_TARGET
{
	return input.color;
}
