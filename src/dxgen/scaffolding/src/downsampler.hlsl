#define RSD "DescriptorTable(SRV(t0), UAV(u0))"

Texture2D<float4> mip0: register(t0);
RWTexture2D<float4> mip1: register(u0);

// static const float coefs[4] doesn't work on HD Graphics 4600
static float coefs[4] = {1./8., 3./8., 3./8., 1./8.};
groupshared float4 color_accum[4][4];

//[RootSignature(RSD)]
[numthreads(4, 4, 1)]
void CSMain(uint3 gtid: SV_GroupThreadId, uint3 gid: SV_GroupId) {
  int w=0;
  int h=0;
  mip0.GetDimensions(w, h);
  float c = coefs[gtid.x]*coefs[gtid.y];
  int2 idx = clamp(gid.xy*2 + gtid.xy + int2(-1,-1), int2(0,0), int2(w-1,h-1));

  color_accum[gtid.x][gtid.y] = mip0[idx]*c;
  GroupMemoryBarrierWithGroupSync();
  if (gtid.x==1 && gtid.y==1) {
    float4 a=float4(0,0,0,0);
    [unroll]
    for(uint i=0; i<4; ++i) {
       a += color_accum[i][0];
       a += color_accum[i][1];
       a += color_accum[i][2];
       a += color_accum[i][3];
    };
    mip1[gid.xy] = a;
  };
}
