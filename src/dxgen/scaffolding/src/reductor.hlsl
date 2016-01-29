#define TotalGroups 32
#define RSDT "RootFlags(0), RootConstants(num32BitConstants=3, b0), DescriptorTable(SRV(t0), UAV(u0), UAV(u1), SRV(t1))" // Descriptor table is required for texture

cbuffer cb0 : register(b0) {
  uint HDispatch;
  uint Width;
  uint Height;
};
Texture2D<float4> tsrc: register(t0);
//// HD 4600 requires RWStructuredBuffer instead of RWBuffer.
// RWBuffer<float> total : register(u0)
RWStructuredBuffer<float> total : register(u0);

groupshared float4 bpacked[TotalGroups*TotalGroups];

float brightness(float4 cl) {
  // TODO: replace with correct implementation
  return dot(float3(1, 1, 1), cl.rgb); //cl.r + cl.g + cl.b;
}

float br(uint x, uint y) {
  //// Could it be non-zero values outsize texture?
  //return brightness((x < Width && y < Height) ? tsrc[uint2(x, y)] : float4(0, 0, 0, 0));
  return brightness(tsrc[uint2(x, y)]);
}

[RootSignature(RSDT)]
[numthreads(TotalGroups,TotalGroups,1)]
void CSTotal(uint3 dtid: SV_DispatchThreadId, uint3 localId : SV_GroupThreadId, uint3 gid : SV_GroupId, uint gi : SV_GroupIndex) {
  uint x = dtid.x * 2;//(groupId.x*TotalGroups + localId.x) * 2;
  uint y = dtid.y * 2;//(groupId.y*TotalGroups + localId.y) * 2;
  float b1 = br(x,y);
  float b2 = br(x + 1, y);
  float b3 = br(x, y + 1);
  float b4 = br(x + 1, y + 1);
  bpacked[gi] = float4(b1,b2,b3,b4);

  GroupMemoryBarrierWithGroupSync();

  [unroll]
  for (uint thres = TotalGroups*TotalGroups / 2; thres > 32; thres /= 2) {
    if (gi < thres) {
      bpacked[gi] += bpacked[gi + thres];
    }
    GroupMemoryBarrierWithGroupSync();
  };
  if (gi < 32) {
    // It works on HD 4600, when TotalGroups equals 32. WTF?
    bpacked[gi] += bpacked[gi + 32];
    bpacked[gi] += bpacked[gi + 16];
    bpacked[gi] += bpacked[gi + 8];
    bpacked[gi] += bpacked[gi + 4];
    bpacked[gi] += bpacked[gi + 2];
    bpacked[gi] += bpacked[gi + 1];
  }

  if (gi == 0) {
    float value = dot(bpacked[0], float4(1, 1, 1, 1));
    total[gid.x + gid.y*HDispatch] = value;
  };
}

#define BufTotal 512
groupshared float stotal[BufTotal];
Buffer<float> r_total : register(t1);
//globallycoherent RWBuffer<float> ui_total : register(u1);
RWByteAddressBuffer ui_total : register(u1);

[RootSignature(RSDT)]
[numthreads(BufTotal, 1, 1)]
void CSBufTotal(uint3 dtid: SV_DispatchThreadId, uint3 localId : SV_GroupThreadId, uint gi : SV_GroupIndex, uint3 gid: SV_GroupID) {
  stotal[gi] = total[dtid.x];
  GroupMemoryBarrierWithGroupSync();

  [unroll]
  for (uint thres = BufTotal / 2; thres > 0; thres /= 2) {
    if (gi < thres) {
      stotal[gi] += stotal[gi + thres];
    }
    GroupMemoryBarrierWithGroupSync();
  };
  // Doesn't work on R7 360
  //if (gi < 32) {
  //  // TODO: Check if this works on HD 4600
  //  stotal[gi] += stotal[gi + 32];
  //  stotal[gi] += stotal[gi + 16];
  //  stotal[gi] += stotal[gi + 8];
  //  stotal[gi] += stotal[gi + 4];
  //  stotal[gi] += stotal[gi + 2];
  //  stotal[gi] += stotal[gi + 1];
  //}

  if (gi == 0) {
    float value = stotal[0];
//    printf("%f", value);
    uint comp, orig = ui_total.Load(0);
    [allow_uav_condition]do
    {
      comp = orig;
      ui_total.InterlockedCompareExchange(0, comp, asuint(asfloat(orig) + value), orig);
    } while (orig != comp);
  };
}

