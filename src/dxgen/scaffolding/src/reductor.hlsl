#define TotalGroups 32
#define RSDT "RootFlags(0), RootConstants(num32BitConstants=3, b0), DescriptorTable(SRV(t0), UAV(u0), UAV(u1), SRV(t1))" // Descriptor table is required for texture


cbuffer cb0 : register(b0) {
  // According to https://msdn.microsoft.com/en-us/library/windows/desktop/bb509632(v=vs.85).aspx
  // these values are packed into one 16 byte group
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
  return log(dot(float3(0.2126, 0.7152, 0.0722), cl.rgb)+0.0001)-log(0.0001);
}

float br(uint x, uint y) {
  return brightness(tsrc[uint2(x, y)]);
}

[RootSignature(RSDT)]
[numthreads(TotalGroups,TotalGroups,1)]
void CSTotal(uint3 dtid: SV_DispatchThreadId, uint3 localId : SV_GroupThreadId, uint3 gid : SV_GroupId, uint gi : SV_GroupIndex) {
  uint x = dtid.x * 2;
  uint y = dtid.y * 2;
  float b1 = br(x,y);
  float b2 = br(x + 1, y);
  float b3 = br(x, y + 1);
  float b4 = br(x + 1, y + 1);
  bpacked[gi] = float4(b1,b2,b3,b4);

  GroupMemoryBarrierWithGroupSync();

  [unroll]
  for (uint thres = TotalGroups*TotalGroups / 2; thres > 0; thres /= 2) {
    if (gi < thres) {
      bpacked[gi] += bpacked[gi + thres];
    }
    GroupMemoryBarrierWithGroupSync();
  };
// Doesn't work on microsoft basic render driver
/*  if (gi < 32) {
    // It works correctly on HD 4600, when TotalGroups equals 32 only. WTF?
    bpacked[gi] += bpacked[gi + 32];
    bpacked[gi] += bpacked[gi + 16];
    bpacked[gi] += bpacked[gi + 8];
    bpacked[gi] += bpacked[gi + 4];
    bpacked[gi] += bpacked[gi + 2];
    bpacked[gi] += bpacked[gi + 1];
  }
*/
  if (gi == 0) {
    float value = dot(bpacked[0], float4(1, 1, 1, 1));
    total[gid.x + gid.y*HDispatch] = value;
  };
}

#define BufTotal 1024
groupshared float stotal[BufTotal];
RWStructuredBuffer<uint> ui_total : register(u1);

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
    uint comp, orig = ui_total[0];
    [allow_uav_condition]do
    {
      comp = orig;
      InterlockedCompareExchange(ui_total[0], comp, asuint(asfloat(orig) + value), orig);
    } while (orig != comp);
  };
}
