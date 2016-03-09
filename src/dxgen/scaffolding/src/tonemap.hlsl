

static const float3x3 rgb2xyz = 
  {
 0.4124564,  0.3575761,  0.1804375,
 0.2126729,  0.7151522,  0.0721750,
 0.0193339,  0.1191920,  0.9503041  };

static const float3x3 xyz2rgb =
{
3.2404542, -1.5371385, -0.4985314,
-0.9692660,  1.8760108,  0.0415560,
 0.0556434, -0.2040259,  1.0572252
};

static const float thres1 = 6*6*6/29/29/29;
static const float c1 = 29*29/6/6/3;

#define RSD "RootFlags(0), DescriptorTable(SRV(t0), SRV(t1), UAV(u0))" \
            ",RootConstants(num32BitConstants=1, b0)" \
            ",StaticSampler(s0, filter=FILTER_MIN_MAG_LINEAR_MIP_POINT, " \
            "addressU=TEXTURE_ADDRESS_CLAMP, addressV=TEXTURE_ADDRESS_CLAMP)"

cbuffer cb0 : register(b0) {
  float key;
};

Texture2D<float4> src: register(t0);
Texture2D<float4> hvtemp: register(t1);
RWTexture2D<float4> dst: register(u0);
SamplerState  linSamp : register(s0);

#define CombineGroups 32

float brightness(float3 cl) {
  return dot(float3(0.2126, 0.7152, 0.0722), cl.rgb);
}

[RootSignature(RSD)]
[numthreads(CombineGroups, CombineGroups, 1)]
void CSMain(uint3 dtid: SV_DispatchThreadId, uint3 gtid: SV_GroupThreadId, uint3 gid: SV_GroupId) {
  uint2 crd = dtid.xy;
  uint w, h;
  dst.GetDimensions(w, h);
  float3 cl = src[crd].rgb*float3(key, key, key) + hvtemp.SampleLevel(linSamp,crd/float2(w-1,h-1), 0).rgb;

  float br = brightness(cl);
  float3 clcl = clamp(cl, float3(0,0,0), float3(1,1,1));
  float brcl = brightness(clcl);
  if (brcl < br) {
    clcl += br - brcl;
  };
  dst[crd] = float4(clcl, 1);
  //dst[crd] = float4(cl, 1);
}

#define RSDH "RootFlags(0), DescriptorTable(SRV(t0), UAV(u0)), RootConstants(num32BitConstants=1, b0)"

#define HTGroups 128
#define KernelSize 16

static const float kernel[KernelSize * 2 + 1] = {
3.34587E-05,
8.8152E-05,
0.000218178,
0.00050728,
0.001108001,
0.002273471,
0.00438223,
0.007935194,
0.013498219,
0.021570093,
0.032380545,
0.045663887,
0.060494822,
0.075287022,
0.088019446,
0.09667045,
0.0997391,
0.09667045,
0.088019446,
0.075287022,
0.060494822,
0.045663887,
0.032380545,
0.021570093,
0.013498219,
0.007935194,
0.00438223,
0.002273471,
0.001108001,
0.00050728,
0.000218178,
8.8152E-05,
3.34587E-05
};

groupshared float3 tmp1[HTGroups+KernelSize*2];

[RootSignature(RSDH)]
[numthreads(HTGroups+KernelSize*2, 1, 1)]
void CSHorizontal(uint3 gtid: SV_GroupThreadId, uint3 gid : SV_GroupId) {
  uint srcx = gid.x * HTGroups + gtid.x - KernelSize;
  uint srcy = gid.y * 2;
  float3 scl = (src[uint2(srcx, srcy)].rgb + src[uint2(srcx, srcy + 1)].rgb)*float3(key/2, key/2, key/2);
  tmp1[gtid.x] = brightness(scl)>1.0 ? scl : float3(0, 0, 0);

  AllMemoryBarrierWithGroupSync();

  uint gtx = gtid.x;

  if (gtx >= KernelSize && gtx < HTGroups + KernelSize && ((gtx & 1) == 0)) {

    float3 acc = float3(0, 0, 0);
    [unroll]
    for (int x = -KernelSize; x <= KernelSize; x += 1) {
      float3 cl = tmp1[x + gtx];
      acc = mad(cl, kernel[x + KernelSize], acc);
    }

    dst[uint2(gid.x * (HTGroups/2) + (gtx-KernelSize)/2, gid.y)] = float4(acc,1);
  };

}

#define VTGroups 128
#define VKernelSize 8

static const float vkernel[VKernelSize * 2 + 1] = {
  6.69163E-05,
  0.000436349,
  0.002215963,
  0.008764304,
  0.026995958,
  0.064759937,
  0.12098749,
  0.176035759,
  0.199474648,
  0.176035759,
  0.12098749,
  0.064759937,
  0.026995958,
  0.008764304,
  0.002215963,
  0.000436349,
  6.69163E-05
};

groupshared float3 tmp[VTGroups + VKernelSize * 2];

#define RSDV "RootFlags(0), DescriptorTable(SRV(t0), UAV(u0))"

[RootSignature(RSDV)]
[numthreads(1, VTGroups+VKernelSize*2, 1)]
void CSVertical(uint3 gtid: SV_GroupThreadId, uint3 gid : SV_GroupId) {
  uint srcx = gid.x;
  uint srcy = gid.y * VTGroups + gtid.y - VKernelSize;
  tmp[gtid.y] = src[uint2(srcx, srcy)].rgb;

  GroupMemoryBarrierWithGroupSync();

  uint gtx = gtid.y;

  if (gtx >= VKernelSize && gtx < VTGroups + VKernelSize) {

    float3 acc = float3(0, 0, 0);
    [unroll]
    for (int x = -VKernelSize; x <= VKernelSize; x += 1) {
      float3 cl = tmp[x + gtx];
      acc = mad(cl, vkernel[x + VKernelSize], acc);
    }

    dst[uint2(srcx, srcy)] = float4(acc, 1);
  };

}
