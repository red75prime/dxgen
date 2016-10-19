float3 rgb2Yxy(float3 cl) {
  const float3x3 RGB2XYZ = { 0.5141364, 0.3238786, 0.16036376, 0.265068, 0.67023428, 0.06409157, 0.0241188, 0.1228178, 0.84442666 };
  float3 xyz = mul(RGB2XYZ, cl);
  return float3(xyz.g, xyz.rg / dot(float3(1,1,1),xyz));
}

float3 Yxy2rgb(float3 Yxy) {
  const float3x3 XYZ2RGB = { 2.5651,-1.1665,-0.3986, -1.0217, 1.9777, 0.0439, 0.0753, -0.2543, 1.1892 };
  float3 xyz = float3(Yxy.r*Yxy.g / Yxy.b, Yxy.r, Yxy.r*(1 - Yxy.g - Yxy.b) / Yxy.b);
  return mul(XYZ2RGB, xyz);
}

#define RSD "RootFlags(0), DescriptorTable(SRV(t0), SRV(t1), UAV(u0))" \
            ",RootConstants(num32BitConstants=2, b0)" \
            ",StaticSampler(s0, filter=FILTER_MIN_MAG_LINEAR_MIP_POINT, " \
            "addressU=TEXTURE_ADDRESS_CLAMP, addressV=TEXTURE_ADDRESS_CLAMP)"

cbuffer cb0 : register(b0) {
  float key;
  float avg_brightness;
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
  float3 cl = src[crd].rgb + hvtemp.SampleLevel(linSamp,crd/float2(w-1,h-1), 0).rgb;

  float3 Yxy = rgb2Yxy(cl);
  Yxy.r *= key;

  dst[crd] = float4(Yxy2rgb(Yxy), 1);

}

#define RSDH "RootFlags(0), DescriptorTable(SRV(t0), UAV(u0)), RootConstants(num32BitConstants=2, b0)"

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
  float3 scl = (src[uint2(srcx, srcy)].rgb + src[uint2(srcx, srcy + 1)].rgb);
  tmp1[gtid.x] = brightness(scl)>avg_brightness*40 ? scl : float3(0, 0, 0);

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
