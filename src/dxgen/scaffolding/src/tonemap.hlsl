

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

#define RSD "RootFlags(0), DescriptorTable(SRV(t0), SRV(t1), UAV(u0))"

Texture2D<float4> src: register(t0);
Texture2D<float4> htemp: register(t1);
RWTexture2D<float4> dst: register(u0);

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

groupshared float3 tmp[VTGroups+VKernelSize*2];

[RootSignature(RSD)]
[numthreads(VTGroups+VKernelSize*2, 1, 1)]
void CSMain(uint3 gtid: SV_GroupThreadId, uint3 gid: SV_GroupId) {
  uint2 crd = uint2(gid.x, gid.y * VTGroups + gtid.x - VKernelSize);
  tmp[gtid.x] = htemp[crd].rgb;

  AllMemoryBarrierWithGroupSync();

  uint gty = gtid.x;

  if (gty >= VKernelSize && gty < VTGroups + VKernelSize) {
      float3 acc = float3(0, 0, 0);
      [unroll]
      for(int y=-VKernelSize;y<=VKernelSize;y+=1) {
        acc = mad(tmp[y + gty], vkernel[y+VKernelSize], acc);
      }

      float3 cl = src[crd].rgb + acc;

      float br = cl.r+cl.g+cl.b;
      float3 clcl = clamp(cl, float3(0,0,0), float3(1,1,1));
      float brcl = clcl.r+clcl.g+clcl.b;
      if (brcl < br) {
        clcl += br - brcl;
      };
      dst[crd] = float4(clcl, 1);
  };
}


#define RSDH "RootFlags(0), DescriptorTable(SRV(t0), UAV(u0))"

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
  uint2 crd = uint2(gid.x * HTGroups + gtid.x - KernelSize, gid.y);
  tmp1[gtid.x] = src[crd].rgb;

  AllMemoryBarrierWithGroupSync();

  uint gtx = gtid.x;

  if (gtx >= KernelSize && gtx < HTGroups + KernelSize) {

    float3 acc = float3(0, 0, 0);
    [unroll]
    for (int x = -KernelSize; x <= KernelSize; x += 1) {
      float3 cl = tmp1[x + gtx];
      if (cl.r + cl.g + cl.b > 1.0) {
        acc = mad(cl, kernel[x + KernelSize], acc);
      };
    }

    dst[crd] = float4(acc,1);
  };

}
