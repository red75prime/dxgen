

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
        acc = mad(tmp[y + gty], exp(-y*y / 8.)*0.2, acc);
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
      acc = mad(tmp1[x+gtx], exp(-x*x / 16.)*0.2, acc);
    }

    dst[crd] = float4(acc,1);
  };

}
