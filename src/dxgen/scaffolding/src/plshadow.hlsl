#define RSD "RootFlags(ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT),CBV(b0),SRV(t0)"

cbuffer cb0 : register(b0) {
  float4x4 view;
  float4x4 proj;
  float3 eye_pos;
  float3 light_pos;
}

struct InstanceData {
  float4x4 world;
  float3x3 n_world;
};

StructuredBuffer<InstanceData> instances : register(t0);

struct VS_INPUT {
  float3 vPosition : POSITION;
  float3 vColor : COLOR;
  float2 texc0 : TEXCOORD0;
  float3 norm : NORMAL;
};

struct VS_OUTPUT {
  float4 pos : SV_Position;
};

[RootSignature(RSD)]
VS_OUTPUT VSMain(VS_INPUT input, uint iidx: SV_InstanceID) {
  VS_OUTPUT ret;
  ret.pos = mul(float4(input.vPosition,1), instances[iidx].world) - float4(light_pos,0);
  return ret;
};

struct PS_IN {
  float4 pos : SV_POSITION;
//  float z : TEXCOORD0;
  uint face : SV_RenderTargetArrayIndex;
};

static const float fr = 200;
static const float nr = 0.1;
static const float k1 = fr/(fr-nr);
static const float k2 = -fr*nr/(fr-nr);

[RootSignature(RSD)]
[maxvertexcount(18)]
void GSMain(triangle VS_OUTPUT input[3], inout TriangleStream<PS_IN> outstream) {
  // Ok. We have triangle in light-centric coordinates. 
  // Let's replicate them onto appropriate cube sides.
  PS_IN outval;
  {
    // x, y  +z
    outval.face = 0;
//    outval.z = 0;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.xy, input[i].pos.z*k1+k2, input[i].pos.z);
//      outval.z = outval.pos.z/outval.pos.w;
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  {
    // x, y  -z
    outval.face = 1;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.xy, -input[i].pos.z*k1+k2, -input[i].pos.z);
//      outval.z = outval.pos.z/outval.pos.w;
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  {
    // z, y  +x
    outval.face = 2;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.zy, input[i].pos.x*k1+k2, input[i].pos.x); //input[i].pos.x, input[i].pos.x);
//      outval.z = outval.pos.z/outval.pos.w;
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  {
    // z, y  -x
    outval.face = 3;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.zy, -input[i].pos.x*k1+k2, -input[i].pos.x);
//      outval.z = outval.pos.z/outval.pos.w;
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  {
    // x, z  +y
    outval.face = 4;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.xz, input[i].pos.y*k1+k2, input[i].pos.y);
//      outval.z = outval.pos.z/outval.pos.w;
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  {
    // x, z  -y
    outval.face = 5;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.xz, -input[i].pos.y*k1+k2, -input[i].pos.y);
//      outval.z = outval.pos.z/outval.pos.w;
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
};

struct PS_OUT {
  float depth : SV_Depth;
};

void PSMain(PS_IN input) {
//  PS_OUT ret;
//  ret.depth = input.z;
//  return ret;
//  return 0.1; //input.z;
}
