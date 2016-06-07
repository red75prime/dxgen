#define RSD "RootFlags(ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT | ALLOW_STREAM_OUTPUT),CBV(b0),SRV(t0)"

cbuffer cb0 : register(b0) {
  float4x4 view;
  float4x4 proj;
  float3 eye_pos;
  float3 light_pos;
}

struct InstanceData {
  float4x4 world;
  float3x3 n_world;
  float3 color;
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

#include "shadow_constants.hlsl"

// index of cube face vector points to
int cube_face(float3 v) {
  float3 va = abs(v);
  if (va.x>=va.y && va.x>=va.z) {
      // x - max
      return v.x>0 ? 0 : 1;
  } else if (va.y>=va.x && va.y>=va.z) {
      // y - max
      return v.y>0 ? 2 : 3;
  } else {
      // z - max
      return v.z>0 ? 4 : 5;
  }
}

[RootSignature(RSD)]
[maxvertexcount(18)]
void GSMain(triangle VS_OUTPUT input[3], inout TriangleStream<PS_IN> outstream) {
  // Ok. We have triangle in light-centric coordinates. 
  // Let's replicate them onto appropriate cube sides.
  int3 faces = int3(cube_face(input[0].pos.xyz),cube_face(input[1].pos.xyz),cube_face(input[2].pos.xyz));
  int oneface = 6;
  if (all(faces == int3(0, 0, 0))) {
    oneface = 0;
  } else if (all(faces == int3(1, 1, 1))) {
    oneface = 1;
  } else if (all(faces == int3(2, 2, 2))) {
    oneface = 2;
  } else if (all(faces == int3(3, 3, 3))) {
    oneface = 3;
  } else if (all(faces == int3(4, 4, 4))) {
    oneface = 4;
  } else if (all(faces == int3(5, 5, 5))) {
    oneface = 5;
  };

  PS_IN outval;
  if (oneface == 0 || oneface == 6) {
    // y,z  +x
    outval.face = 0;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(-input[i].pos.z, input[i].pos.y, input[i].pos.x*k1+k2, input[i].pos.x); //input[i].pos.x, input[i].pos.x);
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  if (oneface == 1 || oneface == 6) {
    // y,z  -x
    outval.face = 1;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.z, input[i].pos.y, -input[i].pos.x*k1+k2, -input[i].pos.x);
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  if (oneface == 2 || oneface == 6) {
    // x, z  +y
    outval.face = 2;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.x, -input[i].pos.z, input[i].pos.y*k1+k2, input[i].pos.y);
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  if (oneface == 3 || oneface == 6) {
    // x, z  -y
    outval.face = 3;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.xz, -input[i].pos.y*k1+k2, -input[i].pos.y);
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  if (oneface == 4 || oneface == 6) {
    // x, y  +z
    outval.face = 4;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(input[i].pos.x, input[i].pos.y, input[i].pos.z*k1+k2, input[i].pos.z);
      outstream.Append(outval);
    }
    outstream.RestartStrip();
  }
  if (oneface == 5 || oneface == 6) {
    // x, y  -z
    outval.face = 5;
    for (uint i = 0; i < 3; i++) {
      outval.pos = float4(-input[i].pos.x, input[i].pos.y, -input[i].pos.z*k1+k2, -input[i].pos.z);
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
