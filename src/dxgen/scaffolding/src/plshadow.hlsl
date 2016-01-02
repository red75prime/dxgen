cbuffer cb0: register(b0) {
  float3 lightPos;
}

cbuffer cb1 : register(b1) {
  uint worldMatIdx;
}

StructuredBuffer<float3x3> worldMats;

