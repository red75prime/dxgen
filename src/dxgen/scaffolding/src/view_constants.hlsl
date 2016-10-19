cbuffer cb0 : register(b0) {
  float4x4 viewproj;
  float4x4 view;
  float3 eye_pos;
  float3 light_pos;
  float2 tfov_xy;
}
