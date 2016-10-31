cbuffer cb0 : register(b0) {
  float4x4 viewproj;
  float4x4 view;
  float3 eye_pos;
  float3 light_pos;
  float2 tfov_xy;
}

float2 sphere_coord(float3 r) {
  const float pi = 3.141592653589;
	return float2(0.5 - atan2(r.x, r.z)/2/pi, 0.5 + atan2(r.y,sqrt(r.x*r.x+r.z*r.z))/pi);  
}

float4 sample_sphere(Texture2D<float4> tex, sampler s, float3 r) {
	return tex.Sample(s, sphere_coord(r));
} 
