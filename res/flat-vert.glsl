
attribute vec4 position;
attribute vec3 normal;
uniform vec4 material_color;
varying vec4 fColor;
uniform mat4 mvp_matrix;
uniform mat4 itmv_matrix;

void main() {
  vec3 light_direction = vec3(0, -1, -1);
  vec4 vertex_color = vec4(0.0, 0.0, 0.0, 1.0);
  float ambient_intensity = 0.2;

  vec4 transformed_normal = itmv_matrix * vec4(normal, 0.0);
  transformed_normal[3] = 0.0;
  transformed_normal = normalize(transformed_normal);

  vertex_color += material_color * ambient_intensity;
  
  vec3 half_plane = normalize(vec3(0, 0, 1) - light_direction);
  float n_dot_h = max(0.0f, dot(transformed_normal.xyz, half_plane));
  vertex_color += material_color * n_dot_h;
  vertex_color += vec4(1, 1, 1, 1) * n_dot_h * 0.05;

  gl_Position = mvp_matrix * position;

  if (material_color.w < 1.0) {
     fColor = material_color;
  }
  else {
     fColor = vertex_color;
  }
}
