
#define MAX_BONES 13

attribute vec4 position;
attribute vec3 normal;
attribute vec4 color;
attribute vec4 weights;
attribute vec4 bone_indices;
uniform mat4 bone_matrices[MAX_BONES];
uniform mat4 inverse_bind_pose_matrices[MAX_BONES];
varying vec4 fColor;
uniform mat4 mvp_matrix;
uniform mat4 itmv_matrix;

void main() {
  vec3 light_direction = vec3(0, -1, -1);
  vec4 vertex_color = vec4(0.0, 0.0, 0.0, 1.0);
  float ambient_intensity = 0.2;

  vec4 transformed_normal = itmv_matrix * vec4(normal, 0.0);
  transformed_normal = vec4(0, 0, 0, 0);

vec4 p = vec4(0.0, 0.0, 0.0, 0.0);
  for (int i = 0; i < 4; i++) {
      int index = int(bone_indices[i]);
      p += bone_matrices[index] *
           inverse_bind_pose_matrices[index] * position * weights[i];
      transformed_normal += bone_matrices[index] * vec4(normal, 0.0) * weights[i];
  }
  transformed_normal = itmv_matrix * transformed_normal;
  transformed_normal[3] = 0.0;
  transformed_normal = normalize(transformed_normal);

  gl_Position = mvp_matrix * p;

  vertex_color += color * ambient_intensity;
  
  vec3 half_plane = normalize(vec3(0, 0, 1) - light_direction);
  float n_dot_h = max(0.0f, dot(transformed_normal.xyz, half_plane));
  vertex_color += color * n_dot_h;
  vertex_color += vec4(1, 1, 1, 1) * n_dot_h * 0.05;
  
  fColor = vertex_color;
}