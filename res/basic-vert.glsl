
attribute vec4 position;
uniform mat4 mvp_matrix;
uniform vec4 material_color;
varying vec4 fColor;

void main() {
  gl_Position = mvp_matrix * position;
  fColor = material_color;
}

