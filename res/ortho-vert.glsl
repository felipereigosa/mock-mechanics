
attribute vec4 position;
attribute vec2 texture_coordinates;
varying vec2 f_texture_coordinates;

void main() {
  gl_Position = position;
  f_texture_coordinates = texture_coordinates;
}
