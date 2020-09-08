
varying vec4 fColor;
uniform sampler2D texture_diffuse;
varying vec2 f_texture_coordinates;

void main(){
  vec4 texture_color = texture2D(texture_diffuse, f_texture_coordinates).gbar;
  gl_FragColor = fColor * texture_color;
}
