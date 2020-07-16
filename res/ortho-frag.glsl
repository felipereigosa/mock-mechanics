
varying vec2 f_texture_coordinates;
uniform sampler2D texture_diffuse; 

void main(){
  vec4 texture_color = texture2D(texture_diffuse, f_texture_coordinates).gbar;
  gl_FragColor = texture_color;
}
