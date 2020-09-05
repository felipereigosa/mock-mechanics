
// // varying vec4 fColor;
// // varying vec2 f_texture_coordinates;
// // uniform sampler2D texture_diffuse; 

// // void main(){
// //   vec4 texture_color = texture2D(texture_diffuse, f_texture_coordinates).gbar;
// //   gl_FragColor = fColor * texture_color;
// // }

// varying vec4 fColor;

// void main(){
//   gl_FragColor = 0.01 * fColor + vec4(0, 1, 0, 1) * 0.99;
// }

varying vec4 fColor;

void main(){
  gl_FragColor = fColor;
}
