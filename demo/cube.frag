#version 330 core

in vec3 vColor;
in float vID;

out vec4 color;

void main(void) {

  color = vec4( vColor  * abs( sin( vID * 10. )) , 1.0 );

}