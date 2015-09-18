#version 330 core

uniform mat4 uMVP;

in vec3 aVertex;
in vec3 aColor;
in float aID;

out vec3 vColor;
out float vID;

void main( void ) { 

  gl_Position = uMVP * vec4( aVertex, 1.0 );

  vColor = aColor;
  vID = aID;

}
