#version 100

precision highp float;

uniform sampler2D uTextureSampler;

varying vec2 vTexPos;

void main() {
    gl_FragColor = texture2D(uTextureSampler, vTexPos);
}
