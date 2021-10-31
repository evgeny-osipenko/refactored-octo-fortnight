#version 100

precision highp float;

uniform vec2 uWindowSize;
uniform vec3 uViewX;
uniform vec3 uViewY;
uniform vec2 uTextureSize;

attribute vec2 aArg;
attribute vec3 aPosX;
attribute vec3 aPosY;
attribute vec4 aTexRect;

varying vec2 vTexPos;

vec2 transformVec(vec3 vx, vec3 vy, vec2 u) {
    mat2 mat = mat2(vx.xy, vy.xy);
    vec2 offset = vec2(vx.z, vy.z);
    return mat * u + offset;
}

void main() {
    vec2 rectExtent = aTexRect.zw - aTexRect.xy;

    vec2 rectPos = aArg * 0.5 + 0.5;
    vec2 texPixelPos = (aTexRect.xy + rectPos * rectExtent);
    vTexPos = texPixelPos * vec2(1.0, -1.0) / uTextureSize;

    vec2 worldPos = transformVec(aPosX, aPosY, aArg * rectExtent * 0.5);
    vec2 viewPos = transformVec(uViewX, uViewY, worldPos);
    vec2 clipPos = 2.0 / uWindowSize * viewPos;
    gl_Position = vec4(clipPos, 0, 1);
}
