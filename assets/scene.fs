#version 330

in vec2 fragTexCoord;
in vec4 fragColor;
in vec3 fragPosition;
in vec3 fragNormal;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec4 colEmission;
uniform vec4 colAmbient;
uniform vec4 colLight;
uniform vec3 lightVector;

uniform mat4 shadowLightMatrix;
uniform float shadowIntensity;
uniform sampler2D shadowMap;

out vec4 finalColor;

void main() {
  vec4 texelColor = texture(texture0, fragTexCoord);
  if (texelColor.a == 0.0) discard;
  
  float diffuseIntensity = abs(dot(lightVector, fragNormal));
  vec4 materialColor = texelColor * colDiffuse;
  finalColor = colEmission * materialColor;
  vec4 diffuseColor = vec4(materialColor.rgb, 1.0) * colLight * diffuseIntensity;
  finalColor += vec4(diffuseColor.rgb * diffuseColor.a, 0.0);
  vec4 ambientColor = vec4(materialColor.rgb, 1.0) * colAmbient * colLight;
  finalColor += vec4(ambientColor.rgb * ambientColor.a, 0.0);
  finalColor *= fragColor;
  
  vec4 lightTexCoord = shadowLightMatrix * vec4(fragPosition, 1.0f);
  lightTexCoord.xy /= lightTexCoord.w;
  lightTexCoord.xy = (lightTexCoord.xy + 1.0f) / 2.0f;
  float shadowAlpha = texture(shadowMap, lightTexCoord.xy).a;
  if (shadowAlpha > 0.0 && all(greaterThanEqual(lightTexCoord.xy, vec2(0.0))) && all(lessThanEqual(lightTexCoord.xy, vec2(1.0))))
    finalColor.rgb *= (1.0 - shadowIntensity * shadowAlpha);
}
