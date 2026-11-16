# ToneMapping

Wolfram Language paclet for HDR tone mapping operators with seamless delegation to built-in `ColorToneMapping`.

## Supported Operators (`$ToneMappingMethods`)

- **CG / Filmic / Perceptual:** `"ACESFilmic"` (Narkowicz 2015), `"AgX"` (Sobotka / Blender 4), `"PBRNeutral"` (Khronos 2024), `"Hable"` (Uncharted 2), `"Reinhard"`, `"ReinhardLuminance"`, `"ReinhardExtended"`, `"Drago"` (2003), `"Durand"` (2002 bilateral), `"Exposure"`
- **Built-in `ColorToneMapping`:** `"BaseLuminance"`, `"DodgingAndBurning"`, `"Luminance"`, `"LogLuminance"`, `"RGB"`

## Quick Start

```mathematica
Needs["ToneMapping`"]

ToneMap[hdrImage, Method -> "AgX"]
ToneMap[hdrImage, Method -> "ACESFilmic", "Exposure" -> 0.5]
ToneMap[hdrImage, Method -> "Luminance"] (* delegates to ColorToneMapping *)
```
