(* ::Package:: *)

BeginPackage["ToneMapping`"];

ToneMap::usage =
  "ToneMap[image, opts] applies an HDR tone mapping operator to an Image or numeric array.\n" <>
  "ToneMap[image, Method -> method] selects a built-in ColorToneMapping method or an HDR operator from $ToneMappingMethods.";

$ToneMappingMethods::usage =
  "$ToneMappingMethods lists all tone mapping methods supported by ToneMap, including built-in ColorToneMapping methods and CG/filmic operators.";

$ToneMappingVersion::usage =
  "$ToneMappingVersion gives the version string of the loaded ToneMapping paclet.";

LinearToSRGB::usage =
  "LinearToSRGB[x] converts linear scene-referred RGB values in [0, 1] to IEC 61966-2-1 sRGB.";

SRGBToLinear::usage =
  "SRGBToLinear[x] converts IEC 61966-2-1 sRGB values in [0, 1] to linear RGB.";

Begin["`Private`"];

$ToneMappingVersion = "1.0.2";

$BuiltInToneMappingMethods = {
  "BaseLuminance",
  "DodgingAndBurning",
  "Luminance",
  "LogLuminance",
  "RGB"
};

$CustomToneMappingMethods = {
  "Exposure",
  "Reinhard",
  "ReinhardLuminance",
  "ReinhardExtended",
  "Drago",
  "Hable",
  "ACESFilmic",
  "AgX",
  "PBRNeutral",
  "Durand"
};

$ToneMappingMethods = Join[$CustomToneMappingMethods, $BuiltInToneMappingMethods];

ToneMap::invmth = "`1` is not a supported method. Use one of `2`.";

(* IEC 61966-2-1 sRGB transfer functions *)
LinearToSRGB[img_Image] := Image[LinearToSRGB[ImageData[img, "Real32"]], "Real32", ColorSpace -> ImageColorSpace[img]];
LinearToSRGB[arr_NumericArray] := NumericArray[LinearToSRGB[Normal[arr]], "Real32"];
LinearToSRGB[x_] := Module[{c = Clip[N[x], {0., 1.}], m},
  m = UnitStep[c - 0.0031308];
  Clip[(1. - m) * (12.92 * c) + m * (1.055 * (Clip[c, {1.*^-8, 1.}]^(1. / 2.4)) - 0.055), {0., 1.}]
];

SRGBToLinear[img_Image] := Image[SRGBToLinear[ImageData[img, "Real32"]], "Real32", ColorSpace -> ImageColorSpace[img]];
SRGBToLinear[arr_NumericArray] := NumericArray[SRGBToLinear[Normal[arr]], "Real32"];
SRGBToLinear[x_] := Module[{c = Clip[N[x], {0., 1.}], m},
  m = UnitStep[c - 0.04045];
  (1. - m) * (c / 12.92) + m * ((c + 0.055) / 1.055)^2.4
];

(* Rec. 709 / sRGB luminance *)
computeLuminance[arr_] := If[ArrayDepth[arr] == 2,
  arr,
  0.2126 * arr[[All, All, 1]] + 0.7152 * arr[[All, All, 2]] + 0.0722 * arr[[All, All, 3]]
];

scaleByLuminance[arr_, lumIn_, lumOut_] := Module[{ratio},
  ratio = lumOut / Clip[lumIn, {1.*^-8, Infinity}];
  If[ArrayDepth[arr] == 2,
    ratio * lumIn,
    arr * Transpose[{ratio, ratio, ratio}, {3, 1, 2}]
  ]
];

(* Operator implementations on non-negative linear arrays; return linear [0, 1] unless noted *)
applyOperator["Exposure", arr_, _] := Clip[arr, {0., 1.}];

applyOperator["Reinhard", arr_, _] := arr / (1. + arr);

applyOperator["ReinhardLuminance", arr_, _] := Module[{lum, lumOut},
  lum = computeLuminance[arr];
  lumOut = lum / (1. + lum);
  Clip[scaleByLuminance[arr, lum, lumOut], {0., 1.}]
];

applyOperator["ReinhardExtended", arr_, opts_Association] := Module[{lum, lw, lumOut},
  lum = computeLuminance[arr];
  lw = Replace[opts["WhitePoint"], Automatic :> Max[Quantile[Flatten[lum], 0.995], 1.]];
  lumOut = (lum * (1. + lum / (lw^2))) / (1. + lum);
  Clip[scaleByLuminance[arr, lum, lumOut], {0., 1.}]
];

applyOperator["Drago", arr_, opts_Association] := Module[{lum, lwa, lmax, b, lw, ld},
  lum = computeLuminance[arr];
  lwa = Exp[Mean[Flatten[Log[Clip[lum, {1.*^-6, Infinity}]]]]];
  lmax = Replace[opts["WhitePoint"], Automatic :> Max[Max[lum], 1.]];
  b = Clip[N[opts["Bias"]], {0.01, 0.99}];
  lw = lum / Max[lwa, 1.*^-6];
  ld = (0.01 * Log[lw + 1.]) / (Log10[lmax / Max[lwa, 1.*^-6] + 1.] * Log[2. + 8. * (lw / Max[lmax / Max[lwa, 1.*^-6], 1.*^-6])^(Log[b] / Log[0.5])]);
  Clip[scaleByLuminance[arr, lum, ld], {0., 1.}]
];

hableCurve[x_] := Module[{a = 0.15, b = 0.50, c = 0.10, d = 0.20, e = 0.02, f = 0.30},
  ((x * (a * x + c * b) + d * e) / (x * (a * x + b) + d * f)) - e / f
];

applyOperator["Hable", arr_, opts_Association] := Module[{exposureBias = 2.0, whiteScale},
  whiteScale = 1. / hableCurve[N[Replace[opts["WhitePoint"], Automatic -> 11.2]]];
  Clip[hableCurve[exposureBias * arr] * whiteScale, {0., 1.}]
];

(* Krzysztof Narkowicz (2015) ACES Filmic approximation *)
applyOperator["ACESFilmic", arr_, _] := Module[{x = 0.6 * arr},
  Clip[(x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14), {0., 1.}]
];

(* Khronos PBR Neutral tone mapper *)
applyOperator["PBRNeutral", arr_, _] := If[ArrayDepth[arr] == 2,
  Module[{startCompression = 0.8 - 0.04, x = arr, d},
    d = 1. - startCompression;
    If[Max[x] <= startCompression, x,
      Module[{m = UnitStep[x - startCompression], newPeak},
        newPeak = 1. - (d^2) / (x + d - startCompression);
        (1. - m) * x + m * newPeak
      ]
    ]
  ],
  Module[{r, g, b, x, peak, startCompression = 0.8 - 0.04, d = 0.24, newPeak, gDesat},
    r = arr[[All, All, 1]]; g = arr[[All, All, 2]]; b = arr[[All, All, 3]];
    x = MapThread[Min, {r, g, b}, 2];
    With[{off = (1. - UnitStep[x - 0.08]) * (x - 6.25 * x^2) + UnitStep[x - 0.08] * 0.04},
      r = r - off; g = g - off; b = b - off;
    ];
    peak = MapThread[Max, {r, g, b}, 2];
    With[{mask = UnitStep[peak - startCompression]},
      newPeak = 1. - (d^2) / (Clip[peak, {1.*^-6, Infinity}] + d - startCompression);
      With[{scale = (1. - mask) + mask * (newPeak / Clip[peak, {1.*^-6, Infinity}])},
        r = r * scale; g = g * scale; b = b * scale;
      ];
      gDesat = 1. - 1. / (0.15 * Clip[peak - newPeak, {0., Infinity}] + 1.);
      r = r + mask * gDesat * (newPeak - r);
      g = g + mask * gDesat * (newPeak - g);
      b = b + mask * gDesat * (newPeak - b);
    ];
    Clip[Transpose[{r, g, b}, {3, 1, 2}], {0., 1.}]
  ]
];

(* Troy Sobotka AgX (returns display sRGB directly) *)
$AgXInsetMatrix = {
  {0.842479062253094, 0.0784335999999992, 0.0792237451477643},
  {0.0423282422610123, 0.878468636469772, 0.0791661274605434},
  {0.0423756549057051, 0.0784336, 0.879142973793104}
};
$AgXOutsetMatrix = Inverse[$AgXInsetMatrix];

agxDefaultContrastApprox[x_] :=
  15.5 * x^6 - 40.14 * x^5 + 31.96 * x^4 - 6.868 * x^3 + 0.4298 * x^2 + 0.1191 * x - 0.00232;

applyAgX[arr_] := Module[{dims = Dimensions[arr], flat, inLog, curved, out},
  If[ArrayDepth[arr] == 2,
    inLog = Clip[(Log2[Clip[arr, {1.*^-10, Infinity}]] + 12.47393) / 16.5, {0., 1.}];
    Clip[agxDefaultContrastApprox[inLog], {0., 1.}],
    flat = Flatten[arr, 1];
    flat = flat . Transpose[$AgXInsetMatrix];
    inLog = Clip[(Log2[Clip[flat, {1.*^-10, Infinity}]] + 12.47393) / 16.5, {0., 1.}];
    curved = agxDefaultContrastApprox[inLog];
    out = curved . Transpose[$AgXOutsetMatrix];
    Clip[ArrayReshape[out, dims], {0., 1.}]
  ]
];

(* Durand & Dorsey (2002) fast bilateral local tone mapping *)
applyOperator["Durand", arr_, opts_Association] := Module[
  {lum, logLum, base, detail, targetContrast, minBase, maxBase, scale, outLog, outLum},
  lum = Clip[computeLuminance[arr], {1.*^-6, Infinity}];
  logLum = Log10[lum];
  base = ImageData[BilateralFilter[Image[logLum, "Real32"], 8, 0.4], "Real32"];
  detail = logLum - base;
  targetContrast = N[opts["Contrast"]];
  {minBase, maxBase} = MinMax[base];
  scale = targetContrast / Max[maxBase - minBase, 1.*^-6];
  outLog = (base - maxBase) * scale + detail;
  outLum = 10.^outLog;
  Clip[scaleByLuminance[arr, lum, outLum], {0., 1.}]
];

encodeDisplay[arr_, "sRGB", _] := LinearToSRGB[arr];
encodeDisplay[arr_, "Linear", _] := Clip[N[arr], {0., 1.}];
encodeDisplay[arr_, "Gamma", gamma_] := Clip[N[arr], {0., 1.}]^(1. / N[gamma]);
encodeDisplay[arr_, _, _] := LinearToSRGB[arr];

Options[ToneMap] = {
  Method -> "ACESFilmic",
  "Exposure" -> 0.,
  "WhitePoint" -> Automatic,
  "Bias" -> 0.85,
  "Contrast" -> 2.0,
  "DisplayEncoding" -> "sRGB",
  "Gamma" -> 2.2
};

ToneMap[img_Image, opts : OptionsPattern[]] := Module[
  {method = OptionValue[Method], exposure = N[OptionValue["Exposure"]],
   enc = OptionValue["DisplayEncoding"], gamma = OptionValue["Gamma"],
   arr, scaled, mapped, encoded, cs},
  If[MemberQ[$BuiltInToneMappingMethods, method],
    scaled = If[exposure == 0., img, ImageMultiply[img, 2.^exposure]];
    Return[ColorToneMapping[scaled, Method -> method]]
  ];
  If[!MemberQ[$CustomToneMappingMethods, method],
    Message[ToneMap::invmth, method, $ToneMappingMethods];
    Return[$Failed]
  ];
  arr = Clip[N[ImageData[img, "Real32"]], {0., Infinity}] * (2.^exposure);
  If[method === "AgX",
    mapped = applyAgX[arr];
    encoded = Switch[enc,
      "Linear", SRGBToLinear[mapped],
      "Gamma", SRGBToLinear[mapped]^(1. / N[gamma]),
      _, mapped
    ],
    mapped = applyOperator[method, arr, <|
      "WhitePoint" -> OptionValue["WhitePoint"],
      "Bias" -> OptionValue["Bias"],
      "Contrast" -> OptionValue["Contrast"]
    |>];
    encoded = encodeDisplay[mapped, enc, gamma];
  ];
  cs = If[ArrayDepth[encoded] == 2, "Grayscale", "RGB"];
  Image[encoded, "Real32", ColorSpace -> cs]
];

ToneMap[arr_NumericArray, opts : OptionsPattern[]] :=
  NumericArray[ToneMap[Normal[arr], opts], "Real32"];

ToneMap[arr_?ArrayQ, opts : OptionsPattern[]] := Module[
  {method = OptionValue[Method], exposure = N[OptionValue["Exposure"]],
   enc = OptionValue["DisplayEncoding"], gamma = OptionValue["Gamma"],
   lin, mapped},
  If[MemberQ[$BuiltInToneMappingMethods, method],
    Return[ImageData[ColorToneMapping[Image[N[arr] * (2.^exposure), "Real32"], Method -> method], "Real32"]]
  ];
  If[!MemberQ[$CustomToneMappingMethods, method],
    Message[ToneMap::invmth, method, $ToneMappingMethods];
    Return[$Failed]
  ];
  lin = Clip[N[arr], {0., Infinity}] * (2.^exposure);
  If[method === "AgX",
    mapped = applyAgX[lin];
    Switch[enc,
      "Linear", SRGBToLinear[mapped],
      "Gamma", SRGBToLinear[mapped]^(1. / N[gamma]),
      _, mapped
    ],
    mapped = applyOperator[method, lin, <|
      "WhitePoint" -> OptionValue["WhitePoint"],
      "Bias" -> OptionValue["Bias"],
      "Contrast" -> OptionValue["Contrast"]
    |>];
    encodeDisplay[mapped, enc, gamma]
  ]
];

End[];
EndPackage[];
