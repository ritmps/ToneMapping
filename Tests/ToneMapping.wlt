(* ::Package:: *)

BeginTestSection["ToneMapping"]

VerificationTest[
  PacletDirectoryLoad[
    If[StringLength[$InputFileName] > 0,
      DirectoryName[DirectoryName[$InputFileName]],
      ExpandFileName["ToneMapping"]
    ]
  ];
  Needs["ToneMapping`"];
  MemberQ[$Packages, "ToneMapping`"],
  True,
  TestID -> "LoadPackage"
]

VerificationTest[
  Length[$ToneMappingMethods] == 15,
  True,
  TestID -> "MethodCount"
]

VerificationTest[
  Max[Abs[SRGBToLinear[LinearToSRGB[{0., 0.002, 0.18, 0.5, 1.0}]] - {0., 0.002, 0.18, 0.5, 1.0}]] < 1.*^-5,
  True,
  TestID -> "SRGBRoundTrip"
]

VerificationTest[
  Module[{arr, img, results},
    arr = ConstantArray[0.18, {16, 16, 3}];
    arr[[1 ;; 4, 1 ;; 4, All]] = 25.0;
    img = Image[arr, "Real32", ColorSpace -> "RGB"];
    results = ToneMap[img, Method -> #] & /@ $ToneMappingMethods;
    AllTrue[results, ImageQ[#] && ImageDimensions[#] === {16, 16} &]
  ],
  True,
  TestID -> "AllMethodsReturnValidImage"
]

VerificationTest[
  Module[{arr, res},
    arr = ConstantArray[0.18, {8, 8, 3}];
    res = ToneMap[arr, Method -> "AgX"];
    ArrayQ[res] && Dimensions[res] === {8, 8, 3} && Min[res] >= 0. && Max[res] <= 1.
  ],
  True,
  TestID -> "ArrayInputReturnsArray"
]

VerificationTest[
  Module[{na, res},
    na = NumericArray[ConstantArray[0.5, {8, 8, 3}], "Real32"];
    res = ToneMap[na, Method -> "PBRNeutral"];
    MatchQ[res, _NumericArray] && Dimensions[res] === {8, 8, 3}
  ],
  True,
  TestID -> "NumericArrayInputReturnsNumericArray"
]

VerificationTest[
  Module[{arr, lowEV, highEV},
    arr = ConstantArray[0.18, {8, 8, 3}];
    lowEV = Mean[Flatten[ToneMap[arr, Method -> "Reinhard", "Exposure" -> -1.]]];
    highEV = Mean[Flatten[ToneMap[arr, Method -> "Reinhard", "Exposure" -> 1.]]];
    lowEV < highEV
  ],
  True,
  TestID -> "ExposureShiftsBrightness"
]

EndTestSection[]
