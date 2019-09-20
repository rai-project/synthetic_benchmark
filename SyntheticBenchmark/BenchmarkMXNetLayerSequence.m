
modelNames = {"Ademxapp Model A Trained on ImageNet Competition Data", "Age \
Estimation VGG-16 Trained on IMDB-WIKI and Looking at People Data", \
"Age Estimation VGG-16 Trained on IMDB-WIKI Data", "CapsNet Trained \
on MNIST Data", "Gender Prediction VGG-16 Trained on IMDB-WIKI Data", \
"Inception V1 Trained on Extended Salient Object Subitizing Data", \
"Inception V1 Trained on ImageNet Competition Data", "Inception V1 \
Trained on Places365 Data", "Inception V3 Trained on ImageNet \
Competition Data", "LeNet Trained on MNIST Data", "MobileNet V2 \
Trained on ImageNet Competition Data", "ResNet-101 Trained on \
ImageNet Competition Data", "ResNet-101 Trained on YFCC100m Geotagged \
Data", "ResNet-152 Trained on ImageNet Competition Data", "ResNet-50 \
Trained on ImageNet Competition Data",  "SqueezeNet V1.1 Trained on ImageNet Competition \
Data", "VGG-16 Trained on ImageNet Competition Data", "VGG-19 Trained \
on ImageNet Competition Data", "Wide ResNet-50-2 Trained on ImageNet \
Competition Data", "Wolfram ImageIdentify Net V1", "Yahoo Open NSFW \
Model V1"};

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


<< NeuralNetworks`
<< MXNetLink`


rootDirectory = FileNameDrop[$InputFileName, -1];
PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]

PrependTo[$ContextPath, "MXNetLink`PackageScope`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;




run[net_, fstLyr_, n_] :=
    Module[{plan, ex, data, res},
        NDArrayWaitForAll[];
        plan = ToNetPlan[net];
        ex = ToNetExecutor[plan, 1, "ArrayCaching" -> False];
        SeedRandom[1];
        data = synthesizeData /@ Inputs[lyr];
        Table[
            NDArraySet[ex["Arrays", "Inputs",  key], data[key]],
            {key, Keys[data]}
        ];
        NDArrayWaitForAll[];
        Table[
            NDArrayWaitForAll[];
            First[AbsoluteTiming[
                NetExecutorForward[ex, (* IsTraining= *) False];
                NDArrayWaitForAll[];
            ]],
            {n}
        ]
    ]

invalidVal = ""
$NumRuns = 50

summarize[t_] := TrimmedMean[t, 0.2]

benchmarkLayers[models_?ListQ] :=
  benchmarkLayers[models, 1]
benchmarkLayers[models_?ListQ, sequenceLength_] :=
  Module[{},
    baseDir = FileNameJoin[{rootDirectory, "..", "data", "mxnet_layer_sequence", "seq_" <> ToString[sequenceLength]}];
    Quiet[CreateDirectory[baseDir]];
    Do[
      runSequenceCache = <||>;
      benchmarkModelLayers[modelName, sequenceLength, $NumRuns],
      {modelName, models}
    ]
  ]

skipModel["Wolfram ImageIdentify Net V1", 9] = True
skipModel["MobileNet V2 Trained on ImageNet Competition Data", 10] = True
skipModel[___] := False

benchmarkModelLayers[modelName_String] :=
    benchmarkModelLayers[modelName, 1, $NumRuns]
benchmarkModelLayers[modelName_String, sequenceLength0_, nRuns_] :=
  Module[{ii},
    sequenceLength = sequenceLength0;
    If[skipModel[modelName, sequenceLength],
      Return[]
    ];
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    gr = NetInformation[model, "LayersGraph"];
    (* topo = TopologicalSort[gr]; *)
    topo = Keys[lyrs];
    Print["benchmarking .... " <> modelName <> " at sequence " <> ToString[sequenceLength]];
    timings = {};
    startLayer = 1;
    end = If[sequenceLength <= 0,
      Length[lyrs],
      Ceiling[Length[lyrs]/sequenceLength]
    ];
    For[ii = 0, ii < end, ii++,
      endLayer = Min[startLayer + sequenceLength, Length[lyrs]];
      xPrint[">>> ", {startLayer,endLayer}];
      seq=runSequence[startLayer, endLayer];
      timings = Flatten[{timings, seq}];
      timings = Select[timings, Lookup[#, "failed", False]===False&];
      startLayer = Lookup[Last[timings], "end_index"]+1;
      If[startLayer > Length[lyrs],
        Break[]
      ];
      xPrint[timings];
    ];
    xPrint[timings];
    Print["writing benchmark results .... " <> modelName <> " at sequence " <> ToString[sequenceLength]];
    writeTistartLayergs[StringReplace[modelName, " " -> "_"], timings]
  ]

writeTistartLayergs[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
    header = Keys[timings[[1]]];
    header = DeleteCases[header, "failed"];
    tbl = Table[ Table[Lookup[t, h, invalidVal], {h,header}], {t, timings}];
    PrependTo[tbl, header];
    Export[FileNameJoin[{baseDir, modelName <> ".csv"}], tbl, "CSV"]
  ]


runSequenceCache = <||>
runSequence[startLayer_, endLayer_] :=
  Module[{r},
    r = Lookup[runSequenceCache, Key[{startLayer, endLayer}]];
    xPrint[Keys@runSequenceCache];
    If[MissingQ[r],
      r = eRunSequence[startLayer, endLayer];
      AssociateTo[runSequenceCache, Key[{startLayer, endLayer}]->r];
    ];
    r
  ]
eRunSequence[startLayer_, endLayer_] :=
  Module[{r, failure},
    xPrint[{startLayer, endLayer}];
    failure = <|
      "failed" -> True,
      "start_index" -> startLayer,
      "end_index" -> endLayer,
      "start_name" -> invalidVal,
      "end_name" -> invalidVal,
      "sequence_length" -> invalidVal,
      "sequence_name" -> invalidVal,
      "start_layer_kind" -> invalidVal,
      "end_layer_kind" -> invalidVal,
      "sequence_layer_kind" -> invalidVal,
      "start_layer_path_time" -> invalidVal,
      "mean_path_time" -> invalidVal,
      "end_layer_path_time" -> invalidVal,
      "raw_path_time" -> invalidVal
    |>;
    If[startLayer === endLayer,
      r = iRunSequence[startLayer, startLayer];
      If[r === $Failed,
        Return[failure].
        Return[r]
      ];
    ];
    If[startLayer > Length[lyrs] || endLayer > Length[lyrs],
      Return[failure]
    ];
    If[startLayer > endLayer,
      Return[{}]
    ];
    r = iRunSequence[startLayer, endLayer];
    If[r =!= $Failed,
      Return[r]
    ];
    Flatten[{
      runSequence[startLayer, startLayer],
      runSequence[startLayer+1, endLayer+1]
    }]
    
  ]
(* 
xrunSequence[startLayer_, endLayer_] :=
  Module[{r},
    xPrint[{startLayer, endLayer}];
    If[startLayer === endLayer,
      r = iRunSequence[startLayer, startLayer];
      If[r =!= $Failed,
        Return[r]
      ];
      If[r === $Failed,
        Return[<|
          "failed" -> True,
          "start_index" -> startLayer,
          "end_index" -> endLayer,
          "start_name" -> invalidVal,
          "end_name" -> invalidVal,
          "sequence_length" -> invalidVal,
          "sequence_name" -> invalidVal,
          "start_layer_kind" -> invalidVal,
          "end_layer_kind" -> invalidVal,
          "sequence_layer_kind" -> invalidVal,
          "start_layer_path_time" -> invalidVal,
          "mean_path_time" -> invalidVal,
          "end_layer_path_time" -> invalidVal,
          "raw_path_time" -> invalidVal
        |>]
      ]
    ];
    If[startLayer > endLayer,
      Return[{}]
    ];
    r = iRunSequence[startLayer, endLayer];
    If[r =!= $Failed,
      Return[{r}]
    ];
    Prepend[
      runSequence[startLayer+1, endLayer],
      runSequence[startLayer, startLayer]
    ]
  ] *)

iRunSequence[startLayer_, endLayer_] :=
  Module[{r},
    Check[
      (* startLayerTime = Quiet@Check[
          CheckAbort[
          lyr = lyrs[[startLayer]];
          net = NetChain[Lookup[lyrs, topo[[startLayer ;; startLayer]]]];
          run[net, lyr, nRuns],
          xPrint["abort. startLayer .."];
          $Failed],
          $Failed
      ]; *)
(* 
      r = Lookup[runSequenceCache, Key[{startLayer, endLayer}]];
      If[!MissingQ[r],
        Return[r]
      ]; *)
      pathTime = Quiet@Check[
          CheckAbort[
          lyr = lyrs[[startLayer]];
          net = NetChain[Lookup[lyrs, topo[[startLayer ;; endLayer]]]];
          run[net, lyr, $NumRuns],
          xPrint["abort. path .."];
          $Failed],
          $Failed
      ];
      If[pathTime === $Failed,
        Return[$Failed]
      ];
      (* endLayerTime = Quiet@Check[
          CheckAbort[
          lyr = lyrs[[endLayer]];
          net = NetChain[Lookup[lyrs, topo[[endLayer ;; endLayer]]]];
          run[net, lyr, nRuns],
          xPrint["abort. endLayer .."];
          $Failed],
          $Failed
      ]; *)
      <|
        "failed" -> False,
        "start_index" -> startLayer,
        "end_index" -> endLayer,
        "start_name" -> StringRiffle[topo[[startLayer]], "/"],
        "end_name" -> StringRiffle[topo[[endLayer]], "/"],
        "sequence_length" -> (endLayer-startLayer),

        "sequence_name" -> StringRiffle[Table[StringRiffle[e, "/"], {e,topo[[startLayer;;endLayer]]}], "-"],
        "start_layer_kind" -> StringTrim[SymbolName[Head[Lookup[lyrs, Key[topo[[startLayer]]]]]], "Layer"],
        "end_layer_kind" -> StringTrim[SymbolName[Head[Lookup[lyrs, Key[topo[[endLayer]]]]]], "Layer"],


        "sequence_layer_kind" -> StringRiffle[
            Table[
                StringTrim[SymbolName[Head[Lookup[lyrs, Key[e]]]], "Layer"],
                {e,topo[[startLayer;;endLayer]]}
            ],
            "-"
        ],

        (* "start_layer_start_index_time" -> If[startLayerTime === $Failed, invalidVal, Round[1000000 * Min[startLayerTime], 0.0001]],
        "mean_start_index_time" -> If[startLayerTime === $Failed, invalidVal, Round[1000000 * summarize[startLayerTime], 0.0001]],
        "end_layer_start_index_time" -> If[startLayerTime === $Failed, invalidVal, Round[1000000 * Max[startLayerTime], 0.0001]],

        "start_layer_end_index_time" -> If[endLayerTime === $Failed, invalidVal, Round[1000000 * Min[endLayerTime], 0.0001]],
        "mean_end_index_time" -> If[endLayerTime === $Failed, invalidVal, Round[1000000 * summarize[endLayerTime], 0.0001]],
        "end_layer_end_index_time" -> If[endLayerTime === $Failed, invalidVal, Round[1000000 * Max[endLayerTime], 0.0001]], *)

        "start_layer_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * Min[pathTime], 0.0001]],
        "mean_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * summarize[pathTime], 0.0001]],
        "end_layer_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * Max[pathTime], 0.0001]],
(*           
        "start_layer_path_start_layer_minus_start_time" -> If[startLayerTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Min[pathTime]-Min[startLayerTime]), 0.0001]],
        "mean_path_start_layer_minus_start_time" -> If[startLayerTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (summarize[pathTime]-summarize[startLayerTime]), 0.0001]],
        "end_layer_path_start_layer_minus_start_time" -> If[startLayerTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Max[pathTime]-Max[startLayerTime]), 0.0001]],

        "start_layer_path_start_layer_minus_end_time" -> If[endLayerTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Min[pathTime]-Min[endLayerTime]), 0.0001]],
        "mean_path_start_layer_minus_end_time" -> If[endLayerTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (summarize[pathTime]-summarize[endLayerTime]), 0.0001]],
        "end_layer_path_start_layer_minus_end_time" -> If[endLayerTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Max[pathTime]-Max[endLayerTime]), 0.0001]], *)

        (* "raw_start_index_time" -> If[startLayerTime === $Failed, invalidVal, Round[1000000 * startLayerTime, 0.0001]], *)
        (* "raw_end_index_time" -> If[endLayerTime === $Failed, invalidVal, Round[1000000 * endLayerTime, 0.0001]], *)
        "raw_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * pathTime, 0.0001]]
      |>,
      $Failed
    ]
  ]


synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;
NetAttachLoss = NeuralNetworks`NetAttachLoss;


measureConstantOverhead[dims_] :=
  iMeasureConstantOverhead[Select[dims, IntegerQ]]
iMeasureConstantOverhead[{}] := ""
iMeasureConstantOverhead[dims_] :=
  Module[{},
    layer = ConstantArrayLayer["Array" -> ConstantArray[1, dims]];
    net = NetChain[{layer}];
    Min[Table[First[AbsoluteTistartLayerg[net[];]], {10}]]
  ]

paramsOf[lyr_[params_, ___]] := params
kernelSize[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["KernelSize"]}, If[ListQ[e], e, {"",""}]]
stride[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Stride"]}, If[ListQ[e], e, {"",""}]]
function[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Function"]},
  If[! MissingQ[e], e /. ValidatedParameter -> Identity, ""]]
dilation[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Dilation"]}, If[ListQ[e], e, {"",""}]]
inputDims[lyr_[params_, ___]] :=
 With[{e = params["Inputs"]},
  With[{r=If[AssociationQ[e],
   If[KeyExistsQ[e, "Input"], e["Input"], e["1"]] /.
    TensorT[x_, _] :> x, {"","",""}]},
    PadRight[r, 3, ""]
  ]]
outputDims[lyr_[params_, ___]] :=
 With[{r=With[{e = params["Outputs"]},
  If[AssociationQ[e], e["Output"] /. TensorT[x_, _] :> x, {"","",""}]]},
    PadRight[r, 3, ""]
 ]

PreemptProtect[
  AbortProtect[
    (* benchmarkLayers[modelNames, 0];
    benchmarkLayers[modelNames, 1];
    benchmarkLayers[modelNames, 2];
    benchmarkLayers[modelNames, 3];
    benchmarkLayers[modelNames, 4];
    benchmarkLayers[modelNames, 5];
    benchmarkLayers[modelNames, 6];
    benchmarkLayers[modelNames, 7];
    benchmarkLayers[modelNames, 8];
     *)
     (* benchmarkLayers[modelNames, 9]; *)
    benchmarkLayers[modelNames, 10];
    (* benchmarkLayers[modelNames, 11];
    benchmarkLayers[modelNames, 12];
    benchmarkLayers[modelNames, 13];
    benchmarkLayers[modelNames, 14];
    benchmarkLayers[modelNames, 15]; *)
  ]
];
Print["done benchmarking...."];
