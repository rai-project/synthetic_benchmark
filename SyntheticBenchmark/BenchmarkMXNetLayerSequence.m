
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
    Module[{plan, ex, data},
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
    baseDir = FileNameJoin[{rootDirectory, "..", "data", "layer_mxnet_sequence", "seq_" <> ToString[sequenceLength]}];
    Quiet[CreateDirectory[baseDir]];
    Do[
      benchmarkModelLayers[modelName, sequenceLength, $NumRuns],
      {modelName, models}
    ]
  ]

benchmarkModelLayers[modelName_String] :=
    benchmarkModelLayers[modelName, 1, $NumRuns]
benchmarkModelLayers[modelName_String, sequenceLength_, nRuns_] :=
  Module[{model, max, timings, minTime, pathTime},
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    gr = NetInformation[model, "LayersGraph"];
    topo = TopologicalSort[gr];
    Print["benchmarking .... " <> modelName];
    timings = Table[
      Quiet@Check[
        max = min+sequenceLength;
        minTime = Quiet@Check[
            CheckAbort[
            lyr = lyrs[[min]];
            net = NetChain[Lookup[lyrs, topo[[min ;; min]]]];
            run[net, lyr, nRuns],
            xPrint["abort. min .."];
            $Failed],
            $Failed
        ];
        pathTime = Quiet@Check[
            CheckAbort[
            lyr = lyrs[[min]];
            net = NetChain[Lookup[lyrs, topo[[min ;; max]]]];
            run[net, lyr, nRuns],
            xPrint["abort. path .."];
            $Failed],
            $Failed
        ];
        maxTime = Quiet@Check[
            CheckAbort[
            lyr = lyrs[[max]];
            net = NetChain[Lookup[lyrs, topo[[max ;; max]]]];
            run[net, lyr, nRuns],
            xPrint["abort. max .."];
            $Failed],
            $Failed
        ];
        <|
          "start_index" -> min,
          "end_index" -> max,
          "start_name" -> StringRiffle[topo[[min]], "/"],
          "end_name" -> StringRiffle[topo[[max]], "/"],
          "sequence_length" -> sequenceLength,

          "sequence_name" -> StringRiffle[Table[StringRiffle[e, "/"], {e,topo[[min;;max]]}], "-"],
          "start_layer_kind" -> StringTrim[SymbolName[Head[Lookup[lyrs, Key[topo[[min]]]]]], "Layer"],
          "end_layer_kind" -> StringTrim[SymbolName[Head[Lookup[lyrs, Key[topo[[max]]]]]], "Layer"],


          "sequence_layer_kind" -> StringRiffle[
              Table[
                  StringTrim[SymbolName[Head[Lookup[lyrs, Key[e]]]], "Layer"],
                  {e,topo[[min;;max]]}
              ],
              "-"
          ],

          "min_start_index_time" -> If[minTime === $Failed, invalidVal, Round[1000000 * Min[minTime], 0.0001]],
          "mean_start_index_time" -> If[minTime === $Failed, invalidVal, Round[1000000 * summarize[minTime], 0.0001]],
          "max_start_index_time" -> If[minTime === $Failed, invalidVal, Round[1000000 * Max[minTime], 0.0001]],

          "min_end_index_time" -> If[maxTime === $Failed, invalidVal, Round[1000000 * Min[maxTime], 0.0001]],
          "mean_end_index_time" -> If[maxTime === $Failed, invalidVal, Round[1000000 * summarize[maxTime], 0.0001]],
          "max_end_index_time" -> If[maxTime === $Failed, invalidVal, Round[1000000 * Max[maxTime], 0.0001]],

          "min_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * Min[pathTime], 0.0001]],
          "mean_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * summarize[pathTime], 0.0001]],
          "max_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * Max[pathTime], 0.0001]],
          
          "min_path_minus_start_time" -> If[minTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Min[pathTime]-Min[minTime]), 0.0001]],
          "mean_path_minus_start_time" -> If[minTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (summarize[pathTime]-summarize[minTime]), 0.0001]],
          "max_path_minus_start_time" -> If[minTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Max[pathTime]-Max[minTime]), 0.0001]],

          "min_path_minus_end_time" -> If[maxTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Min[pathTime]-Min[maxTime]), 0.0001]],
          "mean_path_minus_end_time" -> If[maxTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (summarize[pathTime]-summarize[maxTime]), 0.0001]],
          "max_path_minus_end_time" -> If[maxTime === $Failed || pathTime === $Failed, invalidVal, Round[1000000 * (Max[pathTime]-Max[maxTime]), 0.0001]],

          "raw_start_index_time" -> If[minTime === $Failed, invalidVal, Round[1000000 * minTime, 0.0001]],
          "raw_end_index_time" -> If[maxTime === $Failed, invalidVal, Round[1000000 * maxTime, 0.0001]],
          "raw_path_time" -> If[pathTime === $Failed, invalidVal, Round[1000000 * pathTime, 0.0001]]
        |>,
        <||>
      ],
      {min, Length[lyrs]-sequenceLength}
    ];
    Print["writing benchmark results .... " <> modelName];
    writeTimings[StringReplace[modelName, " " -> "_"], timings]
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
    header = Keys[timings[[1]]];
    tbl = Table[ Table[Lookup[t, h, invalidVal], {h,header}], {t, timings}];
    PrependTo[tbl, header];
    Export[FileNameJoin[{baseDir, modelName <> ".csv"}], tbl, "CSV"]
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
    Min[Table[First[AbsoluteTiming[net[];]], {10}]]
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
    benchmarkLayers[modelNames, 1];
    benchmarkLayers[modelNames, 2];
    benchmarkLayers[modelNames, 3];
    benchmarkLayers[modelNames, 4];
    benchmarkLayers[modelNames, 5];
    benchmarkLayers[modelNames, 6];
    benchmarkLayers[modelNames, 7];
    benchmarkLayers[modelNames, 8];
    benchmarkLayers[modelNames, 9];
    benchmarkLayers[modelNames, 10];
  ]
];
Print["done benchmarking...."];
