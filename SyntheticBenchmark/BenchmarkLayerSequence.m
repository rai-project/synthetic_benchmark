
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
synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;
NetAttachLoss = NeuralNetworks`NetAttachLoss;



rootDirectory = FileNameDrop[$InputFileName, -1];
baseDir = FileNameJoin[{rootDirectory, "..", "data", "layer_sequence"}]

Quiet[CreateDirectory[baseDir]]

PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]

PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;

invalidVal = -999

benchmarkLayers[modelName_, n_:50] :=
  Module[{model, max, timings, t1, t2},
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    Print["benchmarking .... " <> modelName];
    timings = Table[
      max = min+1;
      lyr = lyrs[[min]];
      t1 = Quiet@Check[
          net = NetChain[Values[lyrs][[min ;; min]]];
          SeedRandom[1];
          tdata = synthesizeData /@ Inputs[lyr];
          Min[Table[First[AbsoluteTiming[net[tdata];]], n]],
          invalidVal
      ];
      t2 = Quiet@Check[
          net = NetChain[Values[lyrs][[min ;; max]]];
          SeedRandom[1];
          tdata = synthesizeData /@ Inputs[lyr];
          Min[Table[First[AbsoluteTiming[net[tdata];]], n]],
          invalidVal
      ];
      t3 = Quiet@Check[
          lyr = lyrs[[max]];
          net = NetChain[Values[lyrs][[max ;; max]]];
          SeedRandom[1];
          tdata = synthesizeData /@ Inputs[lyr];
          Min[Table[First[AbsoluteTiming[net[tdata];]], n]],
          invalidVal
      ];
      <|
        "start_index" -> min,
        "end_index" -> max,
        "start_name" -> StringRiffle[Keys[lyrs][[min]], "/"],
        "end_name" -> StringRiffle[Keys[lyrs][[max]], "/"],
        "start_index_time" -> If[t1 === invalidVal, invalidVal, Round[1000000 * t1, 0.01]],
        "end_index_time" -> If[t1 === invalidVal, invalidVal, Round[1000000 * t3, 0.01]],
        "path_time" -> If[t2 === invalidVal, invalidVal, Round[1000000 * t2, 0.01]],
        "path_minus_start_time" -> If[t1 === invalidVal || t2 === invalidVal, invalidVal, Round[1000000 * (t2-t1), 0.01]],
        "path_minus_end_time" -> If[t1 === invalidVal || t2 === invalidVal, invalidVal, Round[1000000 * (t2-t3), 0.01]]
      |>,
      {min, Length[lyrs]-1}
    ];
    Print["writing benchmark results .... " <> modelName];
    writeTimings[StringReplace[modelName, " " -> "_"], timings]
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
  header = {
    "start_index",
    "end_index",
    "start_name",
    "end_name",
    "start_index_time",
    "end_index_time",
    "path_time",
    "path_minus_start_time",
    "path_minus_end_time"
  };
  tbl = Lookup[#, header]& /@ timings;
  PrependTo[tbl, header];
  Export[FileNameJoin[{baseDir, modelName <> ".csv"}], tbl]
]


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



benchmarkLayers /@ modelNames;
Print["done benchmarking...."];
