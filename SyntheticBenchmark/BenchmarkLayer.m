
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

PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]

PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;


benchmarkLayers[modelName_, n_:50] :=
  Module[{model, timings},
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    Print["benchmarking .... " <> modelName];
    timings = Association@Table[
      max = min;
      lyr = lyrs[[min]];
      name = Keys[lyrs][[min]];
      net = NetChain[{lyr}];
      SeedRandom[1];
      tdata = synthesizeData /@ Inputs[lyr];
      name -> TrimmedMean[Table[First[AbsoluteTiming[net[tdata];]], n], 0.2],
      {min, Length[lyrs]}
    ];
    Print["writing benchmark results .... " <> modelName];
    writeTimings[StringReplace[modelName, " " -> "_"], timings]
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
  header = {
    "index",
    "kind",
    "name",
    "time",
    "add_flops",
    "div_flops",
    "comp_flops",
    "exp_flops",
    "mad_flops",
    "input_channel",
    "input_height",
    "input_width",
    "output_channel",
    "output_height",
    "output_width",
    "function",
    "kernel_1",
    "kernel_2",
    "stride_1",
    "stride_2",
    "dilation_1",
    "dilation_2",
    "mad/time"
  };
  idx = 1;
  tbl = Table[
    time = Round[1000000 * timings[k], 0.01];
    flops = FlopCount[lyrs[k]];
    (*Print[flops];
    Print[ time];*)
    <|
      "index" -> idx++,
      "kind" -> StringTrim[SymbolName[Head[lyrs[k]]], "Layer"],
      "name" -> StringRiffle[k, "/"],
      "time" -> time,
      "add_flops" -> flops["Additions"],
      "div_flops" -> flops["Divisions"],
      "cmp_flops" -> flops["Comparisons"],
      "exp_flops" -> flops["Exponentiations"],
      "mad_flops" -> flops["MultiplyAdds"],
      "input_channel" -> inputDims[lyrs[k]][[1]],
      "input_height" -> inputDims[lyrs[k]][[2]],
      "input_width" -> inputDims[lyrs[k]][[3]],
      "output_channel" -> outputDims[lyrs[k]][[1]],
      "output_height" -> outputDims[lyrs[k]][[2]],
      "output_width" -> outputDims[lyrs[k]][[3]],
      "function" -> function[lyrs[k]],
      "kernel_1" -> kernelSize[lyrs[k]][[1]],
      "kernel_2" -> kernelSize[lyrs[k]][[2]],
      "stride_1" -> stride[lyrs[k]][[1]],
      "stride_2" -> stride[lyrs[k]][[2]],
      "dilation_1" -> dilation[lyrs[k]][[1]],
      "dilation_2" -> dilation[lyrs[k]][[2]],
      "mad/time" -> N[flops["MultiplyAdds"] / time]
    |>,
    {k, Keys[timings]}
    ];
  tbl = SortBy[tbl, {
    #["kind"]&,
    #["kernel_1"]&,
    #["kernel_2"]&,
    #["stride_1"]&,
    #["stride_2"]&,
    #["dilation_1"]&,
    #["dilation_2"]&,
    #["input_channel"]&,
    #["input_height"]&,
    #["input_width"]&,
    #["mad"]&,
    #["time"]&,
    #["mad/time"]&
  }];
  tbl = Values /@ tbl;
  PrependTo[tbl, header];
  Export[FileNameJoin[{rootDirectory, "..", "data", modelName <> ".csv"}], tbl]
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
