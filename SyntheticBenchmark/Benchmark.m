
shortModelName = "resnet50";
modelName = "ResNet-50 Trained on ImageNet Competition Data";

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

model = NetModel[modelName];
lyrs = NetInformation[model, "Layers"];

benchmarkModel[modelName_, n_:50] :=
  Module[{model, timings},
    timings = Association@Table[
      max = min;
      lyr = lyrs[[min]];
      name = Keys[lyrs][[min]];
      net = NetChain[{lyr}];
      SeedRandom[1];
      tdata = synthesizeData /@ Inputs[lyr];
      name -> TrimmedMean[Table[First[AbsoluteTiming[net[tdata];]], n], 0.2],
      {min, Length[lyrs]}
    ]
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
  header = {"index", "kind", "layer_name", "timing", "add", "div",
   "comp", "exp", "mad", "input", "output", "function", "kernel",
   "stride", "dilation", "mad/time"};
  idx = 1;
  tbl = Table[
    time = Round[1000000 * timings[k], 0.1];
    flops = FlopCount[lyrs[k]];
    (*Print[flops];
    Print[ time];*)
    {
      idx++,
      StringTrim[SymbolName[Head[lyrs[k]]], "Layer"],
      StringRiffle[k, "/"],
      time,
      flops["Additions"],
      flops["Divisions"],
      flops["Comparisons"],
      flops["Exponentiations"],
      flops["MultiplyAdds"],
      inputDims[lyrs[k]],
      outputDims[lyrs[k]],
      function[lyrs[k]],
      kernelSize[lyrs[k]],
      stride[lyrs[k]],
      dilation[lyrs[k]],
      N[flops["MultiplyAdds"] / time]
      },
    {k, Keys[timings]}
    ];
  tbl = SortBy[tbl, {#[[2]] &, #[[13]] &, #[[14]] &, #[[15]] &, #[[12]] &, #[[10]]&, #[[1]] &, #[[9]] &, #[[16]] &}];
  PrependTo[tbl, header];
  Export[FileNameJoin[{rootDirectory, "..", "data", modelName <> ".csv"}], tbl]
]

paramsOf[lyr_[params_, ___]] := params
kernelSize[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["KernelSize"]}, If[ListQ[e], e, ""]]
stride[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Stride"]}, If[ListQ[e], e, ""]]
function[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Function"]},
  If[! MissingQ[e], e /. ValidatedParameter -> Identity, ""]]
dilation[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Dilation"]}, If[ListQ[e], e, ""]]
inputDims[lyr_[params_, ___]] :=
 With[{e = params["Inputs"]},
  If[AssociationQ[e],
   If[KeyExistsQ[e, "Input"], e["Input"], e["1"]] /.
    TensorT[x_, _] :> x, ""]]
outputDims[lyr_[params_, ___]] :=
 With[{e = params["Outputs"]},
  If[AssociationQ[e], e["Output"] /. TensorT[x_, _] :> x, ""]]



timings = benchmarkModel[modelName];
Print["done running benchmark.... writing"];
writeTimings[shortModelName, timings];
Print["done benchmarking...."];
