
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


PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;

benchmarkModel[modelName_, n_:50] :=
  Module[{model, lyrs, timings},
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    timings = Association@Table[
      max = min;
      lyr = lyrs[[min]];
      name = Keys[lyrs][[min]];
      net = NetChain[Values[lyrs][[min ;; max]]];
      SeedRandom[1];
      tdata = synthesizeData /@ Inputs[lyr];
      name -> TrimmedMean[Table[First[AbsoluteTiming[net[tdata];]], n], 0.2],
      {min, Length[lyrs]}
    ]
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header},
  header = {"index", "kind", "layer_name", "timing", "add", "div",
   "comp", "exp", "mad", "input", "output", "function", "kernel",
   "stride", "dilation"};
  idx = 0;
  tbl = Table[{
      idx++,
      StringTrim[SymbolName[Head[lyrs[k]]], "Layer"],
      StringRiffle[k, "/"],
      timings[k],
      Sequence @@ Sort[FlopCount[lyrs[k]]],
      inputDims[lyrs[k]],
      outputDims[lyrs[k]],
      function[lyrs[k]],
      kernelSize[lyrs[k]],
      stride[lyrs[k]],
      dilation[lyrs[k]]
      },
    {k, Keys[timings]}
    ];
  PrependTo[tbl, header];
  Export[FileNameJoin[{rootDirectory, "..", "data",
   modelName <> ".csv"}], tbl]
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
writeTimings[shortModelName, timings]
