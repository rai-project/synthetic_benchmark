#!/usr/bin/env wolframscript

<< NeuralNetworks`
<< MXNetLink`

Print[$ProcessorCount]


rootDirectory = FileNameDrop[$InputFileName, -1];
PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]
Get["SyntheticBenchmark`Assets`"]


PrependTo[$ContextPath, "MXNetLink`PackageScope`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;


dataDir = FileNameJoin[{rootDirectory, "..", "data"}]
baseDir = FileNameJoin[{dataDir, "mxnet_layer_data"}]
Quiet[CreateDirectory[baseDir]]

run[net_, n_] :=
    Module[{plan, ex, data, res, nres},
        NDArrayWaitForAll[];
        plan = ToNetPlan[net];
        ex = ToNetExecutor[plan, 1, "ArrayCaching" -> False];
        SeedRandom[1];
        data = synthesizeData /@ Inputs[net];
        setter = If[ContainsVarSequenceQ[Inputs[net]],
          NDArraySetUnbatched[#1, #2, 1]&,
          NDArraySet[#1, #2]&
        ];
        xPrint[Head[ex]];
        Table[
            setter[ex["Arrays", "Inputs",  key], data[key]],
            {key, Keys[data]}
        ];
        NDArrayWaitForAll[];
        res = Table[
            First[AbsoluteTiming[
                NetExecutorForward[ex, (* IsTraining= *) False];
                NDArrayWaitForAll[];
            ]],
            {n}
        ];
        If[isLocal, Print[Dimensions /@ data]];
        ClearAll[plan];
        ClearAll[ex];
        ClearAll[data];
        res
    ]

invalidVal = ""
$NumRuns = 50
$NumWarmup = 10

summarize[t_] := TrimmedMean[t, 0.2]

timings = {}

benchmarkModelLayer[modelName_, lyrIdx_, lyrName_, lyr_] :=
  Module[{conv, convLayer, time, model,flops},
    xPrint["benchmarking .... " <> modelName <> "/" <> ToString[lyrIdx]];
    Check[
        CheckAbort[
        model = NetChain[{lyr}];
        run[model, $NumWarmup],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
    time = Check[
        CheckAbort[
        model = NetChain[{lyr}];
        run[model, $NumRuns],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
    If[time === $Failed,
        Print["failed to run..."];
        xPrint[Internal`$LastInternalFailure];
        Return[]
    ];
    flops = FlopCount[lyr];
    row= <|
      "model_name" -> modelName,
      "layer_name" -> StringRiffle[lyrName, "/"],
      "layer_index" -> lyrIdx,
      "kind" -> layerKind[lyr],
      "min_time" -> Min[time],
      "mean_time" -> TrimmedMean[time, 0.2],
      "max_time" -> Max[time],
      "add_flops" -> flops["Additions"],
      "div_flops" -> flops["Divisions"],
      "cmp_flops" -> flops["Comparisons"],
      "exp_flops" -> flops["Exponentiations"],
      "mad_flops" -> flops["MultiplyAdds"],
      "input_channel" -> inputDims[lyr][[1]],
      "input_height" -> inputDims[lyr][[2]],
      "input_width" -> inputDims[lyr][[3]],
      "output_channel" -> outputDims[lyr][[1]],
      "output_height" -> outputDims[lyr][[2]],
      "output_width" -> outputDims[lyr][[3]],
      "function" -> function[lyr],
      "kernel_1" -> kernelSize[lyr][[1]],
      "kernel_2" -> kernelSize[lyr][[2]],
      "stride_1" -> stride[lyr][[1]],
      "stride_2" -> stride[lyr][[2]],
      "dilation_1" -> dilation[lyr][[1]],
      "dilation_2" -> dilation[lyr][[2]],
      "input_form" -> NetInformation[lyr, "InputForm"],
      "topology_hash" -> NetInformation[lyr, "TopologyHash"],
      "mad/min_time" -> N[flops["MultiplyAdds"] / Min[time]],
      "mad/mean_time" -> N[flops["MultiplyAdds"] / TrimmedMean[time, 0.2]],
      "mad/max_time" -> N[flops["MultiplyAdds"] / Max[time]],
      "time" -> time
    |>;
    ClearAll[net];
    AppendTo[timings, row];
    ClearAll[row];
    writeTimings[timings];
    Print["writing benchmark results .... " <> ToString[lyrIdx]];
  ]

writeTimings[timings_] :=
  Module[{tbl, header, time},
    header = Keys[timings[[1]]];
    tbl = Lookup[#, header]& /@ timings;
    PrependTo[tbl, header];
    xPrint["writing..."];
    Export[outputFile, tbl, "CSV"]
  ]

layerKind[lyr_] := StringTrim[SymbolName[Head[lyr]], "Layer"]

synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;
NetAttachLoss = NeuralNetworks`NetAttachLoss;



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

modelName = $ScriptCommandLine[[2]]
lyrIdx = ToExpression[$ScriptCommandLine[[3]]]
isLocal = If[Length[$ScriptCommandLine] > 3, ToExpression[$ScriptCommandLine[[4]]], False]

model = NetModel[modelName]
lyrs = NetInformation[model, "Layers"]
lyrName = Keys[lyrs][[lyrIdx]]
lyr = Values[lyrs][[lyrIdx]]

timeLimit = QuantityMagnitude[UnitConvert[Quantity[2, "Minutes"], "Seconds"]]


getInstanceType[] := If[isLocal,
  "local",
  Quiet@Module[{url,res},
      url = "http://169.254.169.254/latest/meta-data/instance-type";
      res = URLExecute[url];
      If[StringQ[res],
          res,
          Throw["unable to determine instance type"]
      ]
  ]
]

outputDir = FileNameJoin[{dataDir, "raw_mxnet_layer_info", getInstanceType[]}]
Quiet[CreateDirectory[outputDir]];
outputFile = FileNameJoin[{outputDir, layerKind[lyr] <> "_" <> ToString[Hash[{modelName, lyrName, lyr}]] <> ".csv"}];

(* If[FileExistsQ[outputFile], Exit[]]; *)

benchmarkLayer[modelName_, lyrIdx_, lyrName_, lyr_] :=
  Module[{r},
    r = TimeConstrained[
      First@AbsoluteTiming[PreemptProtect[
        benchmarkModelLayer[modelName, lyrIdx, lyrName, lyr]
      ]],
      timeLimit,
      $Failed
    ];
    If[r === $Failed,
      Print["Terminated ", modelName, " for layer ", lyrIdx, " because it exceeded the timeLimit=", timeLimit],
      Print["Completed ", modelName, " for layer ", lyrIdx, " taking ", r, " seconds to run."]
    ]
  ]


PreemptProtect[
  AbortProtect[
    benchmarkLayer[modelName, lyrIdx, lyrName, lyr];
    xPrint[timings];
  ]
];
xPrint["done benchmarking...."];
