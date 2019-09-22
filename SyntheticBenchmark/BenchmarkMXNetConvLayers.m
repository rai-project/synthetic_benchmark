

<< NeuralNetworks`
<< MXNetLink`


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
baseDir = FileNameJoin[{dataDir, "conv_layers"}]
Quiet[CreateDirectory[baseDir]]

run[net_, fstLyr_, n_] :=
    Module[{plan, ex, data},
        NDArrayWaitForAll[];
        plan = ToNetPlan[net];
        ex = ToNetExecutor[plan, 1, "ArrayCaching" -> False];
        SeedRandom[1];
        data = synthesizeData /@ Inputs[fstLyr];
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

timings = {}

idx = 0;
benchmarkConv[info_] :=
    benchmarkConv[info, $NumRuns]
benchmarkConv[info_, n_] :=
  Module[{conv, convLayer, time},
    Print["benchmarking .... " <> ToString[idx++] <> "/" <> ToString[Length[convLayers]]];
    convLayer = NetInitialize@ConvolutionLayer[
        info["output_channel"],
        {info["kernel_1"],info["kernel_2"]},
        "Dilation" -> {info["dilation_1"], info["dilation_2"]},
        "Stride" -> {info["stride_1"], info["stride_2"]},
        "Input" -> {info["input_channel"], info["input_height"], info["input_width"]}
    ];
    time = Quiet@Check[
        CheckAbort[
        model = NetChain[{convLayer}];
        run[model, convLayer, n],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
    If[time === $Failed,
        xPrint[Internal`$LastInternalFailure];
        Return[]
    ];
    row= <|
        "name" -> info["name"],
        "input_channel" -> inputDims[convLayer][[1]],
        "input_height" -> inputDims[convLayer][[2]],
        "input_width" -> inputDims[convLayer][[3]],
        "output_channel" -> outputDims[convLayer][[1]],
        "output_height" -> outputDims[convLayer][[2]],
        "output_width" -> outputDims[convLayer][[3]],
        "function" -> function[convLayer],
        "kernel_1" -> kernelSize[convLayer][[1]],
        "kernel_2" -> kernelSize[convLayer][[2]],
        "stride_1" -> stride[convLayer][[1]],
        "stride_2" -> stride[convLayer][[2]],
        "dilation_1" -> dilation[convLayer][[1]],
        "dilation_2" -> dilation[convLayer][[2]],
        "add_flops" -> Lookup[info, "add_flops", 0],
        "div_flops" -> Lookup[info, "div_flops", 0],
        "cmp_flops" -> Lookup[info, "cmp_flops", 0],
        "exp_flops" -> Lookup[info, "exp_flops", 0],
        "mad_flops" -> Lookup[info, "mad_flops", 0],
        "min_time" -> If[time === $Failed, invalidVal, Round[1000000 * Min[time], 0.0001]],
        "mean_time" -> If[time === $Failed, invalidVal, Round[1000000 * TrimmedMean[time, 0.2], 0.0001]],
        "max_time" -> If[time === $Failed, invalidVal, Round[1000000 * Max[time], 0.0001]],
        "raw_time" -> If[time === $Failed, invalidVal, Round[1000000 * time, 0.0001]]
    |>;
    ClearAll[net];
    AppendTo[timings, row];
    writeTimings[timings];
    Print["writing benchmark results .... " <> ToString[idx] <> "/" <> ToString[Length[convLayers]]];
  ]

writeTimings[timings_] :=
  Module[{tbl, header, time, flops},
    header = Keys[timings[[1]]];
    tbl = Lookup[#, header]& /@ timings;
    PrependTo[tbl, header];
    xPrint["writing..."];
    Export[FileNameJoin[{baseDir, "conv.csv"}], tbl, "CSV"]
  ]



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

convData = Import[FileNameJoin[{dataDir, "mxnet_layer_info", "convolution.csv"}]];
header = First[convData];
convData = AssociationThread[header -> #] & /@ Rest[convData];

convLayers = Flatten@Table[
    outputChannels = e["output_channel"];
    Table[
        Join[
            e,
            <|
                "output_channel" -> 2^ii
            |>
        ],
        {ii, Min[Ceiling[Log2[outputChannels]], 8]}
    ],
    {e, convData}
];


convLayers = convLayers[[;;100]]

Print[Length[convLayers]]

PreemptProtect[
  AbortProtect[
    benchmarkConv /@ convLayers;
    xPrint[timings];
  ]
];
Print["done benchmarking...."];
