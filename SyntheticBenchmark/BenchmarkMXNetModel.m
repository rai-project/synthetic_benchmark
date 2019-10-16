#!/usr/bin/env wolframscript

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


<< NeuralNetworks`
<< MXNetLink`


rootDirectory = FileNameDrop[$InputFileName, -1];
PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]
Get["SyntheticBenchmark`Assets`"]

modelNames = Keys[$Models];

(* modelNames = {"ELMo Contextual Word Representations Trained on 1B Word Benchmark"} *)

PrependTo[$ContextPath, "MXNetLink`PackageScope`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;



getInstanceType[] := getInstanceType[]  =
If[$OperatingSystem === "MacOSX", "local",
 Quiet@Module[{url,res},
    url = "http://169.254.169.254/latest/meta-data/instance-type";
    res = URLExecute[url];
    If[StringQ[res],
        res,
        Throw["unable to determine instance type"]
    ]
]]

baseDir = FileNameJoin[{rootDirectory, "..", "data", "mxnet_model", getInstanceType[]}]
Quiet[CreateDirectory[baseDir]]

run[name_, net_, m_] :=
    Module[{plan, ex, data, res, setter},
        NDArrayWaitForAll[];
        plan = ToNetPlan[net];
        ex = ToNetExecutor[plan, 1, "ArrayCaching" -> False];
        SeedRandom[1];
        data = synthesizeData /@ Inputs[net];
        xPrint[First[plan]["Inputs"]];
        setter = If[ContainsVarSequenceQ[Inputs[net]],
          NDArraySetUnbatched[#1, #2, 1]&,
          NDArraySet[#1, #2]&
        ];
        (* Table[
          Print[{key,data[key]}];
          (* xPrint[ex["Arrays", "Inputs",  key]];
          data[key] = NumericArray[{40434, 17087, 39104}, "Integer32"]; *)
            NDArraySet[ex["Arrays", "Inputs",  key], data[key]],
            {key, Keys[data]}
        ]; *)
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
            {m}
        ];
        ClearAll[plan];
        ClearAll[ex];
        ClearAll[data];
        res
    ]

invalidVal = ""
$NumRuns = 50

<< SyntheticBenchmark`
<< SyntheticBenchmark`StaticAnalysis`
$Models = Keys[SyntheticBenchmark`Assets`Models`$Models];
ClearAll[getModelId];
getModelId[name_] :=
 First[FirstPosition[$Models, name]]

summarize[t_] := TrimmedMean[t, 0.2]

timings = {}

benchmarkModel[modelName_] :=
    benchmarkModel[modelName, $NumRuns]
benchmarkModel[modelName_, m_] :=
  Module[{model, time},
    Print["benchmarking .... " <> modelName];
    model = NetModel[modelName];
    net = model;
    lyrs = NetInformation[model, "Layers"];
    time = Check[
        CheckAbort[
        run[modelName, net, m],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
    xPrint[Internal`$LastInternalFailure];
    time = <|
      "id" -> getModelId[modelName],
      "name" -> modelName,
      "machine_name" -> getInstanceType[],
      "min_time" -> If[time === $Failed, invalidVal, Round[1000000 * Min[time], 0.0001]],
      "mean_time" -> If[time === $Failed, invalidVal, Round[1000000 * TrimmedMean[time, 0.2], 0.0001]],
      "max_time" -> If[time === $Failed, invalidVal, Round[1000000 * Max[time], 0.0001]],
      "raw_time" -> If[time === $Failed, invalidVal, Round[1000000 * time, 0.0001]]
    |>;
    ClearAll[net];
    AppendTo[timings, time];
    writeTimings[StringReplace[modelName, " " -> "_"], {time}];
    Print["writing benchmark results .... " <> modelName];
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
    header = Keys[timings[[1]]];
    tbl = Lookup[#, header]& /@ timings;
    PrependTo[tbl, header];
    Export[FileNameJoin[{baseDir, modelName <> ".csv"}], tbl, "CSV"]
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

synthesizeData[e_["Image", params_, sz_]] /; e === NetEncoder :=
 RandomReal[1, sz /. TensorT[x_, ___] :> Prepend[x, batchSize]]

encLength = 10
synthesizeData[enc:(e_["Function", params_, sz0_, ___])] /; (e === NetEncoder && MatchQ[params["Pattern"], Verbatim[ValidatedParameter[_String]]]):=
  Module[{sz = sz0 //. {TensorT[x_,___]:>x, _LengthVar :> encLength}},
    If[Length[sz] === 2,
      (* First[params["Function"]][StringRiffle[#, " "]& /@ Table[RandomWord[], sz[[1]], sz[[2]]]], *)
      RandomInteger[1, sz],
      First[params["Function"]][StringRiffle[Table[RandomWord[], encLength], " "]]
    ]
  ]
audioLength = sequenceLength
synthesizeData[enc:(e_["Function", params_, sz_, ___])] /; (e === NetEncoder && MatchQ[params["Pattern"], ValidatedParameter[None]]):=
  RandomReal[1, sz /. TensorT[x_, ___] :> Prepend[x /. _LengthVar -> sequenceLength, batchSize]];


tokenLength = 10
synthesizeData[enc:(e_["Tokens", params_, sz_, ___])] /; (e === NetEncoder ):=
  enc[StringRiffle[Table[RandomWord[], tokenLength], " "]];

charLength = 10
chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
synthesizeData[enc:(e_["Characters", params_, sz_, ___])] /; (e === NetEncoder ):=
  enc[FromCharacterCode[RandomChoice[ToCharacterCode[chars], charLength]]];


If[Length[$ScriptCommandLine] == 2,
  modelNames = {$ScriptCommandLine[[2]]}
];

Print[modelNames]
PreemptProtect[
  AbortProtect[
    benchmarkModel /@ modelNames;
  ]
];
xPrint["done benchmarking...."];
