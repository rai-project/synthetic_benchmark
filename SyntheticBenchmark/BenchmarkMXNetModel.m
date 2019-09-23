
imageClassificationModelNames = {"Ademxapp Model A Trained on ImageNet Competition Data", "Age \
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
nlpModelNames = {
 "BERT Trained on BookCorpus and English Wikipedia Data",
 "GPT-2 Transformer Trained on WebText Data", 
 "GPT Transformer Trained on BookCorpus Data"
};

modelNames = imageClassificationModelNames;
modelNames = nlpModelNames;
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

(* modelNames = {"GPT Transformer Trained on BookCorpus Data"} *)

PrependTo[$ContextPath, "MXNetLink`PackageScope`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;


baseDir = FileNameJoin[{rootDirectory, "..", "data", "mxnet_model"}]
Quiet[CreateDirectory[baseDir]]

run[name_, net_, n_] :=
    Module[{plan, ex, data, res},
        NDArrayWaitForAll[];
        plan = ToNetPlan[net];
        ex = ToNetExecutor[plan, 1, "ArrayCaching" -> False];
        SeedRandom[1];
        data = synthesizeData /@ Echo[Inputs[net]];
        xPrint[First[plan]["Inputs"]];
        Table[
          (* xPrint[ex["Arrays", "Inputs",  key]];
          data[key] = NumericArray[{40434, 17087, 39104}, "Integer32"]; *)
            NDArraySet[ex["Arrays", "Inputs",  key], data[key]],
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
        ClearAll[plan];
        ClearAll[ex];
        ClearAll[data];
        res
    ]

invalidVal = ""
$NumRuns = 10

summarize[t_] := TrimmedMean[t, 0.2]

timings = {}

benchmarkModel[modelName_] :=
    benchmarkModel[modelName, $NumRuns]
benchmarkModel[modelName_, n_] :=
  Module[{model, time},
    Print["benchmarking .... " <> modelName];
    model = NetModel[modelName];
    net = model;
    lyrs = NetInformation[model, "Layers"];
    time = Check[
        CheckAbort[
        run[modelName, net, n],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
        Print[Internal`$LastInternalFailure];
    time = <|
      "name" -> StringReplace[modelName, " " -> "_"],
      "min_time" -> If[time === $Failed, invalidVal, Round[1000000 * Min[time], 0.0001]],
      "mean_time" -> If[time === $Failed, invalidVal, Round[1000000 * TrimmedMean[time, 0.2], 0.0001]],
      "max_time" -> If[time === $Failed, invalidVal, Round[1000000 * Max[time], 0.0001]],
      "raw_time" -> If[time === $Failed, invalidVal, Round[1000000 * time, 0.0001]]
    |>;
    ClearAll[net];
    AppendTo[timings, time];
    writeTimings[timings];
    Print["writing benchmark results .... " <> modelName];
  ]

writeTimings[timings_] :=
  Module[{tbl, header, time, flops},
    header = Keys[timings[[1]]];
    tbl = Lookup[#, header]& /@ timings;
    PrependTo[tbl, header];
    Export[FileNameJoin[{baseDir, "mxnet.csv"}], tbl, "CSV"]
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
synthesizeData[enc:(e_["Function", params_, sz_, ___])] /; (e === NetEncoder && MatchQ[params["Pattern"], Verbatim[ValidatedParameter[_String]]]):= 
  First[params["Function"]][StringRiffle[Table[RandomWord[], encLength], " "]];

audioLength = sequenceLength
synthesizeData[enc:(e_["Function", params_, sz_, ___])] /; (e === NetEncoder && MatchQ[params["Pattern"], ValidatedParameter[None]]):= 
  RandomReal[1, sz /. TensorT[x_, ___] :> Prepend[x /. _LengthVar -> sequenceLength, batchSize]];


tokenLength = 10
synthesizeData[enc:(e_["Tokens", params_, sz_, ___])] /; (e === NetEncoder ):= 
  enc[StringRiffle[Table[RandomWord[], tokenLength], " "]];

charLength = 1
chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
synthesizeData[enc:(e_["Characters", params_, sz_, ___])] /; (e === NetEncoder ):= 
  enc[FromCharacterCode[RandomChoice[ToCharacterCode[chars], charLength]]];

PreemptProtect[
  AbortProtect[
    benchmarkModel /@ modelNames;
    writeTimings[timings]
  ]
];
Print["done benchmarking...."];
