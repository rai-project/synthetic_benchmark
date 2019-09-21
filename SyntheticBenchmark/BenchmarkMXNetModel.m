
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

modelNames = Keys[$Models]

PrependTo[$ContextPath, "MXNetLink`PackageScope`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;


baseDir = FileNameJoin[{rootDirectory, "..", "data", "mxnet_model"}]
Quiet[CreateDirectory[baseDir]]

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

timings = {}

benchmarkModel[modelName_] :=
    benchmarkModel[modelName, $NumRuns]
benchmarkModel[modelName_, n_] :=
  Module[{model, time},
    Print["benchmarking .... " <> modelName];
    model = NetModel[modelName];
    net = model;
    lyrs = NetInformation[model, "Layers"];
    time = Quiet@Check[
        CheckAbort[
        lyr = lyrs[[1]];
        run[net, lyr, n],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
    time = <|
      "name" -> StringReplace[modelName, " " -> "_"],
      "min_time" -> If[time === $Failed, invalidVal, Round[1000000 * Min[time], 0.0001]],
      "mean_time" -> If[time === $Failed, invalidVal, Round[1000000 * TrimmedMean[time, 0.2], 0.0001]],
      "max_time" -> If[time === $Failed, invalidVal, Round[1000000 * Max[time], 0.0001]],
      "raw_time" -> If[time === $Failed, invalidVal, Round[1000000 * time, 0.0001]]
    |>;
    AppendTo[timings, time];
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

PreemptProtect[
  AbortProtect[
    benchmarkModel /@ modelNames;
    writeTimings[timings]
  ]
];
Print["done benchmarking...."];
