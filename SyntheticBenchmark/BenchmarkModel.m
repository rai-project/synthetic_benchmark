#!/usr/bin/env wolframscript

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


<< NeuralNetworks`
synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;
NetAttachLoss = NeuralNetworks`NetAttachLoss;

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;


rootDirectory = FileNameDrop[$InputFileName, -1];

PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]

PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];


timings = <||>;

benchmarkModel[modelName_, n_:50] :=
  Module[{model},
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    Print["benchmarking .... " <> modelName];
    (* tdata = Image[Transpose[First[synthesizeData[#]], {3,2,1}]]& /@ Inputs[First[lyrs]]; *)
    tdata = synthesizeData /@ Inputs[First[lyrs]];
    time = Mean[Table[First[AbsoluteTiming[model[tdata];]], n]];
    AppendTo[timings, modelName -> time];
  ]


writeTimings[] :=
  Module[{tbl, header, time, flops},
  header = {
    "index",
    "name",
    "time"
  };
  idx = 1;
  tbl = Table[
    time = Round[1000000 * timings[k], 0.01];
    <|
      "index" -> idx++,
      "name" -> StringReplace[k, " " -> "_"],
      "time" -> time
    |>,
    {k, Keys[timings]}
  ];
  tbl = Values /@ tbl;
  PrependTo[tbl, header];
  Export[FileNameJoin[{rootDirectory, "..", "data", "image_classification.csv"}], tbl]
]

benchmarkModel /@ modelNames;
writeTimings[]