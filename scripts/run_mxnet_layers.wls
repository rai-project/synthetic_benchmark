#!/usr/bin/env wolframscript



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

getInstanceType[] := Quiet@Module[{url,res},
    url = "http://169.254.169.254/latest/meta-data/instance-type";
    res = URLExecute[url];
    If[StringQ[res],
        res,
        Throw["unable to determine instance type"]
    ]
]


dataDir = FileNameJoin[{rootDirectory, "..", "data"}]
baseDir = FileNameJoin[{dataDir, "raw_mxnet_layer_info", getInstanceType[]}]

Quiet[CreateDirectory[baseDir]]

modelNames = Keys[$Models]
xmodelNames = Select[Reverse[modelNames], StringContainsQ[#, "Pix"] &] 
xmodelNames = modelNames[[;;2]]
xmodelNames = modelNames[[;;2]]
modelNames = {"ResNet-101 Trained on ImageNet Competition Data"}

Print[modelNames]

Do[
    model = NetModel[modelName];
    lyrLen = Length[NetInformation[model, "Layers"]];
    Do[

        Print["Running layer ", lyrIdx ,"/", lyrLen, " in model ", modelName];
        Print@RunProcess[
            {
                "wolframscript",
                "-f",
                FileNameJoin[{rootDirectory, "..", "SyntheticBenchmark", "BenchmarkMXNetLayer.m"}],
                modelName,
                lyrIdx
            },
            "StandardOutput"
        ],
        {lyrIdx, lyrLen}
    ],
    {modelName, modelNames}
]
