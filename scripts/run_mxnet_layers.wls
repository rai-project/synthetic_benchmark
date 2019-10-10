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


dataDir = FileNameJoin[{rootDirectory, "..", "data"}]
baseDir = FileNameJoin[{dataDir, "raw_mxnet_layer_info", "c5.2xlarge"}]

Quiet[CreateDirectory[baseDir]]

modelNames = Keys[$Models]
(* modelNames = Select[Reverse[modelNames], StringContainsQ[#, "Multi-scale"] &] *)
modelNames = modelNames[[;;2]]

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
