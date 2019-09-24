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
baseDir = FileNameJoin[{dataDir, "raw_mxnet_layer_info"}]

Quiet[CreateDirectory[baseDir]]

modelNames = Keys[$Models]

Do[
    model = NetModel[modelName];
    lyrLen = Length[NetInformation[model, "Layers"]];
    Do[
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
    {modelName, modelNames[[;;2]]}
]