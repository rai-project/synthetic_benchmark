<< NeuralNetworks`

PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;
NetAttachLoss = NeuralNetworks`NetAttachLoss;

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 32;

modelName = "ResNet-50 Trained on ImageNet Competition Data";

model = NetModel[modelName];
lyrs = NetInformation[model, "Layers"];

Print[Length[lyrs]]

min = 1;
max = 1;
n1 = Keys[lyrs][[min]]
Print["min = ", min, " = ", Keys[lyrs][[min]], " max = ", max, " = ", Keys[lyrs][[max]]]

lyr = lyrs[[min]];
net = NetChain[Values[lyrs][[min ;; max]]];
tdata = synthesizeData /@ Inputs[lyr];
Print[1000000 * Mean[Table[First[AbsoluteTiming[net[tdata];]], 100]]]


benchmarkModel[modelName_, n_:50] :=
  Module[{model},
    model = NetModel[modelName];
    lyrs = NetInformation[model, "Layers"];
    Print["benchmarking .... " <> modelName];
    (* tdata = Image[Transpose[First[synthesizeData[#]], {3,2,1}]]& /@ Inputs[First[lyrs]]; *)
    tdata = synthesizeData /@ Inputs[First[lyrs]];

    time = Mean[Table[First[AbsoluteTiming[model[tdata];]], n]];
    Print[1000000 * time]
  ]

benchmarkModel[modelName, 50]