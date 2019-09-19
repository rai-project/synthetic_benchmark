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

min = 5;
max = 5;
Print["min = ", min, ", max =", max]

lyr = lyrs[[min]];
net = NetChain[Values[lyrs][[min ;; max]]];
tnet = NetInitialize@NetAttachLoss[net, Automatic];
SeedRandom[1];
tdata = synthesizeData /@ Inputs[lyr];
Print[1000000 * Min[Table[First[AbsoluteTiming[net[tdata];]], 100]]]