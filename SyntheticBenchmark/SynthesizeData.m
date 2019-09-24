
BeginPackage["SyntheticBenchmark`SynthesizeData`"]

SynthesizeData

Begin["`Private`"]

Needs["NeuralNetworks`"]

If[FreeQ[$ContextPath, "NeuralNetworks`Private`Benchmarking`"],
    PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"]
];

SynthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
    
End[]


EndPackage[]

