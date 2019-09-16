
BeginPackage["SyntheticBenchmark`StaticAnalysis`"]

FlopCount

Begin["`Private`"]

Needs["NeuralNetworks`"]

If[FreeQ[$ContextPath, "NeuralNetworks`Private`Benchmarking`"],
    PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"]
];

synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;

ClearAll[FlopCount]
FlopCount[e_, opts:OptionsPattern[]] := FlopCount[e, <|opts|>]
FlopCount[Invactive[nm_][assoc_?AssociationQ, ___], opts_?AssociationQ] := flops[nm, assoc["Inputs"], assoc["Outputs"], assoc["Parameters"], opts]
FlopCount[lyr_[assoc_?AssociationQ, ___], opts_?AssociationQ] := flops[lyr, assoc["Inputs"], assoc["Outputs"], assoc["Parameters"], opts]

ClearAll[flops]
flops[ConvolutionLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, outputShapes, nIn, cIn, cOut, hOut, wOut, kernelH, kernelW, batchSize},
        inputShapes = tensorDims[inputs["Input"]];
        outputShapes = tensorDims[outputs["Output"]];

        batchSize = Lookup[opts, "BatchSize", 1];
        nIn = batchSize;
        cIn = inputShapes[[2]];

        cOut = outputShapes[[1]];
        hOut = outputShapes[[2]];
        wOut = outputShapes[[3]];

        kernelH = params["Dilation"][[1]]*(params["KernelSize"][[1]]-1) + 1;
        kernelW = params["Dilation"][[2]]*(params["KernelSize"][[2]]-1) + 1;

        <|
            "MultiplyAdds" -> (kernelH*kernelW*hOut*wOut*cIn*cOut*nIn) / params["ChannelGroups"]
        |>
    ]

paramsOf[lyr_] :=
    NData[lyr]["Parameters"]

tensorDims[TensorT[dims_, _]] := 
	dims

End[]


EndPackage[]

