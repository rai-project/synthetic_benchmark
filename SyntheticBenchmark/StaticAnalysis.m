
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
FlopCount[Invactive[nm_][assoc_?AssociationQ, ___], opts_?AssociationQ] := formatS[flops[nm, assoc["Inputs"], assoc["Outputs"], assoc["Parameters"], opts]]
FlopCount[lyr_[assoc_?AssociationQ, ___], opts_?AssociationQ] := formatS[flops[lyr, assoc["Inputs"], assoc["Outputs"], assoc["Parameters"], opts]]

formatS[assoc_?AssociationQ] :=
    <|
        "MultiplyAdds" -> Lookup[assoc, "MultiplyAdds", 0],
        "Additions" -> Lookup[assoc, "Additions", 0],
        "Divisions" -> Lookup[assoc, "Divisions", 0],
        "Comparisons" -> Lookup[assoc, "Comparisons", 0],
        "Exponentiations" -> Lookup[assoc, "Exponentiations", 0]
    |>
formatS[e_] := e

ClearAll[flops]
flops[ConvolutionLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, outputShapes, nIn, cIn, cOut, hOut, wOut, kernelH, kernelW, batchSize},
        inputShapes = tensorDims[inputs["Input"]];
        outputShapes = tensorDims[outputs["Output"]];

        batchSize = Lookup[opts, "BatchSize", 1];
        nIn = batchSize;
        cIn = params["$InputChannels"];

        cOut = params["OutputChannels"];
        hOut = params["$OutputSize"][[1]];
        wOut = params["$OutputSize"][[2]];

        kernelH = params["Dilation"][[1]]*(params["KernelSize"][[1]]-1) + 1;
        kernelW = params["Dilation"][[2]]*(params["KernelSize"][[2]]-1) + 1;

        <|
            "MultiplyAdds" -> (kernelH*kernelW*hOut*wOut*cIn*cOut*nIn) / params["ChannelGroups"]
        |>
    ];
flops[BatchNormalizationLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, numOps},
        inputShapes = tensorDims[inputs["Input"]];

        numOps = Apply[Times, inputShapes];
        <|
            "Additions" -> numOps,
            "Divisions" -> numOps
        |>
    ];
flops[ElementwiseLayer, inputs_, outputs_, params_, opts_] :=
    Module[{numInputs, inputShapes, fun, numOps},
        inputShapes = tensorDims[inputs["Input"]];

        numInputs = Length[inputs];
        numOps = Apply[Times, inputShapes];
        fun = params["Function"];
        Switch[fun,
            ValidatedParameter[Ramp],
                <| "Comparisons" -> numOps, "MultiplyAdds" -> numOps |>,
            _,
                Print["unhandled ElementwiseLayer case", params]
        ]
    ];
flops[PaddingLayer, inputs_, outputs_, params_, opts_] :=
    <||>
flops[PoolingLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, outputShapes, nOut, cOut, hOut, wOut, fun, batchSize},
        inputShapes = tensorDims[inputs["Input"]];
        outputShapes = tensorDims[outputs["Output"]];

        batchSize = Lookup[opts, "BatchSize", 1];
        
        nOut = batchSize;
        {cOut, hOut, wOut} = outputShapes;

        fun = params["Function"];

        Switch[fun,
            Max,
                <| "Comparisons" -> hOut * wOut * nOut * cOut * Apply[Times, params["KernelSize"]] |>,
            Mean,
                <| "Additions" -> hOut * wOut * nOut * cOut * Apply[Times, params["KernelSize"]] |>,
            _,
                Print["unhandled PoolingLayer case", params]
        ]
    ];
flops[TotalLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, numOps},
        inputShapes = tensorDims[inputs[[1]]];
        numOps = Apply[Times, inputShapes];
        <| "Additions" -> numOps |>
    ];
flops[FlattenLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[LinearLayer, inputs_, outputs_, params_, opts_] :=
    Module[{batchSize, inputShapes, outputShapes, fun, m, n, k},
        inputShapes = tensorDims[inputs["Input"]];
        outputShapes = tensorDims[outputs["Output"]];

        batchSize = Lookup[opts, "BatchSize", 1];
        fun = If[Length[outputShapes] === 1, "GEMV", "GEMM"];
        Switch[fun,
            "GEMV",
                m = inputShapes[[1]];
                k = outputShapes[[1]];
                <| "MultiplyAdds" -> batchSize * m * k |>,
            "GEMM",
                m = inputShapes[[1]];
                n = inputShapes[[2]];
                k = outputShapes[[1]];
                <| "MultiplyAdds" -> 2 * batchSize * m * n * k |>
        ]
    ];
flops[SoftmaxLayer, inputs_, outputs_, params_, opts_] :=
    Module[{numInputs, inputShapes, fun, numOps},
        inputShapes = tensorDims[inputs["Input"]];

        numInputs = Length[inputs];
        numOps = Apply[Times, inputShapes];
        <|
            "Exponentiations" -> numOps,
            "Additions" -> numOps,
            "Divisions" -> numOps
        |>
    ];


paramsOf[lyr_] :=
    NData[lyr]["Parameters"]

tensorDims[TensorT[dims_, _]] := 
	dims

End[]


EndPackage[]

