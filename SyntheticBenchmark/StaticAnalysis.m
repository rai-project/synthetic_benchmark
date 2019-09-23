
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
FlopCount[Inactive[nm_][assoc_?AssociationQ, ___], opts_?AssociationQ] := formatS[flops[nm, assoc["Inputs"], assoc["Outputs"], assoc["Parameters"], opts]]
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
flops[NormalizationLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, numOps, tmp, size},
        inputShapes = tensorDims[inputs["Input"]];

        numOps = Apply[Times, inputShapes];
        tmp = Lookup[params, "AggregationLevels"];
        size = Switch[tmp,
            HoldPattern[NeuralNetworks`ValidatedParameter[n_ ;; All]],
                tmp /. HoldPattern[NeuralNetworks`ValidatedParameter[n_ ;; All]] -> n,
            _,
                Print["unhandled NormalizationLayer case", tmp]
        ];

        <|
            "MultiplyAdds" -> numOps * size,
            "Additions" -> numOps,
            "Exponentiations" -> numOps,
            "Divisions" -> 2 * numOps
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
        numOps = numInputs * Apply[Times, inputShapes];
        fun = params["Function"];
        Switch[fun,
            ValidatedParameter[Ramp],
                <| "Comparisons" -> numOps, "MultiplyAdds" -> numOps |>,
            ValidatedParameter[LogisticSigmoid],
                <| "Additions" -> numOps, "Divisions" -> numOps, "Exponentiations" -> numOps |>,
            ValidatedParameter[Abs],
                <| "Comparisons" -> numOps, "MultiplyAdds" -> numOps |>,
            ValidatedParameter[Sqrt],
                <| "Exponentiations" -> numOps |>,
            ValidatedParameter[Tanh],
                <| "Exponentiations" -> numOps |>,
            ValidatedParameter[Exp],
                <| "Exponentiations" -> numOps |>,
            ValidatedParameter[_NeuralNetworks`Private`ScalarFunctionObject],
                fun = First[fun];
                Switch[NeuralNetworks`Private`ScalarFunctionToPureFunction[fun],
                    Min[6,Max[0,#1]]&,
                        <| "Comparisons" -> 2 * numOps |>,
                    (#1)&,
                        <| |>,
                    (#1+#2)&,
                        <| "Additions" -> numOps |>,
                    (#1-#2)&,
                        <| "Additions" -> numOps |>,
                    (#1^2)& | Function[Subtract[Slot[1],Times[0.5`,Slot[2]]]],
                        <| "MultiplyAdds" -> numOps |>,
                    Function[Times[300,Subtract[1,Slot[1]]]],
                        <| "MultiplyAdds" -> numOps, "Additions" -> numOps |>,
                    Sqrt[#1]/(1+#1)&,
                        <| "Exponentiations" -> numOps, "Divisions" -> numOps, "Additions" -> numOps |>,
                    Min[2, #1]&,
                        <| "Comparisons" -> numOps |>,
                    0.125 #1& | Function[Times[0.2`,Slot[1]]] | Function[Times[9,Slot[1]]] | Function[Times[300,Slot[1]]] | Function[Times[512,Slot[1]]] | Function[Times[512,Subtract[1,Slot[1]]]],
                        <| "MultiplyAdds" -> numOps |>,
                    2.606` #1&,
                        <| "MultiplyAdds" -> numOps |>,
                    0.01` #1&,
                        <| "MultiplyAdds" -> numOps |>,
                    Function[Times[Slot[1],Power[Plus[1.`*^-10,Slot[2]],-1]]],
                        <| "Additions" -> numOps, "Divisions" -> numOps |>,
                    Function[Plus[-1,Times[2,Slot[1]]]],
                        <| "MultiplyAdds" -> numOps, "Additions" -> numOps |>,
                    Function[Plus[0.5`,Times[0.5`,Tanh[Slot[1]]]]],
                        <| "MultiplyAdds" -> numOps, "Exponentiations" -> numOps, "Additions" -> numOps |>,
                    Function[Exp[Times[0.2`,Slot[1]]]],
                        <| "MultiplyAdds" -> numOps, "Exponentiations" -> numOps |>,
                    Function[Log[Max[1.`*^-20,Power[Slot[1],2]]]],
                        <| "MultiplyAdds" -> numOps, "Exponentiations" -> numOps, "Comparisons" -> numOps |>,
                    Function[Log[Plus[0.001`,Slot[1]]]],
                        <| "Additions" -> numOps, "Exponentiations" -> numOps |>,
                    Function[Plus[Max[0,Slot[1]],Times[0.1`,Min[0,Slot[1]]]]],
                        <| "Additions" -> numOps, "Comparisons" -> 2*numOps |>,
                    0.5 (#1 (1+Erf[0.7071067811865475` #1]))&,
                         <| "MultiplyAdds" -> 3*numOps, "Exponentiations" -> numOps, "Additions" -> numOps |>,
                    Function[Log[Plus[1.`*^-14,Slot[1]]]],
                        <| "Additions" -> numOps, "Exponentiations" -> numOps |>,
                    Function[Times[0.5`,Times[Slot[1],Plus[1,Tanh[Times[0.7978845608028654`,Plus[Slot[1],Times[0.044715`,Power[Slot[1],3]]]]]]]]],
                        <| "MultiplyAdds" -> 6*numOps, "Exponentiations" -> numOps |>,
                    (ScaledExponentialLinearUnit[#1]&) | ("ScaledExponentialLinearUnit"[#1]&) | ("SELU"[#1]&),
                        <| "Exponentiations" -> numOps, "Additions" -> numOps |>,
                    Function[Clip[Slot[1],List[0,24]]] | Function[Clip[Slot[1],List[-3,3]]],
                        <| "Comparisons" -> 2*numOps |>,
                    Function[Plus[Max[0,Slot[1]],Times[0.2`,Min[0,Slot[1]]]]],
                        <| "Comparisons" -> 2*numOps, "MultiplyAdds" -> numOps, "Additions" -> numOps |>,
                    ("RectifiedLinearUnit"[#1]&) | ("RELU"[#1]&),
                        <| "Comparisons" -> numOps, "MultiplyAdds" -> numOps |>,
                    ("ExponentialLinearUnit"[#1]&) | ("ELU"[#1]&),
                        <| "Comparisons" -> numOps, "Additions" -> numOps |>,
                    "SoftSign"[#1]&,
                        <| "Comparisons" -> numOps, "Additions" -> numOps, "Divisions" -> numOps |>,
                    "SoftPlus"[#1]&,
                        <| "Exponentiations" -> 2*numOps, "Additions" -> numOps |>,
                    "HardTanh"[#1]&,
                        <| "Comparisons" -> 2*numOps |>,
                    "HardSigmoid"[#1]&,
                        <| "Comparisons" -> 2*numOps, "Divisions" -> numOps |>,
                    "Sigmoid"[#1]&,
                        <| "Exponentiations" -> 2*numOps, "Additions" -> numOps, "Divisions" -> numOps |>,
                    _,
                        Global`es = NeuralNetworks`Private`ScalarFunctionToPureFunction[fun];
                        Print["unhandled ElementwiseLayer ScalarFunctionObject case ", FullForm@NeuralNetworks`Private`ScalarFunctionToPureFunction[fun]]
                ],
            _,
                Print["unhandled ElementwiseLayer case ", fun]

        ]
    ];
flops[ConstantPlusLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, numOps},
        inputShapes = tensorDims[inputs["Input"]];
        numOps = Apply[Times, inputShapes];
        <| "Additions" -> numOps |>
    ];
flops[OrderingLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, numOps},
        inputShapes = tensorDims[inputs["Input"]];
        numOps = Apply[Times, inputShapes];
        <| "Additions" -> numOps * Log2[numOps] |>
    ];
flops[ThreadingLayer, inputs_, outputs_, params_, opts_] :=
    Module[{numInputs, input, inputShapes, fun, numOps},
        input = inputs["Input"];
        If[MissingQ[input],
            input = inputs["1"]
        ];
        inputShapes = tensorDims[input];

        numInputs = Length[inputs];
        numOps = numInputs * Apply[Times, inputShapes];
        fun = params["Function"];
        Switch[fun,
            ValidatedParameter[Plus],
                <| "Additions" -> numOps |>,
            ValidatedParameter[Total],
                <| "Additions" -> numOps |>,
            ValidatedParameter[Times],
                <| "MultiplyAdds" -> numOps |>,
            ValidatedParameter[Divide],
                <| "Divisions" -> numOps |>,
            ValidatedParameter[_NeuralNetworks`Private`ScalarFunctionObject],
                fun = First[fun];
                Switch[NeuralNetworks`Private`ScalarFunctionToPureFunction[fun],
                  (#1+#2)&,
                      <| "Additions" -> numOps |>,
                  Function[Subtract[Slot[1],Times[0.5`,Slot[2]]]] | Function[Plus[Slot[1],Times[0.5`,Slot[2]]]],
                      <| "MultiplyAdds" -> numOps |>,
                  Function[Times[Slot[1],Slot[2]]],
                      <| "MultiplyAdds" -> numOps |>,
                  Function[Log[Plus[1.`*^-14,Slot[1]]]],
                      <| "Additions" -> numOps, "Exponentiations" -> numOps |>,
                  Function[Plus[Slot[1],Minus[Slot[2]]]],
                      <| "Additions" -> numOps |>,
                  Function[Power[Plus[Slot[1],Minus[Slot[2]]],2]],
                      <| "MultiplyAdds" -> numOps, "Additions" -> numOps |>,
                  Function[Times[Slot[1],Power[Plus[1.`*^-6,Slot[2]],-1]]],
                      <| "Additions" -> numOps, "Divisions" -> numOps |>,
                  Function[Times[Slot[2],Subtract[1,Slot[1]]]],
                      <| "Additions" -> numOps, "MultiplyAdds" -> numOps |>,
                  Function[Plus[Times[Slot[1],Slot[2]],Times[Slot[3],Subtract[1,Slot[2]]]]],
                      <| "Additions" -> 2*numOps, "MultiplyAdds" -> 2*numOps |>,
                  Function[Times[Slot[1],Power[Plus[1.`*^-10,Slot[2]],-1]]],
                      <| "Additions" -> numOps, "Divisions" -> numOps |>,
                  #3+(#1+#2)&,
                      <| "Additions" -> 2*numOps |>,
                  _,
                    Global`es = NeuralNetworks`Private`ScalarFunctionToPureFunction[fun];
                    Print["unhandled ThreadingLayer ScalarFunctionObject case ", FullForm@ NeuralNetworks`Private`ScalarFunctionToPureFunction[fun]]
                ],
            _,
                Print["unhandled ThreadingLayer case", params]
        ]
    ];
flops[AggregationLayer, inputs_, outputs_, params_, opts_] :=
    Module[{numInputs, inputShapes, fun, numOps},
        inputShapes = tensorDims[inputs["Input"]];

        numInputs = Length[inputs];
        numOps = numInputs * Apply[Times, inputShapes];
        fun = params["Function"];
        Switch[fun,
            Max | ValidatedParameter[Max],
                <| "Comparisons" -> numOps |>,
            ValidatedParameter[Times],
                <| "MultiplyAdds" -> numOps |>,
            Mean | ValidatedParameter[Mean],
                <| "Additions" -> numOps, "Divisions" -> numOps |>,
            Total | ValidatedParameter[Total],
                <| "Additions" -> numOps |>,
            _,
                Print["unhandled AggregationLayer case", params]
        ]
    ];
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
flops[LinearLayer, inputs_, outputs_, params_, opts_] :=
    Module[{batchSize, inputShapes, outputShapes, fun, m, n, k},
        inputShapes = tensorDims[inputs["Input"]];
        outputShapes = tensorDims[outputs["Output"]];

        If[MatchQ[outputShapes, _NeuralNetworks`ListT],
            Return[<|
                "MultiplyAdds" -> Apply[Times, inputShapes]
            |>]
        ];

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
flops[DropoutLayer, inputs_, outputs_, params_, opts_] :=
    Module[{numInputs, inputShapes, fun, numOps},
        inputShapes = tensorDims[inputs["Input"]];

        numInputs = Length[inputs];
        numOps = Apply[Times, inputShapes];
        <|
            "MultiplyAdds" -> numOps
        |>
    ];
flops[LocalResponseNormalizationLayer, inputs_, outputs_, params_, opts_] :=
    Module[{inputShapes, channel, numOps},
        inputShapes = tensorDims[inputs["Input"]];
        channel = params["ChannelWindowSize"];
        numOps = channel * Apply[Times, inputShapes];
        <| "Additions" -> numOps |>
    ];

flops[PaddingLayer, inputs_, outputs_, params_, opts_] :=
    <||>
flops[FlattenLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[ResizeLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[ReshapeLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[CatenateLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[PartLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[ReplicateLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[TransposeLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[SequenceMostLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[SequenceLastLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[NeuralNetworks`SequenceIndicesLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[ExtractLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
flops[ConstantArrayLayer, inputs_, outputs_, params_, opts_] :=
    <||>;
GatedRecurrentLayer;
EmbeddingLayer;
DeconvolutionLayer;



paramsOf[lyr_] :=
    NData[lyr]["Parameters"]

tensorDims[TensorT[dims_, _]] :=
	dims

End[]


EndPackage[]

