#!/usr/bin/env wolframscript

layer = ConstantArrayLayer["Array" -> RandomReal[1, {64, 128, 128}]];
net = NetChain[{layer}];
Print[TrimmedMean[Table[First[AbsoluteTiming[net[];]], {100}], 0.2]]