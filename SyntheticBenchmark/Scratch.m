multiGPUResNet152[dev_, batch_, prec_:"Real32"] := Module[
  {data, net,
    x = NumericArray[RandomReal[1, {batch, 3, 224, 224}], "Real32"],
    y = NumericArray[RandomReal[1, {batch, 1}], "Real32"]
  },
  net = NetChain[{NetDrop[NetModel["ResNet-152 Trained on ImageNet Competition Data", "UninitializedEvaluationNet"], -1], LinearLayer[1]}];
  net = NetReplacePart[net,"Input" -> {3, 224, 224}];
  NetTrain[net, x->y, 
    "MeanExamplesPerSecond", TargetDevice -> dev, BatchSize -> batch, 
    MaxTrainingRounds -> 100, WorkingPrecision -> "Real32"]
]


benchRNN[dev_, batch_, prec_:"Real32"] := Module[
	{hidden = 1024, net, 
		x = Table[NumericArray[RandomReal[1, {128, 32}], "Real32"], batch],
		y = Table[NumericArray[RandomReal[1, {1}], "Real32"], batch]
	},
	net = NetInitialize@NetChain[{LongShortTermMemoryLayer[hidden], 
		LongShortTermMemoryLayer[hidden], 
		LongShortTermMemoryLayer[hidden],
		LongShortTermMemoryLayer[hidden], SequenceLastLayer[], 
		LinearLayer[1]}, "Input" -> {128, 32}];
		
	NetTrain[net, x->y, 
		"MeanExamplesPerSecond", 
		TargetDevice -> dev, BatchSize -> batch, 
		MaxTrainingRounds -> 5, WorkingPrecision -> prec]
]

