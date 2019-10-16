#!/usr/bin/env wolframscript

modelNames = {
 "Ademxapp Model A Trained on ImageNet Competition Data",
"Age Estimation VGG-16 Trained on IMDB-WIKI and Looking at People Data",
"Age Estimation VGG-16 Trained on IMDB-WIKI Data",
"CapsNet Trained on MNIST Data",
"Gender Prediction VGG-16 Trained on IMDB-WIKI Data",
"Inception V1 Trained on Extended Salient Object Subitizing Data",
"Inception V1 Trained on ImageNet Competition Data",
"Inception V1 Trained on Places365 Data",
"Inception V3 Trained on ImageNet Competition Data",
"MobileNet V2 Trained on ImageNet Competition Data",
"ResNet-101 Trained on ImageNet Competition Data",
"ResNet-101 Trained on YFCC100m Geotagged Data",
"ResNet-152 Trained on ImageNet Competition Data",
"ResNet-50 Trained on ImageNet Competition Data",
"Squeeze-and-Excitation Net Trained on ImageNet Competition Data",
"SqueezeNet V1.1 Trained on ImageNet Competition Data",
"VGG-16 Trained on ImageNet Competition Data",
"VGG-19 Trained on ImageNet Competition Data",
"Wide ResNet-50-2 Trained on ImageNet Competition Data",
"Wolfram ImageIdentify Net V1",
"Yahoo Open NSFW Model V1",
"BERT Trained on BookCorpus and English Wikipedia Data",
"GPT Transformer Trained on BookCorpus Data",
"OpenFace Face Recognition Net Trained on CASIA-WebFace and FaceScrub Data",
"ResNet-101 Trained on Augmented CASIA-WebFace Data",
"AdaIN-Style Trained on MS-COCO and Painter by Numbers Data",
"Colorful Image Colorization Trained on ImageNet Competition Data",
"ColorNet Image Colorization Trained on ImageNet Competition Data",
"ColorNet Image Colorization Trained on Places Data",
"CycleGAN Apple-to-Orange Translation Trained on ImageNet Competition Data",
"CycleGAN Horse-to-Zebra Translation Trained on ImageNet Competition Data",
"CycleGAN Monet-to-Photo Translation",
"CycleGAN Orange-to-Apple Translation Trained on ImageNet Competition Data",
"CycleGAN Photo-to-Cezanne Translation",
"CycleGAN Photo-to-Monet Translation",
"CycleGAN Photo-to-Van Gogh Translation",
"CycleGAN Summer-to-Winter Translation",
"CycleGAN Winter-to-Summer Translation",
"CycleGAN Zebra-to-Horse Translation Trained on ImageNet Competition Data",
"Pix2pix Photo-to-Street-Map Translation",
"Pix2pix Street-Map-to-Photo Translation",
"Very Deep Net for Super-Resolution",
"GPT-2 Transformer Trained on WebText Data",
"Wolfram JavaScript Character-Level Language Model V1",
"SSD-VGG-300 Trained on PASCAL VOC Data",
"SSD-VGG-512 Trained on MS-COCO Data",
"YOLO V2 Trained on MS-COCO Data",
"2D Face Alignment Net Trained on 300W Large Pose Data",
"3D Face Alignment Net Trained on 300W Large Pose Data",
"Single-Image Depth Perception Net Trained on Depth in the Wild Data",
"Single-Image Depth Perception Net Trained on NYU Depth V2 and Depth in the Wild Data",
"Single-Image Depth Perception Net Trained on NYU Depth V2 Data",
"Unguided Volumetric Regression Net for 3D Face Reconstruction",
"Ademxapp Model A1 Trained on ADE20K Data",
"Ademxapp Model A1 Trained on PASCAL VOC2012 and MS-COCO Data",
"Multi-scale Context Aggregation Net Trained on CamVid Data",
"U-Net Trained on Glioblastoma-Astrocytoma U373 Cells on a Polyacrylamide Substrate Data"
 };
 
(*********************************************************************)
(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


<< NeuralNetworks`
<< MXNetLink`


rootDirectory = FileNameDrop[$InputFileName, -1];
PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]
Get["SyntheticBenchmark`Assets`"]

modelNames = Keys[$Models];

(* modelNames = {"ELMo Contextual Word Representations Trained on 1B Word Benchmark"} *)

PrependTo[$ContextPath, "MXNetLink`PackageScope`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`"];
PrependTo[$ContextPath, "NeuralNetworks`Private`Benchmarking`"];

batches = 1;
batchSize = 1;
NeuralNetworks`Private`Benchmarking`dataSize = batches*batchSize;
sequenceLength = 1;


baseDir = FileNameJoin[{rootDirectory, "..", "data", "mxnet_model"}]
Quiet[CreateDirectory[baseDir]]

run[name_, net_, n_] :=
    Module[{plan, ex, data, res, setter},
        NDArrayWaitForAll[];
        plan = ToNetPlan[net];
        ex = ToNetExecutor[plan, 1, "ArrayCaching" -> False];
        SeedRandom[1];
        data = synthesizeData /@ Inputs[net];
        xPrint[First[plan]["Inputs"]];
        setter = If[ContainsVarSequenceQ[Inputs[net]],
          NDArraySetUnbatched[#1, #2, 1]&,
          NDArraySet[#1, #2]&
        ];
        (* Table[
          Print[{key,data[key]}];
          (* xPrint[ex["Arrays", "Inputs",  key]];
          data[key] = NumericArray[{40434, 17087, 39104}, "Integer32"]; *)
            NDArraySet[ex["Arrays", "Inputs",  key], data[key]],
            {key, Keys[data]}
        ]; *)
        xPrint[Head[ex]];
        Table[
            setter[ex["Arrays", "Inputs",  key], data[key]],
            {key, Keys[data]}
        ];
        NDArrayWaitForAll[];
        res = Table[
            First[AbsoluteTiming[
                NetExecutorForward[ex, (* IsTraining= *) False];
                NDArrayWaitForAll[];
            ]],
            {n}
        ];
        ClearAll[plan];
        ClearAll[ex];
        ClearAll[data];
        res
    ]

invalidVal = ""
$NumRuns = 50

<< SyntheticBenchmark`
<< SyntheticBenchmark`StaticAnalysis`
$Models = Keys[SyntheticBenchmark`Assets`Models`$Models];
ClearAll[getModelId]; 
getModelId[name_] := 
 First[FirstPosition[$Models, name]]

summarize[t_] := TrimmedMean[t, 0.2]

timings = {}

benchmarkModel[modelName_] :=
    benchmarkModel[modelName, $NumRuns]
benchmarkModel[modelName_, n_] :=
  Module[{model, time},
    Print["benchmarking .... " <> modelName];
    model = NetModel[modelName];
    net = model;
    lyrs = NetInformation[model, "Layers"];
    time = Check[
        CheckAbort[
        run[modelName, net, n],
        xPrint["abort. path .."];
        $Failed],
        $Failed
    ];
    xPrint[Internal`$LastInternalFailure];
    time = <|
      "id" -> getModelId[modelName],
      "name" -> modelName,
      "min_time" -> If[time === $Failed, invalidVal, Round[1000000 * Min[time], 0.0001]],
      "mean_time" -> If[time === $Failed, invalidVal, Round[1000000 * TrimmedMean[time, 0.2], 0.0001]],
      "max_time" -> If[time === $Failed, invalidVal, Round[1000000 * Max[time], 0.0001]],
      "raw_time" -> If[time === $Failed, invalidVal, Round[1000000 * time, 0.0001]]
    |>;
    ClearAll[net];
    AppendTo[timings, time];
    writeTimings[StringReplace[modelName, " " -> "_"], {time}];
    Print["writing benchmark results .... " <> modelName];
  ]

writeTimings[modelName_, timings_] :=
  Module[{tbl, header, time, flops},
    header = Keys[timings[[1]]];
    tbl = Lookup[#, header]& /@ timings;
    PrependTo[tbl, header];
    Export[FileNameJoin[{baseDir, modelName <> ".csv"}], tbl, "CSV"]
  ]



synthesizeData = NeuralNetworks`Private`Benchmarking`synthesizeData;
Inputs = NeuralNetworks`Private`Inputs;
NetAttachLoss = NeuralNetworks`NetAttachLoss;



paramsOf[lyr_[params_, ___]] := params
kernelSize[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["KernelSize"]}, If[ListQ[e], e, {"",""}]]
stride[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Stride"]}, If[ListQ[e], e, {"",""}]]
function[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Function"]},
  If[! MissingQ[e], e /. ValidatedParameter -> Identity, ""]]
dilation[lyr_[params_, ___]] :=
 With[{e = params["Parameters"]["Dilation"]}, If[ListQ[e], e, {"",""}]]
inputDims[lyr_[params_, ___]] :=
 With[{e = params["Inputs"]},
  With[{r=If[AssociationQ[e],
   If[KeyExistsQ[e, "Input"], e["Input"], e["1"]] /.
    TensorT[x_, _] :> x, {"","",""}]},
    PadRight[r, 3, ""]
  ]]
outputDims[lyr_[params_, ___]] :=
 With[{r=With[{e = params["Outputs"]},
  If[AssociationQ[e], e["Output"] /. TensorT[x_, _] :> x, {"","",""}]]},
    PadRight[r, 3, ""]
 ]

synthesizeData[e_["Image", params_, sz_]] /; e === NetEncoder := 
 RandomReal[1, sz /. TensorT[x_, ___] :> Prepend[x, batchSize]]

encLength = 10
synthesizeData[enc:(e_["Function", params_, sz0_, ___])] /; (e === NetEncoder && MatchQ[params["Pattern"], Verbatim[ValidatedParameter[_String]]]):= 
  Module[{sz = sz0 //. {TensorT[x_,___]:>x, _LengthVar :> encLength}},
    If[Length[sz] === 2,
      (* First[params["Function"]][StringRiffle[#, " "]& /@ Table[RandomWord[], sz[[1]], sz[[2]]]], *)
      RandomInteger[1, sz],
      First[params["Function"]][StringRiffle[Table[RandomWord[], encLength], " "]]
    ]
  ]
audioLength = sequenceLength
synthesizeData[enc:(e_["Function", params_, sz_, ___])] /; (e === NetEncoder && MatchQ[params["Pattern"], ValidatedParameter[None]]):= 
  RandomReal[1, sz /. TensorT[x_, ___] :> Prepend[x /. _LengthVar -> sequenceLength, batchSize]];


tokenLength = 10
synthesizeData[enc:(e_["Tokens", params_, sz_, ___])] /; (e === NetEncoder ):= 
  enc[StringRiffle[Table[RandomWord[], tokenLength], " "]];

charLength = 10
chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
synthesizeData[enc:(e_["Characters", params_, sz_, ___])] /; (e === NetEncoder ):= 
  enc[FromCharacterCode[RandomChoice[ToCharacterCode[chars], charLength]]];


If[Length[$ScriptCommandLine] == 2,
  modelNames = {$ScriptCommandLine[[2]]}
];

Print[modelNames]
PreemptProtect[
  AbortProtect[
    benchmarkModel /@ modelNames;
  ]
];
xPrint["done benchmarking...."];
