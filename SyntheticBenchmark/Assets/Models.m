
BeginPackage["SyntheticBenchmark`Assets`Models`"]

$Models

$ModelNames

DeleteModelWeights

Begin["`Private`"]


rootDirectory = FileNameDrop[$InputFileName, -1];
modelsMXFile = FileNameJoin[{rootDir, "models.mx"}];

If[FileExistsQ[modelsMXFile],
    Get[modelsMXFile]
]


$ModelNames = {"2D Face Alignment Net Trained on 300W Large Pose Data", 
 "3D Face Alignment Net Trained on 300W Large Pose Data", 
 "AdaIN-Style Trained on MS-COCO and Painter by Numbers Data", 
 "Ademxapp Model A1 Trained on ADE20K Data", 
 "Ademxapp Model A1 Trained on Cityscapes Data", 
 "Ademxapp Model A1 Trained on PASCAL VOC2012 and MS-COCO Data", 
 "Ademxapp Model A Trained on ImageNet Competition Data", 
 "Age Estimation VGG-16 Trained on IMDB-WIKI and Looking at People Data", 
 "Age Estimation VGG-16 Trained on IMDB-WIKI Data", 
 "BERT Trained on BookCorpus and English Wikipedia Data", 
 "BPEmb Subword Embeddings Trained on Wikipedia Data", 
 "CapsNet Trained on MNIST Data", "Clinical Concept Embeddings Trained on \
Health Insurance Claims, Clinical Narratives from Stanford and PubMed Journal \
Articles", 
 "Colorful Image Colorization Trained on ImageNet Competition Data", 
 "ColorNet Image Colorization Trained on ImageNet Competition Data", 
 "ColorNet Image Colorization Trained on Places Data", 
 "ConceptNet Numberbatch Word Vectors V17.06", 
 "ConceptNet Numberbatch Word Vectors V17.06 (Raw Model)", 
 "CREPE Pitch Detection Net Trained on Monophonic Signal Data", 
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
 "Deep Speech 2 Trained on Baidu English Data", 
 "Dilated ResNet-105 Trained on Cityscapes Data", 
 "Dilated ResNet-22 Trained on Cityscapes Data", 
 "Dilated ResNet-38 Trained on Cityscapes Data", 
 "ELMo Contextual Word Representations Trained on 1B Word Benchmark", 
 "Enhanced Super-Resolution GAN Trained on DIV2K, Flickr2K and OST Data", 
 "Gender Prediction VGG-16 Trained on IMDB-WIKI Data", 
 "GloVe 100-Dimensional Word Vectors Trained on Tweets", "GloVe \
100-Dimensional Word Vectors Trained on Wikipedia and Gigaword 5 Data", 
 "GloVe 200-Dimensional Word Vectors Trained on Tweets", 
 "GloVe 25-Dimensional Word Vectors Trained on Tweets", 
 "GloVe 300-Dimensional Word Vectors Trained on Common Crawl 42B", 
 "GloVe 300-Dimensional Word Vectors Trained on Common Crawl 840B", "GloVe \
300-Dimensional Word Vectors Trained on Wikipedia and Gigaword 5 Data", 
 "GloVe 50-Dimensional Word Vectors Trained on Tweets", 
 "GloVe 50-Dimensional Word Vectors Trained on Wikipedia and Gigaword 5 Data"\
, "GPT-2 Transformer Trained on WebText Data", 
 "GPT Transformer Trained on BookCorpus Data", 
 "Inception V1 Trained on Extended Salient Object Subitizing Data", 
 "Inception V1 Trained on ImageNet Competition Data", 
 "Inception V1 Trained on Places365 Data", 
 "Inception V3 Trained on ImageNet Competition Data", "LeNet", 
 "LeNet Trained on MNIST Data", 
 "MobileNet V2 Trained on ImageNet Competition Data", 
 "Multi-scale Context Aggregation Net Trained on CamVid Data", 
 "Multi-scale Context Aggregation Net Trained on Cityscapes Data", 
 "Multi-scale Context Aggregation Net Trained on PASCAL VOC2012 Data", 
 "OpenFace Face Recognition Net Trained on CASIA-WebFace and FaceScrub Data", 
 "Pix2pix Photo-to-Street-Map Translation", 
 "Pix2pix Street-Map-to-Photo Translation", 
 "ResNet-101 Trained on Augmented CASIA-WebFace Data", 
 "ResNet-101 Trained on ImageNet Competition Data", 
 "ResNet-101 Trained on YFCC100m Geotagged Data", 
 "ResNet-152 Trained on ImageNet Competition Data", 
 "ResNet-50 Trained on ImageNet Competition Data", 
 "Self-Normalizing Net for Numeric Data", 
 "Sentiment Language Model Trained on Amazon Product Review Data", 
 "Single-Image Depth Perception Net Trained on Depth in the Wild Data", "Sing\
le-Image Depth Perception Net Trained on NYU Depth V2 and Depth in the Wild \
Data", "Single-Image Depth Perception Net Trained on NYU Depth V2 Data", 
 "Squeeze-and-Excitation Net Trained on ImageNet Competition Data", 
 "SqueezeNet V1.1 Trained on ImageNet Competition Data", 
 "SSD-VGG-300 Trained on PASCAL VOC Data", 
 "SSD-VGG-512 Trained on MS-COCO Data", 
 "SSD-VGG-512 Trained on PASCAL VOC2007, PASCAL VOC2012 and MS-COCO Data", "U\
-Net Trained on Glioblastoma-Astrocytoma U373 Cells on a Polyacrylamide \
Substrate Data", 
 "Unguided Volumetric Regression Net for 3D Face Reconstruction", 
 "Vanilla CNN for Facial Landmark Regression", 
 "Very Deep Net for Super-Resolution", 
 "VGG-16 Trained on ImageNet Competition Data", 
 "VGG-19 Trained on ImageNet Competition Data", 
 "VGGish Feature Extractor Trained on YouTube Data", 
 "Wide ResNet-50-2 Trained on ImageNet Competition Data", 
 "Wolfram AudioIdentify V1 Trained on AudioSet Data", 
 "Wolfram C Character-Level Language Model V1", 
 "Wolfram English Character-Level Language Model V1", 
 "Wolfram ImageIdentify Net V1", 
 "Wolfram JavaScript Character-Level Language Model V1", 
 "Wolfram LaTeX Character-Level Language Model V1", 
 "Wolfram Python Character-Level Language Model V1", 
 "Yahoo Open NSFW Model V1", "YOLO V2 Trained on MS-COCO Data"};

ClearAll[proc, trimArraysConstant, inputs, inputs0];
trimArraysConstant[assoc0_] := Module[{assoc = assoc0, arrays},
    If[KeyExistsQ[assoc, "Arrays"],
        arrays = Lookup[assoc, "Arrays"];
        If[KeyExistsQ[arrays, "Scaling"],
            AssociateTo[arrays, "Scaling" -> Dimensions[Lookup[arrays, "Scaling"]]]
        ];
        If[KeyExistsQ[arrays, "Weights"],
            AssociateTo[arrays, "Weights" -> Dimensions[Lookup[arrays, "Weights"]]]
        ];
        If[KeyExistsQ[arrays, "Biases"],
            AssociateTo[arrays, "Biases" -> Dimensions[Lookup[arrays, "Biases"]]]
        ];
        AssociateTo[assoc, "Arrays" -> arrays];
        assoc
    ]
];
deleteLayerWeights[lyr_[assoc0_, r___]] := 
    Module[{assoc = assoc0, trimArrys, arrays, params, net},
        If[KeyExistsQ[assoc, "Arrays"],
            AssociateTo[assoc, "Arrays" -> trimArraysConstant[Lookup[assoc, "Arrays"]]]
        ];
        If[KeyExistsQ[assoc, "Parameters"] && KeyExistsQ[Lookup[assoc, "Parameters"], "Arrays"],
            AssociateTo[assoc, 
                "Parameters" -> Append[Lookup[Lookup[assoc, "Parameters"], "Arrays"], 
                                        "Arrays" -> trimArraysConstant[Lookup[Lookup[assoc, "Parameters"], "Arrays"]]]
            ]
        ]; 
        If[KeyExistsQ[assoc, "Parameters"] && KeyExistsQ[Lookup[assoc, "Parameters"], "Net"],
            params = Lookup[assoc, "Parameters"];
            net = Lookup[params, "Net"];
            If[KeyExistsQ[net, "Arrays"],
                AssociateTo[net, "Arrays" -> trimArraysConstant[Lookup[net, "Arrays"]]];
            ];
            AssociateTo[params, "Net" -> net];
            AssociateTo[assoc, "Parameters" -> params]
        ];
        (*KeyDropFrom[assoc,"Inputs"];
        KeyDropFrom[assoc,"Output"];*)
        Inactive[lyr][assoc, r]
   ];

DeleteModelWeights[model_?StringQ] :=
    With[{
        layers = NetInformation[NetModel[model]]
    },
        model -> (deleteLayerWeights /@ layers)
    ]

DumpSaveModelWeights[] :=
    Module[{names = $ModelNames, info},
        info = DeleteModelWeights /@ names;
        $Models = info;
        DumpSave[modelsMXFile, $Models];
    ]

End[]

EndPackage[]