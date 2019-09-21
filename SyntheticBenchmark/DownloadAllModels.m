

rootDirectory = FileNameDrop[$InputFileName, -1];
PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]
Get["SyntheticBenchmark`Assets`"]

modelNames = Keys[$Models]

 ResourceSystemClient`Private`$throttle = 1;

Table[
    Print[model];
    PreemptProtect[
        AbortProtect[
            net = NetModel[model]
        ]
    ];
    ClearAll[net];
    ,
    {model, modelNames}
]