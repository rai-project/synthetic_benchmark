

rootDirectory = FileNameDrop[$InputFileName, -1];
PrependTo[$Path, ParentDirectory[rootDirectory]]

Get["SyntheticBenchmark`"]
Get["SyntheticBenchmark`Assets`"]

modelNames = Keys[$Models]

Table[
    Print[model];
    NetModel[model],
    {model, modelNames}
]