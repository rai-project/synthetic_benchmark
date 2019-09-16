
BeginPackage["SyntheticBenchmark`Assets`GPULatency`"]

GPUInstructionLatency
GPUMemoryLatency
$GPUArchitectures

Begin["`Private`"]

(* https://github.com/lanl/PPT/blob/master/code/apps/PPT-GPU/configs/arch_latencies_config.py *)

rootDataDirectory = FileNameJoin[{FileNameDrop[$InputFileName, -1], "gpu_config", "data"}];

getALULatencyPath[arch_] := FileNameJoin[{rootDataDirectory, arch, "alu_latencies.json"}]
getMemLatencyPath[arch_] := FileNameJoin[{rootDataDirectory, arch, "mem_latencies.json"}]

$GPUArchitectures = {"RTX", "Volta", "Pascal", "Maxwell", "Kepler"}

If[!AssociationQ[GPUInstructionLatency],
    GPUInstructionLatency = <||>;
    GPUMemoryLatency = <||>;

    Do[
        AssociateTo[GPUInstructionLatency, arch -> Import[getALULatencyPath[arch], "RawJSON"]];
        AssociateTo[GPUMemoryLatency, arch -> Import[getMemLatencyPath[arch], "RawJSON"]];
        ,
        {arch, $GPUArchitectures}
    ]
];

End[]

EndPackage[]