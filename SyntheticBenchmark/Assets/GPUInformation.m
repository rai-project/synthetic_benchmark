
BeginPackage["SyntheticBenchmark`Assets`GPUInformation`"]

GPUInformation
$GPUs

Begin["`Private`"]

Needs["SyntheticBenchmark`Assets`GPULatency`"]

Gbps = 1
GBps = 8

$GPUs = {
    "TESLA V100 SXM2",
    "TESLA V100 PCIE",
    "TESLA P100 SXM2",
    "TESLA P100 PCIE",
    "TITAN Xp",
    "TITAN X",
    "K20",
    "K20X",
    "K40",
    "K80"
}

Infiniband = <|
    "Name" -> "Infiniband",
    "Bandwidth" -> 70 Gbps
|>

HBM2 = <|
    "Name" -> "HBM2",
    "Bandwidth" -> Missing["HBM2 Not Implemented"]
|>

Ethernet = <|
    "Name" -> "Ethernet",
    "Bandwidth" -> 10 Gbps
|>

Ethernet20 = <|
    "Name" -> "Ethernet20",
    "Bandwidth" -> 20 Gbps
|>

NVLink1 = <|
    "Name" -> "NVLink1",
    "Bandwidth" -> 80 GBps
|>

NVLink2 = <|
    "Name" -> "NVLink2",
    "Bandwidth" -> 300 GBps
|>

PCIe1 = <|
    "Name" -> "PCIe1",
    "Bandwidth" -> 2.4 GBps
|>

PCIe2 = <|
    "Name" -> "PCIe2",
    "Bandwidth" -> 8 GBps
|>

PCIe3 = <|
    "Name" -> "PCIe3",
    "Bandwidth" -> 16 GBps
|>

SXM2 = <|
    "Name" -> "SXM2",
    "Bandwidth" -> Missing["SXM2", "Not implemented"]
|>

GPUInformation["TESLA V100 SXM2"] = <|
    "Name" -> "TESLA V100 SXM2",
    "Architecture" -> "Volta",
    "InstructionLatency" -> GPUInstructionLatency["Volta"],
    "MemoryLatency" -> GPUMemoryLatency["Volta"],
    "Interconnect" -> SXM2,
	"ClockRate" -> 1530,
	"PeekGFlops" -> 15000,
	"MemoryBandwidth" -> 900
|>

GPUInformation["TESLA V100 PCIE"] = <|
    "Name" -> "TESLA V100 PCIE",
    "Architecture" -> "Volta",
    "InstructionLatency" -> GPUInstructionLatency["Volta"],
    "MemoryLatency" -> GPUMemoryLatency["Volta"],
    "Interconnect" -> PCIe2,
	"ClockRate" -> 1380,
	"PeekGFlops" -> 14000,
	"MemoryBandwidth" -> 900
|>

GPUInformation["TESLA P100 SXM2"] = <|
    "Name" -> "TESLA P100 SXM2",
    "Architecture" -> "Pascal",
    "InstructionLatency" -> GPUInstructionLatency["Pascal"],
    "MemoryLatency" -> GPUMemoryLatency["Pascal"],
    "Interconnect" -> SXM2,
	"ClockRate" -> 1481,
	"PeekGFlops" -> 10600,
	"MemoryBandwidth" -> 732
|>

GPUInformation["TESLA P100 PCIE"] = <|
    "Name" -> "TESLA P100 PCIE",
    "Architecture" -> "Pascal",
    "InstructionLatency" -> GPUInstructionLatency["Pascal"],
    "MemoryLatency" -> GPUMemoryLatency["Pascal"],
    "Interconnect" -> PCIe2,
	"ClockRate" -> 1328,
	"PeekGFlops" -> 9300,
	"MemoryBandwidth" -> 732
|>

GPUInformation["TITAN Xp"] = <|
    "Name" -> "TITAN Xp",
    "Architecture" -> "Maxwell",
    "InstructionLatency" -> GPUInstructionLatency["Maxwell"],
    "MemoryLatency" -> GPUMemoryLatency["Maxwell"],
    "Interconnect" -> PCIe2,
	"ClockRate" -> 1582,
	"PeekGFlops" -> 12100,
	"MemoryBandwidth" -> 547.7
|>

GPUInformation["TITAN X"] = <|
    "Name" -> "TITAN X",
    "InstructionLatency" -> GPUInstructionLatency["Maxwell"],
    "MemoryLatency" -> GPUMemoryLatency["Maxwell"],
    "Architecture" -> "Maxwell",
    "Interconnect" -> PCIe2,
	"ClockRate" -> 1000,
	"PeekGFlops" -> 6144,
	"MemoryBandwidth" -> 336.5
|>

GPUInformation["K20"] = <|
    "Name" -> "K20",
    "InstructionLatency" -> GPUInstructionLatency["Kepler"],
    "MemoryLatency" -> GPUMemoryLatency["Kepler"],
    "Architecture" -> "Kepler",
    "Interconnect" -> PCIe2,
	"ClockRate" -> 1000,
	"PeekGFlops" -> 3520,
	"MemoryBandwidth" -> 208
|>

GPUInformation["K20X"] = <|
    "Name" -> "K20X",
    "InstructionLatency" -> GPUInstructionLatency["Kepler"],
    "MemoryLatency" -> GPUMemoryLatency["Kepler"],
    "Architecture" -> "Kepler",
    "Interconnect" -> PCIe2,
	"ClockRate" -> 1000,
	"PeekGFlops" -> 3935,
	"MemoryBandwidth" -> 250
|>

GPUInformation["K40"] = <|
    "Name" -> "K40",
    "InstructionLatency" -> GPUInstructionLatency["Kepler"],
    "MemoryLatency" -> GPUMemoryLatency["Kepler"],
    "Architecture" -> "Kepler",
    "Interconnect" -> PCIe2,
	"ClockRate" -> 745,
	"PeekGFlops" -> 4290,
	"MemoryBandwidth" -> 288
|>

GPUInformation["K80"] = <|
    "Name" -> "K80",
    "InstructionLatency" -> GPUInstructionLatency["Kepler"],
    "MemoryLatency" -> GPUMemoryLatency["Kepler"],
    "Architecture" -> "Kepler",
    "Interconnect" -> PCIe2,
	"ClockRate" -> 560,
	"PeekGFlops" -> 5600,
	"MemoryBandwidth" -> 480
|>

End[]

EndPackage[]