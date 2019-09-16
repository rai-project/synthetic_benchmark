#!/usr/env python3
import json
import os

from arch_latencies_config import *
from GPU_config import *

dir_path = os.path.dirname(os.path.realpath(__file__))

archs = ["RTX", "Volta", "Pascal", "Maxwell", "Kepler"]
gpus = ["K40m", "Titanx", "P100", "V100"]


if __name__ == "__main__":
    for arch_name in archs:
        base_dir = os.path.join(dir_path, "data", arch_name)
        try:
            os.mkdir(base_dir)
        except OSError:
            pass
        arch = getattr(sys.modules[__name__], arch_name)
        trgt_file = os.path.join(base_dir, "alu_latencies.json")
        with open(trgt_file, 'w', encoding='utf-8') as f:
            data = get_alu_latencies(arch)
            json.dump(data, f, ensure_ascii=False, indent=4)
        trgt_file = os.path.join(base_dir, "mem_latencies.json")
        with open(trgt_file, 'w', encoding='utf-8') as f:
            data = get_mem_latencies(arch)
            json.dump(data, f, ensure_ascii=False, indent=4)
    for gpu_name in gpus:
        base_dir = os.path.join(dir_path, "data", gpu_name)
        try:
            os.mkdir(base_dir)
        except OSError:
            pass
        gpu = getattr(sys.modules[__name__], gpu_name)
        trgt_file = os.path.join(base_dir, "information.json")
        with open(trgt_file, 'w', encoding='utf-8') as f:
            data = get_gpu_config(gpu)
            json.dump(data, f, ensure_ascii=False, indent=4)
