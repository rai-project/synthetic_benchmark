"""
*********** Performance Prediction Toolkit PPT *********

File: GPU_config.py
Description: Target GPU configurations
Author: Yehia Arafa 
"""

import sys
from arch_latencies_config import *


def get_gpu_config(gpu):
    new_gpu = gpu()
    config = new_gpu.populate_config()
    return config


class K40m():

    def populate_config(self):
        config = {}
        config["gpu_name"] = "K40m"
        # This name must be one of the classes defined in 'arch_latencies_config'
        config["gpu_arch"] = "Kepler"

        mem_latencies = get_mem_latencies(
            getattr(sys.modules[__name__], config["gpu_arch"]))

        config["num_SM"] = 15           # Number of Streaming Multiprocessors
        # Number of Single Precision cores per multiprocessor
        config["num_SP_per_SM"] = 192
        # Number of Special Function units per multiprocessor
        config["num_SF_per_SM"] = 32
        # Number of Double Precision cores per multiprocessor
        config["num_DP_per_SM"] = 64
        # Number of Load & Store units per multiprocessor
        config["num_load_store_units"] = 32
        # Number of warp schedulers available (Max number of warps that can be executed concurrently)
        config["num_warp_schedulers"] = 4
        # Number of instructions that can be issued simultaneously to a given warp
        config["num_inst_per_warp"] = 2
        config["clockspeed"] = 745*10**6    # GPU clock speed in Hertz

        config["num_registers"] = 65536        # Number of registers available

        config["l1_cache_size"] = 16*10**3     # L1 cache size in Bytes
        config["l2_cache_size"] = 1.5*10**6    # L2 cache size in Bytes
        config["global_mem_size"] = 12*10**6	    # Global memory size in Byte
        # Shared memory size in Bytes per multiprocessor
        config["shared_mem_size"] = 48*10**3

        config["l1_mem_latency"] = mem_latencies["l1_cache_access"]
        config["l2_mem_latency"] = mem_latencies["l2_cache_access"]
        config["l2_to_global_mem_latency"] = mem_latencies["global_mem_latency"] - \
            mem_latencies["l2_cache_access"]
        config["local_mem_latency"] = mem_latencies["local_mem_latency"]
        config["const_mem_latency"] = mem_latencies["constant_mem_latency"]
        config["tex_mem_latency"] = mem_latencies["texture_mem_latency"]
        config["tex_cache_latency"] = mem_latencies["texture_cache_latency"]
        config["shared_mem_latency"] = mem_latencies["shared_mem_latency"]

        config["warp_size"] = 32		    # Number of threads in a warp
        # Max number of warps resident on a single SM
        config["max_num_warps_per_SM"] = 64
        # Max number of blocks queued on a single SM
        config["max_num_block_per_SM"] = 32
        # Max number of (software) threads in a block
        config["max_num_threads_per_block"] = 1024
        # Max number of threads queued or active on a single SM
        config["max_num_threads_per_SM"] = 2048

        # Number of memory concurrent transfer from the memory queue
        config["global_mem_return_queue"] = 128
        config["num_memory_ports"] = 1            # Number of memory ports

        return config


class Titanx():

    def populate_config(self):
        config = {}
        config["gpu_name"] = "TitianX"
        # This name must be one of the classes defined in 'arch_latencies_config'
        config["gpu_arch"] = "Maxwell"

        mem_latencies = get_mem_latencies(
            getattr(sys.modules[__name__], config["gpu_arch"]))

        config["num_SM"] = 24           # Number of Streaming Multiprocessors
        # Number of Single Precision cores per multiprocessor
        config["num_SP_per_SM"] = 128
        # Number of Special Function units per multiprocessor
        config["num_SF_per_SM"] = 32
        # Number of Double Precision cores per multiprocessor
        config["num_DP_per_SM"] = 32
        # Number of Load & Store units per multiprocessor
        config["num_load_store_units"] = 32
        # Number of warp schedulers available (Max number of warps that can be executed concurrently)
        config["num_warp_schedulers"] = 4
        # Number of instructions that can be issued simultaneously to a given warp
        config["num_inst_per_warp"] = 2
        config["clockspeed"] = 1000*10**6   # GPU clock speed in Hertz

        config["num_registers"] = 65536        # Number of registers available

        config["l1_cache_size"] = 48*10**3     # L1 cache size in Bytes
        config["l2_cache_size"] = 3*10**6	    # L2 cache size in Bytes
        config["global_mem_size"] = 12*10**6	    # Global memory size in Byte
        # Shared memory size in Bytes per multiprocessor
        config["shared_mem_size"] = 64*10**3

        config["l1_mem_latency"] = mem_latencies["l1_cache_access"]
        config["l2_mem_latency"] = mem_latencies["l2_cache_access"]
        config["l2_to_global_mem_latency"] = mem_latencies["global_mem_latency"] - \
            mem_latencies["l2_cache_access"]
        config["local_mem_latency"] = mem_latencies["local_mem_latency"]
        config["const_mem_latency"] = mem_latencies["constant_mem_latency"]
        config["tex_mem_latency"] = mem_latencies["texture_mem_latency"]
        config["tex_cache_latency"] = mem_latencies["texture_cache_latency"]
        config["shared_mem_latency"] = mem_latencies["shared_mem_latency"]

        config["warp_size"] = 32		    # Number of threads in a warp
        # Max number of warps resident on a single SM
        config["max_num_warps_per_SM"] = 64
        # Max number of blocks queued on a single SM
        config["max_num_block_per_SM"] = 32
        # Max number of (software) threads in a block
        config["max_num_threads_per_block"] = 1024
        # Max number of threads queued or active on a single SM
        config["max_num_threads_per_SM"] = 2048

        # Number of memory concurrent transfer from the memory queue
        config["global_mem_return_queue"] = 128
        config["num_memory_ports"] = 1

        return config


class P100():

    def populate_config(self):
        config = {}
        config["gpu_name"] = "P100"
        # This name must be one of the classes defined in 'arch_latencies_config'
        config["gpu_arch"] = "Pascal"

        mem_latencies = get_mem_latencies(
            getattr(sys.modules[__name__], config["gpu_arch"]))

        config["num_SM"] = 56           # Number of Streaming Multiprocessors
        # Number of Single Precision cores per multiprocessor
        config["num_SP_per_SM"] = 64
        # Number of Special Function usints per multiprocessor
        config["num_SF_per_SM"] = 16
        # Number of Double Precision cores per multiprocessor
        config["num_DP_per_SM"] = 32
        # Number of Load & Store units per multiprocessor
        config["num_load_store_units"] = 16
        # Number of warp schedulers available (Max number of warps that can be executed concurrently)
        config["num_warp_schedulers"] = 4
        # Number of instructions that can be issued simultaneously to a given warp
        config["num_inst_per_warp"] = 2
        config["clockspeed"] = 1190*10**6   # GPU clock speed in Hertz

        config["num_registers"] = 65536        # Number of registers available

        config["l1_cache_size"] = 24*10**3     # L1 cache size in Bytes
        config["l2_cache_size"] = 4*10**6	    # L2 cache size in Bytes
        config["global_mem_size"] = 16*10**6	    # Global memory size in Byte
        # Shared memory size in Bytes per multiprocessor
        config["shared_mem_size"] = 64*10**3

        config["l1_mem_latency"] = mem_latencies["l1_cache_access"]
        config["l2_mem_latency"] = mem_latencies["l2_cache_access"]
        config["l2_to_global_mem_latency"] = mem_latencies["global_mem_latency"] - \
            mem_latencies["l2_cache_access"]
        config["local_mem_latency"] = mem_latencies["local_mem_latency"]
        config["const_mem_latency"] = mem_latencies["constant_mem_latency"]
        config["tex_mem_latency"] = mem_latencies["texture_mem_latency"]
        config["tex_cache_latency"] = mem_latencies["texture_cache_latency"]
        config["shared_mem_latency"] = mem_latencies["shared_mem_latency"]

        config["warp_size"] = 32		    # Number of threads in a warp
        # Max number of warps resident on a single SM
        config["max_num_warps_per_SM"] = 64
        # Max number of blocks queued on a single SM
        config["max_num_block_per_SM"] = 32
        # Max number of (software) threads in a block
        config["max_num_threads_per_block"] = 1024
        # Max number of threads queued or active on a single SM
        config["max_num_threads_per_SM"] = 2048

        # Number of memory concurrent transfer from the memory queue
        config["global_mem_return_queue"] = 128
        config["num_memory_ports"] = 1

        return config


class V100():

    def populate_config(self):
        config = {}
        config["gpu_name"] = "V100"
        # This name must be one of the classes defined in 'arch_latencies_config'
        config["gpu_arch"] = "Volta"

        mem_latencies = get_mem_latencies(
            getattr(sys.modules[__name__], config["gpu_arch"]))

        config["num_SM"] = 60           # Number of Streaming Multiprocessors
        # Number of Single Precision cores per multiprocessor
        config["num_SP_per_SM"] = 64
        # Number of Special Function usints per multiprocessor
        config["num_SF_per_SM"] = 4
        # Number of Double Precision cores per multiprocessor
        config["num_DP_per_SM"] = 32
        # Number of Load & Store units per multiprocessor
        config["num_load_store_units"] = 16
        # Number of warp schedulers available (Max number of warps that can be executed concurrently)
        config["num_warp_schedulers"] = 4
        # Number of instructions that can be issued simultaneously to a given warp
        config["num_inst_per_warp"] = 2
        config["clockspeed"] = 1246*10**6   # GPU clock speed in Hertz

        config["num_registers"] = 65536        # Number of registers available

        config["l1_cache_size"] = 128*10**3    # L1 cache size in Bytes
        config["l2_cache_size"] = 6*10**6	    # L2 cache size in Bytes
        config["global_mem_size"] = 12288*10**6  # Global memory size in Byte
        # Shared memory size in Bytes per multiprocessor
        config["shared_mem_size"] = 96*10**3

        config["l1_mem_latency"] = mem_latencies["l1_cache_access"]
        config["l2_mem_latency"] = mem_latencies["l2_cache_access"]
        config["l2_to_global_mem_latency"] = mem_latencies["global_mem_latency"] - \
            mem_latencies["l2_cache_access"]
        config["local_mem_latency"] = mem_latencies["local_mem_latency"]
        config["const_mem_latency"] = mem_latencies["constant_mem_latency"]
        config["tex_mem_latency"] = mem_latencies["texture_mem_latency"]
        config["tex_cache_latency"] = mem_latencies["texture_cache_latency"]
        config["shared_mem_latency"] = mem_latencies["shared_mem_latency"]

        config["warp_size"] = 32		    # Number of threads in a warp
        # Max number of warps resident on a single SM
        config["max_num_warps_per_SM"] = 64
        # Max number of blocks queued on a single SM
        config["max_num_block_per_SM"] = 32
        # Max number of (software) threads in a block
        config["max_num_threads_per_block"] = 1024
        # Max number of threads queued or active on a single SM
        config["max_num_threads_per_SM"] = 2048

        # Number of memory concurrent transfer from the memory queue
        config["global_mem_return_queue"] = 128
        config["num_memory_ports"] = 1

        return config
