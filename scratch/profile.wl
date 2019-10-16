SetEnvironment[{"MXNET_PROFILER_AUTOSTART" -> "1", 
  "MXNET_EXEC_BULK_EXEC_INFERENCE" -> "0", 
  "MXNET_EXEC_BULK_EXEC_MAX_NODE_TRAIN" -> "0", 
  "MXNET_EXEC_BULK_EXEC_TRAIN" -> "0"(*,
  "MXNET_PROFILER_MODE"\[Rule]"1"*)}]

SetEnvironment["MXNET_OMP_MAX_THREADS" -> 8]
SetEnvironment["MXNET_VERBOSE_TUNING_INFO" -> "1"]
SetEnvironment["MXNET_ENGINE_TYPE" -> "ThreadedEnginePerDevice"]
SetEnvironment["MXNET_EXEC_VERBOSE_LOGGING" -> "1"]
SetEnvironment["MXNET_SUBGRAPH_VERBOSE" -> "1"]

net = NetModel[
   "BERT Trained on BookCorpus and English Wikipedia Data"];

net["Hello world! I am here"];
net["Hello world! I am here"];

