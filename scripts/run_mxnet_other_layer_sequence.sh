#!/bin/bash

trap "exit" EXIT


declare -a sequences=(
  1 \
  2 \
  3 \
  4 \
  5 \
  6 \
  7 \
  8 \
  9 \
)


declare -a models=(
    "BERT Trained on BookCorpus and English Wikipedia Data"
    "GPT Transformer Trained on BookCorpus Data"
    "GPT-2 Transformer Trained on WebText Data"
    "OpenFace Face Recognition Net Trained on CASIA-WebFace and FaceScrub Data"
)

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

for model in "${models[@]}"; do
    ${DIR}/../SyntheticBenchmark/BenchmarkMXNetLayerSequence.m "${model}" $1
done
