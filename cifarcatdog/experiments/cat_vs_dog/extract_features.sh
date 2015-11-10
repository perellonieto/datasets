#!/bin/bash

. /etc/profile.d/modules.sh
module use-append ${HOME}/privatemodules
module load caffe/2015.10.28

dataset=${HOME}/datasets/cifarcatdog
folder="${PWD##*/}"
script=${0##*/}
script=${script%.*}
LOG="${dataset}/log/${folder}/${script}"
SNAPSHOTS="snapshots"

mkdir -p features
mkdir -p log
mkdir -p ${LOG}
ln -fs ${LOG} log/

export GLOG_log_dir=${LOG}

SNAPSHOT=`ls ${SNAPSHOTS} | grep caffemodel | sort -V | tail -1`

MODELS=('train' 'test')
MINI_BATCHES=(100 20)

for i in "${!MODELS[@]}"
do
    model="${MODELS[$i]}"
    mini_batches="${MINI_BATCHES[$i]}"
    BLOBS=( $(./caffe_get_blobs.py model/${model}.prototxt) )
    BLOBS_COMASEPARATED=$(IFS=,; echo "${BLOBS[*]}")
    BLOBS_DIRS=( "${BLOBS[@]/#/features\/${model}\/}" )
    BLOBS_DIRS_COMA=$(IFS=,; echo "${BLOBS_DIRS[*]}")

    echo "Removing old blobs dirs"
    rm -rf "${BLOBS_DIRS[@]}"

    mkdir -p features/${model}
    extract_features.bin "${SNAPSHOTS}/${SNAPSHOT}" \
        "model/${model}.prototxt" ${BLOBS_COMASEPARATED} \
        ${BLOBS_DIRS_COMA} ${mini_batches} lmdb
done
