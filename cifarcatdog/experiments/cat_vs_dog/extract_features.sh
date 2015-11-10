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

BLOBS=`./caffe_get_blobs.py model/train_test.prototxt`
for blob in $BLOBS; do
    rm -rf "features/${blob}"
    extract_features.bin "${SNAPSHOTS}/${SNAPSHOT}" \
        "model/train_test.prototxt" $blob "features/${blob}" 10 lmdb
done
