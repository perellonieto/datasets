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

BLOBS=`./caffe_get_blobs.py model/test.prototxt`
for blob in $BLOBS; do
    rm -rf "features/${blob}"
done
BLOBS_LIST=`./caffe_get_blobs.py model/test.prototxt | tr '\n' ','`
echo $BLOBS_LIST

mkdir -p features/test
extract_features.bin "${SNAPSHOTS}/${SNAPSHOT}" \
    "model/test.prototxt" ${BLOBS_LIST} "features/test/${BLOBS_LIST}" 20 lmdb
