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

mkdir -p ${SNAPSHOTS}
mkdir -p log
mkdir -p ${LOG}
ln -fs ${LOG} log/

export GLOG_log_dir=${LOG}

SNAPSHOT=`ls ${SNAPSHOTS} | grep caffemodel | sort -V | tail -1`
caffe.bin test --model=model/train_test.prototxt \
    --weights=${SNAPSHOTS}/${SNAPSHOT}
