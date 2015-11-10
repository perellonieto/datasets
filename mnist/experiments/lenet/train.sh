#!/usr/bin/env sh

. /etc/profile.d/modules.sh
module use-append ${HOME}/privatemodules
module load caffe/2015.10.28

dataset=${HOME}/datasets/mnist
folder="${PWD##*/}"
script=${0##*/}
script=${script%.*}
LOG="${dataset}/log/${folder}/${script}"

mkdir -p snapshots
mkdir -p log
mkdir -p ${LOG}
ln -fs ${LOG} log/

caffe.bin train --solver=solver/model_solver.prototxt
