#!/bin/bash

function show_usage() {
    echo "Usage: $0" 1>&2;
    exit 1;
}

function show_help() {
    echo "Help usage: $0 [-h]" 1>&2;
    echo " -h|?                 Show the help"
    echo " -all|--all-features  Extracts all the features of the network"
    echo " -s|--snapshot        If defined uses the specified snapshot, if not"
    echo "                      it uses the snapshot with the higher iteration"
    echo " -f|--features        List of features separated by coma"
    exit 1;
}

. /etc/profile.d/modules.sh
module use-append ${HOME}/privatemodules
module load caffe/2015.10.28

dataset=${HOME}/datasets/cifar10
folder="${PWD##*/}"
script=${0##*/}
script=${script%.*}
LOG="${dataset}/log/${folder}/${script}"
SNAPSHOTS_FOLDER="snapshots"
DBTYPE="leveldb" # lmdb

mkdir -p features
mkdir -p log
mkdir -p ${LOG}
ln -fs ${LOG} log/

export GLOG_log_dir=${LOG}

MODELS=('train' 'test')
MINI_BATCHES=(500 100)

while [[ $# -gt 0 ]]; do
    opt="$1"
    shift;
    case "$opt" in
        "-all"|"--all-features")
            ALL_FEATURES=true
            exit 0
            ;;
        "-s"|"--snapshot")
            SNAPSHOT="$1"
            shift;
            ;;
        "-f"|"--featues")
            BLOBS_COMASEPARATED="$1"
            shift;
            ;;
        "-h"|"-?"|"--help")
            show_help
            exit 0
            ;;
        *)
            show_usage
            exit 0
            ;;
    esac
done

if [ -z ${SNAPSHOT+x} ]
then
    SNAPSHOT=`find ${SNAPSHOTS_FOLDER} | grep caffemodel | sort -V | tail -1`
fi
SNAPSHOT_NAME=$(basename "$SNAPSHOT")
SNAPSHOT_NAME="${SNAPSHOT_NAME%.caffemodel*}"

echo "Extracting features from ${SNAPSHOT_NAME}"

for i in "${!MODELS[@]}"
do
    model="${MODELS[$i]}"
    mini_batches="${MINI_BATCHES[$i]}"
    if [ $ALL_FEATURES ]
    then
        BLOBS=( $(./caffe_get_blobs.py model/${model}.prototxt) )
        BLOBS_COMASEPARATED=$(IFS=,; echo "${BLOBS[*]}")
        BLOBS_DIRS=( "${BLOBS[@]/#/features\/${SNAPSHOT_NAME}\/${model}\/}" )
        BLOBS_DIRS_COMA=$(IFS=,; echo "${BLOBS_DIRS[*]}")
    else
        IFS=',' read -r -a BLOBS <<< "$BLOBS_COMASEPARATED"
        echo ${BLOBS[*]}
        BLOBS_DIRS=( "${BLOBS[@]/#/features\/${SNAPSHOT_NAME}\/${model}\/}" )
        BLOBS_DIRS_COMA=$(IFS=,; echo "${BLOBS_DIRS[*]}")
    fi

    echo "Preparing to extract features ${BLOBS}"
    echo "Removing old blobs dirs ${BLOBS_DIRS[*]}"
    rm -rf "${BLOBS_DIRS[@]}"

    mkdir -p "features/${SNAPSHOT_NAME}/${model}"
    echo "Extracing features ${BLOBS_COMASEPARATED}"
    extract_features.bin "${SNAPSHOT}" \
        "model/${model}.prototxt" ${BLOBS_COMASEPARATED} \
        ${BLOBS_DIRS_COMA} ${mini_batches} $DBTYPE
done
