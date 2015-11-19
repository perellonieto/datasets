#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

if [ $# -ge 1 ]
then
    features=${1%/}
    snapshot_name=$(basename "$features")
    snapshot_name="${snapshot_name%.*.*}"
else
    echo "You need to specify the features folder"
    exit
fi

../../scripts/explore_db.py "${features}/train/prob" -csv \
    "${features}/train/prob.csv" -db leveldb
../../scripts/explore_db.py "${features}/test/prob" -csv \
    "${features}/test/prob.csv" -db leveldb

train_accuracy=`./compute_accuracy.py "${features}/train/prob.csv" \
                db/train_label.csv`
test_accuracy=`./compute_accuracy.py "${features}/test/prob.csv" \
               db/test_label.csv`

mkdir -p "summary/${snapshot_name}"

printf 'Train accuracy:\t%s\nTest accuracy:\t%s\n' ${train_accuracy} \
       ${test_accuracy} > "summary/${snapshot_name}/performance.txt"
