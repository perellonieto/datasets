#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

../../scripts/explore_lmdb.py features/train/prob -csv features/train/prob.csv
../../scripts/explore_lmdb.py features/test/prob -csv features/test/prob.csv

train_accuracy=`./compute_accuracy.py features/train/prob.csv \
                ../../db/train_label.csv`
test_accuracy=`./compute_accuracy.py features/test/prob.csv \
               ../../db/test_label.csv`

printf 'Train accuracy:\t%s\nTest accuracy:\t%s\n' ${train_accuracy} \
       ${test_accuracy} > summary/performance.txt
