#!/usr/bin/env sh
# This script converts the cifar data into leveldb format.

. /etc/profile.d/modules.sh
module use-append ${HOME}/privatemodules
module load caffe/2015.10.28

EXAMPLE=../db
DATA=../downloads

BACKEND="lmdb"

echo "Creating ${BACKEND}..."
mkdir -p ${EXAMPLE}

rm -rf $EXAMPLE/cifarcatdog_train_$BACKEND
rm -rf $EXAMPLE/cifarcatdog_test_$BACKEND

convert_cifar_cat_dog_data.bin $DATA $EXAMPLE $BACKEND

echo "Computing image mean..."

compute_image_mean.bin -backend=${BACKEND} \
  $EXAMPLE/cifarcatdog_train_${BACKEND} $EXAMPLE/mean.binaryproto

echo "Done."
