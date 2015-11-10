#!/bin/bash

grep Test caffe.bin.INFO | grep accuracy | tr -s ' ' | rev | cut -f 1 -d ' ' | rev
