#!/bin/bash

CLASS_NAMES=('airplane' 'automobile' 'bird' 'cat' 'deer'
             'dog' 'frog' 'horse' 'ship' 'truck')

OUTPUT_FILES=('all_calibration_sorted_scores.pdf' \
              'all_calibration_width_binning.pdf' \
              'all_calibration_size_binning.pdf' \
              'all_calibration_hist_pos_scores.pdf' \
              'all_calibration_platt.pdf')

PREFIX_FILES=('calibration_' \
              'calibration_' \
              'calibration_' \
              'calibration_' \
              'calibration_')

SUFIX_FILES=('_sorted_scores.pdf' \
             '_width_binning.pdf' \
             '_size_binning.pdf' \
             '_hist_pos_scores.pdf' \
             '_platt.pdf')

for i in "${!OUTPUT_FILES[@]}"; do
    printf "%i : %s\n" $i ${OUTPUT_FILES[$i]}
    FILES=( "${CLASS_NAMES[@]/%/${SUFIX_FILES[$i]}}" )
    FILES=( "${FILES[@]/#/${PREFIX_FILES[$i]}}" )
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite \
        -sOutputFile=${OUTPUT_FILES[$i]} ${FILES[@]}
done
