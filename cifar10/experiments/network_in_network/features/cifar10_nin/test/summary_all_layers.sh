#!/bin/bash

BLOBS=('cccp3' 'cccp4' 'pool2' 'conv3' 'cccp5' 'cccp6' 'pool3' 'prob')

OUTPUT_FILES=('all_mis_pc1_pc2.pdf' \
              'all_bin_tsne.pdf' \
              'all_mis_tsne.pdf' \
              'all_multiclass_tsne.pdf' \
              'all_hist_50nn_mean.pdf' \
              'all_dens_50nn_mean.pdf' \
              'all_hist_100nn_mean.pdf' \
              'all_dens_100nn_mean.pdf')

SUFIX_FILES=('_mis_pc1_pc2.pdf' \
             '_mis_pc1_pc2.pdf' \
             '_tsne.pdf' \
             '_mis_tsne.pdf' \
             '_multiclass_tsne.pdf' \
             '_hist_50nn_mean.pdf' \
             '_hist_100nn_mean.pdf' \
             '_dens_50nn_mean.pdf' \
             '_dens_100nn_mean.pdf')

for i in "${!OUTPUT_FILES[@]}"; do
    printf "%i : %s\n" $i ${OUTPUT_FILES[$i]}
    FILES=( "${BLOBS[@]/%/${SUFIX_FILES[$i]}}" )
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite \
        -sOutputFile=${OUTPUT_FILES[$i]} ${FILES[@]}
done
