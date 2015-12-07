#!/usr/bin/Rscript
require(MASS)

# Defaults
blob.name <- 'prob'
blob.width <- 1
blob.height <- 1
skip=0

args <- commandArgs(trailingOnly = TRUE)

if(!is.na(args[1])){
    blob.name <- args[1]
}

if(!is.na(args[2])){
    blob.width <- as.numeric(args[2])
    blob.height <- as.numeric(args[2])
}

sprintf('Summarizing blob %s of size (%i %i)', blob.name, blob.width,
        blob.height)
# The index consideres the first class = 1 = airplane (R format)
# Consequently, all the csv label files need to be increased by one
id_positive <- 4

print('Loading labels')
labels <- read.csv('label.csv', sep=',', header=FALSE, skip=skip)
all.labels <- as.matrix(labels)+1 # Add one to achieve the same format as R

# Creating binary labels one agains the rest
bin.labels <- all.labels
bin.labels[bin.labels == id_positive] <- -1
bin.labels[bin.labels != -1] <- 0
bin.labels <- bin.labels*-1

print('Loading posterior probabilities')
all.prob <- read.csv('prob.csv', sep=',', header=FALSE, skip=skip)
all.prob <- as.matrix(all.prob)

# TODO: Ask Meelis about other way to apply argmax
all.predictions <- apply(all.prob, MARGIN=1, which.max)
bin.predictions <- all.predictions
bin.predictions[bin.predictions == id_positive] <- -1
bin.predictions[bin.predictions != -1] <- 0
bin.predictions <- bin.predictions*-1

# TODO: Ask Meelis? When I transform the scores of a multiclass into a
# two class one-vs-rest the threshold is not anymore 0.5 but it depends
# on the particular case. e.g. [0.6 0.1 0.3] and [0.4 0.3 0.3]
bin.prob <- all.prob[,id_positive]
print('Loading hidden activations')
X <- read.csv(paste(blob.name, '.csv', sep=''), sep=',', header=FALSE,
              skip=skip)

#' Export to a PDF the result of running k-nn sum of distances from 2:k
#'
#' @param X A matrix of NxM where N is the number of samples and M the number
#'          of features
#' @param labels A vector of size N with numbers from [1,2] indicating the
#'               class of each sample
#' @param class_names A vector of size 2 with the name of each class
#' @param file_prefix A string to add as a prefix to the generated PDF file
export_two_class_knn_distances <- function(X, k, labels, class_names,
                                           file_prefix) {
    require(FNN)
    require(ggplot2)
    nndis <- knn.dist(X, k=k)
    for(i in c(1:k)){
        nndis_1 <- data.frame(distance=rowMeans(nndis[,1:i, drop=FALSE])[labels==1],
                              class=class_names[1])
        nndis_2 <- data.frame(distance=rowMeans(nndis[,1:i, drop=FALSE])[labels==2],
                              class=class_names[2])
        nndis_all <- rbind(nndis_1, nndis_2)

        ggplot(nndis_all, aes(distance, fill=class),
               environment=environment()) +
               geom_histogram(position='dodge') +
               scale_x_log10() + theme(legend.position='top') +
               ggtitle(sprintf('%s mean distance to %i-nn', file_prefix, i))

        ggsave(paste(file_prefix, '_hist_', i, 'nn_mean.pdf', sep=''))

        ggplot(nndis_all, aes(distance, fill=class),
               environment=environment()) +
               geom_density(alpha=0.3) +
               scale_x_log10() + theme(legend.position='top') +
               ggtitle(sprintf('%s mean distance to %i-nn', file_prefix, i))

        ggsave(paste(file_prefix, '_dens_', i, 'nn_mean.pdf', sep=''))
    }
}

#' Export to a PDF the result of running PCA
#'
#' @param X A matrix of NxM where N is the number of samples and M the number
#'          of features
#' @param labels A vector of size N with numbers from 1:D+1 indicating the
#'               class of each sample
#' @param col A vector of size D with the color of each class
#' @param pch A vector of size D with the shape of each class
#' @param class_names A vector of size D with the name of each class
#' @param file_prefix A string to add as a prefix to the generated PDF file
export_pca <- function(X, labels, col, pch, class_names, file_prefix,
                       pca_out=FALSE){
    if(pca_out==FALSE){
        X.pca <- prcomp(X, center=TRUE, scale.=TRUE)
        pdf(paste(file_prefix, '_pca.pdf', sep=''))
        plot(X.pca, type='l', main="PCA[1:10]")
        garbage <- dev.off()

        pdf(paste(file_prefix, '_pca_std.pdf', sep=''))
        plot(X.pca$sdev, type='l', main="PCA",
             xlab="Component", ylab="standard deviation")
        garbage <- dev.off()
    }
    pc12 <- as.matrix(X) %*% as.matrix(X.pca$rotation[,1:2])

    pdf(paste(file_prefix, '_pc1_pc2.pdf', sep=''))
    plot(pc12, col=col[labels], pch=pch[labels], main='PCA [1:2]', cex=0.5)
    legend('topright', class_names, col=col, pch=pch)
    garbage <- dev.off()
    return(X.pca)
}

#' Export to a PDF the result of running TSNE
#'
#' @param X A matrix of NxM where N is the number of samples and M the number
#'          of features.
#'          If the flag tsne_out is TRUE, it also contains the precomputed
#'          tsne function output
#' @param labels A vector of size N with numbers from 1:D+1 indicating the
#'               class of each sample
#' @param col A vector of size D with the color of each class
#' @param pch A vector of size D with the shape of each class
#' @param class_names A vector of size D with the name of each class
#' @param file_prefix A string to add as a prefix to the generated PDF file
#' @param tsne_out A logical value indicating if the first parameter X is the
#'                 input data or an already computed TSNE output
export_tsne <- function(X, labels, col, pch, class_names, file_prefix,
                        tsne_out=FALSE){
    require(Rtsne)
    if(tsne_out == FALSE){
        X.tsne <- Rtsne(as.matrix(X))
    }
    pdf(paste(file_prefix, '_tsne.pdf', sep=''))
    plot(X.tsne$Y, col=col[labels],
         pch=pch[labels], main='TSNE [1:2]',
         cex=0.5)
    legend('topright', class_names, col=col, pch=pch)
    garbage <- dev.off()
    return(X.tsne)
}

export_lda <- function(X, Y, conf_mat, blob_name){
    lda_out <- lda(as.matrix(X), as.matrix(Y))
    pdf(paste(blob_name, '_lda.pdf', sep=''))
    plot(lda_out)
    garbage <- dev.off()

    plda <- predict(object=lda_out, newdata = X)
    pdf(paste(blob_name, '_plda.pdf', sep=''))
    plot(plda)
    garbage <- dev.off()
}

export_calibration <- function(score,target,number_bins){
    pdf('calibration_sorted_scores.pdf')
    score_sorted <- sort(score, index.return=TRUE)
    plot(target[score_sorted$ix], pch=3, main='Sorted scores')
    points(score_sorted$x, pch='.', col='red')
    legend('right', c('target','score'), col=c('black', 'red'),
           pch=c('+', '.'))
    garbage <- dev.off()

    # Width binning
    breaks=c(0:number_bins)/number_bins
    h_total <- hist(score, breaks=breaks, plot=FALSE)
    h_pos <- hist(score[target==1], breaks=breaks, plot=FALSE)
    pdf('calibration_width_binning.pdf')
    plot(h_total$mids, h_pos$counts/h_total$counts, xlim=c(0,1), ylim=c(0,1),
         xlab='score', ylab='fraction of positives',
         main='Width binning curve')
    lines(h_total$mids, h_pos$counts/h_total$counts)
    lines(c(0,1), c(0,1), lty=2)
    garbage <- dev.off()

    # Size binning
    breaks=c(0:number_bins)*2000/number_bins
    h_total <- array(1:number_bins)
    h_pos <- array(1:number_bins)
    bin_mean <- array(1:number_bins)
    for(i in 1:number_bins){
        bin_mean[i] <- mean(score_sorted$x[breaks[i]:breaks[i+1]])
        h_total[i] <- breaks[i+1] - breaks[i]
        h_pos[i] <- sum(target[score_sorted$ix[c(breaks[i]:breaks[i+1])]])
    }
    pdf('calibration_size_binning.pdf')
    plot(bin_mean, h_pos/h_total, xlim=c(0,1), ylim=c(0,1),
         xlab='score', ylab='fraction of positives',
         main='Size binning curve')
    lines(bin_mean, h_pos/h_total)
    lines(c(0,1), c(0,1), lty=2)
    garbage <- dev.off()

    n_positive = sum(target)
    n_negative = length(target) - n_positive
    pdf('calibration_hist_pos_scores.pdf')
    h <- hist(score[target == 1], breaks=20, plot=FALSE)
    h$counts=h$counts/sum(h$counts)
    plot(h, main='Positive scores', ylab='Proportion')
    garbage <- dev.off()

    pdf('calibration_hist_neg_scores.pdf')
    h <- hist(score[target == 0], breaks=20, plot=FALSE)
    h$counts=h$counts/sum(h$counts)
    plot(h, main='Negative scores', ylab='Proportion')
    garbage <- dev.off()
}

#' Export to a PDF the result of running heat map
#'
#' @param X A matrix of NxM where N is the number of samples and M the number
#'          of features.
#'          If the flag tsne_out is TRUE, it also contains the precomputed
#'          tsne function output
#' @param labels A vector of size N with numbers from 1:D+1 indicating the
#'               class of each sample
#' @param col A vector of size D with the color of each class
#' @param pch A vector of size D with the shape of each class
#' @param class_names A vector of size D with the name of each class
#' @param file_prefix A string to add as a prefix to the generated PDF file
#' @param tsne_out A logical value indicating if the first parameter X is the
#'                 input data or an already computed TSNE output
export_heatmap <- function(X, height, width, blob_name){
    require(pheatmap)
    require(reshape2)
    a=colSums(X)
    df=data.frame(a=a,ro=1:height,co=rep(1:width,each=height))
    #dfx=dcast(df,co+ro~.,fun.aggregate=mean,value.var="a")
    #dfy=dcast(dfx,co+ro~.)
    # TODO: look if the ro~co is the correct order
    # Caffe stores the data in the format:
    # number N x channel K x height H x width W
    # where with the index (n,k,h,w) is physically stored as
    #  ((n * K + k) * H + h) * W + w
    dfy=dcast(df,ro~co,fun.aggregate=mean,value.var="a")
    pdf(paste(blob_name, '_heatmap.pdf', sep=''))
    pheatmap(as.matrix(dfy[,2:(width+1)]), cluster_cols=F,cluster_rows=F)
    garbage <- dev.off()
}

# Plot the heat map
if(blob.height > 1){
    print('Creating heatmap plots')
    export_heatmap(X, blob.height, blob.width, blob.name)
}

# Plot class names, color and shape
all.class_names <- c("airplane","automobile","bird","cat","deer",
                 "dog","frog","horse","ship","truck")
all.col <- rainbow(length(all.class_names))
all.pch <- rep(c(15,16,17,18), length(all.class_names))

# Plot class names, color and shape
conf_mat.class_names <- c(paste('true', all.class_names[id_positive]),
                          'true negative',
                          paste('false', all.class_names[id_positive]),
                          'false negative')
conf_mat.col <- c('red', 'blue', 'darkcyan', 'darkorange')
conf_mat.pch <- c(15, 16, 17, 18)
print('computing confusion matrix')
conf_mat.true_positives <- bin.labels==1 & bin.predictions==1
conf_mat.true_negatives <- bin.labels==0 & bin.predictions==0
conf_mat.false_positives <- bin.labels==0 & bin.predictions==1
conf_mat.false_negatives <- bin.labels==1 & bin.predictions==0
conf_mat.classes <- (bin.labels-2)*-1
conf_mat.classes[conf_mat.false_positives] <- 3
conf_mat.classes[conf_mat.false_negatives] <- 4
conf_mat.names <- conf_mat.class_names[conf_mat.classes]

# Plot class names, color and shape
mis.class_names <- c('correct', 'mis')
mis.col <- c('red', 'blue')
mis.pch <- c(15, 16)
mis.labels <- abs(all.predictions-all.labels)
mis.labels <- ifelse(mis.labels==0, 0, mis.labels/mis.labels)+1

sprintf('Accuracy on all the classes is = %f',
        100-mean(all.predictions != all.labels)*100)
sprintf('Accuracy on class %i %s = %f', id_positive, all.class_names[id_positive],
        100-mean(abs(bin.predictions-bin.labels))*100)

print('Creating misclassified k-nearest neighbour distances')
export_two_class_knn_distances(X, 10, mis.labels, mis.class_names, blob.name)
# print('Creating calibration plots')
# target <- bin.labels
# export_calibration(bin.prob,target,number_bins=10)
# print('Creating Principal Component Analysis (PCA) plots')
# X.pca <- export_pca(X, conf_mat.classes, conf_mat.col, conf_mat.pch,
#                     conf_mat.class_names, blob.name)
# print('Ploting PCA with all the classes')
# X.pca <- export_pca(X, all.labels, col=all.col, pch=all.pch, all.class_names,
#                     paste(blob.name, '_multiclass', sep=''), pca_out=TRUE)
# print('Ploting PCA with correct vs mis')
# X.pca <- export_pca(X, mis.labels, col=mis.col, pch=mis.pch, mis.class_names,
#                     paste(blob.name, '_mis', sep=''), pca_out=TRUE)
# print('Creating Stochastic Neighbour Embedding (T-SNE) plots')
# X.tsne <- export_tsne(X, conf_mat.classes, conf_mat.col, conf_mat.pch,
#                         conf_mat.class_names, blob.name)
# print('Ploting T-SNE with all the classes')
# X.tsne <- export_tsne(X, all.labels, col=all.col, pch=all.pch, all.class_names,
#                       paste(blob.name, '_multiclass', sep=''), tsne_out=TRUE)
# print('Ploting T-SNE with correct vs mis')
# X.tsne <- export_tsne(X, mis.labels, col=mis.col, pch=mis.pch, mis.class_names,
#                       paste(blob.name, '_mis', sep=''), tsne_out=TRUE)
# print('Creating Linear Discriminant Analysis (LDA) plots')
# export_lda(X, bin.predictions, conf_mat, blob.name)