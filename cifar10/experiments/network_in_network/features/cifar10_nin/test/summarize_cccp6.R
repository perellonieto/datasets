#!/usr/bin/Rscript
library(SDMTools)
library(Rtsne)
library(MASS)

layer_name <- 'cccp6'
# The index consideres the first class = 1 = airplane (R format)
# Consequently, all the csv label files need to be increased by one
id_positive <- 4
class_names <- c("airplane","automobile","bird","cat","deer",
                 "dog","frog","horse","ship","truck")
conf_mat.class_names <- c(paste('true', class_names[id_positive]),
                          'true negative',
                          paste('false', class_names[id_positive]),
                          'false negative')
conf_mat.col <- c('red', 'blue', 'darkcyan', 'darkorange')
conf_mat.pch <- c(20, 20, 20, 20)

print('loading data')
labels <- read.csv('label.csv', sep=',', header=FALSE)
labels <- as.matrix(labels)+1 # Add one to achieve the same format as R
labels_multiclas <- labels

labels[labels == id_positive] <- -1
labels[labels != -1] <- 0
labels <- labels*-1

prob <- read.csv('prob.csv', sep=',', header=FALSE)
prob <- as.matrix(prob)

# TODO: Ask Meelis about other way to apply argmax
predictions <- apply(prob, MARGIN=1, which.max)
predictions[predictions == id_positive] <- -1
predictions[predictions != -1] <- 0
predictions <- predictions*-1

sprintf('Accuracy on class %i %s = %f', id_positive, class_names[id_positive],
        100-mean(abs(predictions-labels))*100)

# TODO: Ask Meelis? When I transform the scores of a multiclass into a
# two class one-vs-rest the threshold is not anymore 0.5 but it depends
# on the particular case. e.g. [0.6 0.1 0.3] and [0.4 0.3 0.3]
prob <- prob[,id_positive]

if (!(file.exists(paste(layer_name, '.csv', sep='')))){
    system(paste("../../../../../scripts/explore_db.py -db leveldb",
                 layer_name, '-csv',
                 paste(layer_name, '.csv', sep=''), sep=' '))
}
X <- read.csv(paste(layer_name, '.csv', sep=''), sep=',', header=FALSE)

print('computing confusion matrix')
conf_mat.true_positives <- labels==1 & predictions==1
conf_mat.true_negatives <- labels==0 & predictions==0
conf_mat.false_positives <- labels==0 & predictions==1
conf_mat.false_negatives <- labels==1 & predictions==0
conf_mat.classes <- (labels-2)*-1
conf_mat.classes[conf_mat.false_positives] <- 3
conf_mat.classes[conf_mat.false_negatives] <- 4
conf_mat.names <- conf_mat.class_names[conf_mat.classes]

#' Export to a PDF the result of runing PCA
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
        plot(X.pca, type='lines', main="PCA[1:10]")
        garbage <- dev.off()

        pdf(paste(file_prefix, '_pca_std.pdf', sep=''))
        plot(X.pca$sdev, type='lines', main="PCA",
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

#' Export to a PDF the result of runing TSNE
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

export_lda <- function(X, Y, conf_mat, layer_name){
    lda_out <- lda(as.matrix(X), as.matrix(Y))
    pdf(paste(layer_name, '_lda.pdf', sep=''))
    plot(lda_out)
    garbage <- dev.off()

    plda <- predict(object=lda_out, newdata = X)
    pdf(paste(layer_name, '_plda.pdf', sep=''))
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

print('Creating calibration plots')
target <- labels
export_calibration(prob,target,number_bins=20)
print('Creating Principal Component Analysis (PCA) plots')
X.pca <- export_pca(X, conf_mat.classes, conf_mat.col, conf_mat.pch,
                    conf_mat.class_names, layer_name)
X.pca <- export_pca(X, labels_multiclas, col=rainbow(length(class_names)),
           pch=rep(20, length(class_names)),
           class_names, paste(layer_name, '_multiclass', sep=''),
           pca_out=TRUE)
print('Creating Stochastic Neighbour Embedding (T-SNE) plots')
X.tsne <- export_tsne(X, conf_mat.classes, conf_mat.col, conf_mat.pch,
                        conf_mat.class_names, layer_name)
X.tsne <- export_tsne(X, labels_multiclas, col=rainbow(length(class_names)),
            pch=rep(20, length(class_names)),
            class_names, paste(layer_name, '_multiclass', sep=''),
            tsne_out=TRUE)
print('Creating Linear Discriminant Analysis (LDA) plots')
export_lda(X, predictions, conf_mat, layer_name)
