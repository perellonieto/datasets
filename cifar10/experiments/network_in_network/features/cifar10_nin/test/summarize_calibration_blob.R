#!/usr/bin/Rscript
require(MASS)

blob.name <- 'pool3'
train_instances <- 5000
skip <- 0
CLASS_NAMES <- c("airplane","automobile","bird","cat","deer",
                 "dog","frog","horse","ship","truck")
BINS <- c(2,4,8,16,32,64,128,256, 512, 1024, 248)

brier_score <- function(score, target){
    return(sum((score-target)^2)/length(target))
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
         pch=pch[labels], cex=0.5,
         main=sprintf("layer[%s] T-SNE", file_prefix))
    legend('topright', class_names, col=col, pch=pch)
    garbage <- dev.off()
    return(X.tsne)
}

export_lda <- function(X, Y, conf_mat, blob_name){
    lda_out <- lda(as.matrix(X), as.matrix(Y))
    pdf(paste(blob_name, '_lda.pdf', sep=''))
    plot(lda_out,
         main=sprintf("layer[%s] LDA", file_prefix))
    garbage <- dev.off()

    plda <- predict(object=lda_out, newdata = X)
    pdf(paste(blob_name, '_plda.pdf', sep=''))
    plot(plda,
         main=sprintf("layer[%s] LDA projection", file_prefix))
    garbage <- dev.off()
}

platt_toy_example <- function(){
    prob_splits = 500
    breaks=seq(0,1,length.out=prob_splits)
    score = rep(breaks, prob_splits)
    y = as.vector(lower.tri(matrix(ncol=prob_splits,
                                       nrow=prob_splits),
                                diag=FALSE)+0)
    df <- data.frame(y=y, score=score)

    glm_out <- glm(y~score, data=df, family=binomial(logit))
    pdf(sprintf('calibration_example_%i_platt.pdf',prob_splits))
    plot(y~score, data=df, pch=3, main='Platt scaling already calibrated model')
    his.pos = hist(score[y==1],breaks=breaks,plot=FALSE)
    his.total = hist(score,breaks=breaks,plot=FALSE)
    barplot(his.pos$counts/his.total$counts,width=1/(length(breaks)-1),
            space=0,col='white',add=TRUE)
    points(df$score, glm_out$fitted)
    predictions <- predict(glm_out, newdata=data.frame(score=seq(0,1,0.05)),
                           type='response')
    lines(seq(0,1,0.05), predictions, col='red')
    dev.off()
}

platt_toy_example_2 <- function(){
    prob_splits = c(16,32,64,128,256, 300, 400, 500, 1000, 2000)
    predict_size <- 20
    pdf(sprintf('calibration_example_comparison_platt.pdf',prob_splits))
    plot(0,0,xlim=c(0,1),ylim=c(0,1),type = "n")
    for(i in seq_along(prob_splits)){
        breaks=seq(0,1,length.out=prob_splits[i])
        score = rep(breaks, prob_splits[i])
        y = as.vector(lower.tri(matrix(ncol=prob_splits[i],
                                           nrow=prob_splits[i]),
                                    diag=FALSE)+0)
        df <- data.frame(y=y, score=score)

        glm_out <- glm(y~score, data=df, family=binomial(logit))
        predictions <- predict(glm_out,
                               newdata=data.frame(score=seq(0,1,length.out=predict_size)),
                               type='response')
        lines(seq(0,1,length.out=predict_size), predictions, type='l')
    }
    dev.off()
}

logistic_function <- function(){
    k <- c(0.1,0.5,2,4,8,16,20,32,64,128)
    predict_size <- 100
    require(dichromat)
    color <- colorRampPalette(colorschemes$BrowntoBlue.10,
                              space='Lab')(length(k))
    pdf('logistic_function_values.pdf')
    plot(0,0,xlim=c(0,1),ylim=c(0,1), type = "n")
    for(i in seq_along(k)){
        x <- seq(0,1,length.out=predict_size)
        y <- 1/(1+exp(-k[i]*(x-0.5)))
        lines(x, y, type='l', col=color[i], lwd=2)
    }
    legend('topleft', col=color, legend=k, lty=1)
    dev.off()
}


export_cal_sorted_scores <- function(score, target, class_name){
    pdf(sprintf('calibration_%s_sorted_scores.pdf', class_name))
    score_sorted <- sort(score, index.return=TRUE)
    plot(target[score_sorted$ix], pch=3, main=sprintf('%s Sorted scores',
                                                      class_name))
    points(score_sorted$x, pch='.', col='red')
    legend('right', c('target','score'), col=c('black', 'red'),
           pch=c('+', '.'))
    garbage <- dev.off()
}

export_cal_platt_scaling <- function(score,target,class_name){
    # Platts scaling
    df <- data.frame(y=target, score=score)
    glm_out <- glm(y~score, data=df, family=binomial(logit))
    pdf(sprintf('calibration_%s_platt.pdf', class_name))
    plot(y~score, data=df, pch=3, main=sprintf('%s Platt scaling', class_name))
    points(df$score, glm_out$fitted)
    predictions <- predict(glm_out, newdata=data.frame(score=seq(0,1,0.05)),
                           type='response')
    lines(seq(0,1,0.05), predictions, col='red')
    garbage <- dev.off()

    return(brier_score(glm_out$fitted, target))
}

export_cal_width_binning <- function(score,target,number_bins,
                                            class_name){
    # Width binning
    breaks=c(0:number_bins)/number_bins
    h_total <- hist(score, breaks=breaks, plot=FALSE)
    h_pos <- hist(score[target==1], breaks=breaks, plot=FALSE)
    proportions <- h_pos$counts/h_total$counts
    pdf(sprintf('calibration_%i_%s_width_binning.pdf', number_bins, class_name))
    plot(h_total$mids, proportions, xlim=c(0,1), ylim=c(0,1), col='red',
         xlab='score', ylab='fraction of positives',
         main=sprintf('%s Width binning curve (1/%i = %f)', class_name,
                      number_bins, 1/number_bins))
    lines(h_total$mids, h_pos$counts/h_total$counts, col='red')
    lines(c(0,1), c(0,1), lty=2, col='blue')
    barplot(proportions, width=breaks[-1] - breaks[-length(breaks)], space=0,
            add=T, col=rgb(0,0,0,0), border=rgb(0,0,0,0.3))
    garbage <- dev.off()

    new_scores <- proportions[findInterval(score,breaks, all.inside=TRUE)]
    return(brier_score(new_scores,target))
}


export_cal_size_binning <- function(score,target,number_bins,
                                            class_name){
    # Size binning
    score_sorted <- sort(score, index.return=TRUE)
    index_breaks=ceiling(c(0:number_bins)*length(score)/number_bins)
    breaks <- score_sorted$x[c(1,index_breaks)]
    h_total <- array(1:number_bins)
    h_pos <- array(1:number_bins)
    bin_mean <- array(1:number_bins)
    for(i in 1:number_bins){
        bin_mean[i] <- mean(score_sorted$x[(index_breaks[i]+1):index_breaks[i+1]])
        h_total[i] <- index_breaks[i+1] - index_breaks[i]
        h_pos[i] <- sum(target[score_sorted$ix[c((index_breaks[i]+1):index_breaks[i+1])]])
    }
    pdf(sprintf('calibration_%i_%s_size_binning.pdf', number_bins, class_name))
    plot(bin_mean, h_pos/h_total, xlim=c(0,1), ylim=c(0,1), col='red',
         xlab='score', ylab='fraction of positives',
         main=sprintf('%s Size binning curve (%i/%i = %i)', class_name,
                      length(score), number_bins, index_breaks[2]))
    lines(bin_mean, h_pos/h_total, col='red')
    lines(c(0,1), c(0,1), lty=2, col='blue')
    barplot(h_pos/h_total, width=breaks[-1] - breaks[-length(breaks)], space=0,
            add=T, col=rgb(0,0,0,0), border=rgb(0,0,0,0.3))
    garbage <- dev.off()

    intervals <- findInterval(score,breaks, all.inside=TRUE)
    new_scores <- bin_mean[intervals]
    return(brier_score(new_scores, target))
}

export_cal_hist_pos_scores <- function(score,target,number_bins,
                                               class_name){
    n_positive = sum(target)
    n_negative = length(target) - n_positive
    pdf(sprintf('calibration_%s_%i_hist_pos_scores.pdf', class_name,
                number_bins))
    h <- hist(score[target == 1], breaks=number_bins, plot=FALSE)
    h$counts=h$counts/sum(h$counts)
    plot(h, main=sprintf('%s Positive scores', class_name), ylab='Proportion')
    garbage <- dev.off()
}

export_cal <- function(score,target,number_bins, class_name){
    #pdf(sprintf('calibration_%s_hist_neg_scores.pdf', class_name))
    #h <- hist(score[target == 0], breaks=20, plot=FALSE)
    #h$counts=h$counts/sum(h$counts)
    #plot(h, main=sprintf('%s Negative scores', class_name), ylab='Proportion')
    #garbage <- dev.off()
}



print('Loading labels')
labels <- read.csv('label.csv', sep=',', header=FALSE, skip=skip)
all.labels <- as.matrix(labels)+1 # Add one to achieve the same format as R

print('Loading posterior probabilities')
all.prob <- read.csv('prob.csv', sep=',', header=FALSE, skip=skip)
all.prob <- as.matrix(all.prob)

print('Loading hidden activations')
X <- read.csv(paste(blob.name, '.csv', sep=''), sep=',', header=FALSE,
              skip=skip)

# Only keep the specified number of instances for training
all.labels <- all.labels[1:train_instances,]
all.prob <- all.prob[1:train_instances,]
X <- X[1:train_instances,]

col_names <- c('original', 'platt', paste('width', 1/BINS),
               paste('size', ceiling(length(all.labels)/BINS)))
error_bs <- matrix(nrow=length(CLASS_NAMES), ncol=length(col_names))
for(i in seq_along(CLASS_NAMES)){
    print(sprintf('Creating plots for class[%i] = %s', i, CLASS_NAMES[i]))
    # The index consideres the first class = 1 = airplane (R format)
    # Consequently, all the csv label files need to be increased by one
    id_positive <- i

    # Creating binary labels one agains the rest
    bin.labels <- all.labels
    bin.labels[bin.labels == id_positive] <- -1
    bin.labels[bin.labels != -1] <- 0
    bin.labels <- bin.labels*-1


    all.predictions <- apply(all.prob, MARGIN=1, which.max)
    bin.predictions <- all.predictions
    bin.predictions[bin.predictions == id_positive] <- -1
    bin.predictions[bin.predictions != -1] <- 0
    bin.predictions <- bin.predictions*-1

    bin.prob <- all.prob[,id_positive]


    # Plot class names, color and shape
    all.class_names <- CLASS_NAMES
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

    print('Creating calibration plots')
    target <- bin.labels

    error_bs[id_positive,1] <- brier_score(bin.prob, target)
    print(sprintf("Brier score for class %s = %f", CLASS_NAMES[id_positive],
                  error_bs[id_positive]))
    score <- bin.prob
    class_name <- CLASS_NAMES[id_positive]
    error_bs[id_positive,2] <- export_cal_platt_scaling(score,target,class_name)
    export_cal_sorted_scores(score,target,class_name)
    for(j in seq_along(BINS)){
        number_bins <- BINS[j]
        error_bs[id_positive, 2+j] <- export_cal_width_binning(
                                          score,target,number_bins,class_name)

        error_bs[id_positive, 2+length(BINS)+j] <-
                 export_cal_size_binning(score,target,number_bins,class_name)

        export_cal_hist_pos_scores(score,target,number_bins,class_name)
   }
}
require(pheatmap)
pdf('calibration_brier_scores.pdf')
pheatmap(error_bs, cluster_cols=F, cluster_rows=F, display_numbers=T,
         number_format="%.2e", labels_row=CLASS_NAMES, labels_col=col_names)
dev.off()
pdf('calibration_barplot_scores.pdf')
barplot(error_bs, names.arg=col_names, angle=45, legend.text=CLASS_NAMES,
        las=2, args.legend=c(ncol=2, cex=0.9))
dev.off()
