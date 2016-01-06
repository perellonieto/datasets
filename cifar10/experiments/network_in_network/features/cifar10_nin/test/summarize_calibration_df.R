#!/usr/bin/Rscript
require(MASS)
require(bisoreg) # fits.isoreg
require(pheatmap) # heat map
require(reshape2) # Reshape a data frame
require(ggplot2)

plot_folder <- 'calibration/'
CROSS_VAL <- TRUE
train_instances <- 5000 #Number of instances to use for training (inc. val.)
valid_instances <- 2500 #Number of instances to use for validation (from tra.)
skip <- 0               #Number of instances to skip from the CSV files
CLASS_NAMES <- c("airplane","automobile","bird","cat","deer",
                 "dog","frog","horse","ship","truck")
width_bins<-c(2,3,4,5,6,7,8,9,10,11,16,32,40,50,64,128,256,512,768,1024,1536,
              2048)
size_bins <- c(15,20,30,50,60,61,62,63,64,65,67,68,69,70,80,90,100,110,120,
               150,160,180,200,230,250,280,310)

brier_score <- function(score, target){
    return(sum((score-target)^2)/length(target))
}

# ========================= #
# EXPORT TO PDF FUNCTIONS   #
# ========================= #
export_cal_sorted_scores <- function(score, target, class_name, folder=''){
    pdf(sprintf('%scalibration_%s_sorted_scores.pdf', plot_folder, class_name))
    score_sorted <- sort(score, index.return=TRUE)
    plot(target[score_sorted$ix], pch=3, main=sprintf('%s Sorted scores',
                                                      class_name))
    points(score_sorted$x, pch='.', col='red')
    legend('right', c('target','score'), col=c('black', 'red'),
           pch=c('+', '.'))
    garbage <- dev.off()
}

export_cal_isoreg <- function(score,target,class_name, folder=''){
    # Isotonic regression
    isoreg_out <- isoreg(score, target)
    pdf(sprintf('%scalibration_%s_isoreg.pdf', plot_folder, class_name))
    plot(isoreg_out, main=sprintf('%s Isotonic regression',
                                               class_name))
    garbage <- dev.off()

    return(isoreg_out)
}


export_cal_platt_scaling <- function(glm_out, class_name, folder=''){
    pdf(sprintf('%scalibration_%s_platt.pdf', plot_folder, class_name))
    plot(glm_out$data$score, glm_out$data$y, pch=3, main=sprintf('%s Platt scaling', class_name))
    points(glm_out$data$score, glm_out$fitted)
    predictions <- predict(glm_out, newdata=data.frame(score=seq(0,1,0.05)),
                           type='response')
    lines(seq(0,1,0.05), predictions, col='red')
    garbage <- dev.off()
}

export_cal_width_binning <- function(cal,class_name, folder=''){
    pdf(sprintf('%scalibration_%i_%s_width_binning.pdf', plot_folder, cal$n_bins, class_name))
    plot(cal$mids, cal$score, xlim=c(0,1), ylim=c(0,1), col='red',
         xlab='score', ylab='fraction of positives',
         main=sprintf('%s Width binning curve (1/%i = %f)', class_name,
                      cal$n_bins, 1/cal$n_bins))
    lines(cal$mids, cal$score, col='red')
    lines(c(0,1), c(0,1), lty=2, col='blue')
    barplot(cal$score, width=cal$breaks[-1] - cal$breaks[-length(cal$breaks)],
            space=0, add=T, col=rgb(0,0,0,0), border=rgb(0,0,0,0.3))
    garbage <- dev.off()
}

export_cal_size_binning <- function(cal,class_name, folder=''){
    pdf(sprintf('%scalibration_%i_%s_size_binning.pdf', plot_folder, cal$number_bins, class_name))
    plot(cal$mids, cal$score, xlim=c(0,1), ylim=c(0,1), col='red',
         xlab='score', ylab='fraction of positives',
         main=sprintf('%s Size binning curve (%i/%i = %i)', class_name,
                      length(cal$score), cal$number_bins,
                      (ceiling(length(cal$score)/cal$number_bins))))
    lines(cal$mids, cal$score, col='red')
    lines(c(0,1), c(0,1), lty=2, col='blue')
    barplot(cal$score, width=cal$breaks[-1] - cal$breaks[-length(cal$breaks)], space=0,
            add=T, col=rgb(0,0,0,0), border=rgb(0,0,0,0.3))
    garbage <- dev.off()
}

export_cal_hist_pos_scores <- function(score,target,number_bins,
                                       class_name, folder=''){
    n_positive = sum(target)
    n_negative = length(target) - n_positive
    pdf(sprintf('%scalibration_%s_%i_hist_pos_scores.pdf', plot_folder, class_name,
                number_bins))
    h <- hist(score[target == 1], breaks=number_bins, plot=FALSE)
    h$counts=h$counts/sum(h$counts)
    plot(h, main=sprintf('%s Positive scores', class_name), ylab='Proportion',
         ylim=c(0,1), panel.first = grid(lty = 2, lwd = 2))
    garbage <- dev.off()
}

# ========================= #
# AUXILIAR FUNCTIONS        #
# ========================= #
#' Apply pooling to different regions of the vector x
#'
#' @param x a vector
#' @param ncol size of the window where to apply pooling
#' @param func function to use for the pooling
#' Example 1:
#'  apply_pooling(1:8, ncol=2, func=max)
#'   [2 4 6 8]
#'
#' Example 2:
#'  t(apply(matrix(1:8, nrow=2, byrow=T), MARGIN=1, apply_pooling, ncol=2)
#'  [[1.5 3.5] [5.5 7.5]]
apply_pooling <- function(x, ncol=1, func=mean){
    aux <- matrix(x, ncol=ncol, byrow=T)
    return(apply(aux, MARGIN=1, func))
}

# ========================= #
# CALIBRATION FUNCTIONS     #
# ========================= #
calibration_width_binning <- function(score,target,number_bins){
    # Width binning
    breaks <- c(0:number_bins)/number_bins
    h_total <- hist(score, breaks=breaks, plot=FALSE)
    h_pos <- hist(score[target==1], breaks=breaks, plot=FALSE)
    score <- h_pos$counts/(h_total$counts+1)

    return(list(breaks=breaks, mids=h_total$mids, score=score,
                n_bins=number_bins))
}

calibration_size_binning <- function(score,target,number_bins){
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

    return(list(breaks=breaks, mids=bin_mean, score=h_pos/(h_total+1),
                n_bins=number_bins))
}

# ========================= #
# MAIN CODE                 #
# ========================= #
print('Loading labels')
labels <- read.csv('label.csv', sep=',', header=FALSE, skip=skip)
all <- list()
all$labels <- as.matrix(labels)+1 # Add one to get the same format as R

print('Loading posterior probabilities')
all$prob <- read.csv('prob.csv', sep=',', header=FALSE, skip=skip)
all$prob <- as.matrix(all$prob)

print('Loading pool3 layer')
all$pool3 <- read.csv('pool3.csv', sep=',', header=FALSE, skip=skip)
all$pool3 <- as.matrix(all$pool3)

if(file.exists('cccp5_pool_mean.csv')){
    print('Loading cccp5 pooling mean')
    load('cccp5_pool_mean.csv')
    all$cccp5_pool_mean <- cccp5_pool_mean
} else{
    print('Loading cccp5 layer')
    all$cccp5 <- read.csv('cccp5.csv', sep=',', header=FALSE, skip=skip)
    all$cccp5<- as.matrix(all$cccp5)

    height = 8
    width = 8
    all$cccp5_pool_mean <- t(apply(all$cccp5, MARGIN=1, apply_pooling,
                             ncol=height*width, func=mean))
    cccp5_pool_mean <- cccp5_pool_mean
    save(cccp5_pool_mean, file='cccp5_pool_mean.csv')
}

# FIXME: this is only a test:
all$pool5 <- all$cccp5_pool_mean
# The test finishes here

# Only keep the specified number of instances for training
all$labels <- all$labels[1:train_instances,]
all$prob <- all$prob[1:train_instances,]
num_samples <- length(all$labels)

# Computing the prediction for each instance
all$predictions <- apply(all$prob, MARGIN=1, which.max)

sprintf('Accuracy on all the classes is = %f',
        mean(all$predictions == all$labels))

# Creating the cross-validation partitions
if(CROSS_VAL){
    n_folds <- floor(train_instances/valid_instances)
    all_ids <- 1:train_instances
    folds_val <- matrix(all_ids, nrow=n_folds,byrow=TRUE)
    folds_tra <- matrix(,nrow=n_folds,ncol=train_instances-valid_instances)
    for(i in 1:n_folds){
        folds_tra[i,] <- all_ids[-(folds_val[i,])]
    }
    folds <- list(train=folds_tra, valid=folds_val)
} else{
    print('Not implemented without cross-validation')
    quit()
}

# Results of all methods except Binning
# FIXME: I need to create one empty row... with nrow=0 it does not work
# If I set the names beforehand, I need to create this empty matrix with
# one row and the desired number of columns
# tra_err_oth_df <- data.frame(matrix(NA, nrow=1, ncol=4))
# names(tra_err_oth_df) <- c("class","method", "fold", "error")
# Otherwise, I can set the names at the end of the script
tra_err_oth_df <- data.frame()
val_err_oth_df <- data.frame()

# Results of Size Binning
tra_err_siz_df <- data.frame()
val_err_siz_df <- data.frame()

# Results of Width Binning
tra_err_wid_df <- data.frame()
val_err_wid_df <- data.frame()

for(i in seq_along(CLASS_NAMES)){
    print(sprintf('Creating plots for class[%i] = %s', i, CLASS_NAMES[i]))
    # The actual positive class
    id_pos <- i
    class_name <- CLASS_NAMES[id_pos]

    # Creating binary labels one against the rest
    bin <- list()
    bin$labels <- ifelse(all$labels==id_pos, 1, 0)

    # Creating binary predictions one against the rest
    bin$predictions <- ifelse(all$predictions==id_pos, 1, 0)

    # Predicted probability for the current class
    bin$prob <- all$prob[,id_pos]
    score <- bin$prob
    scores <- all$prob

    print(sprintf('Accuracy on class %i %s = %f', id_pos,
                  all$class_names[id_pos],
                  mean(bin$predictions==bin$labels)))

    print('Creating calibration plots')
    target <- bin$labels

    # Export the sorted scores for this class
    export_cal_sorted_scores(score,target,class_name)

    # Export histogram of positive scores
    for(j in seq_along(width_bins)){
        number_bins <- width_bins[j]
        export_cal_hist_pos_scores(score,target,number_bins,class_name,
                                   plot_folder)
    }

    for(fold_id in 1:n_folds){
        method <- 'Original'
        # Training and Validation partitions
        train_idx <- folds$train[fold_id,]
        valid_idx <- folds$valid[fold_id,]

        # Original error on Training fold
        error <- brier_score(bin$prob[train_idx], target[train_idx])
        new_row <- c(class_name, method, fold_id, error)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        tra_err_oth_df <- rbind(tra_err_oth_df, new_row)

        # Original error on Validation fold
        error <- brier_score(bin$prob[valid_idx], target[valid_idx])
        new_row <- c(class_name, method, fold_id, error)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        val_err_oth_df <- rbind(val_err_oth_df, new_row)

        print(tail(tra_err_oth_df,1))
        print(tail(val_err_oth_df,1))

        # ========================= #
        # PLATT SCALING in one score#
        # ========================= #
        method <- 'Platt'
        tra_fold_scores <- score[train_idx]
        tra_fold_target <- target[train_idx]
        val_fold_scores <- score[valid_idx]
        val_fold_target <- target[valid_idx]

        df <- data.frame(y=tra_fold_target, score=tra_fold_scores)
        glm_out <- glm(y ~ ., data=df, family=binomial(logit))

        export_cal_platt_scaling(glm_out,class_name,plot_folder)

        # Training Error
        predictions <- predict(glm_out, newdata=data.frame(
                          score=tra_fold_scores), type='response')
        error <- brier_score(predictions, tra_fold_target)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        tra_err_oth_df <- rbind(tra_err_oth_df, new_row)

        # Validation Error
        predictions <- predict(glm_out, newdata=data.frame(
                          score=val_fold_scores), type='response')
        error <- brier_score(predictions, val_fold_target)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        val_err_oth_df <- rbind(val_err_oth_df, new_row)

        print(tail(tra_err_oth_df,1))
        print(tail(val_err_oth_df,1))

        # ========================= #
        # MULTICLASS PLATT SCALING  #
        # ========================= #
        method <- 'Multi-Platt'
        tra_fold_scores <- scores[train_idx,]
        tra_fold_target <- target[train_idx]
        val_fold_scores <- scores[valid_idx,]
        val_fold_target <- target[valid_idx]

        df <- data.frame(y=tra_fold_target, score=tra_fold_scores)
        glm_out <- glm(y ~ ., data=df, family=binomial(logit))

        # Training Error
        predictions <- predict(glm_out, newdata=data.frame(
                          score=tra_fold_scores), type='response')
        error <- brier_score(predictions, tra_fold_target)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        tra_err_oth_df <- rbind(tra_err_oth_df, new_row)

        # Validation Error
        predictions <- predict(glm_out, newdata=data.frame(
                          score=val_fold_scores), type='response')
        error <- brier_score(predictions, val_fold_target)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        val_err_oth_df <- rbind(val_err_oth_df, new_row)

        print(tail(tra_err_oth_df,1))
        print(tail(val_err_oth_df,1))

        # ========================= #
        # MULTI PLATT SCALING POOL3 #
        # ========================= #
        method <- 'Platt-pool3'
        tra_fold_scores <- all$pool3[train_idx,]
        tra_fold_target <- target[train_idx]
        val_fold_scores <- all$pool3[valid_idx,]
        val_fold_target <- target[valid_idx]

        df <- data.frame(y=tra_fold_target, score=tra_fold_scores)
        glm_out <- glm(y ~ ., data=df, family=binomial(logit))

        # Training Error
        predictions <- predict(glm_out, newdata=data.frame(
                          score=tra_fold_scores), type='response')
        error <- brier_score(predictions, tra_fold_target)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        tra_err_oth_df <- rbind(tra_err_oth_df, new_row)

        # Validation Error
        predictions <- predict(glm_out, newdata=data.frame(
                          score=val_fold_scores), type='response')
        error <- brier_score(predictions, val_fold_target)
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        val_err_oth_df <- rbind(val_err_oth_df, new_row)

        print(tail(tra_err_oth_df,1))
        print(tail(val_err_oth_df,1))

        # ========================= #
        # ISOTONIC REGRESSION       #
        # ========================= #
        method <- 'Isotonic'
        # Removing duplicated
        idx <- duplicated(score[train_idx])
        score_train_unique <- (score[train_idx])[!idx]
        target_train_unique <- (target[train_idx])[!idx]
        isoreg_out <- export_cal_isoreg(score_train_unique,target_train_unique,
                                        class_name,plot_folder)

        # Training Error
        predictions <- fits.isoreg(isoreg_out, score[train_idx])
        error <- brier_score(predictions, target[train_idx])
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        tra_err_oth_df <- rbind(tra_err_oth_df, new_row)

        # Validation Error
        predictions <- fits.isoreg(isoreg_out, score[valid_idx])
        error <- brier_score(predictions, target[valid_idx])
        new_row <- data.frame(class=class_name, method=method, fold=fold_id,
                              error=error)
        val_err_oth_df <- rbind(val_err_oth_df, new_row)

        print(tail(tra_err_oth_df,1))
        print(tail(val_err_oth_df,1))

        # ========================= #
        # WIDTH BINNING             #
        # ========================= #
        for(j in seq_along(width_bins)){
            number_bins <- width_bins[j]
            cal <- calibration_width_binning(score[train_idx],target[train_idx],
                                             number_bins)
            export_cal_width_binning(cal,class_name,plot_folder)

            # Training error
            prediction <- cal$score[findInterval(score[train_idx],cal$breaks,
                                                 all.inside=TRUE)]
            error <- brier_score(prediction,target[train_idx])
            new_row <- data.frame(class=class_name, bins=number_bins,
                                  fold=fold_id, error=error)
            tra_err_wid_df <- rbind(tra_err_wid_df, new_row)

            # Validation error
            prediction <- cal$score[findInterval(score[valid_idx],cal$breaks,
                                                 all.inside=TRUE)]
            error <- brier_score(prediction,target[valid_idx])
            new_row <- data.frame(class=class_name, bins=number_bins,
                                  fold=fold_id, error=error)
            val_err_wid_df <- rbind(val_err_wid_df, new_row)

            print(tail(tra_err_wid_df,1))
            print(tail(val_err_wid_df,1))
        }
        # ========================= #
        # SIZE BINNING              #
        # ========================= #
        for(j in seq_along(size_bins)){
            number_bins <- size_bins[j]
            cal <- calibration_size_binning(score[train_idx],target[train_idx],
                                            number_bins)
            export_cal_size_binning(cal,class_name,plot_folder)

            # Training error
            prediction <- cal$score[findInterval(score[train_idx],cal$breaks,
                                                 all.inside=TRUE)]
            error <- brier_score(prediction,target[train_idx])
            new_row <- data.frame(class=class_name, bins=number_bins,
                                  fold=fold_id, error=error)
            tra_err_siz_df <- rbind(tra_err_siz_df, new_row)

            # Validation error
            prediction <- cal$score[findInterval(score[valid_idx],cal$breaks,
                                                 all.inside=TRUE)]
            error <- brier_score(prediction,target[valid_idx])
            new_row <- data.frame(class=class_name, bins=number_bins,
                                  fold=fold_id, error=error)
            val_err_siz_df <- rbind(val_err_siz_df, new_row)

            print(tail(tra_err_siz_df,1))
            print(tail(val_err_siz_df,1))
        }
    }
}

# Change Variable Names
names(tra_err_oth_df) <- c("class","method", "fold", "error")
names(val_err_oth_df) <- c("class","method", "fold", "error")
names(tra_err_siz_df) <- c("class","size", "fold", "error")
names(val_err_siz_df) <- c("class","size", "fold", "error")
names(tra_err_wid_df) <- c("class","width", "fold", "error")
names(val_err_wid_df) <- c("class","width", "fold", "error")

# Training errors
print(tra_err_oth_df)
print(tra_err_wid_df)
print(tra_err_siz_df)

qplot(method, error, data=tra_err_oth_df)
qplot((train_instances-valid_instances)/size, error, data=tra_err_siz_df)
qplot(1/width, error, data=tra_err_wid_df)

ggplot(tra_err_oth_df, aes(x=method, y=error, fill=method)) + geom_boxplot()

# Validation errors
print(val_err_oth_df)
print(val_err_wid_df)
print(val_err_siz_df)
ggplot(val_err_oth_df, aes(x=method, y=error, fill=method)) + geom_boxplot()
ggplot(val_err_siz_df, aes(x=size, y=error, fill=size)) + geom_boxplot()
