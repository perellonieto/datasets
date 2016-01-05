#!/usr/bin/Rscript
require(MASS)
require(bisoreg) # fits.isoreg
require(pheatmap) # heat map

plot_folder <- 'calibration/'
CROSS_VAL <- TRUE
train_instances <- 5000 #Number of instances to use for training (inc. val.)
valid_instances <- 1000 #Number of instances to use for validation (from tra.)
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

# Only keep the specified number of instances for training
all$labels <- all$labels[1:train_instances,]
all$prob <- all$prob[1:train_instances,]
num_samples <- length(all$labels)

# Computing the prediction for each instance
all$predictions <- apply(all$prob, MARGIN=1, which.max)

sprintf('Accuracy on all the classes is = %f',
        mean(all$predictions == all$labels))

# Plotting options: class names, color and shape
all$class_names <- CLASS_NAMES
all$col <- rainbow(length(all$class_names))
all$pch <- rep(c(15,16,17,18), length(all$class_names))

# List of names for each calibration method
width_names <- paste('width', 1/width_bins)
size_names <- paste('size', ceiling(num_samples/size_bins))
other_names <- c('original', 'platt', 'isoreg', 'multi-platt', 'multi-pool')
col_names <- c(other_names, width_names,size_names)
width_idx <- length(other_names)+1
size_idx <- width_idx + length(width_names)

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

# Training and Validation tables of results for each class, model and fold
train_error_bs <- array(,c(length(CLASS_NAMES), length(col_names),n_folds))
valid_error_bs <- array(,c(length(CLASS_NAMES), length(col_names),n_folds))

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

#    for(fold_id in 1:n_folds){
#        # Training and Validation partitions
#        train_idx <- folds$train[fold_id,]
#        valid_idx <- folds$valid[fold_id,]
#
#        train_error_bs[id_pos,1,fold_id] <- brier_score(bin$prob[train_idx],
#                                                        target[train_idx])
#        print(sprintf("Brier score training for class %s = %f", class_name,
#                      train_error_bs[id_pos]))
#
#        valid_error_bs[id_pos,1,fold_id] <- brier_score(bin$prob[valid_idx],
#                                                        target[valid_idx])
#        print(sprintf("Brier score validation for class %s = %f", class_name,
#                      valid_error_bs[id_pos]))
#
#        # ========================= #
#        # PLATT SCALING in one score#
#        # ========================= #
#        model_idx <- 2
#        tra_fold_scores <- score[train_idx]
#        tra_fold_target <- target[train_idx]
#        val_fold_scores <- score[valid_idx]
#        val_fold_target <- target[valid_idx]
#
#        df <- data.frame(y=tra_fold_target, score=tra_fold_scores)
#        glm_out <- glm(y ~ ., data=df, family=binomial(logit))
#
#        export_cal_platt_scaling(glm_out,class_name,plot_folder)
#        predictions <- predict(glm_out, newdata=data.frame(
#                                                score=tra_fold_scores),
#                                        type='response')
#        train_error_bs[id_pos,model_idx,fold_id] <- brier_score(predictions,
#                                                        tra_fold_target)
#        predictions <- predict(glm_out, newdata=data.frame(
#                                                score=val_fold_scores),
#                                        type='response')
#        valid_error_bs[id_pos,model_idx,fold_id] <- brier_score(predictions,
#                                                        val_fold_target)
#
#
#        print(sprintf('Platt training error = %f',
#                      train_error_bs[id_pos,model_idx,fold_id]))
#        print(sprintf('Platt validation error = %f',
#                      valid_error_bs[id_pos,model_idx,fold_id]))
#
#        # ========================= #
#        # MULTICLASS PLATT SCALING  #
#        # ========================= #
#        model_idx <- 4
#        tra_fold_scores <- scores[train_idx,]
#        tra_fold_target <- target[train_idx]
#        val_fold_scores <- scores[valid_idx,]
#        val_fold_target <- target[valid_idx]
#
#        df <- data.frame(y=tra_fold_target, score=tra_fold_scores)
#        glm_out <- glm(y ~ ., data=df, family=binomial(logit))
#
#        predictions <- predict(glm_out, newdata=data.frame(
#                                                score=tra_fold_scores),
#                                        type='response')
#        train_error_bs[id_pos,model_idx,fold_id] <- brier_score(predictions,
#                                                        tra_fold_target)
#        predictions <- predict(glm_out, newdata=data.frame(
#                                                score=val_fold_scores),
#                                        type='response')
#        valid_error_bs[id_pos,model_idx,fold_id] <- brier_score(predictions,
#                                                        val_fold_target)
#
#        print(sprintf('Multi Platt training error = %f',
#                      train_error_bs[id_pos,model_idx,fold_id]))
#        print(sprintf('Multi Platt validation error = %f',
#                      valid_error_bs[id_pos,model_idx,fold_id]))
#
#        # ========================= #
#        # MULTIPOOL PLATT SCALING   #
#        # ========================= #
#        model_idx <- 5
#        tra_fold_scores <- all$pool3[train_idx,]
#        tra_fold_target <- target[train_idx]
#        val_fold_scores <- all$pool3[valid_idx,]
#        val_fold_target <- target[valid_idx]
#
#        df <- data.frame(y=tra_fold_target, score=tra_fold_scores)
#        glm_out <- glm(y ~ ., data=df, family=binomial(logit))
#
#        predictions <- predict(glm_out, newdata=data.frame(
#                                                score=tra_fold_scores),
#                                        type='response')
#        train_error_bs[id_pos,model_idx,fold_id] <- brier_score(predictions,
#                                                        tra_fold_target)
#        predictions <- predict(glm_out, newdata=data.frame(
#                                                score=val_fold_scores),
#                                        type='response')
#        valid_error_bs[id_pos,model_idx,fold_id] <- brier_score(predictions,
#                                                        val_fold_target)
#
#        print(sprintf('Pool3 Platt training error = %f',
#                      train_error_bs[id_pos,model_idx,fold_id]))
#        print(sprintf('Pool3 Platt validation error = %f',
#                      valid_error_bs[id_pos,model_idx,fold_id]))
#
#        # ========================= #
#        # ISOTONIC REGRESSION       #
#        # ========================= #
#        idx <- duplicated(score[train_idx])
#        score_train_unique <- (score[train_idx])[!idx]
#        target_train_unique <- (target[train_idx])[!idx]
#        isoreg_out <- export_cal_isoreg(score_train_unique,target_train_unique,
#                                        class_name,plot_folder)
#        predictions <- fits.isoreg(isoreg_out, score[train_idx])
#        train_error_bs[id_pos,3,fold_id] <- brier_score(predictions,
#                                                        target[train_idx])
#
#        predictions <- fits.isoreg(isoreg_out, score[valid_idx])
#        valid_error_bs[id_pos,3,fold_id] <- brier_score(predictions,
#                                                        target[valid_idx])
#
#        # ========================= #
#        # WIDTH BINNING             #
#        # ========================= #
#        for(j in seq_along(width_bins)){
#            number_bins <- width_bins[j]
#            cal <- calibration_width_binning(score[train_idx],target[train_idx],
#                                             number_bins)
#            export_cal_width_binning(cal,class_name,plot_folder)
#            prediction <- cal$score[findInterval(score[train_idx],cal$breaks,
#                                                 all.inside=TRUE)]
#
#            # Training error
#            train_error_bs[id_pos,width_idx+j-1,fold_id] <-
#                brier_score(prediction,target[train_idx])
#            prediction <- cal$score[findInterval(score[valid_idx],cal$breaks,
#                                                 all.inside=TRUE)]
#
#            # Validation error
#            valid_error_bs[id_pos,width_idx+j-1,fold_id] <-
#                brier_score(prediction,target[valid_idx])
#        }
#        # ========================= #
#        # SIZE BINNING              #
#        # ========================= #
#        for(j in seq_along(size_bins)){
#            number_bins <- size_bins[j]
#            cal <- calibration_size_binning(score[train_idx],target[train_idx],
#                                            number_bins)
#            export_cal_size_binning(cal,class_name,plot_folder)
#
#            # Training error
#            prediction <- cal$score[findInterval(score[train_idx],cal$breaks,
#                                                 all.inside=TRUE)]
#            train_error_bs[id_pos, size_idx+j-1,fold_id] <-
#                brier_score(prediction,target[train_idx])
#
#            # Validation error
#            prediction <- cal$score[findInterval(score[valid_idx],cal$breaks,
#                                                 all.inside=TRUE)]
#            valid_error_bs[id_pos, size_idx+j-1,fold_id] <-
#                brier_score(prediction,target[valid_idx])
#        }
#    }
}

mean_train_error_bs <- apply(train_error_bs, c(1,2), mean)
mean_valid_error_bs <- apply(valid_error_bs, c(1,2), mean)

pdf(sprintf('%scalibration_heatmap_scores.pdf', plot_folder))
pheatmap(mean_train_error_bs, cluster_cols=F, cluster_rows=F, display_numbers=T,
         number_format="%.2e", labels_row=CLASS_NAMES, labels_col=col_names)
dev.off()
pdf(sprintf('%scalibration_barplot_train_scores.pdf', plot_folder))
barplot(mean_train_error_bs, names.arg=col_names, angle=45, legend.text=CLASS_NAMES,
        las=2, args.legend=c(ncol=2, cex=0.9), main='training')
dev.off()
pdf(sprintf('%scalibration_barplot_valid_scores.pdf', plot_folder))
barplot(mean_valid_error_bs, names.arg=col_names, angle=45, legend.text=CLASS_NAMES,
        las=2, args.legend=c(ncol=2, cex=0.9), main='validation')
dev.off()

pdf(sprintf('%scalibration_width_brier_scores.pdf', plot_folder))
l_wb <- length(width_bins)
plot(1/width_bins, colMeans(mean_train_error_bs[,width_idx:(size_idx-1)]), type='b',
     col='purple', log='x', ylim=c(0.01,0.020),
     xlab='width (log scale)', ylab='brier score',
     main='Average error per class Width binning')
lines(1/width_bins, colMeans(mean_valid_error_bs[,width_idx:(size_idx-1)]), type='o')
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_train_error_bs[,1]),2),
      type='l', col='blue', lty=2, lwd=2)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_train_error_bs[,2]),2),
      type='l', col='red', lty=2)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_valid_error_bs[,2]),2),
      type='l', col='red', lty=1)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_train_error_bs[,3]),2),
      type='l', col='orange', lty=2)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_valid_error_bs[,3]),2),
      type='l', col='orange', lty=1)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_train_error_bs[,4]),2),
      type='l', col='brown', lty=2)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_valid_error_bs[,4]),2),
      type='l', col='brown', lty=1)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_train_error_bs[,5]),2),
      type='l', col='green', lty=2)
lines(c(1/width_bins[1], 1/width_bins[l_wb]), rep(mean(mean_valid_error_bs[,5]),2),
      type='l', col='green', lty=1)
legend('bottomright', c('Original Tra.',
                        'Platt Tra.', 'Platt Val.',
                        'Iso. Reg. Tra.', 'Iso. Reg Val.',
                        'Mult.Platt Tra.', 'Mult.Platt Val.',
                        'Pool platt Tra.', 'Pool Platt Val.',
                        'Training', 'Validation'),
       col=c('blue', 'red', 'red',
             'orange', 'orange',
             'brown', 'brown',
             'green', 'green',
             'purple', 'black'),
       lty=c(2,2,1,2,1,2,1,2,1,2,1))
dev.off()

pdf(sprintf('%scalibration_size_brier_scores.pdf', plot_folder))
sizes <- ceiling(num_samples/size_bins)
l_s <- length(sizes)
plot(sizes,colMeans(mean_train_error_bs[,size_idx:(size_idx+l_s-1)]),
     type='b', log='x', col='purple', ylim=c(0.01,0.020),
     xlab='size (log scale)', ylab='brier score',
     main='Average error per class Size binning')
lines(sizes, colMeans(mean_valid_error_bs[,size_idx:(size_idx+l_s-1)]),
      type='o')
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_train_error_bs[,1]),2),
      type='l', col='blue', lty=2, lwd=2)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_train_error_bs[,2]),2),
      type='l', col='red', lty=2)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_valid_error_bs[,2]),2),
      type='l', col='red', lty=1)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_train_error_bs[,3]),2),
      type='l', col='orange', lty=2)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_valid_error_bs[,3]),2),
      type='l', col='orange', lty=1)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_train_error_bs[,4]),2),
      type='l', col='brown', lty=2)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_valid_error_bs[,4]),2),
      type='l', col='brown', lty=1)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_train_error_bs[,5]),2),
      type='l', col='green', lty=2)
lines(c(sizes[1], sizes[l_s]), rep(mean(mean_valid_error_bs[,5]),2),
      type='l', col='green', lty=1)
#legend('topleft', c('Original',
#                    'Platt Tra.', 'Platt Val.',
#                    'Iso. Reg. Tra.', 'Iso. Reg Val.',
#                    'Mult.Platt Tra.', 'Mult.Platt Val.',
#                    'Pool platt Tra.', 'Pool Platt Val.',
#                    'Training', 'Validation'),
#       col=c('blue', 'red', 'red',
#             'orange', 'orange',
#             'brown', 'brown',
#             'green', 'green',
#             'purple', 'black'),
#       lty=c(2,2,1,2,1,2,1,2,1,2,1))
dev.off()
