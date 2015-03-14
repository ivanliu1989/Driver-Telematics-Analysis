# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

datadirectory <- 'results' # 'results/best'
ensemble <- list.files(datadirectory,full.names = T)
datadirectory <- 'data/drivers/'
drive.list <- sort(as.numeric(list.files(datadirectory)))
pred <- matrix(0, nrow=length(drive.list), ncol=200)

for (j in ensemble) {
    sub <- as.data.frame(fread(j,header = T, stringsAsFactor = F))
    p <- matrix(NA, nrow=0, ncol=200)
    for (i in 1:length(drive.list)) {
        d <- drive.list[i]
        labels <- sapply(1:200, function(x) paste0(d,'_', x))
        tmp <- sub[which(sub[, 1] %in% labels), 2]
        p <- rbind(p, tmp)
    }
    pred <- pred + p # * anything changes not the order so not the AUC
}

submission <- NULL
for (i in 1:nrow(pred)) {
    d <- drive.list[i]
    tmp <- pred[i, ]
    labels <- sapply(1:200, function(x) paste0(d,'_', x))
    result <- cbind(labels, tmp)
    submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "Driver/submission_ensemble.csv", row.names=F, quote=F)