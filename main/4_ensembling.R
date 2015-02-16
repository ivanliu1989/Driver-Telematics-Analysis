# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

svm <- data.frame(fread('submission_svm.csv',header = T, stringsAsFactor = F))
gbm <- data.frame(fread('submission_gbm.csv',header = T, stringsAsFactor = F))
head(svm);head(gbm)
ensemble <- data.frame(driver_trip=svm[,1], prob=(svm[,2]+gbm[,2])/2)
head(ensemble)

write.csv(ensemble, file = 'submission_gbm_svm.csv', quote = F, row.names = F)
