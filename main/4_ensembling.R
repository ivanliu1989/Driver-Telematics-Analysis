# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

svm <- data.frame(fread('submission_svm.csv',header = T, stringsAsFactor = F))
gbm <- data.frame(fread('submission_gbm.csv',header = T, stringsAsFactor = F))
knn <- data.frame(fread('submission_knn.csv',header = T, stringsAsFactor = F))
rf <- data.frame(fread('submission_rf.csv',header = T, stringsAsFactor = F))
lg <- data.frame(fread('submission_lg.csv',header = T, stringsAsFactor = F))
nnet <- data.frame(fread('submission_nnet.csv',header = T, stringsAsFactor = F))
rf2 <- data.frame(fread('submission_rf_feature.csv',header = T, stringsAsFactor = F))

head(svm);head(gbm);head(knn);head(rf);head(lg);head(nnet)

ensem_prob <- (svm[,2] + gbm[,2]  + rf[,2] + rf2[,2] + knn[,2] + nnet[,2]  + lg[,2])/7
# ensem_prob <- 4/(1/svm[,2] + 1/gbm[,2] + 1/nb[,2] + 1/rf[,2])

ensemble <- data.frame(driver_trip=svm[,1], prob=ensem_prob)
head(ensemble)

write.csv(ensemble, file = 'submissions7.csv', quote = F, row.names = F)
