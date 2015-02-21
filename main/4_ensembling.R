# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

svm1 <- data.frame(fread('submission_svm.csv',header = T, stringsAsFactor = F))
svm2 <- data.frame(fread('submission_svmLinear_182.csv',header = T, stringsAsFactor = F))
gbm1 <- data.frame(fread('submission_gbm.csv',header = T, stringsAsFactor = F))
gbm2 <- data.frame(fread('submission_gbm_136.csv',header = T, stringsAsFactor = F))
gbm3 <- data.frame(fread('submission_gbm_182.csv',header = T, stringsAsFactor = F))
knn1 <- data.frame(fread('submission_knn.csv',header = T, stringsAsFactor = F))
knn2 <- data.frame(fread('submission_knn_136.csv',header = T, stringsAsFactor = F))
rf1 <- data.frame(fread('submission_rf.csv',header = T, stringsAsFactor = F))
rf2 <- data.frame(fread('submission_rf_3.csv',header = T, stringsAsFactor = F))
rf3 <- data.frame(fread('submission_rf_136.csv',header = T, stringsAsFactor = F))
rf4 <- data.frame(fread('submission_rf_feature.csv',header = T, stringsAsFactor = F))
lg1 <- data.frame(fread('submission_lg.csv',header = T, stringsAsFactor = F))
nnet1 <- data.frame(fread('submission_nnet.csv',header = T, stringsAsFactor = F))
nnet2 <- data.frame(fread('submission_nnet_136.csv',header = T, stringsAsFactor = F))

head(svm);head(gbm);head(rf);head(knn);head(lg);head(nnet)

ensem_prob <- (svm1[,2] + svm2[,2] + gbm1[,2] + gbm2[,2] + gbm3[,2] + knn1[,2] + knn2[,2] + 
                   rf1[,2] + rf2[,2] + rf3[,2] + rf4[,2] + lg1[,2] + nnet1[,2] + nnet2[,2])/14
# ensem_prob <- 4/(1/svm[,2] + 1/gbm[,2] + 1/nb[,2] + 1/rf[,2])

ensemble <- data.frame(driver_trip=svm1[,1], prob=ensem_prob)
head(ensemble)

write.csv(ensemble, file = 'en14.csv', quote = F, row.names = F)
