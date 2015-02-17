# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

svm <- data.frame(fread('submission_svm.csv',header = T, stringsAsFactor = F))
gbm <- data.frame(fread('submission_gbm.csv',header = T, stringsAsFactor = F))
nb <- data.frame(fread('submission_nb.csv',header = T, stringsAsFactor = F))
rf <- data.frame(fread('submission_rf.csv',header = T, stringsAsFactor = F))

head(svm);head(gbm);head(nb);head(rf)

ensem_prob <- (svm[,2] + gbm[,2] + nb[,2] + rf[,2])/4
ensem_prob <- 4/(1/svm[,2] + 1/gbm[,2] + 1/nb[,2] + 1/rf[,2])

ensemble <- data.frame(driver_trip=svm[,1], prob=ensem_prob)
head(ensemble)

write.csv(ensemble, file = 'submissions.csv', quote = F, row.names = F)
