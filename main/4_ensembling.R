# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

# svm1 <- data.frame(fread('submission_svm_0.76341.csv',header = T, stringsAsFactor = F))
# svm2 <- data.frame(fread('submission_svmLinear_182_0.80957.csv',header = T, stringsAsFactor = F))
# gbm1 <- data.frame(fread('submission_gbm_0.80510.csv',header = T, stringsAsFactor = F))
# gbm2 <- data.frame(fread('submission_gbm_136_0.83521.csv',header = T, stringsAsFactor = F))
# gbm3 <- data.frame(fread('submission_gbm_182_0.84145.csv',header = T, stringsAsFactor = F))
# knn1 <- data.frame(fread('submission_knn_0.76644.csv',header = T, stringsAsFactor = F))
# knn2 <- data.frame(fread('submission_knn_136_0.79242.csv',header = T, stringsAsFactor = F))
# rf1 <- data.frame(fread('submission_rf_0.84880.csv',header = T, stringsAsFactor = F))
# rf2 <- data.frame(fread('submission_rf_3_0.84629.csv',header = T, stringsAsFactor = F))
# rf3 <- data.frame(fread('submission_rf_136_0.84154.csv',header = T, stringsAsFactor = F))
# rf4 <- data.frame(fread('submission_rf_136_59_0.83532.csv',header = T, stringsAsFactor = F))
# rf5 <- data.frame(fread('submission_rf_136_66_0.81474.csv',header = T, stringsAsFactor = F))
# rf6 <- data.frame(fread('submission_rf_136_no_norm_0.84077.csv',header = T, stringsAsFactor = F))
# rf7 <- data.frame(fread('submission_rf_136_none_curdist_0.83843.csv',header = T, stringsAsFactor = F))
# rf8 <- data.frame(fread('submission_rf_feature_0.84877.csv',header = T, stringsAsFactor = F))
# lg1 <- data.frame(fread('submission_lg_0.76609.csv',header = T, stringsAsFactor = F))
# nnet1 <- data.frame(fread('submission_nnet_0.77582.csv',header = T, stringsAsFactor = F))
# nnet2 <- data.frame(fread('submission_nnet_136_0.80379.csv',header = T, stringsAsFactor = F))
# 
# head(svm);head(gbm);head(rf);head(knn);head(lg);head(nnet)
# 
# ensem_prob <- (svm1[,2] + svm2[,2] + gbm1[,2] + gbm2[,2] + gbm3[,2] + knn1[,2] + knn2[,2] + 
#                    rf1[,2] + rf2[,2] + rf3[,2] + rf4[,2] + rf5[,2] + rf6[,2] + rf7[,2] + rf8[,2] +
#                    lg1[,2] + nnet1[,2] + nnet2[,2])/18
# ensem_prob <- 4/(1/svm[,2] + 1/gbm[,2] + 1/nb[,2] + 1/rf[,2])

# ensemble <- data.frame(driver_trip=svm1[,1], prob=ensem_prob)
# head(ensemble)

# write.csv(ensemble, file = 'en18.csv', quote = F, row.names = F)

### Auto ###
datadirectory <- 'results' # 'results/best'
files <- list.files(datadirectory,full.names = T)
i <- 0
ensem_prob <- matrix(0, nrow = 547200, ncol = 1, dimnames = list(NULL, NULL))

for(file in files[-1]){
    result <- data.frame(fread(file,header = T, stringsAsFactor = F))
    ensem_prob <- ensem_prob + result[,2]
    i = i + 1;
}

final_prob <- ensem_prob/i
ensemble <- data.frame(driver_trip=result[,1], prob=final_prob)
write.csv(ensemble, file = 'First_try_40.csv', quote = F, row.names = F)

### repeat ###
load('Driver-Telematics-Analysis/repeated_trips/repeated_map_thereshold_0.03_driver_1_2173.RData')
load('Driver-Telematics-Analysis/repeated_trips/repeated_map_thereshold_0.03_driver_2174_3612.RData')
match_matrix <- match_matrix[which(match_matrix[,1]>0),]
ensemble <- data.table(ensemble)
ensemble[driver_trip %in% match_matrix[,1],prob]
ensemble[driver_trip %in% match_matrix[,1],prob:=1]
write.csv(ensemble, file = 'First_try_40_repeated.csv', quote = F, row.names = F)

