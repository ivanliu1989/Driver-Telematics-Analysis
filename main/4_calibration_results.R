# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

# main_df[which(main_df[,'speed_100_pct']>85),c(1,2)]
# main_df[which(main_df[,'trip_distance']/main_df[,'drive_time']>85),c(1,2)]

results <- (fread('submission_rf_172_17_0.87142.csv',header = T, stringsAsFactor = F))

# results[which(main_df[,'speed_100_pct']>85),2] <- 0
# results[which(main_df[,'trip_distance']/main_df[,'drive_time']>85),2] <- 0
# 
# write.csv(results, file = 'calibra.csv', quote = F, row.names = F)

### step 2 ###
# results[1:200,][match_matrix[which(match_matrix[,1]==1),2],2] <- 1
# write.csv(results, file = 'calibra_1.csv', quote = F, row.names = F)

load('Driver-Telematics-Analysis/repeated_trips/repeated_map_thereshold_0.03_driver_1_2173.RData')
load('Driver-Telematics-Analysis/repeated_trips/repeated_map_thereshold_0.03_driver_2174_3612.RData')
match_matrix <- match_matrix[which(match_matrix[,1]>0),]
results[driver_trip %in% match_matrix[,1],prob]
results[driver_trip %in% match_matrix[,1],prob:=1]
write.csv(results, file = 'calibra_all_0.8.csv', quote = F, row.names = F)

### distance quantile ###
# qdist <- seq(0.01,1, by = 0.01)
# quantile(dist, qdist)

results[(driver_trip %in% match_matrix[,1]) & (prob > 0.8),prob:=1]
