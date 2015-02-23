# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

main_df[which(main_df[,'speed_100_pct']>85),c(1,2)]
main_df[which(main_df[,'trip_distance']/main_df[,'drive_time']>85),c(1,2)]

results <- (fread('submission_rf_0.81715.csv',header = T, stringsAsFactor = F))

# results[which(main_df[,'speed_100_pct']>85),2] <- 0
# results[which(main_df[,'trip_distance']/main_df[,'drive_time']>85),2] <- 0
# 
# write.csv(results, file = 'calibra.csv', quote = F, row.names = F)

### step 2 ###
# results[1:200,][match_matrix[which(match_matrix[,1]==1),2],2] <- 1
# write.csv(results, file = 'calibra_1.csv', quote = F, row.names = F)

match_matrix <- match_matrix[which(match_matrix[,1]>0),]
results_s <- results[,Driver := strsplit(driver_trip,split = '_')[[1]][1],by=driver_trip]

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

# for(i in names(table(match_matrix[,1]))){
#     tp <- as.integer(names(table(match_matrix[which(match_matrix[,1]==i),2])))
#     results_s[Driver==i,prob:=1]
# }

