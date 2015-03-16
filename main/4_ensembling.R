# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table)

### Auto ###
datadirectory <- 'results' # 'results/best'
files <- list.files(datadirectory,full.names = T)
i <- 0
ensem_prob <- matrix(0, nrow = 547200, ncol = 1, dimnames = list(NULL, NULL))

substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x)-4)
}

accuracy <- sum(as.numeric(substrRight(files, 11)))
for(file in files){
    result <- data.frame(fread(file,header = T, stringsAsFactor = F))
    ensem_prob <- ensem_prob + result[,2]*as.numeric(substrRight(file, 11))/accuracy
}
final_prob <- ensem_prob

# for(file in files){
#     result <- data.frame(fread(file,header = T, stringsAsFactor = F))
#     ensem_prob <- ensem_prob + result[,2]
#     i = i + 1;
# }
# 
# final_prob <- ensem_prob/i
ensemble <- data.frame(driver_trip=result[,1], prob=final_prob)
write.csv(ensemble, file = 'Fourth_Try.csv', quote = F, row.names = F)

### repeat ###
load('Driver-Telematics-Analysis/repeated_trips/repeated_map_thereshold_0.03_ALL.RData')
# load('Driver-Telematics-Analysis/match_trip/repeated_map_thereshold_0.05_ALL.RData')
ensemble <- data.table(ensemble)
ensemble[driver_trip %in% match_matrix,prob]
ensemble[driver_trip %in% match_matrix,prob:=1]
write.csv(ensemble, file = 'Final_Try_trip_match_910.csv', quote = F, row.names = F)

### Another Try ###
result <- data.frame(fread('submission_ensemble.csv',header = T, stringsAsFactor = F))
result2 <- data.frame(fread('Final_Try_trip_match_910.csv',header = T, stringsAsFactor = F))
result[,2] <- apply(data.matrix(result[,2]), MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
head(result);head(result2)
load('Driver-Telematics-Analysis/repeated_trips/repeated_map_thereshold_0.03_ALL.RData')
ensemble <- data.table(result)
ensemble[driver_trip %in% match_matrix,prob]
ensemble[driver_trip %in% match_matrix,prob:=1]
write.csv(ensemble, file = 'Final_Try_trip_match_ensemble2.csv', quote = F, row.names = F)
