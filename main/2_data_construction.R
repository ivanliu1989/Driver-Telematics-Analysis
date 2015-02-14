# setwd('/Users/ivan/Work_directory/DTA')
# setwd('H:/Machine_Learning/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table);require(zoo);
source('Driver-Telematics-Analysis/main/kalman_filtering.R')

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
trips <- 1:200
path <- "data/drivers/"

### functions ###
speedDist <- function(trip) {
    dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
    speed <- dist * 3.6
    quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
}

### main ###
date(); d_num <- 0
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 23, dimnames = list(NULL, NULL))
for (driver in drivers){
    
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(read.csv(files,header = T,stringsAsFactor=F))
        trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        feature_speed <- speedDist(trip_data)
        # df
        main_df[d_num,] <- c(driver, trip, t(feature_speed), target)
        
        if (trip==200) {
            cat(paste0(files, ' | ' ,date())) 
        }
    }
}
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip', names(feature_speed), 'target')
dim(main_df);head(main_df)
save(main_df, file='data/main_df.RData')
write.csv(main_df, file = 'data/main_df.csv', quote = F, row.names = F)
