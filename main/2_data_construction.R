# setwd('/Users/ivan/Work_directory/DTA')
setwd('H:/Machine_Learning/DTA')
rm(list=ls());gc()
require(data.table);require(zoo);
source('Driver-Telematics-Analysis/main/kalman_filtering.R')

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
trips <- 1:200
path <- "data/drivers/"
main_df <- data.frame()

### functions ###
speedDist <- function(trip) {
    dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
    speed <- dist * 3.6
    quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
}

### main ###
date(); d_num <- 0
for (driver in drivers){
    d_num <- d_num + 1
    for (trip in trips){
        sub_df <- data.frame()
        files <- paste0(path, driver, '/', trip, ".csv")
        if (trip==200) {
            print(paste0(files, ' | ' ,date())) 
            }
        trip_data <- read.csv(files,header = T,stringsAsFactor=F)
        trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # id
        driver_trip <- paste0(driver,'_',trip)
        # target
        target <- 0
        # speed
        feature_speed <- speedDist(trip_data)
        
        # df
        sub_df <- cbind(driver_trip, t(feature_speed), target)
        main_df <- rbind(main_df,sub_df)
    }
    if(d_num%%100==0){
        names(main_df) <- c('driver_trip', names(feature_speed), 'target')
        dim(main_df);head(main_df)
        save(main_df, file=paste('data/main_df_',d_num,'.RData',sep = ''))
    }
}

names(main_df) <- c('driver_trip', names(feature_speed), 'target')
dim(main_df);head(main_df)
save(main_df, file='data/main_df.RData')

