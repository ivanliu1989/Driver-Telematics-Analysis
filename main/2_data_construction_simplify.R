setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(data.table);require(zoo);
source('Driver-Telematics-Analysis/main/kalman_filtering.R')
source('Driver-Telematics-Analysis/main/feature_functions.R')

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
trips <- 1:200
path <- "data/drivers/"

### main ###
date(); d_num <- 0
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 43, dimnames = list(NULL, NULL))
for (driver in drivers){
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        dist = sqrt(diff(trip_data[,1])^2 + diff(trip_data[,2])^2) # distance
        smoothDist <- rollapply(dist, width = 5, FUN = median) # rolling median smooth
        speed <- smoothDist * 3.6
        feature_speed <- generateDistribution(speed, 'speed') # 
        # acceleration
        acceleration <- diff(speed)
        feature_acceleration <- generateDistribution(acceleration, 'acceleration')
        
        # df
        main_df[d_num,] <- c(driver,trip,t(feature_speed),t(feature_acceleration),target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}

main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip',names(feature_speed),names(feature_acceleration),'target')

save(main_df, file='data/main_df_43features.RData')
