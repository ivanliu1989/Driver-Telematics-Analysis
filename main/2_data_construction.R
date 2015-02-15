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
    #quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
}

### main ###
date(); d_num <- 0
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 27, dimnames = list(NULL, NULL))
for (driver in drivers){
    
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(read.csv(files,header = T,stringsAsFactor=F))
        trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        speed <- speedDist(trip_data)
        feature_speed <- quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
        # sd_speed
        sd_speed <- sd(speed)
        # avg_speed
        avg_speed <- mean(speed)
        # avg_speed_stop
        avg_speed <- mean(speed[which(speed >= 1)])
        # drive_time
        drive_time <- nrow(trip_data)
        # standstill_time
        standstill_time <- sum(which(speed <= 0.1))/drive_time
        # acceleration/deceleration
        adceleration <- speed - vector(0,speed[1:(length(speed)-1)])
        acceleration <- adceleration[which(adceleration>0)]
        deceleration <- adceleration[which(adceleration<-0)]
        constant <- adceleration[which(adceleration==0)]
        ex_acc <- adceleration[which(adceleration>3)]
        ex_dec <- adceleration[which(adceleration<-3)]
        # acceleration/deceleration features
        avg_acc <- mean(acceleration)
        avg_dec <- mean(deceleration)
        feature_acc <- quantile(feature_acc, seq(0.05,1, by = 0.05), na.rm=T)
        feature_dec <- quantile(feature_dec, seq(0.05,1, by = 0.05), na.rm=T)
        sd_acc <- sd(acceleration)
        sd_dec <- sd(deceleration)
        acc_time <- length(acceleration)/length(adceleration)
        dec_time <- length(deceleration)/length(adceleration)
        cons_time <- length(constant)/length(adceleration)
        ex_acc_time <- length(ex_acc)/length(adceleration)
        ex_dec_time <- length(ex_dec)/length(adceleration)
        # df
        main_df[d_num,] <- c(driver, trip, t(feature_speed), sd_speed, avg_speed, drive_time, standstill_time, target)
        
        if (trip==200) {
            cat(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200))) 
        }
    }
}
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip', names(feature_speed), 'sd_speed', 'avg_speed', 'drive_time', 'standstill_time', 'target')
dim(main_df);head(main_df)
save(main_df, file='data/main_df.RData')
write.csv(main_df, file = 'data/main_df.csv', quote = F, row.names = F)
