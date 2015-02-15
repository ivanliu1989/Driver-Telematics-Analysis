setwd('/Users/ivan/Work_directory/DTA')
# setwd('H:/Machine_Learning/DTA')
# setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
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
    return(dist)
    #quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
}
degree_cal <- function(trip){
    radian <- atan2(diff(trip[,2]),diff(trip[,1]))
    degrees <- radian * 180 / pi    
    turning <- abs(diff(degrees))
    return(turning)
}

### main ###
date(); d_num <- 0
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 89, dimnames = list(NULL, NULL))
for (driver in drivers){
    
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(read.csv(files,header = T,stringsAsFactor=F))
        trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        dist <- speedDist(trip_data)
        speed <- dist * 3.6
        feature_speed <- quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
        # sd_speed
        sd_speed <- sd(speed, na.rm=T)
        # avg_speed
        avg_speed <- mean(speed, na.rm = T)
        # avg_speed_stop
        avg_speed_stop <- mean(speed[which(speed >= 1)], na.rm = T)
        # drive_time
        drive_time <- nrow(trip_data)
        # standstill_time
        standstill_time <- length(which(speed < 1))/drive_time
        # acceleration/deceleration
        adceleration <- diff(speed)
        acceleration <- adceleration[which(adceleration>= 0.1)]
        deceleration <- adceleration[which(adceleration<= -0.1)]
        constant <- adceleration[which(adceleration> -0.1 & adceleration < 0.1)]
        ex_acc <- adceleration[which(adceleration > 3)]
        ex_dec <- adceleration[which(adceleration< -3)]
        # acceleration/deceleration features
        avg_acc <- mean(acceleration)
        avg_dec <- mean(deceleration)
        feature_acc <- quantile(acceleration, seq(0.05,1, by = 0.05), na.rm=T)
        feature_dec <- quantile(deceleration, seq(0.05,1, by = 0.05), na.rm=T)
        sd_acc <- sd(acceleration)
        sd_dec <- sd(deceleration)
        acc_time <- length(acceleration)/length(adceleration)
        dec_time <- length(deceleration)/length(adceleration)
        cons_time <- length(constant)/length(adceleration)
        ex_acc_time <- length(ex_acc)/length(adceleration)
        ex_dec_time <- length(ex_dec)/length(adceleration)
        # turning
        turning <- degree_cal(trip_data)
        turning[which(turning>180)] <- 360-turning[which(turning>180)]
        turn_point <- turning[which(turning>=30)]
        turn_speed <- speed[which(turning>=30)]
        
        # turning speed/radius/turning point/ex_turning/centrifugal acceleration\
        ex_turn <- length(turn_speed[which(turn_speed>=20)])/length(turn_point)
        turn_point_mean <- length(turn_point)/length(turning)
        feature_turn <- quantile(turn_point, seq(0.1,1, by = 0.1), na.rm=T)
        
        # df
        main_df[d_num,] <- c(driver, trip, t(feature_speed), sd_speed, avg_speed, avg_speed_stop, drive_time, standstill_time, avg_acc, avg_dec, 
                             t(feature_acc), t(feature_dec), sd_acc, sd_dec, acc_time, dec_time, cons_time, ex_acc_time, ex_dec_time,
                             ex_turn, turn_point_mean, feature_turn, target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}

main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver', 'trip', paste0('speed_',names(feature_speed)), 'sd_speed', 'avg_speed', 'avg_speed_stop', 'drive_time', 'standstill_time', 'avg_acc', 'avg_dec', 
                    paste0('acc_',names(feature_acc)), paste0('dec_',names(feature_dec)), 'sd_acc', 'sd_dec', 'acc_time', 'dec_time', 'cons_time', 'ex_acc_time', 'ex_dec_time', 
                    'ex_turn', 'turn_point_mean', paste0('turn_',names(feature_turn)), 'target')
dim(main_df);head(main_df)

save(main_df, file='data/main_df_89features.RData')
write.csv(main_df, file = 'data/main_df_89features.csv', quote = F, row.names = F)
