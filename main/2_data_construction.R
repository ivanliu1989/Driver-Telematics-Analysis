setwd('/Users/ivan/Work_directory/DTA')
# setwd('H:/Machine_Learning/DTA')
# setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
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
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 158, dimnames = list(NULL, NULL))
for (driver in drivers){
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        trip_distance <- distance(trip_data)
        speed <- calcSpeed(trip_data)
        feature_speed <- generateDistribution(speed, 'speed')
        # sd_speed
        sd_speed <- sd(speed, na.rm=T)
        # avg_speed
        avg_speed <- mean(speed, na.rm = T)
        # avg_speed_stop
#         avg_speed_stop <- mean(speed[which(speed >= 0.36)], na.rm = T)
        # drive_time
        drive_time <- nrow(trip_data)
        # standstill_time
        standstill_time <- length(which(speed < 0.36))/drive_time
        
        # Tangential/Normal acceleration | Curvature
        cur <- calcCurvature(trip_data,1) #
        tanAcc <- calcTangAccel(trip_data) #
        norAcc <- calcNormAccel(speed,cur) #
        feature_tanAcc <- generateDistribution(tanAcc,'tanAcc')
        feature_norAcc <- generateDistribution(norAcc,'norAcc')
        feature_totAcc <- TotalAccelDistribution(tanAcc,norAcc)
        feature_curvature <- curvatureDistribution(cur,1)
        sd_tanAcc <- sd(tanAcc,na.rm = T)
        sd_norAcc <- sd(norAcc,na.rm = T)

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
        feature_acc <- generateDistribution(acceleration,'Acc')
        feature_dec <- generateDistribution(deceleration,'Dec')
        sd_acc <- sd(acceleration,na.rm = T)
        sd_dec <- sd(deceleration,na.rm = T)
        acc_time <- length(acceleration)/length(adceleration)
        dec_time <- length(deceleration)/length(adceleration)
        cons_time <- length(constant)/length(adceleration)
        ex_acc_time <- length(ex_acc)/length(adceleration)
        ex_dec_time <- length(ex_dec)/length(adceleration)

#         # circular statistics
#         polar <- Cartesian_to_Polar(trip_data)
#         turning <- degree_cal(polar)
#         turn_point <- turning[which(turning>=45)]
#         turn_speed <- speed[which(turning>=45)]
#         centrifugal_acc <- speed[-1]^2/polar[,1]
#         # turning speed/radius/turning point/ex_turning/centrifugal acceleration\
#         mean_direction <- mean(turning)
#         mean_turn_sp <- mean(turn_speed)
#         sd_circular <- sd(turn_point)
#         ex_turn <- length(turn_speed[which(turn_speed>=20 & turn_point >= 120)])/length(turn_point)
#         turn_point_mean <- length(turn_point)/length(turning)
#         feature_turn <- quantile(turn_point, seq(0.1,1, by = 0.1), na.rm=T)
#         feature_centrifugal <- quantile(centrifugal_acc, seq(0.1,1, by = 0.1), na.rm=T)
        # df
        main_df[d_num,] <- c(driver,trip,trip_distance,t(feature_speed),sd_speed,avg_speed,standstill_time,t(feature_tanAcc),t(feature_norAcc),
                             t(feature_totAcc),t(feature_curvature),sd_tanAcc,sd_norAcc,avg_acc,avg_dec,t(feature_acc),t(feature_dec),
                             sd_acc,sd_dec,acc_time,dec_time,cons_time,ex_acc_time,ex_dec_time,target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}
for (i in 1:103){
    if(sum(is.na(main_df[,i]))>0){
        print(i)
    }   
}
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip','trip_distance',names(feature_speed),'sd_speed','avg_speed','standstill_time',names(feature_tanAcc),names(feature_norAcc),
                    names(feature_totAcc),names(feature_curvature),'sd_tanAcc','sd_norAcc','avg_acc','avg_dec',names(feature_acc),names(feature_dec),
                    'sd_acc','sd_dec','acc_time','dec_time','cons_time','ex_acc_time','ex_dec_time','target')
dim(main_df);head(main_df)

save(main_df, file='data/main_df_155features.RData')
write.csv(main_df, file = 'data/main_df_155features.csv', quote = F, row.names = F)
