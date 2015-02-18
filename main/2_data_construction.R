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
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 218, dimnames = list(NULL, NULL))
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
        feature_curvature <- curvatureDistribution(cur)
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
        
        # circular statistics
        polar <- Cartesian_to_Polar(trip_data,1)
        turning <- degree_cal(polar)
        turn_point <- turning[which(turning>=80)]
        #speed_all <- calcSpeed(trip_data,1)
        turn_speed <- speed[which(turning>=80)]
        centrifugal_acc <- speed[-c(1:1)]^2/polar[,1]
        adc_turn <- adceleration[which(turning>=80)]
        adc_straight <- adceleration[which(turning<80)]
        
        dec_turn <- adc_turn[which(adc_turn<= -0.1)]
        dec_straight <- adc_straight[which(adc_straight<= -0.1)]
        acc_turn <- adc_turn[which(adc_turn>= 0.1)]
        acc_straight <- adc_straight[which(adc_straight>= 0.1)]
        
        # turning speed/radius/turning point/ex_turning/centrifugal acceleration\
        mean_direction <- mean(turning,na.rm = T)
        #mean_turn_sp <- mean(turn_speed,na.rm = T)
        sd_circular <- sd(turn_point,na.rm = T)
        ex_turn <- length(turn_speed[which(turn_speed>=20 & turn_point >= 120)])/length(turn_point)
        turn_point_mean <- length(turn_point)/length(turning)
        feature_turn <- generateDistribution(turn_point, 'turn')
        feature_centrifugal <- generateDistribution(centrifugal_acc, 'centAcc')
        
        sd_acc_turn <- sd(acc_turn, na.rm = T)
        sd_acc_straight <- sd(acc_straight, na.rm = T)
        sd_dec_turn <- sd(dec_turn, na.rm = T)
        sd_dec_straight <- sd(dec_straight, na.rm = T)
        
        avg_acc_turn <- mean(acc_turn, na.rm = T)
        avg_acc_straight <- mean(acc_straight, na.rm = T)
        avg_dec_turn <- mean(dec_turn, na.rm = T)
        avg_dec_straight <- mean(dec_straight, na.rm = T)
        
        max_acc_turn <- max(acc_turn, na.rm = T)
        max_acc_straight <- max(acc_straight, na.rm = T)
        max_dec_turn <- max(dec_turn, na.rm = T)
        max_dec_straight <- max(dec_straight, na.rm = T)
        
        min_acc_turn <- min(acc_turn, na.rm = T)
        min_acc_straight <- min(acc_straight, na.rm = T)
        min_dec_turn <- min(dec_turn, na.rm = T)
        min_dec_straight <- min(dec_straight, na.rm = T)
        
        # df
        main_df[d_num,] <- c(driver,trip,trip_distance,t(feature_speed),sd_speed,avg_speed,standstill_time,t(feature_tanAcc),t(feature_norAcc),
                             t(feature_totAcc),t(feature_curvature),sd_tanAcc,sd_norAcc,avg_acc,avg_dec,t(feature_acc),t(feature_dec),
                             sd_acc,sd_dec,acc_time,dec_time,cons_time,ex_acc_time,ex_dec_time,
                             mean_direction, sd_circular, ex_turn, turn_point_mean, t(feature_turn), t(feature_centrifugal), 
                             sd_acc_turn,sd_acc_straight,sd_dec_turn,sd_dec_straight,avg_acc_turn,avg_acc_straight,avg_dec_turn,avg_dec_straight,
                             max_acc_turn,max_acc_straight,max_dec_turn,max_dec_straight,min_acc_turn,min_acc_straight,min_dec_turn,min_dec_straight
                             ,target)
        
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
                    'sd_acc','sd_dec','acc_time','dec_time','cons_time','ex_acc_time','ex_dec_time',
                    'mean_direction', 'sd_circular', 'ex_turn', 'turn_point_mean', names(feature_turn), names(feature_centrifugal), 
                    'sd_acc_turn','sd_acc_straight','sd_dec_turn','sd_dec_straight','avg_acc_turn','avg_acc_straight','avg_dec_turn','avg_dec_straight',
                    'max_acc_turn','max_acc_straight','max_dec_turn','max_dec_straight','min_acc_turn','min_acc_straight','min_dec_turn','min_dec_straight'
                    ,'target')
dim(main_df);head(main_df)

save(main_df, file='data/main_df_218features.RData')
write.csv(main_df, file = 'data/main_df_218features.csv', quote = F, row.names = F)
