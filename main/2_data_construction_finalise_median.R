setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
source('Driver-Telematics-Analysis/main/feature_functions.R')

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
trips <- 1:200
path <- "data/drivers/"

#################
### MAIN_PROC ###
#################
date(); d_num <- 0
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 214, dimnames = list(NULL, NULL))
#sp_limits <- 280/3.6
for (driver in drivers){
    for (trip in trips){
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        
        ### Basic Features ###
        target <- 0
        trip_distance <- distance(trip_data)
        driver_time <- nrow(trip_data)
        fly_distance <- sqrt(trip_data[driver_time,1]^2 + trip_data[driver_time,1]^2)
        
        ### Speed Features ###
        #jp <- detectJumps(calcSpeed(trip_data),sp_limits) #jp
        speed <- rollapply(calcSpeed(trip_data), width = 5, FUN = median, na.rm=T) # rolling median smooth #speed
        
        feature_speed <- generateDistribution(speed, 'speed', 0.05)
        avg_speed <- mean(speed, na.rm = T)
        sd_speed <- sd(speed, na.rm=T)
        med_speed <- median(speed, na.rm = T)
        tot_curvature <- fly_distance/trip_distance
        standstill_time <- length(which(speed == 0))/length(speed)
        avg_speed_run <- mean(speed[which(speed > 0)], na.rm = T)
        sd_speed_run <- sd(speed[which(speed > 0)], na.rm = T)
        med_speed_run <- median(speed[which(speed > 0)], na.rm = T)
        
        ### Acceleration Features ###
        adceleration <- diff(speed) #speed change
        acceleration <- adceleration[which(adceleration >= 0.01)] #acceleration
        deceleration <- adceleration[which(adceleration <= -0.01)] #deceleration
        ex_acc <- adceleration[which(adceleration>= 3)] #extreme acceleration
        ex_dec <- adceleration[which(adceleration<= -3)] #extreme deceleration
        
        feature_acc <- generateDistribution(acceleration,'Acc',0.05)
        feature_dec <- generateDistribution(deceleration,'Dec',0.05)
        avg_acc <- mean(acceleration,na.rm = T)
        avg_dec <- mean(deceleration,na.rm = T)
        acc_time <- length(acceleration)/length(adceleration)
        dec_time <- length(deceleration)/length(adceleration)
        sd_acc <- sd(acceleration,na.rm = T)
        sd_dec <- sd(deceleration,na.rm = T)
        med_acc <- median(acceleration,na.rm = T)
        med_dec <- median(deceleration,na.rm = T)
        ex_acc_time <- length(ex_acc)/length(adceleration)
        ex_dec_time <- length(ex_dec)/length(adceleration)
        
        ### Extreme Driving Features ###
        high_speed <- length(which(speed > 22))/length(speed)
        low_speed <- length(which(speed < 2.6))/length(speed)
        inter_speed <- length(which(speed < 17 & speed > 7))/length(speed)
        
        ### Heading / Angle Features ###
        cur <- rollapply(calcCurvature(trip_data,2)[,3], width = 5, FUN = median, na.rm=T)
        r <- Cartesian_to_Polar(trip_data,2) #initial radius **calcCurvature()**
#         r <- rollapply(r, width = 5, FUN = median, na.rm=T) #radius & theta
        angle <- degree_cal(r,2) #angle(0-180)
        tp2 <- which(cur <= 100) #cur points
        tanAcc <- rollapply(calcTangAccel(trip_data,2), width = 5, FUN = median, na.rm=T) #tangential acceleration
        norAcc <- calcNormAccel(speed,cur) #normal acceleration
        #         norAcc[which(norAcc == Inf)] <- 0 #normal acceleration
        totAcc <- totalAccel(tanAcc,norAcc) #total acceleration
        
        feature_heading <- generateDistribution(angle,'heading', 0.05)
        feature_curvature <- curvatureDistribution(cur[tp2], 0.05) #cur[tp,3]
        feature_tanAcc <- generateDistribution(tanAcc,'tanAcc', 0.05)
        feature_norAcc <- generateDistribution(norAcc,'norAcc', 0.05)
        feature_totAcc <- generateDistribution(totAcc,'totAcc', 0.05)
        sd_tanAcc <- sd(tanAcc,na.rm = T)
        sd_norAcc <- sd(norAcc,na.rm = T)
        sd_cur <- sd(cur,na.rm = T)
        sd_totAcc <- sd(totAcc,na.rm = T)
        avg_tanAcc <- mean(tanAcc,na.rm = T)
        avg_norAcc <- mean(norAcc,na.rm = T)
        avg_cur <- mean(cur,na.rm = T)
        avg_totAcc <- mean(totAcc,na.rm = T)
        med_tanAcc <- median(tanAcc,na.rm = T)
        med_norAcc <- median(norAcc,na.rm = T)
        med_cur <- median(cur,na.rm = T)
        med_totAcc <- median(totAcc,na.rm = T)
        
        ### Turn Features ###
        tp <- which(angle >= 15) #turn points
        turn_speed <- speed[tp] #turn speed
        
        feature_turn_sp <- generateDistribution(turn_speed,'turn_sp',0.1)
        turn_time <- length(tp)/length(angle)       
        avg_turn_sp <- mean(turn_speed, na.rm = T)
        sd_turn_sp <- sd(turn_speed, na.rm = T)
        med_turn_sp <- median(turn_speed, na.rm = T)
        ex_turn <- length(which(turn_speed>=13.9))/length(tp)
        
        ### Insert Observation ###
        main_df[d_num,] <- c(driver,trip,trip_distance,driver_time,fly_distance,
                             t(feature_speed),avg_speed,sd_speed,med_speed,tot_curvature,standstill_time,avg_speed_run,sd_speed_run,med_speed_run,
                             t(feature_acc),t(feature_dec),avg_acc,avg_dec,acc_time,dec_time,sd_acc,sd_dec,med_acc,med_dec,ex_acc_time,ex_dec_time,
                             high_speed,low_speed,inter_speed,
                             t(feature_heading),t(feature_curvature),t(feature_tanAcc),t(feature_norAcc),t(feature_totAcc),sd_tanAcc,sd_norAcc,sd_cur,sd_totAcc,
                             avg_tanAcc,avg_norAcc,avg_cur,avg_totAcc,med_tanAcc,med_norAcc,med_cur,med_totAcc,
                             t(feature_turn_sp),turn_time,avg_turn_sp,sd_turn_sp,med_turn_sp,ex_turn,target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}

#######################
### OUTPUT_DATASETS ###
#######################
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip','trip_distance','driver_time','fly_distance',
                    names(feature_speed),'avg_speed','sd_speed','med_speed','tot_curvature','standstill_time','avg_speed_run','sd_speed_run','med_speed_run',
                    names(feature_acc),names(feature_dec),'avg_acc','avg_dec','acc_time','dec_time','sd_acc','sd_dec','med_acc','med_dec','ex_acc_time','ex_dec_time',
                    'high_speed','low_speed','inter_speed',
                    names(feature_heading),names(feature_curvature),names(feature_tanAcc),names(feature_norAcc),names(feature_totAcc),'sd_tanAcc','sd_norAcc','sd_cur','sd_totAcc',
                    'avg_tanAcc','avg_norAcc','avg_cur','avg_totAcc','med_tanAcc','med_norAcc','med_cur','med_totAcc',
                    names(feature_turn_sp),'turn_time','avg_turn_sp','sd_turn_sp','med_turn_sp','ex_turn','target')
# main_df[,67:76][which(main_df[,67:76]>100),] <- 101
save(main_df, file='data/main_df_214features_moving_avg.RData')
