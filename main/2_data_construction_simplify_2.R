setwd('/Users/ivan/Work_directory/DTA')
# setwd('H:/Machine_Learning/DTA')
# setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(data.table);require(zoo);
# source('Driver-Telematics-Analysis/main/kalman_filtering.R')
source('Driver-Telematics-Analysis/main/feature_functions.R')

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
trips <- 1:200
path <- "data/drivers/"

### main ###
date(); d_num <- 0
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 189, dimnames = list(NULL, NULL))
sp_limits <- 280/3.6
for (driver in drivers){
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        
        # target
        target <- 0
        # speed
        trip_distance <- distance(trip_data)
        jp <- detectJumps(calcSpeed(trip_data),sp_limits)
        speed <- calcSpeed(trip_data)[jp]
        feature_speed <- generateDistribution(speed, 'speed')
        # sd_speed
        sd_speed <- sd(speed, na.rm=T)
        # avg_speed
        avg_speed <- mean(speed, na.rm = T)
        # avg_speed_stop
        avg_speed_stop <- mean(speed[which(speed > 0)], na.rm = T) # zero 
        # drive_time
#         drive_time <- nrow(trip_data)
        # standstill_time
        standstill_time <- length(which(speed == 0))/length(speed)
        
        # Tangential/Normal acceleration | Curvature
        cur <- calcCurvature(trip_data,1)[jp,] # zero
#         cur <- Cartesian_to_Polar(trip_data,5)[jp,]
#         degree <- degree_cal(cur)
#         r <- Cartesian_to_Polar(trip_data,1)[jp]
        angle <- degree_cal(Cartesian_to_Polar(trip_data,2),2)[jp]
#         tp <- which(cur[,3] <= 100)
        tp <- which(angle >= 15) # plot(trip_data);points(trip_data[jp,][tp,],col='red')
        
        tanAcc <- calcTangAccel(trip_data,1)[jp][tp] #
        norAcc <- calcNormAccel(speed[tp],cur[tp,3]) # zero
        totAcc <- totalAccel(tanAcc,norAcc) # zero
#         smoothAcc <- removeAccOutliers2(totAcc,tanAcc,norAcc,9.8) # smooth three Acc
#         tanAcc <- smoothAcc[,1] #
#         norAcc <- smoothAcc[,2] #
#         totAcc <- smoothAcc[,3] #
        
        feature_tanAcc <- generateDistribution(tanAcc,'tanAcc')
        feature_norAcc <- generateDistribution(norAcc,'norAcc')
        feature_totAcc <- generateDistribution(totAcc,'totAcc')
#         feature_curvature <- curvatureDistribution(cur[tp,3])
        feature_heading <- generateDistribution(angle,'heading')
        
        sd_tanAcc <- sd(tanAcc,na.rm = T)
        sd_norAcc <- sd(norAcc,na.rm = T)
        sd_cur <- sd(cur[tp,3],na.rm = T)
        sd_totAcc <- sd(totAcc,na.rm = T)
        avg_tanAcc <- mean(tanAcc,na.rm = T)
        avg_norAcc <- mean(norAcc,na.rm = T)
        avg_cur <- mean(cur[tp,3],na.rm = T)
        avg_totAcc <- mean(totAcc,na.rm = T)
        
        # acceleration/deceleration
        # totalAcc <- sqrt(tanAcc^2 + norAcc^2)
        adceleration <- diff(calcSpeed(trip_data))[jp] #
        acceleration <- adceleration[which(adceleration>= 0.01)] #
        deceleration <- adceleration[which(adceleration<= -0.01)] #
        constant <- adceleration[which(abs(adceleration) < 0.01)] #
        #         acceleration <- totAcc[which(totAcc > 3)] #
        
        
        # acceleration/deceleration features
        avg_acc <- mean(acceleration,na.rm = T)
        avg_dec <- mean(deceleration,na.rm = T)
        feature_acc <- generateDistribution(acceleration,'Acc')
        feature_dec <- generateDistribution(deceleration,'Dec')
        sd_acc <- sd(acceleration,na.rm = T)
        sd_dec <- sd(deceleration,na.rm = T)
        cons_time <- length(constant)/length(adceleration)
        dec_time <- length(deceleration)/length(adceleration)
        acc_time <- length(acceleration)/length(adceleration)
        #         cons_time <- length(constant)/length(adceleration)
        ex_acc <- adceleration[which(adceleration>= 3)]
        ex_acc_time <- length(ex_acc)/length(adceleration)
        ex_dec <- adceleration[which(adceleration<= -3)]
        ex_dec_time <- length(ex_dec)/length(adceleration)
        
        # circular statistics
        #         turn_point <- trip_data[which(cur[,3]<=100),]
        turn_speed <- speed[tp] #
        
        # turning speed/radius/turning point/ex_turning/centrifugal acceleration\
        ex_turn <- length(which(turn_speed>=13.9))/length(tp)
        turn_point_mean <- length(tp)/length(angle)
        feature_turn_sp <- generateDistribution(turn_speed,'turn_sp')
        avg_turn_sp <- mean(turn_speed, na.rm = T)
        sd_turn_sp <- sd(turn_speed, na.rm = T)
        
        # df
        main_df[d_num,] <- c(driver,trip,trip_distance,t(feature_speed),sd_speed,avg_speed,avg_speed_stop,standstill_time,
                             t(feature_tanAcc),t(feature_norAcc),t(feature_totAcc),t(feature_heading),sd_tanAcc,sd_norAcc,sd_cur,sd_totAcc,
                             avg_tanAcc,avg_norAcc,avg_cur,avg_totAcc,avg_acc,avg_dec,t(feature_acc),t(feature_dec),sd_acc,sd_dec,
                             cons_time,dec_time,acc_time,ex_acc_time,ex_dec_time,ex_turn,turn_point_mean,t(feature_turn_sp),avg_turn_sp,sd_turn_sp,target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}
list <- c()
for (i in 1:189){
    if(sum(is.na(main_df[,i]))>0){
        print(i)
        list <- c(list, i)
    }   
}
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip','trip_distance',names(feature_speed),'sd_speed','avg_speed','avg_speed_stop','standstill_time',
                    names(feature_tanAcc),names(feature_norAcc),names(feature_totAcc),names(feature_heading),'sd_tanAcc','sd_norAcc','sd_cur','sd_totAcc',
                    'avg_tanAcc','avg_norAcc','avg_cur','avg_totAcc','avg_acc','avg_dec',names(feature_acc),names(feature_dec),'sd_acc','sd_dec',
                    'cons_time','dec_time','acc_time','ex_acc_time','ex_dec_time','ex_turn','turn_point_mean',names(feature_turn_sp),'avg_turn_sp','sd_turn_sp','target')

dim(main_df);head(main_df)

save(main_df, file='data/main_df_189features.RData')
write.csv(main_df, file = 'data/main_df_190features.csv', quote = F, row.names = F)
