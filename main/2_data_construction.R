# setwd('/Users/ivan/Work_directory/DTA')
# setwd('H:/Machine_Learning/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
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
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 136, dimnames = list(NULL, NULL))
for (driver in drivers){
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        trip_data <- Kalman_Filter(trip_data,1,1,12.5) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        trip_distance <- distance(trip_data)
        speed <- removeOutliers(calcSpeed(trip_data),9.8)
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
        standstill_time <- length(which(speed < 0.1))/drive_time
        
        # Tangential/Normal acceleration | Curvature
        cur <- calcCurvature(trip_data,1) #
        tanAcc <- calcTangAccel(trip_data) #
        norAcc <- calcNormAccel(speed,cur) #
        totAcc <- totalAccel(tanAcc,norAcc) #
        smoothAcc <- removeAccOutliers(totAcc,tanAcc,norAcc,9.8) # smooth three Acc
        tanAcc <- smoothAcc[,1] #
        norAcc <- smoothAcc[,2] #
        totAcc <- smoothAcc[,3] #
        
        feature_tanAcc <- generateDistribution(tanAcc,'tanAcc')
        feature_norAcc <- generateDistribution(norAcc,'norAcc')
        feature_totAcc <- generateDistribution(totAcc,'totAcc')
        feature_curvature <- curvatureDistribution(cur)
        sd_tanAcc <- sd(tanAcc,na.rm = T)
        sd_norAcc <- sd(norAcc,na.rm = T)
        sd_cur <- sd(cur[which(cur[,3]<=100),3],na.rm = T)
        sd_totAcc <- sd(totAcc,na.rm = T)
        
        # acceleration/deceleration
        # totalAcc <- sqrt(tanAcc^2 + norAcc^2)
#         adceleration <- diff(speed) 
#         acceleration <- adceleration[which(adceleration>= 0.1)]
#         deceleration <- adceleration[which(adceleration<= -0.1)]
        constant <- totAcc[which(totAcc < 0.1)] #
        ex_acc <- totAcc[which(totAcc > 3)] #
        
        
        # acceleration/deceleration features
#         avg_acc <- mean(acceleration)
#         avg_dec <- mean(deceleration)
#         feature_acc <- generateDistribution(acceleration,'Acc')
#         feature_dec <- generateDistribution(deceleration,'Dec')
#         sd_acc <- sd(acceleration,na.rm = T)
#         sd_dec <- sd(deceleration,na.rm = T)
        cons_time <- length(constant)/length(totAcc)
#         dec_time <- length(deceleration)/length(adceleration)
#         cons_time <- length(constant)/length(adceleration)
        ex_acc_time <- length(ex_acc)/length(totAcc)
#         ex_dec_time <- length(ex_dec)/length(adceleration)
        
        # circular statistics
#         turn_point <- trip_data[which(cur[,3]<=100),]
        turn_speed <- speed[which(cur[,3]<=100)] #
        
        # turning speed/radius/turning point/ex_turning/centrifugal acceleration\
        ex_turn <- length(which(turn_speed>=13.9))/length(which(cur[,3]<=100))
        turn_point_mean <- length(which(cur[,3]<=100))/length(cur[,3])
        feature_turn_sp <- generateDistribution(turn_speed,'turn_sp')

        # df
        main_df[d_num,] <- c(driver,trip,trip_distance,t(feature_speed),sd_speed,avg_speed,drive_time,standstill_time,
                             t(feature_tanAcc),t(feature_norAcc),t(feature_totAcc),t(feature_curvature),sd_tanAcc,sd_norAcc,sd_cur,sd_totAcc,
                             cons_time,ex_acc_time,ex_turn,turn_point_mean,t(feature_turn_sp),target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}
for (i in 1:136){
    if(sum(is.na(main_df[,i]))>0){
        print(i)
    }   
}
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip','trip_distance',names(feature_speed),'sd_speed','avg_speed','drive_time','standstill_time',
                    names(feature_tanAcc),names(feature_norAcc),names(feature_totAcc),names(feature_curvature),'sd_tanAcc','sd_norAcc','sd_cur',
                    'avg_acc','avg_dec',names(feature_acc),names(feature_dec),'sd_acc','sd_dec','acc_time','dec_time','cons_time','ex_acc_time','ex_dec_time',
                    'ex_turn','turn_point_mean',names(feature_turn_sp),'target')
dim(main_df);head(main_df)

save(main_df, file='data/main_df_136features.RData')
write.csv(main_df, file = 'data/main_df_136features.csv', quote = F, row.names = F)
