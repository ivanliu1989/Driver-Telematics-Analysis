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
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 109, dimnames = list(NULL, NULL))
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
        
        
        # df
        main_df[d_num,] <- c(driver,trip,trip_distance,t(feature_speed),sd_speed,avg_speed,standstill_time,t(feature_tanAcc),t(feature_norAcc),
                             t(feature_totAcc),t(feature_curvature),sd_tanAcc,sd_norAcc,target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}
for (i in 1:218){
    if(sum(is.na(main_df[,i]))>0){
        print(i)
    }   
}
main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip','trip_distance',names(feature_speed),'sd_speed','avg_speed','standstill_time',names(feature_tanAcc),names(feature_norAcc),
                    names(feature_totAcc),names(feature_curvature),'sd_tanAcc','target')
dim(main_df);head(main_df)

save(main_df, file='data/main_df_109features.RData')
write.csv(main_df, file = 'data/main_df_109features.csv', quote = F, row.names = F)
