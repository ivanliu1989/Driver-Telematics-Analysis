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
main_df <- matrix(0, nrow = length(drivers)*200, ncol = 83, dimnames = list(NULL, NULL))
for (driver in drivers){
    for (trip in trips){
        
        d_num <- d_num + 1
        files <- paste0(path, driver, '/', trip, ".csv")
        trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        #trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
        
        # target
        target <- 0
        # speed
        dist = sqrt(diff(trip_data[,1])^2 + diff(trip_data[,2])^2) # distance
        smoothDist <- rollapply(dist, width = 5, FUN = median) # rolling median smooth
        speed <- smoothDist
        feature_speed <- generateDistribution(speed, 'speed') # 
        #         sd_speed <- sd(speed, na.rm=T)
        
        # Tangential/Normal acceleration | Curvature
        cur <- calcCurvature(trip_data,1) # zero
        cur <- rollapply(cur[,3], width = 5, FUN = median,na.rm = TRUE)
        tanAcc <- calcTangAccel(trip_data) #
        tanAcc <- rollapply(tanAcc, width = 5, FUN = median,na.rm = TRUE)
        
        norAcc <- calcNormAccel2(speed,cur[-1]) # zero
        totAcc <- totalAccel(tanAcc[-1],norAcc) # zero
        
        feature_tanAcc <- generateDistribution(tanAcc,'tanAcc')
        feature_norAcc <- generateDistribution(norAcc,'norAcc')
        feature_totAcc <- generateDistribution(totAcc,'totAcc')
        #         feature_curvature <- curvatureDistribution2(cur)
        #         sd_tanAcc <- sd(tanAcc,na.rm = T)
        #         sd_norAcc <- sd(norAcc,na.rm = T)
        #         sd_cur <- sd(cur[which(cur<=100)],na.rm = T)
        #         sd_totAcc <- sd(totAcc,na.rm = T)
        
        # heading
        #polar <- Cartesian_to_Polar(trip_data,5)
        #heading <- degree_cal(polar)
        
        # acceleration
        #         adceleration <- diff(speed)
        #         acceleration <- adceleration[which(adceleration>= 0.01)] #
        #         deceleration <- adceleration[which(adceleration<= -0.01)] #
        #         constant <- adceleration[which(abs(adceleration) < 0.01)] #
        #         feature_acceleration <- generateDistribution(acceleration, 'acceleration')
        #         feature_dec <- generateDistribution(deceleration,'deceleration')
        #         sd_acc <- sd(acceleration,na.rm = T)
        #         sd_dec <- sd(deceleration,na.rm = T)
        #         ex_acc <- adceleration[which(adceleration>= 3)]
        #         ex_acc_time <- length(ex_acc)/length(totAcc)
        #         ex_dec <- adceleration[which(adceleration<= -3)]
        #         ex_dec_time <- length(ex_dec)/length(adceleration)
        
        #         turn_speed <- speed[which(cur<=100)] #
        
        # turning speed/radius/turning point/ex_turning/centrifugal acceleration\
        #         ex_turn <- length(which(turn_speed>=13.9))/length(which(cur<=100))
        #         turn_point_mean <- length(which(cur<=100))/length(cur)
        #         feature_turn_sp <- generateDistribution(turn_speed,'turn_sp')
        #         sd_turn_sp <- sd(turn_speed, na.rm = T)
        
        # df
        main_df[d_num,] <- c(driver,trip,t(feature_speed),t(feature_tanAcc),t(feature_norAcc),
                             t(feature_totAcc),
                             target)
        
        if (trip==200) {
            print(paste0(files, ' | ', date(), ' | ', d_num/(length(drivers)*200)*100)) 
        }
    }
}

main_df <- data.frame(main_df,stringsAsFactors = F)
names(main_df) <- c('driver','trip',names(feature_speed),names(feature_tanAcc),names(feature_norAcc),
                    names(feature_totAcc), 
                    'target')

save(main_df, file='data/main_df_83.RData')
