#setwd('H:/Machine_Learning/DTA')
#rm(list=ls());gc()

Kalman_Filter <- function(xy_measurement, accuracy=1, TimeInc_milliseconds = 1, Q_metres_per_second = 10){
    
    MinAccuracy <- 1
    variance <- -1
    
    for (i in 1:nrow(xy_measurement)){
        
        if (accuracy < MinAccuracy) accuracy <- MinAccuracy
        if (variance < 0) {
            # if variance < 0, object is unitialised, so initialise with current values
            lat <- xy_measurement[i,1]
            lng <- xy_measurement[i,2]
            variance <- accuracy*accuracy 
        } else {
            # else apply Kalman filter methodology
            if (TimeInc_milliseconds > 0) {
                # time has moved on, so the uncertainty in the current position increases
                variance <- variance + TimeInc_milliseconds * Q_metres_per_second * Q_metres_per_second / 1000;
                # TO DO: USE VELOCITY INFORMATION HERE TO GET A BETTER ESTIMATE OF CURRENT POSITION
            }
            # Kalman gain matrix K = Covarariance * Inverse(Covariance + MeasurementVariance)
            # NB: because K is dimensionless, it doesn't matter that variance has different units to lat and lng
            K <- variance / (variance + accuracy * accuracy);
            # apply K
            lat <- lat + K * (xy_measurement[i,1] - lat);
            lng <- lng + K * (xy_measurement[i,2] - lng);
            # new Covarariance  matrix is (IdentityMatrix - K) * Covarariance 
            variance <- (1 - K) * variance;
        }
        xy_measurement[i,1] <- lat
        xy_measurement[i,2] <- lng
    }
    
    return(xy_measurement)
}
# 
# trip_smooth <- Kalman_Filter(trip)
# 
# plot(trip)
# plot(Kalman_Filter(trip))
# 
# head(trip);head(trip_smooth)
