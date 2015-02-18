driver <- 1223
trip <- sample(1:200,9)

par(mfcol=c(3,3))
for (i in trip){
    files <- paste0(path, driver, '/', i, ".csv")
    trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
    trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
    
    cur <- calcCurvature(trip_data,1)
    
    a <- trip_data[which(cur[,3]<=100),]
    plot(trip_data)
    lines(a,type = 'o',col='red')    
    
}

for (i in trip){
    files <- paste0(path, driver, '/', i, ".csv")
    trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
    trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
    
    cur <- calcCurvature(trip_data,1)
    speed <- calcSpeed(trip_data)
    
    a <- speed[-3,][which(cur[,3]<=100),]
    plot(a)
    #lines(a,type = 'o',col='red')    
    
}