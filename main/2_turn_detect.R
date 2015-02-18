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
    
    plot(speed)
    points(speed[which(cur[,3]<=100)],col='red')
    #lines(a,type = 'o',col='red')    
    
}


driver trip
83       1   83
102      1  102
120      1  120
136      1  136
148      1  148
183      1  183
200      1  200
345      2  145
393      2  193
551      3  151
589      3  189