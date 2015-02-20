rotational <- function(theta){
    rotation_mat <- matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),c(2,2),byrow = T)
    return (rotation_mat)
}

flip <- function(x){
    if(sum(sign(x[,2]))>0){
        x <- x %*% matrix(c(1,0,0,-1),c(2,2),byrow = T)
    }
    return(x)
}

rotate_trip <- function(trip){
    w0 <- atan2(trip[nrow(trip),1],trip[nrow(trip),2])
    trip_rotate <- trip %*% rotational(w0)
    return (trip_rotate)
}

getxy <- function(a){
    xy <- c(a[1]*cos(a[2]),a[1]*sin(a[2]))
    return(xy)
}

getwd <- function(a){
    xy <- c(sqrt(a[1]^2 + a[2]^2),
        atan2(a[2],a[1]))
    return(xy)
}

update_trips <- function(tripl,trip,tripname,tripcounter,tripother){
    if(nrow(tripl)==0){
        tripl[tripname] <- trip
        tripcounter[tripname] <- 0
        tripother[tripname] <- ''
    }else{
        for(t in tripl){
            if(sametrip(tripl[t],trip)){
                tripcounter[tripname] <- tripcounter[tripname]+1
                tripother[t] <- c(tripother[t], tripname)
                for (xx in tripother[t]){
                    tripcounter[xx] <- tripcounter[t]
                }
                return(list[tripl,tripcounter,tripother])
            }
        }
        tripl[tripname] <- trip
        tripcounter[tripname] <- 0
        tripother[tripname] <- []
    }
    return(list[tripl,tripcounter,tripother])
}

getdd <- function(tx,ty){
    gap <- nrow(tx)-nrow(ty)
    if (gap>0){
        
    }else if(gap<0){
        
    }else{
        result <- 'pass'
    }
    return(result)
}




### rotate + flip
path <- "data/drivers/"
driver <- 1
trip <- 183
files <- paste0(path, driver, '/', trip, ".csv")
trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
trip_data <- Kalman_Filter(trip_data,1,1,12.5) #Q_metres_per_second = 50*1000/3600

trip_data_rot <- rotate_trip(trip_data)

sum(sign(trip_data[,2]))
trip_data_flip <- flip(trip_data_rot)

par(mfcol=c(1,3))
plot(trip_data)
plot(trip_data_rot)
plot(trip_data_flip)