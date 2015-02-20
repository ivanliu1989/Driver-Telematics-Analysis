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

getdd <- function(tx,ty){
    gap <- nrow(tx)-nrow(ty)
    if (gap>0){
        ty <- data.matrix(rbind(data.frame(ty),data.frame(X1=rep(ty[nrow(ty),1],gap),X2=rep(ty[nrow(ty),1],gap))))
    }else if(gap<0){
        tx <- data.matrix(rbind(data.frame(tx),data.frame(X1=rep(tx[nrow(tx),1],-gap),X2=rep(tx[nrow(tx),1],-gap))))
    }else{
        result <- 'pass'
    }
    result <- sum(sqrt((ty[,1]-tx[,1])^2+(ty[,2]-tx[,2])^2))
    return(result)
}

sametrip <- function(tx,ty){
    mm <- nrow(tx)*threshold
    txx <- data.matrix(rbind(data.frame(tx[c(mm:nrow(tx)),]),data.frame(tx[-c(mm:nrow),])))
    dd <- getdd(tx,txx)
    return(getdd(tx,ty)<=dd)
}

checktrip <- function(tripcounter,tripname){
    return (tripcounter[tripname]>2)
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
        tripother[tripname] <- ''
    }
    return(list[tripl,tripcounter,tripother])
}


#####################
### rotate + flip ###
#####################
path <- "data/drivers/"
driver <- 2312
trip <- 123
files <- paste0(path, driver, '/', trip, ".csv")
trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
trip_data <- Kalman_Filter(trip_data,1,1,12.5) #Q_metres_per_second = 50*1000/3600

trip_data_rot <- rotate_trip(trip_data)

sum(sign(trip_data[,2]))
trip_data_flip <- flip(trip_data_rot)

par(mfcol=c(1,3))
plot(trip_data)
cur <- calcCurvature(trip_data,1)
a <- trip_data[which(cur[,3]<=100),]
points(a,col='blue')  

plot(trip_data_rot)
cur <- calcCurvature(trip_data_rot,1)
a <- trip_data_rot[which(cur[,3]<=100),]
points(a,col='blue')

plot(trip_data_flip)
cur <- calcCurvature(trip_data_flip,1)
a <- trip_data_flip[which(cur[,3]<=100),]
points(a,col='blue') 


### compare ###
t1 <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
t1 <- Kalman_Filter(t1,1,1,12.5) #Q_metres_per_second = 50*1000/3600
t1 <- rotate_trip(t1)
t1 <- flip(t1)

t2 <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
t2 <- Kalman_Filter(t2,1,1,12.5) #Q_metres_per_second = 50*1000/3600
t2 <- rotate_trip(t2)
t2 <- flip(t2)

threshold <- 5
getdd(t1,t2)


###
par(mfcol=c(3,3))
files <- paste0(path, driver, '/', trip, ".csv")
for(trip in sample(1:200,9)){
    files <- paste0(path, driver, '/', trip, ".csv")
    trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
    trip_data <- Kalman_Filter(trip_data,1,1,12.5) #Q_metres_per_second = 50*1000/3600
    trip_data_rot <- rotate_trip(trip_data)
    
    sum(sign(trip_data[,2]))
    trip_data_flip <- flip(trip_data_rot)
#     plot(trip_data)
#     cur <- calcCurvature(trip_data,1)
#     a <- trip_data[which(cur[,3]<=100),]
#     points(a,col='blue')  
#     
#     plot(trip_data_rot)
#     cur <- calcCurvature(trip_data_rot,1)
#     a <- trip_data_rot[which(cur[,3]<=100),]
#     points(a,col='blue')
#     
    plot(trip_data_flip)
    cur <- calcCurvature(trip_data_flip,1)
    a <- trip_data_flip[which(cur[,3]<=100),]
    points(a,col='red') 
}