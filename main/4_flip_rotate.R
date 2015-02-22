setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
source('Driver-Telematics-Analysis/main/kalman_filtering.R')
source('Driver-Telematics-Analysis/main/feature_functions.R')

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
    w0 <- atan2(trip[nrow(trip),2],trip[nrow(trip),1])
    trip_rotate <- trip %*% rotational(w0)
    return (trip_rotate)
}

### same trip ###
# getxy <- function(a){
#     xy <- c(a[1]*cos(a[2]),a[1]*sin(a[2]))
#     return(xy)
# }
# 
# getwd <- function(a){
#     xy <- c(sqrt(a[1]^2 + a[2]^2),
#         atan2(a[2],a[1]))
#     return(xy)
# }

getdd <- function(tx,ty){
    gap <- nrow(tx)-nrow(ty)
    if (gap>0){
        ty <- data.matrix(rbind(data.frame(ty),data.frame(X1=rep(ty[nrow(ty),1],gap),X2=rep(ty[nrow(ty),2],gap))))
    }else if(gap<0){
        tx <- data.matrix(rbind(data.frame(tx),data.frame(X1=rep(tx[nrow(tx),1],-gap),X2=rep(tx[nrow(tx),2],-gap))))
    }
    result <- sum(sqrt((ty[,1]-tx[,1])^2+(ty[,2]-tx[,2])^2))
    return(result)
}

sametrip <- function(tx,ty){
    if((abs(nrow(tx)-nrow(ty))/max(nrow(tx),nrow(ty))>0.8)&(abs(distance(tx)-distance(ty))/max(distance(tx),distance(ty))>0.8)){
        mm <- as.integer(nrow(tx)*threshold)
        n <- nrow(tx)
        txx <- tx
        txx[1:(n-mm),] <- tx[(mm+1):n,]
        txx[(n-mm+1):n,] <- tx[(n-mm+1):n,]
        dd <- getdd(tx,txx)
        return(getdd(tx,ty)<=dd)
    }else{
        return(FALSE)
    }
}

### other functions ###
# checktrip <- function(tripcounter,tripname){
#     return (tripcounter[tripname]>2)
# }
# 
# update_trips <- function(tripl,trip,tripname,tripcounter,tripother){
#     if(nrow(tripl)==0){
#         tripl[tripname] <- trip
#         tripcounter[tripname] <- 0
#         tripother[tripname] <- ''
#     }else{
#         for(t in tripl){
#             if(sametrip(tripl[t],trip)){
#                 tripcounter[tripname] <- tripcounter[tripname]+1
#                 tripother[t] <- c(tripother[t], tripname)
#                 for (xx in tripother[t]){
#                     tripcounter[xx] <- tripcounter[t]
#                 }
#                 return(list[tripl,tripcounter,tripother])
#             }
#         }
#         tripl[tripname] <- trip
#         tripcounter[tripname] <- 0
#         tripother[tripname] <- ''
#     }
#     return(list[tripl,tripcounter,tripother])
# }


#####################
### rotate + flip ###
#####################
# path <- "data/drivers/"
# driver <- 2212
# trip <- 123
# files <- paste0(path, driver, '/', trip, ".csv")
# trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
# trip_data <- Kalman_Filter(trip_data,1,1,12.5) #Q_metres_per_second = 50*1000/3600
# 
# trip_data_rot <- rotate_trip(trip_data)
# 
# sum(sign(trip_data[,2]))
# trip_data_flip <- flip(trip_data_rot)
# 
# par(mfcol=c(3,1))
# plot(trip_data)
# cur <- calcCurvature(trip_data,1)
# a <- trip_data[which(cur[,3]<=100),]
# points(a,col='blue')  
# 
# plot(trip_data_rot)
# cur <- calcCurvature(trip_data_rot,1)
# a <- trip_data_rot[which(cur[,3]<=100),]
# points(a,col='blue')
# 
# plot(trip_data_flip)
# cur <- calcCurvature(trip_data_flip,1)
# a <- trip_data_flip[which(cur[,3]<=100),]
# points(a,col='blue') 
# 
# 
# ### compare ###
# t1 <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
# t1 <- Kalman_Filter(t1,1,1,12.5) #Q_metres_per_second = 50*1000/3600
# t1 <- rotate_trip(t1)
# t1 <- flip(t1)
# 
# t2 <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
# t2 <- Kalman_Filter(t2,1,1,12.5) #Q_metres_per_second = 50*1000/3600
# t2 <- rotate_trip(t2)
# t2 <- flip(t2)
# 
# threshold <- 5
# getdd(t1,t2)
# 
# 
# ### plot ###
# par(mfcol=c(3,3))
# files <- paste0(path, driver, '/', trip, ".csv")
# for(trip in sample(1:200,9)){
#     files <- paste0(path, driver, '/', trip, ".csv")
#     trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
#     trip_data <- Kalman_Filter(trip_data,1,1,12.5) #Q_metres_per_second = 50*1000/3600
#     trip_data_rot <- rotate_trip(trip_data)
#     
#     sum(sign(trip_data[,2]))
#     trip_data_flip <- flip(trip_data_rot)
# #     plot(trip_data)
# #     cur <- calcCurvature(trip_data,1)
# #     a <- trip_data[which(cur[,3]<=100),]
# #     points(a,col='blue')  
# #     
# #     plot(trip_data_rot)
# #     cur <- calcCurvature(trip_data_rot,1)
# #     a <- trip_data_rot[which(cur[,3]<=100),]
# #     points(a,col='blue')
# #     
#     plot(trip_data_flip)
#     cur <- calcCurvature(trip_data_flip,1)
#     a <- trip_data_flip[which(cur[,3]<=100),]
#     points(a,col='red') 
# }

####################
### test functions #
####################
# path <- "data/drivers/"
# driver <- 2212
# trip <- 123
# files <- paste0(path, driver, '/', trip, ".csv")
# tx <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
# tx <- Kalman_Filter(tx,1,1,12.5) #Q_metres_per_second = 50*1000/3600
# 
# driver <- 2212
# trip <- 153
# files <- paste0(path, driver, '/', trip, ".csv")
# ty <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
# ty <- Kalman_Filter(ty,1,1,12.5) #Q_metres_per_second = 50*1000/3600
# 
# tx <- rotate_trip(tx)
# sum(sign(tx[,2]))
# tx <- flip(tx)
# 
# ty <- rotate_trip(ty)
# sum(sign(ty[,2]))
# ty <- flip(ty)
# 
# par(mfcol = c(2,1))
# plot(tx);plot(ty)
# 
# getdd(tx,ty)
# sametrip(tx,ty)

########################
### Detect Same Trip ###
########################
threshold <- 0.02
d_num <- 0
match_matrix <- matrix(0, nrow = length(drivers)*100, ncol = 3, dimnames = list(NULL, NULL))
start <- date()
print(start)
for(driver in drivers){
    print(date())
    for (trip in 1:200){
        if(trip >= 200){
            break
        }
        
        # print(paste0('Driver: ', driver, ' Trip: ', trip))
        files <- paste0(datadirectory, driver, '/', trip, ".csv")
        tx <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        # tx <- Kalman_Filter(tx,1,1,12.5) #Q_metres_per_second = 50*1000/3600
        tx <- rotate_trip(tx)
        tx <- flip(tx)
        
        for(other in c((trip+1):200)){
            files <- paste0(datadirectory, driver, '/', other, ".csv")
            ty <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
            # ty <- Kalman_Filter(ty,1,1,12.5) #Q_metres_per_second = 50*1000/3600
            ty <- flip(rotate_trip(ty))
            
            if(sametrip(tx,ty)){
                d_num <- d_num + 1
                print(paste0('Driver ',driver,' Trips Match: ', trip, ' | ', other, '!!!'))
                plot(tx);points(ty,col='red')
                match_matrix[d_num,] <- c(driver,trip,other)    
            }        
        }
    }
}



