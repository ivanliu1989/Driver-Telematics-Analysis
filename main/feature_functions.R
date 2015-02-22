library(data.table);library(plotrix);library(data.table);library(parallel);library(caret)

files <- "data/drivers/1/1.csv"
trip_data <- data.matrix(read.csv(files,header = T,stringsAsFactor=F))

# Total Distance
distance <- function(trip,nlag=1){
    dx <- diff(trip[,1],lag=nlag,differences=1)
    dy <- diff(trip[,2],lag=nlag,differences=1)
    delta_dist <- sqrt(dx^2 + dy^2)
    dist = sum(delta_dist)
    return(dist)
}

# Speed
calcSpeed <- function(trip,nlag=1) {
    dx <- diff(trip[,1],lag=nlag,differences=1)
    dy <- diff(trip[,2],lag=nlag,differences=1)
    speed = sqrt(dx^2 + dy^2)/nlag
    rtn = c(rep(NA,nlag),speed)
    return(rtn)
}

# Distribution
generateDistribution <- function(x,name) {
    x_wo_na <- x[!is.na(x)]
    qdist <- seq(0.05,1, by = 0.05)
    if (length(x_wo_na)<(2*length(qdist))) {
        dist <- quantile(x_wo_na, qdist)
    } else {
        x_wo_peaks <- x_wo_na[abs(x_wo_na-mean(x_wo_na,na.rm = TRUE)) 
                              < 5*sd(x_wo_na,na.rm = TRUE)]
        dist <- quantile(x_wo_peaks, qdist)
    }
    names(dist) = paste(name,names(dist),sep='_')
    names(dist) = gsub("%", "_pct", names(dist))
    return(dist)
}

# Tangential acceleration
calcTangAccel <- function(trip,nlag=1) {
    dx2 <- diff(trip[,1],lag=nlag,differences=2)
    dy2 <- diff(trip[,2],lag=nlag,differences=2)
    accel_fps2 = 3.28084*sqrt(dx2^2 + dy2^2)/nlag
    accel_fps2 = c(rep(NA,2*nlag),accel_fps2)
    return(accel_fps2)
}

# Normal acceleration
calcNormAccel <- function(sp,cur,nlag) {
    accel_fps2 = (sp^2) / cur[,3]
    return(accel_fps2)
}

# Total acceleration 
totalAccel <- function(accel_fps2_tang,accel_fps2_norm){
    accel_fps2 = sqrt(accel_fps2_tang^2 + accel_fps2_norm^2)
    return(accel_fps2)  
}

# Curvature
calcCurvature <- function(trip,nlag) {
    ib=seq(2,nrow(trip)-1)
    ia=ib-1
    ic=ib+1
    A_x = trip[ia,1]
    B_x = trip[ib,1]
    C_x = trip[ic,1]
    A_y = trip[ia,2]
    B_y = trip[ib,2]
    C_y = trip[ic,2]
    D = 2 * (A_x*(B_y - C_y) + B_x*(C_y - A_y) + C_x*(A_y - B_y) )
    U_x = ((A_x^2 + A_y^2) * (B_y - C_y) + (B_x^2 + B_y^2) * (C_y - A_y) + (C_x^2 + C_y^2) * (A_y - B_y)) / D
    U_y = ((A_x^2 + A_y^2) * (C_x - B_x) + (B_x^2 + B_y^2) * (A_x - C_x) + (C_x^2 + C_y^2) * (B_x - A_x)) / D
    R = sqrt((A_x - U_x)^2 + (A_y - U_y)^2)
    
    mlag <- nlag
    if (nlag %% 2 == 0) {
        mlag <- nlag + 1
    }
    f21 <- rep(1/mlag,mlag)
    smth_x <- filter(U_x, f21, sides=2)
    smth_y <- filter(U_y, f21, sides=2)
    smth_R <- filter(R, f21, sides=2)
    cur_smooth <- data.matrix(data.table(center_x = c(NA, smth_x, NA),center_y = c(NA, smth_y, NA),
                                         radius = c(NA, smth_R, NA)))
    return(cur_smooth)
}

# Curvature Distribution
curvatureDistribution <- function(cur){
    radius = cur[,3]
    values <- radius[is.finite(radius)]
    tryCatch(rtn<-generateDistribution(values,'cur'), 
             error = function(e) {
                 e
                 generateDistribution(values,'cur')
                 rtn<-NULL
             }
    )
    return(rtn)  
}

# Speed Outliers
removeOutliers <- function(speed, limits=9.8){
    while(length(which(diff(speed,rm.na=T)>limits))>0) {
        outlier <- which(diff(speed,rm.na=T)>limits)
        #print(outlier)
        for (i in outlier){
            speed[i+1] <- speed[i] + max(diff(speed,rm.na=T)[i-1],9.5) #median(speed[(i-2):(i+2)], na.rm = T)
        }
    }
    return(speed)
} 

# Acceleration Outliers
removeAccOutliers <- function(totAcc,tanAcc,norAcc, limits=9.8){
    while(length(which(totAcc>limits))>0) {
        outlier <- which(totAcc>limits)
        #print(outlier)
        for (i in outlier){
            totAcc[i] <- totAcc[i-1]
            tanAcc[i] <- sqrt(totAcc[i]^2-norAcc[i]^2)
        }
    }
    acc <- data.matrix(data.table(totAcc = totAcc,tanAcc = tanAcc, norAcc = norAcc))
    return(acc)
} 

# # Cartesian to Polar coordinates
# Cartesian_to_Polar <- function(trip,nlag){
#     r <- sqrt(diff(trip[,2],lag=nlag)^2 + diff(trip[,1],lag=nlag)^2)
#     theta <- atan2(diff(trip[,2],lag=nlag),diff(trip[,1],lag=nlag))
#     Polar <- matrix(0, nrow = length(theta), ncol = 2, dimnames = list(NULL, c("r", "theta")))
#     Polar[,1] <- r
#     Polar[,2] <- theta
#     return(Polar)
# }
# 
# # Heading degree
# degree_cal <- function(polar){
#     degrees <- abs(polar[,2] * 180 / pi)
#     return(degrees)
# }

