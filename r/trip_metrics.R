library(data.table)
library(plotrix)
library(data.table)
library(parallel)
library(caret)

calcSpeed <- function(trip,nlag=NULL) {
  dx <- diff(trip$x,lag=nlag,differences=1)
  dy <- diff(trip$y,lag=nlag,differences=1)
  speed = sqrt(dx^2 + dy^2)/nlag
  rtn = c(rep(NA,nlag),speed)
  return(rtn)
}
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
  names(dist) = gsub("%", "_pcnt", names(dist))
  return(dist)
}
speedDistribution <- function(trip,nlag)
{
  speed_fps = calcSpeed(trip,nlag)
  return(generateDistribution(speed_fps,'speed'))  
}
calcTangAccel <- function(trip,nlag=NULL) {
  dx2 <- diff(trip$x,lag=nlag,differences=2)
  dy2 <- diff(trip$y,lag=nlag,differences=2)
  accel_fps2 = 3.28084*sqrt(dx2^2 + dy2^2)/nlag
  accel_fps2 = c(rep(NA,2*nlag),accel_fps2)
  return(accel_fps2)
}
TangAccelDistribution <- function(trip,nlag)
{
  accel_fps2 = calcTangAccel(trip,nlag)
  return(generateDistribution(accel_fps2,'tang_accel'))  
}
calcNormAccel <- function(trip,nlag) {
  sp <- calcSpeed(trip,nlag)
  cur <- calcCurvature(trip,nlag)
  accel_fps2 = sp / cur$radius
  return(accel_fps2)
}
NormAccelDistribution <- function(trip,nlag)
{
  accel_fps2 = calcNormAccel(trip,nlag)
  return(generateDistribution(accel_fps2,'norm_accel'))  
}
TotalAccelDistribution <- function(trip,nlag)
{
  accel_fps2_tang = calcTangAccel(trip,nlag)
  accel_fps2_norm = calcNormAccel(trip,nlag)
  accel_fps2 = accel_fps2_tang + accel_fps2_norm
  return(generateDistribution(accel_fps2,'total_accel'))  
}
calcCurvature <- function(trip,nlag) {
  # kt = (d2y/dt2) / (1+(dy/dt)^2)^(3/2)
  #
  # ===Cartesian coordinates====
  # ref: http://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates
  # The [[Cartesian coordinates]] of the circumcenter are 
  # U_x = [(A_x^2 + A_y^2)(B_y - C_y) + (B_x^2 + B_y^2)(C_y - A_y) + (C_x^2 + C_y^2)(A_y - B_y)] / D
  # U_y = [(A_x^2 + A_y^2)(C_x - B_x) + (B_x^2 + B_y^2)(A_x - C_x) + (C_x^2 + C_y^2)(B_x - A_x)]/ D
  # with  
  # D = 2 [A_x(B_y - C_y) + B_x(C_y - A_y) + C_x(A_y - B_y) ]
  ib=seq(2,nrow(trip)-1)
  ia=ib-1
  ic=ib+1
  A_x = trip$x[ia]
  B_x = trip$x[ib]
  C_x = trip$x[ic]
  A_y = trip$y[ia]
  B_y = trip$y[ib]
  C_y = trip$y[ic]
  D = 2 * (A_x*(B_y - C_y) + B_x*(C_y - A_y) + C_x*(A_y - B_y) )
  U_x = ((A_x^2 + A_y^2) * (B_y - C_y) + (B_x^2 + B_y^2) * (C_y - A_y) + (C_x^2 + C_y^2) * (A_y - B_y)) / D
  U_y = ((A_x^2 + A_y^2) * (C_x - B_x) + (B_x^2 + B_y^2) * (A_x - C_x) + (C_x^2 + C_y^2) * (B_x - A_x)) / D
  R = sqrt((A_x - U_x)^2 + (A_y - U_y)^2)
  #   cur <- data.table(center_x = c(NA, U_x, NA),
  #                     center_y = c(NA, U_y, NA),
  #                     radius = c(NA, R, NA))
  mlag <- nlag
  if (nlag %% 2 == 0) {
    mlag <- nlag + 1
  }
  f21 <- rep(1/mlag,mlag)
  smth_x <- filter(U_x, f21, sides=2)
  smth_y <- filter(U_y, f21, sides=2)
  smth_R <- filter(R, f21, sides=2)
  cur_smooth <- data.table(center_x = c(NA, smth_x, NA),
                           center_y = c(NA, smth_y, NA),
                           radius = c(NA, smth_R, NA))
  return(cur_smooth)
}
curvatureDistribution <- function(trip,nlag)
{
  cur = calcCurvature(trip,nlag)
  radius = cur$radius
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
distance <- function(trip,nlag=NULL)
{
  # kt = (d2y/dt2) / (1+(dy/dt)^2)^(3/2)
  dx <- diff(trip$x,lag=nlag,differences=1)
  dy <- diff(trip$y,lag=nlag,differences=1)
  delta_dist <- sqrt(dx^2 + dy^2)
  dist = sum(delta_dist)
  names(dist) = paste('distance')
  return(dist)
}

tripFeatures<-function(trip,target,nlag) {
  sd <- speedDistribution(trip,nlag)
  at <- TangAccelDistribution(trip,nlag)
  cd <- curvatureDistribution(trip,nlag)
  an <- NormAccelDistribution(trip,nlag)
  atotal <- TotalAccelDistribution(trip,nlag)
  distance <- distance(trip,nlag)
  t <- target
  names(t) <- 'target'
  tr <- unique(trip$drive)
  names(tr) <- 'tripId'
  rtn <- c(sd,at,an,atotal,cd,distance,t,tr)
  return(rtn)
}
tripsFeatures<-function(trips,target,driver,nlag) {
  tmp <- by(trips,trips$drive,tripFeatures,target,nlag)
  rowNames = paste('Driver',driver,'Trip',1:length(tmp))
  df <- data.frame(matrix(unlist(tmp), nrow=length(tmp), byrow=T),
                   row.names=rowNames)
  names(df)<-names(tmp[[1]])
  #   dist_distribution <- quantile(df$distance, seq(0.05,1, by = 0.05), na.rm=T)
  #   names(dist_distribution) <- paste('distance',names(dist_distribution),sep='_')
  #   mat_dist_distribution <- matrix(dist_distribution,nrow=nrow(df),ncol=length(dist_distribution))
  #   df2 <- cbind(df, mat_dist_distribution)
  #   names(df2)[(-19:0)+length(names(df2))] <- names(dist_distribution)
  dt<-data.table(df)
  return(dt)
}

getTrips <- function(driver) {
  trips <- fread(driver, header=T, sep=",")
}

testFunctions<-function() {
  # r = r0 + vr * t
  # psi = omega_0 * t
  # x = (r0 + vr * t) * cos(omega_0 * t)
  # y = (r0 + vr * t) * sin(omega_0 * t)
  # dx_dt = vr * cos(omega_0 * t)
  #       - omega_0 * (r0 + vr * t) * sin(omega_0 * t)
  # dy_dt = vr * sin(omega_0 * t)
  #       + omega_0 * (r0 + vr * t) * cos(omega_0 * t)
  nt <- 200
  t <- seq(nt)
  n_revs = 4
  omega_0 <- 2*pi*n_revs/nt
  vr<-2
  r0<-1
  psi <- omega_0 * t
  r <- r0 + vr * t
  trip <- data.table(x = r*cos(psi), y = r*sin(psi))
  plot(trip,type='l',col="black",main='Curvature Check',asp = 1, xlim = c(-400, 400))
  cur<-calcCurvature(trip)
  i_cir = 20
  draw.circle(cur$center_x[i_cir], cur$center_y[i_cir], cur$radius[i_cir], 
              nv = 1000, border = "red", col = NULL, lty = 1, lwd = 1)
  i_cir = 100
  draw.circle(cur$center_x[i_cir], cur$center_y[i_cir], cur$radius[i_cir], 
              nv = 1000, border = "red", col = NULL, lty = 1, lwd = 1)
  i_cir = 150
  draw.circle(cur$center_x[i_cir], cur$center_y[i_cir], cur$radius[i_cir], 
              nv = 1000, border = "red", col = NULL, lty = 1, lwd = 1)
  i_cir = 199
  draw.circle(cur$center_x[i_cir], cur$center_y[i_cir], cur$radius[i_cir], 
              nv = 1000, border = "red", col = NULL, lty = 1, lwd = 1)
  dx_dt = vr * cos(omega_0 * t) -
    omega_0 * (r0 + vr * t) * sin(omega_0 * t)
  dy_dt = vr * sin(omega_0 * t) +
    omega_0 * (r0 + vr * t) * cos(omega_0 * t)
  spd <- calcSpeed(trip) 
  analy_spd <- sqrt(dx_dt^2 + dy_dt^2)
  plot(t,analy_spd,type='l',col = "black",xlab='Time',ylab='Speed',
       main='Derivative Check')
  lines(t,spd,col = "green")
}

plotTrip <- function(allTrips,tripID) {
  trip <- allTrips[allTrips$drive==tripID,]
  spd <- calcSpeed(trip)
  o <- data.frame(x = 1:10,y = 1:10,col = 1:10)
  ggplot(so,aes(x = x, y = y)) + 
    geom_line(aes(group = 1,colour = col))
  plot(tripxy,type='l',asp=1)
  t=5
}
