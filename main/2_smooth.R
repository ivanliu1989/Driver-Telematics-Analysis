#setwd('/Users/ivan/Work_directory/DTA')
setwd('H:/Machine_Learning/DTA')
rm(list=ls());gc()
require(data.table);require(ggplot2);require(plyr);
require(dlm);require(zoo);

i <- 180
trip = read.csv(paste0('data/drivers/1/',i,'.csv',sep = ''))

### function
speedDist <- function(trip) {
    dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
    speed <- dist * 3.6
}

speedDist_smooth <- function(trip) {
    dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
    smoothDist <- dlmSmooth(dist, dlmModPoly(1, dV=exp(dist[1]), dW = exp(dist[2]))) # V 10^2 degree , dV = 15100, dW = 1470
    #smoothDist <- rollapply(dist, width = 5, FUN = median) # rolling median smooth
    speed <- dropFirst(smoothDist$s) * 3.6
}

speedDist_smooth_median <- function(trip) {
    dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
    smoothDist <- rollapply(dist, width = 5, FUN = median) # rolling median smooth
    speed <- smoothDist * 3.6
}

trip_sp <- speedDist(trip)
trip_sp_smooth <- speedDist_smooth(trip)
trip_sp_smooth_median <- speedDist_smooth_median(trip)

### ggplot
#p1 = ggplot(data.frame(trip_sp),aes(y = trip_sp, x = seq(1, length(trip_sp)))) + 
#    geom_line(aes(colour=1, group=1), size=1) +
#    ggtitle("Trajectories") + 
#    geom_point(data = data.frame(seq(1, length(trip_sp_smooth)),trip_sp_smooth),aes(colour=2, group=2), size=3)
#plot(p1)

### diagram
opts = c("p","l","o","b","c","s","S","h") 
plot(trip_sp, type="n") 
lines(trip_sp, type=opts[2])
lines(trip_sp_smooth, type=opts[2], col = 'blue')
lines(trip_sp_smooth_median, type=opts[2],col = 'red')

quantile(trip_sp_smooth, seq(0.05,1, by = 0.05), na.rm=T)
quantile(trip_sp_smooth_median, seq(0.05,1, by = 0.05), na.rm=T)
