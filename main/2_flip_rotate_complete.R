setwd('H:/Machine_Learning/DTA')
rm(list=ls());gc()
require(sp);require(maptools);require(rgeos)
source('Driver-Telematics-Analysis/main/kalman_filtering.R')

datadirectory <- 'data/drivers/'
driver <- 1
trip <- 1#20,125,197
path <- "data/drivers/"
files <- paste0(path, driver, '/', trip, ".csv")
trip_data <- data.matrix(read.csv(files,header = T,stringsAsFactor=F))
trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600

###############
### process ###
###############
# load data
spatial_df <- coordinates(trip_data)
spatial_line <- Line(spatial_df)
Sr <- Polygons(list(Polygon(spatial_line)), ID="Sr")
SpP = SpatialPolygons(list(Sr))
SpP@coords
coordinates(SpP)

# rotate
SpP_2 <- elide(SpP, rotate=360, center=apply(bbox(SpP), 1, mean))
opar <- par(mfrow=c(1,2))
plot(SpP, axes=TRUE)
plot(SpP_2, axes=TRUE)

# compare
gCentroid(SpP, byid=FALSE, id = NULL)
gCentroid(SpP_2, byid=FALSE, id = NULL)

gDifference(SpP, SpP_2, byid=FALSE, id=NULL, drop_lower_td=FALSE)

gEquals(SpP, SpP_2, byid = FALSE, returnDense=TRUE)
gEqualsExact(SpP, SpP_2, tol=0.0, byid = FALSE, returnDense=TRUE)
