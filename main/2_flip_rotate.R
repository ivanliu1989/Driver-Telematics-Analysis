setwd('H:/Machine_Learning/DTA')
rm(list=ls());gc()
require(sp);require(maptools);require(rgeos)
source('Driver-Telematics-Analysis/main/kalman_filtering.R')

# convert the xy points for each journey into a SpatialLines object (package sp).
# use the elide routine from package maptools to rotate and flip the journeys
# use gdistance from package rgeos to measure the distance or similarity between journeys

datadirectory <- 'data/drivers/'
driver <- 1
trip <- 20#20,125,197
trip2 <- 125
trip3 <- 197
path <- "data/drivers/"
files <- paste0(path, driver, '/', trip, ".csv")
files2 <- paste0(path, driver, '/', trip2, ".csv")
files3 <- paste0(path, driver, '/', trip3, ".csv")
trip_data <- data.matrix(read.csv(files,header = T,stringsAsFactor=F))
trip_data <- Kalman_Filter(trip_data,1,1,10) #Q_metres_per_second = 50*1000/3600
trip_data2 <- data.matrix(read.csv(files2,header = T,stringsAsFactor=F))
trip_data2 <- Kalman_Filter(trip_data2,1,1,10) #Q_metres_per_second = 50*1000/3600
trip_data3 <- data.matrix(read.csv(files3,header = T,stringsAsFactor=F))
trip_data3 <- Kalman_Filter(trip_data3,1,1,10) #Q_metres_per_second = 50*1000/3600

# sp object
spatial_df <- coordinates(trip_data)
spatial_line <- Line(spatial_df)
spatial_df2 <- coordinates(trip_data2)
spatial_line2 <- Line(spatial_df2)
spatial_df3 <- coordinates(trip_data3)
spatial_line3 <- Line(spatial_df3)

LineLength(spatial_line2, longlat=FALSE, sum=TRUE) # which equal to trip_length

Sr1 <- Polygon(spatial_line)
Srs1 = Polygons(list(Sr1), "s1")
Sr2 <- Polygon(spatial_line2)
Srs2 = Polygons(list(Sr2), "s2")
Sr2 <- Polygon(spatial_line3)
Srs3 = Polygons(list(Sr2), "s3")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")
plot(spatial_df)

# rgeos
gCentroid(SpP, byid=FALSE, id = NULL)

gContains(SpP[1], SpP[2], byid = FALSE, prepared=TRUE,returnDense=TRUE, STRsubset=FALSE)
gCovers(SpP[1], SpP[2], byid = FALSE, returnDense=TRUE)
gWithin(SpP[1], SpP[2], byid = FALSE, returnDense=TRUE)

gDifference(SpP[2], SpP[3], byid=FALSE, id=NULL, drop_lower_td=FALSE)

gEquals(SpP[2], SpP[3], byid = FALSE, returnDense=TRUE)
gEqualsExact(SpP[2], SpP[3], tol=0.0, byid = FALSE, returnDense=TRUE)
