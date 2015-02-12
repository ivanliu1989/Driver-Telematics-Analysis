setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(data.table);require(zoo);require(Rcpp)
sourceCpp('main/functions.cpp')

datadirectory <- 'data/drivers/'
drivers <- list.files(datadirectory)
trips <- 1:200
path <- "data/drivers/"

main_df <- data.frame()

### functions ###
speedDist <- function(trip) {
    dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
    smoothDist <- rollapply(dist, width = 5, FUN = median) # rolling median smooth
    speed <- smoothDist * 3.6
    quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
}

### main ###
for (driver in drivers){
    for (trip in trips){
        sub_df <- data.frame()
        files <- paste0(path, driver, '/', trip, ".csv")
        if (trip==1) print(files)
        trip_data <- read.csv(files,header = T,stringsAsFactor=F)
        # id
        driver_trip <- paste0(driver,'_',trip)
        # target
        target <- 0
        # speed
        feature_speed <- speedDist(trip_data)
        
        # df
        sub_df <- cbind(driver_trip, t(feature_speed), target)
        main_df <- rbind(main_df,sub_df)
    }
}
names(main_df) <- c('driver_trip', names(feature_speed), 'target')
dim(main_df);head(main_df)
save(main_df, file='data/main_df.RData')

