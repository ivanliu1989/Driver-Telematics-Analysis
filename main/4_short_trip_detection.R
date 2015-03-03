setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
source('Driver-Telematics-Analysis/main/feature_functions.R')

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
trips <- 1:200
dist_Limit <- 100
d_num <- 0; p_num <- 0
match_matrix <- matrix(0, nrow = length(drivers)*100, ncol = 2, dimnames = list(NULL, c('driver_trip', 'Dist')))
start <- date()
print(start)
for(driver in drivers){
    p_num <- p_num + 1
    for (trip in trips){
        files <- paste0(datadirectory, driver, '/', trip, ".csv")
        tx <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        dist <- distance(tx)
        if(dist<dist_Limit){
            d_num <- d_num + 1
            match_matrix[d_num,] <- c(paste0(driver,'_',trip),dist)   
            print(paste0(driver,'_',trip, ' | ', date(), ' | ', p_num/(length(drivers)*200)*100))
        }
    }
}

match_matrix <- match_matrix[which(match_matrix[,1]>0),]
head(match_matrix)
save(match_matrix, file=paste0('short_trip_detections.RData'))
