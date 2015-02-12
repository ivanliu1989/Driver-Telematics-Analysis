setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(data.table);require(ggplot2);require(plyr);

# drivers <- list.files("data/drivers",full.names=TRUE)
# tripsFiles <- paste0(drivers,"/",1:200,".csv")
# 
# trips <- llply(tripsFiles,read.csv,header=TRUE,stringsAsFactors=FALSE)  

### diagram
plotDriver <- function(driver, driver2=driver){
    trips=NULL
    sq=(driver:driver2)
    for(j in sq){
        driver=as.character(j)
        for(i in 1:200)
        {
            trip = read.csv(paste0('data/drivers/1/',i,'.csv',sep = ''))
            trip=cbind(trip,i,j)
            trips=rbind(trips,trip)
        }
    }
    p1 = ggplot(trips, aes(x, y)) +
        geom_point(aes(colour=i, group=i), size=0.5) +
        facet_wrap(~ j,ncol=3) +
        ggtitle("Trajectories")
    
    plot(p1)
}

plotDriver(1,9)
