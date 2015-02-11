drivers <- list.files("data/drivers",full.names=TRUE)

zscore <- function(x)
{
  z <- scale(x)
  p <- 2*pnorm(-abs(z))
  p
}

doDriver <- function(driver)
{
  require(plyr)
  
  tripsFiles <- paste0(driver,"/",1:200,".csv")
  trips <- llply(tripsFiles,read.csv,header=TRUE,stringsAsFactors=FALSE)  
  
  distances <- sapply(1:length(trips),function(i) sum(sqrt(diff(trips[[i]][,1])^2 + diff(trips[[i]][,2])^2)))
  prob <- zscore(distances)
  
  data.frame(driver_trip=paste0(sub(driver,pattern="./drivers/",replace=""),
                                "_",
                                1:200),
             prob=prob,
             row.names=NULL,
             stringsAsFactors=FALSE)
}

doAllDrivers <- function()
{
  require(plyr)
  
  ldply(drivers,doDriver,.progress="tk")
}

submission <- doAllDrivers()
write.csv(submission,gzfile("BeatTheBenchmark.csv.gz"),row.names=FALSE)
