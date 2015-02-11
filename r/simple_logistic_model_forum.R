# Solution by St?phane Soulier @pypass
# Vecorized by Leo B?ttiker @leobuettiker
# New in previous versions:
# * Add a split for the reference data
# * new smoothing version
# * cache trips in a file per driver
# * use rbindlist instead of do.call(rbind.data.frame,...)
# * use clustering
date()
require(zoo)
require(doSNOW)
require(data.table)

datadirectory <- "data/drivers/"
cachedir <- "data/intermediate/drivers"
nrOfCores <- 2
nrOfDriversToCompare <- 5

features <- function(trip) {
  dist = sqrt(diff(trip[,1])^2 + diff(trip[,2])^2) # distance
  smoothDist <- rollapply(dist, width = 5, FUN = median) # rolling median smooth
  speed <- smoothDist * 3.6
  quantile(speed, seq(0.05,1, by = 0.05), na.rm=T)
}

loadDriverData <- function(driver, target) {
  cachefile <- paste0(cachedir, '/trips_', driver,'.Rmd')
  if(file.exists(cachefile)) {
    load(cachefile)
  } else {
    files <- paste0(datadirectory, driver, '/', 1:200, ".csv")
    trips <- lapply(files, read.csv)
    save(trips, file=cachefile)
  }
  allRefData <- as.data.frame(t(sapply(trips, function(trip) {
    c(features=features(trip), target=target)
  })))
  allRefData
}

createDataForMultipleDrivers <- function(myDrivers) {
  refData <- lapply(myDrivers, loadDriverData, target=0)
  rbindlist(refData)
}

set.seed(123)
drivers = list.files(datadirectory)
middle <- floor(length(drivers)/2)
randomDriversFirstHalf <- sample(drivers[1:middle], size = nrOfDriversToCompare)
randomDriversSecondHalf <- sample(drivers[middle:length(drivers)], size = nrOfDriversToCompare)
refDataFirstHalf <- createDataForMultipleDrivers(randomDriversFirstHalf)
refDataSecondHalf <- createDataForMultipleDrivers(randomDriversSecondHalf)

cl<-makeCluster(nrOfCores)
registerDoSNOW(cl)
clusterExport(cl, c("loadDriverData","rollapply","datadirectory","cachedir","drivers","middle","refDataSecondHalf","refDataFirstHalf", "features"))
submission <- parLapply(cl, drivers, function(driver) {
  currentData <- loadDriverData(driver, target=1)
  
  if(driver %in% drivers[1:middle]) {
    train = rbind(currentData, refDataSecondHalf)
  } else {
    train = rbind(currentData, refDataFirstHalf)
  }
  
  g = glm(target ~ ., data=train, family = binomial("logit"))
  p = predict(g, currentData, type = "response")
  labels = paste0(driver,'_', 1:200)
  data.frame(driver_trip=labels, prob=p)
})
stopCluster(cl)

submissionDf <- rbindlist(submission)
write.csv(submissionDf, gzfile("simple_logistic_model_forum.csv.gz"), row.names=F, quote=F)
hist(submissionDf$prob)
date()
