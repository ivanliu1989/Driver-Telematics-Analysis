speedDistribution <- function(trip)
{
  vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  return(quantile(vitesse, seq(0.05,1, by = 0.05)))
}

drivers = list.files("kaggle/Axa/drivers")
randomDrivers = sample(drivers, size = 5)

refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
  dirPath = paste0("kaggle/Axa/drivers/", driver, '/')
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistribution(trip), target)
    refData = rbind(refData, features)
  }
}

target = 1
names(target) = "target"
submission = NULL
for(driver in drivers)
{
  print(driver)
  dirPath = paste0("kaggle/Axa/drivers/", driver, '/')
  currentData = NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistribution(trip), target)
    currentData = rbind(currentData, features)
  }
  train = rbind(currentData, refData)
  train = as.data.frame(train)
  g = glm(target ~ ., data=train, family = binomial("logit"))
  currentData = as.data.frame(currentData)
  p =predict(g, currentData, type = "response")
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "kaggle/Axa/submission.csv", row.names=F, quote=F)
