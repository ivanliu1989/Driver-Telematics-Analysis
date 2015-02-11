#library("ggplot2")  # this library is needed

plotDriver <- function(driver, driver2=driver){
trips=NULL
sq=(driver:driver2)
for(j in sq){
  driver=as.character(j)
  dirPath = paste0("drivers/", driver, '/')
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    trip=cbind(trip,i,j)
    trips=rbind(trips,trip)
  }
}
p1 = ggplot(trips, aes(x, y)) +
  geom_point(aes(colour=i, group=i), size=0.01) +
  facet_wrap(~ j,ncol=2) +
  ggtitle("Trajectories")

plot(p1)
}