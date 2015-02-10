require(parallel)

maxDrivers = 3612
maxTrips=  200

createTripFrame <-function(n){
  master = matrix(nrow = maxTrips , ncol = 6 , byrow = T)
  for( i in 1:maxTrips){
    fname = paste(n , "/" , i , ".csv" , sep = "")
    tf = read.csv(fname)
    r = processCSV(tf , n, i)
    master[i,] = r;
  }
  
  return(master)
}

processCSV <-function(tripFrame , n , tripNum){
  x = tripFrame[,1]
  y= tripFrame[,2]

# Do your CSV processing here
 return( # Your Row)
}

mergeTrips <-function(masterTrip , oneTrip){
   masterTrip = rbind(masterTrip , oneTrip)
   return (masterTrip)
}


dirs = list.dirs(path = ".")

for(i in 1:length(dirs)){
  dirs[i] = substring(dirs[i] ,3 )  
}


trips = mclapply(dirs[2:length(dirs)] , createTripFrame ,
                 mc.set.seed = TRUE,
                 mc.silent = FALSE, 
                 mc.cleanup = TRUE)

m = data.frame(driver_trip = character() ,
               # Other things you want to measure)



for(i in 1:length(dirs)){
  t = as.data.frame(trips[i])
  m = rbind(m, t)
}

write.csv(m , file = "mastersummary.csv")



