setwd('H:/Machine_Learning/DTA')
rm(list=ls());gc()

theta <- 90

### functions
rotational <- function(theta){
    array(c(cos(theta),sin(theta),-sin(theta),cos(theta)),dim = c(2,2))
    }

### flip
flip <- function(){
    
}

rotate_trip <- function(){
    
    
}    
t1r <- flip(rotate_trip(trip))
update_trips(t1r)

y1 <- 2
x1 <- 2
y2<-2
x2<-3

degree_cal <- function(x1,y1,x2,y2){
    radian <- atan2((y2 - y1),(x2 - x1))
    degrees <- radian * 180 / pi
        
    return(degrees)
}
