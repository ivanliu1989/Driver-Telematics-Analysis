setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
source('Driver-Telematics-Analysis/main/feature_functions.R')

rotational <- function(theta){
    rotation_mat <- matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),c(2,2),byrow = T)
    return (rotation_mat)
}

flip <- function(x){
    if(sum(sign(x[,2]))>0){
        x <- x %*% matrix(c(1,0,0,-1),c(2,2),byrow = T)
    }
    return(x)
}

rotate_trip <- function(trip){
    w0 <- atan2(trip[nrow(trip),2],trip[nrow(trip),1])
    trip_rotate <- trip %*% rotational(w0)
    return (trip_rotate)
}

getdd <- function(tx,ty){
    gap <- nrow(tx)-nrow(ty)
    if (gap>0){
        ty <- data.matrix(rbind(data.frame(ty),data.frame(X1=rep(ty[nrow(ty),1],gap),X2=rep(ty[nrow(ty),2],gap))))
    }else if(gap<0){
        tx <- data.matrix(rbind(data.frame(tx),data.frame(X1=rep(tx[nrow(tx),1],-gap),X2=rep(tx[nrow(tx),2],-gap))))
    }
    result <- sum(sqrt((ty[,1]-tx[,1])^2+(ty[,2]-tx[,2])^2))
    return(result)
}

sametrip <- function(tx,ty){
    if((abs(nrow(tx)-nrow(ty))/max(nrow(tx),nrow(ty))<0.2)&(abs(distance(tx)-distance(ty))/max(distance(tx),distance(ty))<0.2)){
        mm <- as.integer(nrow(tx)*threshold)
        n <- nrow(tx)
        txx <- tx
        txx[1:(n-mm),] <- tx[(mm+1):n,]
        txx[(n-mm+1):n,] <- tx[(n-mm+1):n,]
        dd <- getdd(tx,txx)
        return(getdd(tx,ty)<=dd)
    }else{
        return(FALSE)
    }
}

########################
### Detect Same Trip ###
########################
threshold <- 0.1
d_num <- 0
dist <- 2.552923#e+01
match_matrix <- matrix(0, nrow = length(drivers)*100, ncol = 2, dimnames = list(NULL, c('driver_trip', 'matched_trip')))
start <- date()
print(start)
sub_drivers <- drivers[1:(length(drivers)/2)]
for(driver in sub_drivers){
    match_matrix_single <- matrix(0, nrow = 4000, ncol = 2, dimnames = list(NULL, c('driver_trip', 'matched_trip')))
    print(date())
    cb_num <- 0
    for (trip in 1:200){
        if(trip >= 200){
            match_matrix_single <- match_matrix_single[which(match_matrix_single[,1]>0),]
            cb_num <- length(table(match_matrix_single))
            print(paste0('Driver ',driver,' Trips Match: ', trip, ' | ', other, '!!! Total:', cb_num))
            break
        }
        files <- paste0(datadirectory, driver, '/', trip, ".csv")
        tx <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
        
        if(distance(tx)>=dist){
            tx <- flip(rotate_trip(tx))
            
            for(other in c((trip+1):200)){
                files <- paste0(datadirectory, driver, '/', other, ".csv")
                ty <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
                
                if(distance(ty)>=dist){
                    ty <- flip(rotate_trip(ty))
                    if(sametrip(tx,ty)){
                        cb_num <- cb_num + 1
                        d_num <- d_num + 1
                        #plot(tx,col='blue');points(ty,col='red')
                        match_matrix[d_num,] <- c(paste0(driver,'_',trip),paste0(driver,'_',other)) 
                        match_matrix_single[cb_num,] <- c(paste0(driver,'_',trip),paste0(driver,'_',other))   
                         
                        if(d_num %% 5000 == 0 ){
                            save(match_matrix, file=paste0('repeated_map_thereshold_',threshold,'_driver_',driver,'_num_',d_num,'.RData'))
                        }
                    }   
                }   
            }
        }
    }
}
match_matrix <- match_matrix[which(match_matrix[,1]>0),]
length(table(match_matrix))
save(match_matrix, file=paste0('repeated_map_thereshold_',threshold,'_driver_',sub_drivers[1],'_',sub_drivers[length(sub_drivers)],'.RData'))

########################
### T.TEST Detection ###
########################
match_matrix <- matrix(0, nrow = length(drivers)*100, ncol = 2, dimnames = list(NULL, c('driver_trip', 'matched_trip')))
start <- date()
print(start)
sub_drivers <- 1 # drivers[1:(length(drivers)/2)]
d_num <- 0
for(driver in drivers){
    print(date())
    for (trip in 1:200){
        if(trip >= 200){
            break
        }
        tx <- main_df[which(main_df[,1]==driver&main_df[,2]==trip),-c(1,2,172)]
        for(other in c((trip+1):200)){
            ttest_diff <- c()
            ty <- main_df[which(main_df[,1]==driver&main_df[,2]==other),-c(1,2,172)]
            ttest_same <- t.test(tx,ty)$p.value
            
            test_driver <- sample(drivers[which(drivers!=driver)],50)    
            test_trip <- sample(1:200,5) 
            
            for (i in test_driver){
                for (j in test_trip){
                    tz <- main_df[which(main_df[,1]==i&main_df[,2]==j),-c(1,2,172)]
                    ttest_diff <- c(ttest_diff, t.test(tx,tz)$p.value)
                }
            }
            print(paste0('TTEST1: ',ttest_same,' TTEST2: ', max(ttest_diff)))
            if(ttest_same>max(ttest_diff)){
                d_num <- d_num + 1
                print(paste0('Driver ',driver,' Trips Match: ', trip, ' | ', other, '!!!'))
                match_matrix[d_num,] <- c(paste0(driver,'_',trip),paste0(driver,'_',other))    
                
                if(d_num %% 5000 == 0 ){
                    save(match_matrix, file=paste0('repeated_map_ttest_',threshold,'_driver_',driver,'_num_',d_num,'.RData'))
                }
            }
        }
    }
}
match_matrix <- match_matrix[which(match_matrix[,1]>0),]
length(table(match_matrix))
save(match_matrix, file=paste0('repeated_map_thereshold_',threshold,'_driver_',sub_drivers[1],'_',sub_drivers[length(sub_drivers)],'.RData'))


t.test(main_df[17,-c(1,2,ncol(main_df))],main_df[170,-c(1,2,ncol(main_df))])$p.value
t.test(main_df[31,-c(1,2,ncol(main_df))],main_df[160,-c(1,2,ncol(main_df))])$p.value
