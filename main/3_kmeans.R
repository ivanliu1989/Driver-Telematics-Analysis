# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

load(file='data/main_df_136features.RData')

preProcValues <- preProcess(main_df[,-c(1,2,136)], method = c("center", "scale"))
main_df[,-c(1,2,136)] <- predict(preProcValues, main_df[,-c(1,2,136)])


### classifier ###
classifier <- function(driver, model='gbm', nrOfDriversToCompare=5, features) {
    
    refData <-  main_df[-which(main_df[,1]==driver),]
    # cluster
    fit.km <- kmeans(refData[,-c(1,2,136)], algorithm="Lloyd", centers = 200, nstart = 25, iter.max = 20)
    
    result <- data.frame(driver=driver, trip=driver, fit.km$centers, target = 0)
    return(result)
}


### main process
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
submission <- data.frame()

for (driver in drivers){
    gc()
    result <- classifier(driver,'rf',5,feature_list)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}

write.csv(submission, file = 'test_kmeans.csv', quote = F, row.names = F)
sum(is.na(submission))

#####################
### one_train_set ###
#####################
fit.km <- kmeans(main_df[,-c(1,2,136)], algorithm="Lloyd", centers = 500, nstart = 25, iter.max = 20)

result <- data.frame(driver=driver, trip=driver, fit.km$centers, target = 0)

for (driver in drivers){
    refData <-  main_df[-which(main_df[,1]==driver),]
    N <- nrow(refData)
    if(N<200){
        print(paste0('driver: ', driver, ' | nrow: ' ,N))
    }
}
write.csv(submission, file = 'test_kmeans.csv', quote = F, row.names = F)
sum(is.na(submission))