setwd('H:/Machine_Learning/DTA')
rm(list=ls());gc()

load(file='data/main_df.RData')
head(main_df)

datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

### main process
nrOfDriversToCompare <- 5
submission <- data.frame()
model <- function(driver) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 1
    
    test_num <- sample(drivers[which(drivers!=driver)],nrOfDriversToCompare)
    refData <-  main_df[main_df[,1] %in% test_num,]
    train <- rbind(currentData, refData)
    
    #model
    g = glm(as.numeric(target) ~ ., data=train[,-c(1,2)], family = binomial("logit"))
    p = predict(g, currentData, type = "response")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p)
    return(result)
}

### main loop
for (driver in drivers){
    
    result <- model(driver)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}