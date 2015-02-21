datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

classifier <- function(driver, model='gbm',refData) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    refData$target <- 'No'
    train <- rbind(currentData, refData)
    
    #model
    g <- train(x = data.matrix(train[,-c(1,2,136)]), y = as.factor(train$target), method = model,trControl = fitControl, 
               tuneLength = 6,metric = "ROC", tuneGrid = gbmGrid)#preProc = c("center", "scale"),
    p <- predict(g, newdata = data.matrix(currentData[,-c(1,2,136)]), type = "prob")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p$Yes)
    return(result)
}

###
load('Driver-Telematics-Analysis/kmeans/test_kmeans.RData')

set.seed(888)
fitControl <- trainControl(method = "none",number = 10,repeats = 3,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 4,alpha = 0.05,method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(mtry=18)
feature_list <- colnames(main_df)[-c(1,2,136)]
submission <- data.frame()

preProcValues <- preProcess(main_df[,-c(1,2,136)], method = c("center", "scale"))
main_df[,-c(1,2,136)] <- predict(preProcValues, main_df[,-c(1,2,136)])

for (driver in drivers){
    result <- classifier(driver,'rf', result_centriods)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}

write.csv(submission, file = 'submission_lg_136.csv', quote = F, row.names = F)
sum(is.na(submission))

### classifier 2 ###
classifier <- function(driver, model='gbm',refData) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    refData <- main_df[main_df[,1]!=driver,]
    fit.km <- kmeans(refData[,-c(1,2,136)], algorithm="Lloyd", centers = 200, iter.max = 5)
    refData <- data.frame(driver=1, trip=1, fit.km$centers, target = 'No')
    train <- rbind(currentData, refData)
    
    #model
    g <- train(x = data.matrix(train[,-c(1,2,136)]), y = as.factor(train$target), method = model,trControl = fitControl, 
               tuneLength = 6,metric = "ROC", tuneGrid = gbmGrid)#preProc = c("center", "scale"),
    p <- predict(g, newdata = data.matrix(currentData[,-c(1,2,136)]), type = "prob")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p$Yes)
    return(result)
}