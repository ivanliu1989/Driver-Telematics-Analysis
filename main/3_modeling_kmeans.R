datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

classifier <- function(driver, model='gbm', nrOfDriversToCompare=5, features, refData) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    refData$target <- 'No'
    train <- rbind(currentData, refData)
    
    #model
    g <- train(x = data.matrix(train[,c(features)]), y = as.factor(train$target), method = model,trControl = fitControl, 
               tuneLength = 6,metric = "ROC", preProc = c("center", "scale"),tuneGrid = gbmGrid)
    p <- predict(g, newdata = data.matrix(currentData[,c(features)]), type = "prob")
    
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

for (driver in drivers){
    result <- classifier(driver,'rf',5,feature_list, result_centriods)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}

write.csv(submission, file = 'submission_lg_136.csv', quote = F, row.names = F)
sum(is.na(submission))
