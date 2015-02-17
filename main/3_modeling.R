# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

# main_df <- data.frame(fread('data/main_df_103features.csv',header = T, stringsAsFactor = F))
# load(file='data/main_df_103features.RData')
head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

##################
### Classifier ###
##################
#Recursive Partitioning and Regression Trees 
#svmLinear() | gbm(ntree150,inter4) | rf(mtry17) | glm | nb(LF0,kernel=T) | nnet(size1,decay0.1) | RFlda()
classifier <- function(driver, model='gbm', nrOfDriversToCompare=5, features) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    test_num <- sample(drivers[which(drivers!=driver)],nrOfDriversToCompare)
    refData <-  main_df[main_df[,1] %in% test_num,]
    refData$target <- 'No'
    train <- rbind(currentData, refData)
    
    #model
    system.time(g <- train(x = data.matrix(train[,c(rfe_var)]), y = as.factor(train$target), method = model,trControl = fitControl, 
               verbose = F, preProc = c("center", "scale"),tuneLength = 6,metric = "ROC",tuneGrid = gbmGrid))
    p <- predict(g, newdata = data.matrix(currentData[,c(rfe_var)]), type = "prob")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p$Yes)
    return(result)
}

classifier <- function(driver, model='gbm', nrOfDriversToCompare=5, features) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    test_num <- sample(drivers[which(drivers!=driver)],300)
    refData <-  main_df[main_df[,1] %in% test_num,]
    refData$target <- 'No'
    train <- rbind(currentData, refData)
    # 1st
    g <- train(x = data.matrix(train[,-c(1,2,80)]), y = as.factor(train$target), method = model,trControl = fitControl, 
               verbose = F, preProc = c("center", "scale"),tuneLength = 6,metric = "ROC",tuneGrid = gbmGrid)
    
    p_other <- predict(g, newdata = data.matrix(refData[,-c(1,2,80)]), type = "prob")
    result_other <- data.frame(refData, prob=p_other$Yes)
    refData <- head(head(result_other[order(result_other[,'prob'],decreasing = T),],1000)[,-ncol(result_other)])
    # 2nd
    colnames(currentData) <- colnames(refData)
    train <- rbind(currentData, refData)
    g <- train(x = data.matrix(train[,-c(1,2,80)]), y = as.factor(train$target), method = model,trControl = fitControl, 
               verbose = F, preProc = c("center", "scale"),tuneLength = 6,metric = "ROC",tuneGrid = gbmGrid)
    p <- predict(g, newdata = data.matrix(currentData[,-c(1,2,80)]), type = "prob")
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p$Yes)
}

#################
### Main Loop ###
#################
library(doMC)
registerDoMC(cores = 2)
load('Driver-Telematics-Analysis/feature_selection/rfe_var.RData')
set.seed(888)
fitControl <- trainControl(method = "none",number = 10,repeats = 3,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 4,alpha = 0.05,method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(mtry = 18)
gbmGrid <-  expand.grid(interaction.depth = 2,
                        n.trees = 200,
                        shrinkage = 0.1)
submission <- data.frame()

for (driver in drivers){
    result <- classifier(driver,'gbm',5,rfe_var)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}

write.csv(submission, file = 'submission_gbm_retrain.csv', quote = F, row.names = F)
sum(is.na(submission))
