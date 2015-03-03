# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

# main_df <- data.frame(fread('data/main_df_182features.csv',header = T, stringsAsFactor = F))
# load(file='data/main_df_138features.RData')
head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

##################
### Classifier ###
##################
classifier <- function(driver, model='gbm', nrOfDriversToCompare=5, features) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    test_num <- sample(drivers[which(drivers!=driver)],nrOfDriversToCompare)
    
    refData <-  main_df[main_df[,1] %in% test_num,]
#     refData <-  result_centriods
    refData$target <- 'No'
    train <- rbind(currentData, refData)
    
    #model

    g <- train(x = data.matrix(train[,c(features)]), y = as.factor(train$target), method = model,trControl = fitControl, 
                tuneLength = 6,metric = "ROC",tuneGrid = gbmGrid ,preProc = c("center", "scale", "pca"), repeats = 15, trace = FALSE)
    p <- predict(g, newdata = data.matrix(currentData[,c(features)]), type = "prob")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p$Yes)
    return(result)
}

#################
### Main Loop ###
#################
library(doMC)
registerDoMC(cores = 2)
# load('Driver-Telematics-Analysis/feature_selection/rfe_var_190.RData')
set.seed(18)
fitControl <- trainControl(method = "none",number = 10,repeats = 3,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 4,alpha = 0.05,method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(size = 9, decay = 0.1, bag = FALSE)
feature_list <- colnames(main_df[,-c(1,2,187)])
submission <- data.frame()

for (driver in drivers){ #avNNet
    result <- classifier(driver,'avNNet',5,feature_list)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}

write.csv(submission, file = 'submission_rf_187_18.csv', quote = F, row.names = F)
sum(is.na(submission))
