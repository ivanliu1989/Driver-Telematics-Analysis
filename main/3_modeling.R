# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

# main_df <- data.frame(fread('data/main_df_103features.csv',header = T, stringsAsFactor = F))
# load(file='data/main_df_89features.RData')
head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

##################
### Classifier ###
##################
#Recursive Partitioning and Regression Trees | svmLinear | gbm | rf | glm | nb | nnet | RFlda
classifier <- function(driver, model='gbm', nrOfDriversToCompare=5) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 'Yes'
    
    test_num <- sample(drivers[which(drivers!=driver)],nrOfDriversToCompare)
    refData <-  main_df[main_df[,1] %in% test_num,]
    refData$target <- 'No'
    train <- rbind(currentData, refData)
    
    #model
    g <- train(as.factor(target) ~ ., data = train[,-c(1,2)], method = model,trControl = fitControl, 
               verbose = T, preProc = c("center", "scale"),tuneLength = 6,metric = "ROC",tuneGrid = gbmGrid)
    p <- predict(g, newdata = currentData[,-c(1,2)], type = "prob")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p$Yes)
    return(result)
}

#################
### Main Loop ###
#################
library(doMC)
registerDoMC(cores = 2)
set.seed(888)
fitControl <- trainControl(method = "none",number = 10,repeats = 3,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 4,alpha = 0.05,method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(interaction.depth = 4,
                        n.trees = 150,
                        shrinkage = 0.1)
submission <- data.frame()

for (driver in drivers){
    result <- classifier(driver,'gbm',5)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}