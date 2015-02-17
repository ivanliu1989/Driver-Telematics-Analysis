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
    g <- train(as.factor(target) ~ ., data = train[,c(features,'target')], method = model,trControl = fitControl, 
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
load('Driver-Telematics-Analysis/feature_selection/rfe_var.RData')
set.seed(888)
fitControl <- trainControl(method = "none",number = 10,repeats = 3,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 4,alpha = 0.05,method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(interaction.depth = c(1,3,9),
                        n.trees = c(50,100,150,250),
                        shrinkage = c(0.05,0.1))
gbmGrid <-  expand.grid(mtry = 18)
submission <- data.frame()

for (driver in drivers){
    result <- classifier(driver,'rf',5,rfe_var)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}

write.csv(submission, file = 'submission_rf.csv', quote = F, row.names = F)
sum(is.na(submission))
