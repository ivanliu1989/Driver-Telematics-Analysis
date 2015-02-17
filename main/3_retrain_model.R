setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
driver <- 1
model <- 'gbm'

fitControl <- trainControl(method = "none",number = 10,repeats = 3,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 4,alpha = 0.05,method = "BT",complete = TRUE))
gbmGrid <-  expand.grid(interaction.depth = 3,
                        n.trees = 100,
                        shrinkage = 0.1)
gbmGrid <-  expand.grid(mtry = 18)

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
