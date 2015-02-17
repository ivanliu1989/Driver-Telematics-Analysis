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

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'

test_num <- sample(drivers[which(drivers!=driver)],2000)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)

g <- train(as.factor(target) ~ ., data = train[,-c(1,2)], method = model,trControl = fitControl, 
           verbose = T, preProc = c("center", "scale"),metric = "ROC",tuneGrid = gbmGrid)
p_other <- predict(g, newdata = refData[,-c(1,2)], type = "prob")
result_other <- data.frame(driver_trip=paste0(refData[,1],'_',refData[,2],sep=''), prob=p_other$Yes)
head(result_other[order(result_other[,2],decreasing = T),],1000)
