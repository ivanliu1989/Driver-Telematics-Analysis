setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
driver <- 1
model <- 'knn'

fitControl <- trainControl(method = "adaptive_cv",number = 10,repeats = 5,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 12,alpha = 0.05,method = "BT",complete = TRUE))

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'
test_num <- sample(drivers[which(drivers!=driver)],5)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)

g <- train(as.factor(target) ~ ., data = train[,-c(1,2)], method = model,trControl = fitControl, 
           verbose = T, preProc = c("center", "scale"),metric = "ROC",tuneLength=12)

### Models:
# 1. bdk
# 2. gamBoost
# 3. glmnet
# 4. knn
# 5. logreg
# 6. svmLinear | 0.76341
# 7. svmRadial
# 8. gbm - ntree150,inter4 | 0.80510
# 9. nb - LF0,kernel=T | 0.72578
# 10. rf - mtry17 | 0.81715
# 11. glm | 0.76609
# 12. nnet - size1,decay0.1