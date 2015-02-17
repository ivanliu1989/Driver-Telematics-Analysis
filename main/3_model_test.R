setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
driver <- 1
model <- 'gbm'

set.seed(888)
fitControl <- trainControl(method = "adaptive_cv",number = 10,repeats = 5,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 12,alpha = 0.05,method = "BT",complete = TRUE))

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'
test_num <- sample(drivers[which(drivers!=driver)],5)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)

system.time(g <- train(x = data.matrix(train[,-c(1,2,80)]), y = as.factor(train$target), method = model, trControl = fitControl, 
           preProc = c("center", "scale"), metric = "ROC", tuneLength=12, verbose=F))
p <- predict(g, newdata = data.matrix(currentData[,-c(1,2,80)]), type = "prob")

### Models:
# 4. knn - k = 13 | (0.8336250)
# 6. svmLinear | 0.76341
# 7. svmRadial | 
# 8. gbm - ntree150,inter4 | n.trees = 200, interaction.depth = 2 and shrinkage = 0.1 | (0.8933900) 0.80510
# 10. rf - mtry17 | 0.81715
# 11. glm | 0.76609
# 12. nnet - size1,decay0.1 | 


## 1. bdk/xyf(som) - xdim = 7, ydim = 9, xweight = 0.7857143 and topo = hexagonal | (0.7294500)
## 2. gamboost | *
## 3. glmnet - alpha = 0.7545455 and lambda = 0.1 | (0.6896600)
## 5. logreg | *
## 9. nb - LF0,kernel=T | 0.72578
## 13. LMT - | *
## 14. bayesglm - | *