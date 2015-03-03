setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
driver <- 2048
model <- 'avNNet'

set.seed(888)
fitControl <- trainControl(method = "adaptive_cv",number = 10,repeats = 5,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 8,alpha = 0.05,method = "BT",complete = TRUE))

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'
test_num <- sample(drivers[which(drivers!=driver)],5)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)
feature_list <- colnames(main_df[,-c(1,2,187)])

g <- train(x = data.matrix(train[,c(feature_list)]), y = as.factor(train$target), method = model, trControl = fitControl, 
           metric = "ROC", tuneLength=8, preProc = c("center", "scale", "pca"), repeats = 15, trace = FALSE)  #verbose=T,
# p <- predict(g, newdata = data.matrix(currentData[,-c(1,2,80)]), type = "prob")

### Models:
# 4. knn - k = 13 | (0.8336250) 0.76644 | 0.79242
# 6. svmLinear | 0.76341 | 0.80957
# 7. svmRadial - sigma=0.01546107 C=8 | 0.69989
# 8. gbm - ntree150,inter4 | n.trees = 450, interaction.depth = 8 and shrinkage = 0.1 
# | (0.8933900) 0.80510 | retrain: 0.51472 | 0.84145 | 0.83521
# 10. rf - mtry17 | 0.81715 / 0.81915 | 0.84880 | 0.84629 /0.84154 | 0.84077 none pre
# 11. glm | 0.76609 | 0.65470
# 12. nnet - size1,decay0.1 | 0.77582 | 0.80379 || neuralnet
# ensemble 0.88327
# 15. AdaBag / ada
# 16. avNNet - size = 9, decay = 0.1, bag = FALSE


## 1. bdk/xyf(som) - xdim = 7, ydim = 9, xweight = 0.7857143 and topo = hexagonal | (0.7294500)
## 2. gamboost | *
## 3. glmnet - alpha = 0.7545455 and lambda = 0.1 | (0.6896600)
## 5. logreg | *
## 9. nb - LF0,kernel=T | 0.72578
## 13. LMT - | *
## 14. bayesglm - | *