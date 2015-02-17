setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
driver <- 1
model <- 'gbm'

#model
fitControl <- trainControl(method = "adaptive_cv",number = 10,repeats = 5,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 8,alpha = 0.05,method = "BT",complete = TRUE))

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'
test_num <- sample(drivers[which(drivers!=driver)],5)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)

g <- train(as.factor(target) ~ ., data = train[,-c(1,2)], method = model,trControl = fitControl, 
           verbose = T, preProc = c("center", "scale"),metric = "ROC")
p <- predict(g, newdata = currentData[,-c(1,2)], type = "prob")

# feature selection
gbmImp <- varImp(g, scale = FALSE)
gbmImp
plot(gbmImp, top = 20)
RocImp <- filterVarImp(x = training[, -ncol(training)], y = training$Class)
head(RocImp)

set.seed(10)
ctrl <- rfeControl(functions = lmFuncs,method = "repeatedcv",
                   repeats = 5,verbose = FALSE)
lmProfile <- rfe(x, y,sizes = subsets,rfeControl = ctrl)
lmProfile