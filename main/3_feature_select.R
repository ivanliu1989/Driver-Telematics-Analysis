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
                           summaryFunction = twoClassSummary,adaptive = list(min = 12,alpha = 0.05,method = "BT",complete = TRUE))

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'
test_num <- sample(drivers[which(drivers!=driver)],5)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)

g <- train(as.factor(target) ~ ., data = train[,-c(1,2)], method = model,trControl = fitControl, 
           verbose = T, preProc = c("center", "scale"),metric = "ROC")
p <- predict(g, newdata = currentData[,-c(1,2)], type = "prob")

### feature selection
# var imp
gbmImp2 <- varImp(g, scale = F)
gbmImp2
plot(gbmImp2, top = 100)

a_df <- data.frame(gbmImp2[1])
nonImpCol <- rownames(a_df)[which(a_df==0)]
nonImpCol <- gsub("`","",nonImpCol)
main_df <- main_df[,-which(colnames(main_df) %in% nonImpCol)]

# rfe
set.seed(888)
ctrl <- rfeControl(functions = lmFuncs,method = "repeatedcv",
                   repeats = 5,verbose = FALSE)
lmProfile <- rfe(x, y,sizes = subsets,rfeControl = ctrl)
lmProfile