setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))
driver <- 1
model <- 'rf'

#model sbf_var rfe_var
load('Driver-Telematics-Analysis/feature_selection/rfe_var.RData')
fitControl <- trainControl(method = "adaptive_cv",number = 10,repeats = 5,classProbs = TRUE,
                           summaryFunction = twoClassSummary,adaptive = list(min = 12,alpha = 0.05,method = "BT",complete = TRUE))

currentData <- main_df[main_df[,1]==driver,]
currentData$target <- 'Yes'
test_num <- sample(drivers[which(drivers!=driver)],5)
refData <-  main_df[main_df[,1] %in% test_num,]
refData$target <- 'No'
train <- rbind(currentData, refData)

g <- train(as.factor(target) ~ ., data = train[,c(rfe_var,'target')], method = model,trControl = fitControl, 
           verbose = T, preProc = c("center", "scale"),metric = "ROC",tuneLength=12)
p <- predict(g, newdata = currentData[,-c(1,2)], type = "prob")


### feature selection ### 
# var imp
gbmImp2 <- varImp(g, scale = F)
gbmImp2
plot(gbmImp2, top = 100)

a_df <- data.frame(gbmImp2[1])
nonImpCol <- rownames(a_df)[which(a_df==0)]
nonImpCol <- gsub("`","",nonImpCol)
main_df <- main_df[,-which(colnames(main_df) %in% nonImpCol)]

# rfe 40
set.seed(888)
cols <- c(5,10,20,40,60,76)
ctrl <- rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 5,verbose = FALSE)
lmProfile <- rfe(x=train[,-c(1,2,80)], y=as.factor(train$target),sizes = cols,rfeControl = ctrl)
lmProfile
rfe_var <- predictors(lmProfile)
head(lmProfile$resample)
trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))
save(rfe_var, file='rfe_var.RData')

# sbf
filterCtrl <- sbfControl(functions = rfSBF,method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(x=train[,-c(1,2,80)], y=as.factor(train$target), sbfControl = filterCtrl)
rfWithFilter
sbf_var <- predictors(rfWithFilter)
head(rfWithFilter$resample)
trellis.par.set(caretTheme())
save(sbf_var, file='sbf_var.RData')

# GA
library(mlbench)
ga_ctrl <- gafsControl(functions = rfGA,method = "repeatedcv",repeats = 5)
set.seed(10)
rf_ga <- gafs(x=train[,-c(1,2,80)], y=as.factor(train$target),iters = 200,gafsControl = ga_ctrl)
rf_ga
plot(rf_ga) + theme_bw()

# SA
sa_ctrl <- safsControl(functions = rfSA,method = "repeatedcv",repeats = 5,improve = 50)
set.seed(10)
rf_sa <- safs(x=train[,-c(1,2,80)], y=as.factor(train$target),iters = 1000,safsControl = sa_ctrl)
rf_sa
plot(rf_sa) + theme_bw()


