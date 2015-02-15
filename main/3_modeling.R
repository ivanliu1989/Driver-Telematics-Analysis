# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

main_df <- data.frame(fread('data/main_df_89features.csv',header = T, stringsAsFactor = F))
# load(file='data/main_df_89features.RData')
head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

### Near Zero Variance 
nzv <- nearZeroVar(main_df, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
main_df <- main_df[, -77] # nzv: ex_turn

### Identifying Correlated Predictors
descrCor <- cor(main_df[,-c(1,2,88)])
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]

### Linear Dependencies
comboInfo <- findLinearCombos(main_df)
comboInfo
main_df[, -comboInfo$remove]

### Centering and Scaling
preProcValues <- preProcess(main_df, method = c("center", "scale"))
main_dfTransformed <- predict(preProcValues, main_df)

### main process
#Recursive Partitioning and Regression Trees
set.seed(888)
nrOfDriversToCompare <- 5
submission <- data.frame()
model <- function(driver) {
    currentData <- main_df[main_df[,1]==driver,]
    currentData$target <- 1
    
    test_num <- sample(drivers[which(drivers!=driver)],nrOfDriversToCompare)
    refData <-  main_df[main_df[,1] %in% test_num,]
    train <- rbind(currentData, refData)
    
    #model
    g = glm(as.factor(target) ~ ., data=train[,-c(1,2)], family = binomial("logit"))
    p = predict(g, currentData, type = "response")
    
    result <- data.frame(driver_trip=paste0(currentData[,1],'_',currentData[,2],sep=''), prob=p)
    return(result)
}

### main loop
for (driver in drivers){
    result <- model(driver)
    print(paste0('driver: ', driver, ' | ' ,date())) 
    
    submission <- rbind(submission, result)
}