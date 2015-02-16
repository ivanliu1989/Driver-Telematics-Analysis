# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

main_df <- data.frame(fread('data/main_df_89features.csv',header = T, stringsAsFactor = F))
# load(file='data/main_df_89features.RData')
head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

##########################
### Near Zero Variance ###
##########################
nzv <- nearZeroVar(main_df, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
main_df <- main_df[, -77] # nzv: ex_turn

#########################################
### Identifying Correlated Predictors ###
#########################################
descrCor <- cor(main_df[,-c(1,2,88)])
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]

###########################
### Linear Dependencies ###
###########################
comboInfo <- findLinearCombos(main_df)
comboInfo
main_df[, -comboInfo$remove]

#############################
### Centering and Scaling ###
#############################
preProcValues <- preProcess(main_df, method = c("center", "scale"))
main_dfTransformed <- predict(preProcValues, main_df)
