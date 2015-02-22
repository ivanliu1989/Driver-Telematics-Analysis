# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

# main_df <- data.frame(fread('data/main_df_136features.csv',header = T, stringsAsFactor = F))
load(file='data/main_df_136features.RData')
head(main_df)

##################
### Null Value ###
##################
mean(is.na(main_df))
null_col <- c()
for (col in 1:length(colnames(main_df))){
    if(sum(is.na(main_df[,col])>0)){
        null_col <- c(null_col, col)
        print(col)
        main_df[which(is.na(main_df[,col])), col] <- 0
    }
}
colnames(main_df[,null_col])

##############################
### Data Range Calibration ###
##############################





##########################
### Near Zero Variance ###
##########################
nzv <- nearZeroVar(main_df, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

#########################################
### Identifying Correlated Predictors ###
#########################################
descrCor <- cor(main_df[,-c(1,2,136)])
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
highlyCorName <- colnames(main_df[,-c(1,2,136)][,highlyCorDescr])
head(main_df[,highlyCorName])
main_df <- main_df[,-which(colnames(main_df) %in% highlyCorName)]
colnames(main_df)

###########################
### Linear Dependencies ###
###########################
comboInfo <- findLinearCombos(main_df[,-c(1,2,136)])
colnames(main_df[,-c(1,2,136)][,comboInfo$remove])
main_df[, -comboInfo$remove]

#############################
### Centering and Scaling ###
#############################
# preProcValues <- preProcess(main_df, method = c("center", "scale"))
# main_dfTransformed <- predict(preProcValues, main_df)
