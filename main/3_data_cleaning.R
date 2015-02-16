# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

# main_df <- data.frame(fread('data/main_df_103features.csv',header = T, stringsAsFactor = F))
load(file='data/main_df_103features.RData')
head(main_df)
datadirectory <- 'data/drivers/'
drivers <- sort(as.numeric(list.files(datadirectory)))

##################
### Null Value ###
##################
null_col <- c()
for (col in 1:length(colnames(main_df))){
    if(sum(is.na(main_df[,col])>0)){
        null_col <- c(null_col, col)
        print(col)
        main_df[which(is.na(main_df[,col])), col] <- 0
    }
}
# a <- main_df[which(is.na(main_df[,null_col[1]])),]

##########################
### Near Zero Variance ###
##########################
nzv <- nearZeroVar(main_df, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
# main_df <- main_df[, -77] # nzv: ex_turn

#########################################
### Identifying Correlated Predictors ###
#########################################
descrCor <- cor(main_df[,-c(1,2,103)])
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
highlyCorName <- colnames(main_df[,-c(1,2,103)][,highlyCorDescr])
head(main_df[,highlyCorName])
main_df <- main_df[,-which(colnames(main_df) %in% highlyCorName)]

###########################
### Linear Dependencies ###
###########################
comboInfo <- findLinearCombos(main_df[,-c(1,2,80)])
comboInfo
main_df[, -comboInfo$remove]

#############################
### Centering and Scaling ###
#############################
# preProcValues <- preProcess(main_df, method = c("center", "scale"))
# main_dfTransformed <- predict(preProcValues, main_df)
