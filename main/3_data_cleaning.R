# setwd('H:/Machine_Learning/DTA')
setwd('/Users/ivan/Work_directory/DTA')
setwd('C:/Users/Ivan.Liuyanfeng/Desktop/Data_Mining_Work_Space/DTA')
rm(list=ls());gc()
require(caret);require(data.table)

# main_df <- data.frame(fread('data/main_df_136features.csv',header = T, stringsAsFactor = F))
load(file='data/main_df_190features.RData')
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
col_names <- colnames(main_df)
stat <- c()
calbri <- sapply(col_names, function(x){
    rng <- range(main_df[,x],na.rm = T)
    stat <- c(stat, rng)
})
rownames(calbri) <- c('min', 'max')
calbri

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

######################
### Abnormal Trips ###
######################
main_df[which(main_df[,'speed_100_pct']>120),c(1,2)]

speed_90_pct - 2.787522e+02
speed_95_pct - 3.922603e+02
speed_100_pct - 587.44258858
norAcc_100_pct - 53.99385598

# sp outliers > 120 (432)
driver trip
113  152 # 2 points
1080  100 # 2 points
1634  136 # 2 points
# 1635   71 large jumps
1635   88 # 2 points
# 1635  160 large jumps
1899  164
2056   21
2441   11
2441   76
2441  160
2441  185
2774   13
2973  130
2982   72
# 3105   15 large jumps
3562   95

driver <- 1899; trip <- 164; files <- paste0(path, driver, '/', trip, ".csv")
trip_data <- data.matrix(fread(files, header=T, sep="," ,stringsAsFactor=F))
plot(trip_data)