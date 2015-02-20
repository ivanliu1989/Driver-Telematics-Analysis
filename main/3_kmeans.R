df <- scale(main_df[,-c(1,2,136)])
df <- preProcess(main_df[,-c(1,2,136)],method = c('center','scale'))
df <- predict(df,main_df[,-c(1,2,136)])

fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers