boost_trip <- table(match_matrix[,3])
boost_trip <- as.integer(names(boost_trip[-1]))

rf1 <- data.frame(fread('submission_rf_0.84880.csv',header = T, stringsAsFactor = F))
head(rf1)

rf1[1:200,][boost_trip,2] <- 1

write.csv(rf1, file = 'submission_rf_0.84880_bst.csv', quote = F, row.names = F)
