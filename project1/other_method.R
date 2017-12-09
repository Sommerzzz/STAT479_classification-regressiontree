mydata_1 = read.table("mydata_1.rdata", header = TRUE)
mytrain = mydata_1[1:2922,]
mytest = mydata_1[2923:13597,]
mytrain_data = mytrain[,-which(colnames(mytrain) == "INTRDVX")]
mytrain_response = mytrain$INTRDVX
mytest_data = mytest[,-which(colnames(mytest) == "INTRDVX")]
########## rpart ##########
library(rpart)
## cv
mse_rpart <- 0; ncv <- 10
cvgroup <- sample.int(ncv,nrow(mytrain),replace=TRUE)
for(j in 1:ncv){ # first let j = 1 to create a cv.rdata
  mytrain$wt <- rep(0,nrow(mytrain)) # create a weight variable
  mytrain$wt[cvgroup != j] <- 1 
  train_data = mytrain[which(mytrain[,ncol(mytrain)] == 1),-ncol(mytrain)]
  test_data = mytrain[which(mytrain[,ncol(mytrain)] != 1),-ncol(mytrain)]
  fit_rpart <- rpart(train_data$INTRDVX ~ ., method = "anova", data = train_data)
  predict_rpart <- predict(fit_rpart, newdata = test_data)
  prefit_rpart <- data.frame(obs = test_data$INTRDVX, predict_rpart)
  mse_rpart <- mse_rpart+sum((prefit_rpart$predict_rpart - prefit_rpart$obs)^2)
}
mse_rpart <- mse_rpart/nrow(mytrain) # 102943042
## prediction
fit_rpart <- rpart(mytrain$INTRDVX ~ ., method = "anova", data = mytrain[,-ncol(mytrain)])
predict_rpart <- predict(fit_rpart, newdata = mytest_data)
write.table(predict_rpart, "prefit_rpart.txt", row.names = FALSE)

########## party ##########
library(party)
## cv
mse_ctree <- 0; ncv <- 10
cvgroup <- sample.int(ncv,nrow(mytrain),replace=TRUE)
for(j in 1:ncv){ # first let j = 1 to create a cv.rdata
  mytrain$wt <- rep(0,nrow(mytrain)) # create a weight variable
  mytrain$wt[cvgroup != j] <- 1 
  train_data = mytrain[which(mytrain[,ncol(mytrain)] == 1),-ncol(mytrain)]
  test_data = mytrain[which(mytrain[,ncol(mytrain)] != 1),-ncol(mytrain)]
  fit_ctree <- ctree(train_data$INTRDVX ~ ., data = train_data)
  predict_ctree <- predict(fit_ctree, newdata = test_data)
  prefit_ctree <- data.frame(obs = test_data$INTRDVX, predict_ctree)
  mse_ctree <- mse_ctree+sum((prefit_ctree[,2] - prefit_ctree[,1])^2)
}
mse_ctree <- mse_ctree/nrow(mytrain) # 101552174
## prediction
fit_ctree <- ctree(mytrain$INTRDVX ~ ., data = mytrain[,-ncol(mytrain)])
predict_ctree <- predict(fit_ctree, newdata = mytest_data, type = "response")
write.table(predict_ctree, "prefit_ctree.txt", row.names = FALSE)

########## randomForest ##########
library(randomForest)
## cv
mse_rf <- 0; ncv <- 10
cvgroup <- sample.int(ncv,nrow(mytrain),replace=TRUE)
for(j in 1:ncv){ # first let j = 1 to create a cv.rdata
  mytrain$wt <- rep(0,nrow(mytrain)) # create a weight variable
  mytrain$wt[cvgroup != j] <- 1 
  train_data = mytrain[which(mytrain[,ncol(mytrain)] == 1),-ncol(mytrain)]
  test_data = mytrain[which(mytrain[,ncol(mytrain)] != 1),-ncol(mytrain)]
  fit_rf <- randomForest(train_data$INTRDVX ~ ., data = train_data)
  predict_rf <- predict(fit_rf, newdata = test_data)
  prefit_rf <- data.frame(obs = test_data$INTRDVX, predict_rf)
  mse_rf <- mse_rf+sum((prefit_rf[,2] - prefit_rf[,1])^2)
}
mse_rf <- mse_rf/nrow(mytrain) # 73069480
## prediction
fit_rf <- randomForest(mytrain$INTRDVX ~ ., data = mytrain[,-ncol(mytrain)])
predict_rf <- predict(fit_rf, newdata = mytest_data, type = "response")
write.table(predict_rf, "prefit_rf.txt", row.names = FALSE)
## variable importance
rf_frame <- data.frame(variable = attr(fit_rf$importance,"dimnames")[[1]][order(fit_rf$importance, decreasing = TRUE)],
                       imp.score = fit_rf$importance[order(fit_rf$importance, decreasing = TRUE)])

########## ranger ##########
library(ranger)
## cv
mse_ranger <- 0; ncv <- 10
cvgroup <- sample.int(ncv,nrow(mytrain),replace=TRUE)
for(j in 1:ncv){ # first let j = 1 to create a cv.rdata
  mytrain$wt <- rep(0,nrow(mytrain)) # create a weight variable
  mytrain$wt[cvgroup != j] <- 1 
  train_data = mytrain[which(mytrain[,ncol(mytrain)] == 1),-ncol(mytrain)]
  test_data = mytrain[which(mytrain[,ncol(mytrain)] != 1),-ncol(mytrain)]
  fit_ranger <- ranger(train_data$INTRDVX ~ ., data = train_data)
  predict_ranger <- predict(fit_ranger, data = test_data, type = "response")
  prefit_ranger <- data.frame(obs = test_data$INTRDVX, predict_ranger$predictions)
  mse_ranger <- mse_ranger+sum((prefit_ranger[,2] - prefit_ranger[,1])^2)
}
mse_ranger <- mse_ranger/nrow(mytrain) # 122875860
## prediction
fit_ranger <- ranger(mytrain$INTRDVX ~ ., data = mytrain[,-ncol(mytrain)])
predict_ranger <- predict(fit_ranger, data = mytest_data, type = "response")
predict_ranger <- predict_ranger$predictions
write.table(predict_ranger, "prefit_ranger.txt", row.names = FALSE)

########## write the final results ##########
method1 <- read.table("prefit_5.txt", header = TRUE)
method1 <- method1[which(method1$train == "n"), 3]
method2 <- read.table("prefit_rf.txt", header = TRUE)
method2 <- method2[,1]
result <- cbind(method1, method2)
write.table(result, "results_project1.txt", row.names = FALSE)
