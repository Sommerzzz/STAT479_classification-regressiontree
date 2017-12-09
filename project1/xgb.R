install.packages("xgboost")
require(xgboost)
require(tidyverse)
require(Matrix)
require(data.table)

data("agaricus.train")
data("agaricus.test")
bst <- xgboost(agaricus.train$data, agaricus.train$label, max.depth = 2, eta = 1, nround = 2, objective = "binary:logistic")
pred <- predict(bst, agaricus.test$data)
cv.res <- xgb.cv(data = agaricus.train$data, label = agaricus.train$label, max.depth = 2, eta = 1, nround = 2, objective = "binary:logistic", nfold = 5)

xgb.DMatrix(data = agaricus.train$data, label = agaricus.train$label)

########## data preparation ##########
## transform xgbdata with categorical variables into a very sparse matrix of numeric features
xgbdata = read.table("mydata_1.rdata", header = TRUE)
xgb_train = xgbdata[1:2922,]
xgb_train = data.table(xgb_train, keep.rownames = FALSE)
xgb_train_data = xgb_train[,-223]
xgb_test_data = xgbdata[2923:13597,] %>% select(-INTRDVX)
xgb_test_data = data.table(xgb_test_data, keep.rownames = FALSE)
# one-hot coding step
sparse_xgb_train_data = sparse.model.matrix(~.-1, data = xgb_train_data)
sparse_xgb_test_data = sparse.model.matrix(~.-1, data = xgb_test_data)
########## modelcv ##########
cv.res <- xgb.cv(data = sparse_xgb_train_data, label = xgb_train$INTRDVX, max.depth = 5, 
                 eta = 1, nthread = 2, nround = 20, objective = "reg:linear", 
                 nfold = 10)
bst <- xgboost(data = sparse_xgb_train_data, label = xgb_train$INTRDVX, max.depth = 5, 
               eta = 1, nthread = 2, nround = 20, objective = "reg:linear")

mse <- 0; ncv <- 10
cvgroup <- sample.int(ncv,nrow(sparse_xgb_train_data),replace=TRUE)
for(j in 1:ncv){ # first let j = 1 to create a cv.rdata
  sparse_xgb_train_data_wt <- cbind(sparse_xgb_train_data, rep(0,nrow(sparse_xgb_train_data))) # create a weight variable
  sparse_xgb_train_data_wt[,ncol(sparse_xgb_train_data_wt)][cvgroup != j] <- 1 
  train_data = sparse_xgb_train_data_wt[which(sparse_xgb_train_data_wt[,ncol(sparse_xgb_train_data_wt)] == 1),-ncol(sparse_xgb_train_data_wt)]
  train_label = xgb_train$INTRDVX[cvgroup != j]
  bst <- xgboost(data = train_data, label = train_label, max.depth = 5, 
                 eta = 1, nthread = 2, nround = 20, objective = "reg:linear")
  pred <- predict(bst, sparse_xgb_train_data_wt[which(sparse_xgb_train_data_wt[,ncol(sparse_xgb_train_data_wt)] != 1),-ncol(sparse_xgb_train_data_wt)])
  prefit_xgb <- data.frame(obs = xgb_train$INTRDVX[cvgroup == j], pred) 
  for (i in 1:length(pred)) {
    if (prefit_xgb$pred[i] < 0) {
      prefit_xgb$pred[i] = 0
    }
  }
  mse <- mse+sum((prefit_xgb$pred - prefit_xgb$obs)^2)
}
mse <- mse/nrow(sparse_xgb_train_data) # 59803411
########## prediction ##########
bst <- xgboost(data = sparse_xgb_train_data, label = xgb_train$INTRDVX, max.depth = 5, 
               eta = 1, nthread = 2, nround = 20, objective = "reg:linear")
pred_train <- predict(bst, sparse_xgb_train_data)
for (i in 1:length(pred_train)) {
  if (pred_train[i] < 0) {
    pred_train[i] = 0
  }
}
mse_train <- sum((pred_train-xgb_train$INTRDVX)^2)/nrow(xgb_train)
pred_test <- predict(bst, sparse_xgb_test_data)
for (i in 1:length(pred_test)) {
  if (pred_test[i] < 0) {
    pred_test[i] = 0
  }
}
write.table(pred_test, "prefit_xgb.txt", row.names = FALSE)
