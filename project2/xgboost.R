require(xgboost)
require(dplyr)
require(Matrix)
require(data.table)

########## data preparation ##########
## transform xgbdata with categorical variables into a very sparse matrix of numeric features
a <- "factor"; n <- "numeric"
vartype <- c(a,a,a,a,n,a,n,n,n,n,a,n,a,a,a,a,a,a,
             n,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,n,a,
             a,a,a,a,a,n,a,n,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,n,n,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,n,n,a,a,a,a,n,a,a,a,a,
             a,a,a,n,a)
xgbdata = read.table("data_st_imputed.rdata", header = TRUE, colClasses = vartype)
xgb_train = xgbdata[1:720000,]
xgb_train = data.table(xgb_train, keep.rownames = FALSE)
xgb_train_data = xgb_train[,-1]
xgb_train_label = as.numeric(xgb_train$lowbwt)
xgb_train_label[xgb_train_label==1] = 0
xgb_train_label[xgb_train_label==2] = 1
xgb_test = xgbdata[720001:738000,]
xgb_test = data.table(xgb_test, keep.rownames = FALSE)
xgb_test_data = xgb_test[,-1]
xgb_test_label = as.numeric(xgb_test$lowbwt)
xgb_test_label[xgb_test_label==1] = 0
xgb_test_label[xgb_test_label==2] = 1
# one-hot coding step
sparse_xgb_train_data = sparse.model.matrix(~.-1, data = xgb_train_data)
sparse_xgb_test_data = sparse.model.matrix(~.-1, data = xgb_test_data)
########## model ##########
bst <- xgboost(data = sparse_xgb_train_data, label = xgb_train_label, max.depth = 5, eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")
bst_linear <- xgboost(data = sparse_xgb_train_data, label = xgb_train_label, booster = "gblinear", max.depth = 5, nthread = 2, nround = 10, objective = "binary:logistic")
########## feature importance ##########
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
xgb.dump(bst, with.stats = T)
########## validation ##########
pred <- predict(bst_linear, sparse_xgb_test_data)
threshold = 0.08
prediction <- as.numeric(pred > threshold)
result <- as.data.frame(cbind(xgb_test_label, prediction))
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(result)[1]) {
  if (result$xgb_test_label[i]==0 && result$prediction[i]==0) {
    TP = TP+1
  } else if (result$xgb_test_label[i]==0 && result$prediction[i]==1) {
    FN = FN+1
  } else if (result$xgb_test_label[i]==1 && result$prediction[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}
error_rate <- (FP*10+FN)/18000
error_rate # linear 0.2762222
### 2,1,2,2
# threshold: 0.1, 0.15, 0.2, 0.25, 0.3, 0.35
# error_rate: 0.3002222, 0.326, 0.345, 0.3620556, 0.3620556, 0.3847778
### 5,1,2,10
# threshold: 0.08, 0.1, 0.15
# error_rate: 0.2679444, 0.2747778, 0.2906667 
### 5,1,2,20
# threshold: 0.08
# error_rate: 0.2705556
### 8,1,2,10
# threshold: 0.08
# error_rate: 0.2734444
########## prediction ##########
a <- "factor"; n <- "numeric"
vartype <- c(a,a,a,n,a,n,n,n,n,a,n,a,a,a,a,a,a,
             n,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,n,a,
             a,a,a,a,a,n,a,n,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,n,n,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,n,n,a,a,a,a,n,a,a,a,a,
             a,a,a,n,a)
testsample = read.table("data_st_imputed_test.rdata", header = TRUE, colClasses = vartype)
test = data.table(testsample, keep.rownames = FALSE)
sparse_test = sparse.model.matrix(~.-1, data = test)
pred <- predict(bst_linear, sparse_test)
threshold = 0.08
prediction <- as.data.frame(as.numeric(pred > threshold))
colnames(prediction) <- "pred"
write.table(prediction, file = "xgboost_linear.txt", row.names=FALSE, quote=TRUE)

### save and load model
xgb.save(bst, "xgboost.model")
xgb.save(bst_linear, "xgboost_linear.model")
# load binary model to R
bst2 <- xgb.load("xgboost.model")
bst_linear2 <- xgb.load("xgboost_linear.model")
xgbpre <- predict(bst_linear2, sparse_xgb_test_data)
# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(xgbpre-pred))))



########## logistic regression ##########
### model fitting
glmmodel <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train)
### validation
val.results <- predict(glmmodel,newdata=xgb_test_data,type='response')
threshold = 0.08
prediction <- as.numeric(val.results > threshold)
result <- as.data.frame(cbind(xgb_test_label, prediction))
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(result)[1]) {
  if (result$xgb_test_label[i]==0 && result$prediction[i]==0) {
    TP = TP+1
  } else if (result$xgb_test_label[i]==0 && result$prediction[i]==1) {
    FN = FN+1
  } else if (result$xgb_test_label[i]==1 && result$prediction[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}
error_rate <- (FP*10+FN)/18000
error_rate # 0.2698333
glmtable <- summary(glmmodel)$coefficients
glmtable[order(-abs(glmtable[,3])),]
########## prediction ##########
a <- "factor"; n <- "numeric"
vartype <- c(a,a,a,n,a,n,n,n,n,a,n,a,a,a,a,a,a,
             n,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,n,a,
             a,a,a,a,a,n,a,n,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,n,n,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,n,n,a,a,a,a,n,a,a,a,a,
             a,a,a,n,a)
testsample = read.table("data_st_imputed_test.rdata", header = TRUE, colClasses = vartype)
test = data.table(testsample, keep.rownames = FALSE)
sparse_test = sparse.model.matrix(~.-1, data = test)
pred <- predict(glmmodel,newdata=test,type='response')
threshold = 0.08
prediction <- as.data.frame(as.numeric(pred > threshold))
colnames(prediction) <- "pred"
write.table(prediction, file = "logisticregression.txt", row.names=FALSE, quote=TRUE)

########## compare with guide ##########
pred1 <- read.table("guidesingletree.txt", header = T)
pred2 <- read.table("guideforest.txt", header = T)
pred3 <- read.table("guidebagging.txt", header = T)
pred4 <- read.table("xgboost.txt", header = T)
pred5 <- read.table("xgboost_linear.txt", header = T)
pred6 <- read.table("logisticregression.txt", header = T) 
pre <- cbind(pred1, pred2, pred3, pred4, pred5, pred6)
colnames(pre) <- c("pre1","pre2","pre3","pre4","pre5","pre6")

sum(pre$pre1 == pre$pre2)
sum(pre$pre1 == pre$pre3)
sum(pre$pre2 == pre$pre3)
sum(pre$pre1 == pre$pre4)
sum(pre$pre2 == pre$pre4)
sum(pre$pre3 == pre$pre4)
sum(pre$pre1 == pre$pre6)
sum(pre$pre2 == pre$pre6)
sum(pre$pre3 == pre$pre6)
sum(pre$pre4 == pre$pre6)
sum(pre$pre1 == pre$pre5)
sum(pre$pre2 == pre$pre5)
sum(pre$pre3 == pre$pre5)
sum(pre$pre4 == pre$pre5)
sum(pre$pre6 == pre$pre5)
write.table(pre, file = "prediction.txt", row.names=FALSE, quote=TRUE)
########## write results_project2.txt ##########
guideforest <- pred2
logisticregression <- pred6
predicted <- as.data.frame(cbind(guideforest,logisticregression))
colnames(predicted) <- c("guideforest", "logisticregression")
write.table(predicted, file = "results_project2.txt", row.names=FALSE, quote=TRUE)




########## some plots ##########
library(ggplot2)
e <- ggplot(xgb_test, aes(feduc, lowbwt))
e + stat_bin_2d()
library(Amelia)
validation_set <- read.table("validation_set.rdata", header = T)
validation_set_missing <- validation_set %>% 
  select_("bmi","cig_0", "cig_1", "cig_2", "cig_3", 
         "combgest", "dwgt_r", "fagecomb", "illb_r", 
         "ilop_r", "m_ht_in", "mager", "priorterm", 
         "pwgt_r", "rf_cesarn", "wtgain")
missmap(validation_set_missing)



















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
