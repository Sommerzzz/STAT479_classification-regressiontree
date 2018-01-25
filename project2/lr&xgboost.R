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
xgb_train_1 = xgbdata[1:120000,] 
xgb_train_2 = xgbdata[120001:240000,]
xgb_train_3 = xgbdata[240001:360000,]
xgb_train_4 = xgbdata[360001:480000,]
xgb_train_5 = xgbdata[480001:600000,]
xgb_train_6 = xgbdata[600001:720000,]

xgb_train_1 = data.table(xgb_train_1, keep.rownames = FALSE)
xgb_train_2 = data.table(xgb_train_2, keep.rownames = FALSE)
xgb_train_3 = data.table(xgb_train_3, keep.rownames = FALSE)
xgb_train_4 = data.table(xgb_train_4, keep.rownames = FALSE)
xgb_train_5 = data.table(xgb_train_5, keep.rownames = FALSE)
xgb_train_6 = data.table(xgb_train_6, keep.rownames = FALSE)

xgb_train_data_1 = xgb_train_1[,-1]
xgb_train_data_2 = xgb_train_2[,-1]
xgb_train_data_3 = xgb_train_3[,-1]
xgb_train_data_4 = xgb_train_4[,-1]
xgb_train_data_5 = xgb_train_5[,-1]
xgb_train_data_6 = xgb_train_6[,-1]

xgb_train_label_1 = as.numeric(xgb_train_1$lowbwt)
xgb_train_label_2 = as.numeric(xgb_train_2$lowbwt)
xgb_train_label_3 = as.numeric(xgb_train_3$lowbwt)
xgb_train_label_4 = as.numeric(xgb_train_4$lowbwt)
xgb_train_label_5 = as.numeric(xgb_train_5$lowbwt)
xgb_train_label_6 = as.numeric(xgb_train_6$lowbwt)

xgb_train_label_1[xgb_train_label_1==1] = 0
xgb_train_label_2[xgb_train_label_2==1] = 0
xgb_train_label_3[xgb_train_label_3==1] = 0
xgb_train_label_4[xgb_train_label_4==1] = 0
xgb_train_label_5[xgb_train_label_5==1] = 0
xgb_train_label_6[xgb_train_label_6==1] = 0

xgb_train_label_1[xgb_train_label_1==2] = 1
xgb_train_label_2[xgb_train_label_2==2] = 1
xgb_train_label_3[xgb_train_label_3==2] = 1
xgb_train_label_4[xgb_train_label_4==2] = 1
xgb_train_label_5[xgb_train_label_5==2] = 1
xgb_train_label_6[xgb_train_label_6==2] = 1

xgb_test = xgbdata[720001:738000,]
xgb_test = data.table(xgb_test, keep.rownames = FALSE)
xgb_test_data = xgb_test[,-1]
xgb_test_label = as.numeric(xgb_test$lowbwt)
xgb_test_label[xgb_test_label==1] = 0
xgb_test_label[xgb_test_label==2] = 1
# one-hot coding step
sparse_xgb_train_data_1 = sparse.model.matrix(~.-1, data = xgb_train_data_1)
sparse_xgb_train_data_2 = sparse.model.matrix(~.-1, data = xgb_train_data_2)
sparse_xgb_train_data_3 = sparse.model.matrix(~.-1, data = xgb_train_data_3)
sparse_xgb_train_data_4 = sparse.model.matrix(~.-1, data = xgb_train_data_4)
sparse_xgb_train_data_5 = sparse.model.matrix(~.-1, data = xgb_train_data_5)
sparse_xgb_train_data_6 = sparse.model.matrix(~.-1, data = xgb_train_data_6)

sparse_xgb_test_data = sparse.model.matrix(~.-1, data = xgb_test_data)
########## logistic regression ##########
### model fitting
glmmodel_1 <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train_1)
glmmodel_2 <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train_2)
glmmodel_3 <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train_3)
glmmodel_4 <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train_4)
glmmodel_5 <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train_5)
glmmodel_6 <- glm(lowbwt ~.,family=binomial(link='logit'),data=xgb_train_6)

model.results_1 <- predict(glmmodel_1,newdata=xgb_train_1[,-1],type='response')
model.results_2 <- predict(glmmodel_2,newdata=xgb_train_2[,-1],type='response')
model.results_3 <- predict(glmmodel_3,newdata=xgb_train_3[,-1],type='response')
model.results_4 <- predict(glmmodel_4,newdata=xgb_train_4[,-1],type='response')
model.results_5 <- predict(glmmodel_5,newdata=xgb_train_5[,-1],type='response')
model.results_6 <- predict(glmmodel_6,newdata=xgb_train_6[,-1],type='response')

threshold = 0.08
prediction <- as.numeric(model.results_1 > threshold)
result <- as.data.frame(xgb_train_label_1, prediction))

TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(result)[1]) {
  if (result$xgb_train_label_1[i]==0 && result$prediction[i]==0) {
    TP = TP+1
  } else if (result6$xgb_train_label_1[i]==0 && result$prediction[i]==1) {
    FN = FN+1
  } else if (result6$xgb_train_label_1[i]==1 && result$prediction[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/120000
error_rate 

### validation
val.results_1 <- predict(glmmodel_1,newdata=xgb_test_data,type='response')
val.results_2 <- predict(glmmodel_2,newdata=xgb_test_data,type='response')
val.results_3 <- predict(glmmodel_3,newdata=xgb_test_data,type='response')
val.results_4 <- predict(glmmodel_4,newdata=xgb_test_data,type='response')
val.results_5 <- predict(glmmodel_5,newdata=xgb_test_data,type='response')
val.results_6 <- predict(glmmodel_6,newdata=xgb_test_data,type='response')

validation_combined <- as.data.frame(cbind(xgb_test_label, validation_2$predicted, validation_3$predicted, validation_4$predicted, validation_5$predicted, validation_6$predicted))
colnames(validation_combined) <- c("observed", "predicted1", "predicted2", "predicted3", "predicted4", "predicted5", "predicted6")

pred <- rep(-1,dim(validation_combined)[1])
for (i in 1:(dim(validation_combined)[1])) {
  if (sum(validation_combined[i,-1] == 0) >= sum(validation_combined[i,-1] == 1)) {
    pred[i] <- 0
  } else if (sum(validation_combined[i,-1] == 0) < sum(validation_combined[i,-1] == 1)) {
    pred[i] <- 1
  }
}

validation_combined <- cbind(validation_combined, pred)

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
error_rate