########## GUIDE: singel tree ##########
## read results from 6 subsets
for (i in 1:6) {
  assign(paste("result", i, sep = ""), read.table(paste("subset_", i, "_fit.txt", sep = ""), header = T))
}

## error rate
# on each training set
for (i in 1:6) {
  error_rate <- eval(parse(text = paste("sum(result",i,"$observed != result",i,"$predicted)/120000",sep = "")))
  print(error_rate)
}

# on validation set
for (i in 1:6) {
  assign(paste("validation_",i,sep = ""), read.table(paste("validation_",i,".txt",sep = ""), header = T))
}

for (i in 1:6) {
  error_rate <- eval(parse(text = paste("sum(validation_",i,"$observed != validation_",i,"$predicted)/18000",sep = "")))
  print(error_rate)
}

validation_combined <- cbind(validation_1, validation_2$predicted, validation_3$predicted, validation_4$predicted, validation_5$predicted, validation_6$predicted)
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
sum(validation_combined$observed != validation_combined$pred)/18000

## error rate considering cost
# on each training set
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(result6)[1]) {
  if (result6$observed[i]==0 && result6$predicted[i]==0) {
    TP = TP+1
  } else if (result6$observed[i]==0 && result6$predicted[i]==1) {
    FN = FN+1
  } else if (result6$observed[i]==1 && result6$predicted[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/120000 
error_rate # 0.28455 0.2842833 0.285975 0.289525 0.2867583 0.27865 

# on validation set
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(validation_6)[1]) {
  if (validation_6$observed[i]==0 && validation_6$predicted[i]==0) {
    TP = TP+1
  } else if (validation_6$observed[i]==0 && validation_6$predicted[i]==1) {
    FN = FN+1
  } else if (validation_6$observed[i]==1 && validation_6$predicted[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/18000 
error_rate # 0.2934444 0.2956111 0.2909444 0.2923333 0.2893333 0.2916111

TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(validation_combined)[1]) {
  if (validation_combined$observed[i]==0 && validation_combined$pred[i]==0) {
    TP = TP+1
  } else if (validation_combined$observed[i]==0 && validation_combined$pred[i]==1) {
    FN = FN+1
  } else if (validation_combined$observed[i]==1 && validation_combined$pred[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/18000 
error_rate # 0.2895


########## GUIDE: guide forest ##########
## read results from 6 subsets
for (i in 1:6) {
  assign(paste("result", i, "_gf", sep = ""), read.table(paste("subset_", i, "_fit_gf.txt", sep = ""), header = T))
}

## error rate
# on each training set
result1_gf_train <- result1_gf %>% filter(train == "y")
result2_gf_train <- result2_gf %>% filter(train == "y")
result3_gf_train <- result3_gf %>% filter(train == "y")
result4_gf_train <- result4_gf %>% filter(train == "y")
result5_gf_train <- result5_gf %>% filter(train == "y")
result6_gf_train <- result6_gf %>% filter(train == "y")

for (i in 1:6) {
  error_rate <- eval(parse(text = paste("sum(result",i,"_gf_train$observed != result",i,"_gf_train$predicted)/120000",sep = "")))
  print(error_rate)
}

# on validation set
result1_gf_val <- result1_gf %>% filter(train == "n")
result2_gf_val <- result2_gf %>% filter(train == "n")
result3_gf_val <- result3_gf %>% filter(train == "n")
result4_gf_val <- result4_gf %>% filter(train == "n")
result5_gf_val <- result5_gf %>% filter(train == "n")
result6_gf_val <- result6_gf %>% filter(train == "n")

validation <- fread("validation_set.rdata")

validation_combined_gf <- data.frame(cbind(validation$lowbwt, result1_gf_val$predicted, result2_gf_val$predicted, result3_gf_val$predicted, result4_gf_val$predicted, result5_gf_val$predicted, result6_gf_val$predicted))
colnames(validation_combined_gf) <- c("observed", "predicted1", "predicted2", "predicted3", "predicted4", "predicted5", "predicted6")

pred <- rep(-1,dim(validation_combined_gf)[1])
for (i in 1:(dim(validation_combined_gf)[1])) {
  if (sum(validation_combined_gf[i,-1] == 0) >= sum(validation_combined_gf[i,-1] == 1)) {
    pred[i] <- 0
  } else if (sum(validation_combined_gf[i,-1] == 0) < sum(validation_combined_gf[i,-1] == 1)) {
    pred[i] <- 1
  }
}

validation_combined_gf <- cbind(validation_combined_gf, pred)
sum(validation_combined_gf$observed != validation_combined_gf$pred)/18000

for (i in 2:7) {
  error_rate <- eval(parse(text = paste("sum(validation_combined_gf[,1] != validation_combined_gf[,",i,"])/18000",sep = "")))
  print(error_rate)  
}

## error rate considering cost
# on each training set
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(result6_gf_train)[1]) {
  if (result6_gf_train$observed[i]==0 && result6_gf_train$predicted[i]==0) {
    TP = TP+1
  } else if (result6_gf_train$observed[i]==0 && result6_gf_train$predicted[i]==1) {
    FN = FN+1
  } else if (result6_gf_train$observed[i]==1 && result6_gf_train$predicted[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/120000 
error_rate # 0.2765917 0.2779583 0.2792 0.2813 0.2768583 0.2704917

# on validation set
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(validation_combined_gf)[1]) {
  if (validation_combined_gf$observed[i]==0 && validation_combined_gf$predicted6[i]==0) {
    TP = TP+1
  } else if (validation_combined_gf$observed[i]==0 && validation_combined_gf$predicted6[i]==1) {
    FN = FN+1
  } else if (validation_combined_gf$observed[i]==1 && validation_combined_gf$predicted6[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/18000 
error_rate # 0.2761111 0.2719444 0.2786667 0.2815556 0.2746111 0.2757778

TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(validation_combined_gf)[1]) {
  if (validation_combined_gf$observed[i]==0 && validation_combined_gf$pred[i]==0) {
    TP = TP+1
  } else if (validation_combined_gf$observed[i]==0 && validation_combined_gf$pred[i]==1) {
    FN = FN+1
  } else if (validation_combined_gf$observed[i]==1 && validation_combined_gf$pred[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/18000 
error_rate # 0.2750556




########## GUIDE: guide bagging ##########
## read results from 6 subsets
for (i in 1:6) {
  assign(paste("result", i, "_bg", sep = ""), read.table(paste("subset_", i, "_fit_bg.txt", sep = ""), header = T))
}

## error rate
# on each training set
result1_bg_train <- result1_bg %>% filter(train == "y")
result2_bg_train <- result2_bg %>% filter(train == "y")
result3_bg_train <- result3_bg %>% filter(train == "y")
result4_bg_train <- result4_bg %>% filter(train == "y")
result5_bg_train <- result5_bg %>% filter(train == "y")
result6_bg_train <- result6_bg %>% filter(train == "y")

for (i in 1:6) {
  error_rate <- eval(parse(text = paste("sum(result",i,"_bg_train$observed != result",i,"_bg_train$predicted)/120000",sep = "")))
  print(error_rate)
}

# on validation set
result1_bg_val <- result1_bg %>% filter(train == "n")
result2_bg_val <- result2_bg %>% filter(train == "n")
result3_bg_val <- result3_bg %>% filter(train == "n")
result4_bg_val <- result4_bg %>% filter(train == "n")
result5_bg_val <- result5_bg %>% filter(train == "n")
result6_bg_val <- result6_bg %>% filter(train == "n")

validation <- fread("validation_set.rdata")

validation_combined_bg <- data.frame(cbind(validation$lowbwt, result1_bg_val$predicted, result2_bg_val$predicted, result3_bg_val$predicted, result4_bg_val$predicted, result5_bg_val$predicted, result6_bg_val$predicted))
colnames(validation_combined_bg) <- c("observed", "predicted1", "predicted2", "predicted3", "predicted4", "predicted5", "predicted6")

pred <- rep(-1,dim(validation_combined_bg)[1])
for (i in 1:(dim(validation_combined_bg)[1])) {
  if (sum(validation_combined_bg[i,-1] == 0) >= sum(validation_combined_bg[i,-1] == 1)) {
    pred[i] <- 0
  } else if (sum(validation_combined_bg[i,-1] == 0) < sum(validation_combined_bg[i,-1] == 1)) {
    pred[i] <- 1
  }
}

validation_combined_bg <- cbind(validation_combined_bg, pred)
sum(validation_combined_bg$observed != validation_combined_bg$pred)/18000

for (i in 2:7) {
  error_rate <- eval(parse(text = paste("sum(validation_combined_bg[,1] != validation_combined_bg[,",i,"])/18000",sep = "")))
  print(error_rate)  
}

## error rate considering cost
# on each training set
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(result6_gf_train)[1]) {
  if (result6_gf_train$observed[i]==0 && result6_gf_train$predicted[i]==0) {
    TP = TP+1
  } else if (result6_gf_train$observed[i]==0 && result6_gf_train$predicted[i]==1) {
    FN = FN+1
  } else if (result6_gf_train$observed[i]==1 && result6_gf_train$predicted[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/120000 
error_rate # 0.2765917 0.2779583 0.2792 0.2813 0.2768583 0.2704917

# on validation set
TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(validation_combined_bg)[1]) {
  if (validation_combined_bg$observed[i]==0 && validation_combined_bg$predicted6[i]==0) {
    TP = TP+1
  } else if (validation_combined_bg$observed[i]==0 && validation_combined_bg$predicted6[i]==1) {
    FN = FN+1
  } else if (validation_combined_bg$observed[i]==1 && validation_combined_bg$predicted6[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/18000 
error_rate # 0.2889444 0.2873333 0.2894444 0.2875 0.2833333 0.2916667

TP = 0; FP = 0; FN = 0; TN = 0;
for (i in 1:dim(validation_combined_bg)[1]) {
  if (validation_combined_bg$observed[i]==0 && validation_combined_bg$pred[i]==0) {
    TP = TP+1
  } else if (validation_combined_bg$observed[i]==0 && validation_combined_bg$pred[i]==1) {
    FN = FN+1
  } else if (validation_combined_bg$observed[i]==1 && validation_combined_bg$pred[i]==0) {
    FP = FP+1
  } else {
    TN = TN+1
  }
}

error_rate <- (FP*10+FN)/18000 
error_rate # 0.2893889













































# read results from 43 subsets
for (i in 1:43) {
  assign(paste("result", i, sep = ""), read.table(paste("subset", i, "_fit.txt", sep = ""), header = T))
}

# error rate on training set
for (i in 1:43) {
  error_rate <- eval(parse(text = paste("sum(result",i,"$observed != result",i,"$predicted)/540000",sep = "")))
  print(error_rate)
}

# error rate on validation set
