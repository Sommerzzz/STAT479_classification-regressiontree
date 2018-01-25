########## GUIDE: single tree ##########
for (i in 1:6) {
  assign(paste("prediction", i, sep = ""), read.table(paste("prediction_", i, ".txt", sep = ""), header = T))
}

prediction_combined <- cbind(prediction1$predicted_1, prediction2$predicted_2, prediction3$predicted_3, prediction4$predicted_4, prediction5$predicted_5, prediction6$predicted_6)
colnames(prediction_combined) <- c("predicted1", "predicted2", "predicted3", "predicted4", "predicted5", "predicted6")

pred <- rep(-1,dim(prediction_combined)[1])
for (i in 1:(dim(prediction_combined)[1])) {
  if (sum(prediction_combined[i,-1] == 0) >= sum(prediction_combined[i,-1] == 1)) {
    pred[i] <- 0
  } else if (sum(prediction_combined[i,-1] == 0) < sum(prediction_combined[i,-1] == 1)) {
    pred[i] <- 1
  }
}

write.table(pred, file = "guidesingletree.txt", row.names=FALSE, quote=TRUE)

########## GUIDE: guide forest ##########
for (i in 1:6) {
  assign(paste("prediction", i, sep = ""), read.table(paste("subset_", i, "_fit_gf_pr.txt", sep = ""), header = T))
}

prediction1 <- prediction1[prediction1$train == "n",]
prediction2 <- prediction2[prediction2$train == "n",]
prediction3 <- prediction3[prediction3$train == "n",]
prediction4 <- prediction4[prediction4$train == "n",]
prediction5 <- prediction5[prediction5$train == "n",]
prediction6 <- prediction6[prediction6$train == "n",]

prediction_combined <- cbind(prediction1$predicted,prediction2$predicted,prediction3$predicted,prediction4$predicted,prediction5$predicted,prediction6$predicted)
colnames(prediction_combined) <- c("predicted1", "predicted2", "predicted3", "predicted4", "predicted5", "predicted6")

pred <- rep(-1,dim(prediction_combined)[1])
for (i in 1:(dim(prediction_combined)[1])) {
  if (sum(prediction_combined[i,-1] == 0) >= sum(prediction_combined[i,-1] == 1)) {
    pred[i] <- 0
  } else if (sum(prediction_combined[i,-1] == 0) < sum(prediction_combined[i,-1] == 1)) {
    pred[i] <- 1
  }
}

write.table(pred, file = "guideforest.txt", row.names=FALSE, quote=TRUE)

########## GUIDE: bagging #########
for (i in 1:6) {
  assign(paste("prediction", i, sep = ""), read.table(paste("subset_", i, "_fit_bg_pr.txt", sep = ""), header = T))
}

prediction1 <- prediction1[prediction1$train == "n",]
prediction2 <- prediction2[prediction2$train == "n",]
prediction3 <- prediction3[prediction3$train == "n",]
prediction4 <- prediction4[prediction4$train == "n",]
prediction5 <- prediction5[prediction5$train == "n",]
prediction6 <- prediction6[prediction6$train == "n",]

prediction_combined <- cbind(prediction1$predicted,prediction2$predicted,prediction3$predicted,prediction4$predicted,prediction5$predicted,prediction6$predicted)
colnames(prediction_combined) <- c("predicted1", "predicted2", "predicted3", "predicted4", "predicted5", "predicted6")

pred <- rep(-1,dim(prediction_combined)[1])
for (i in 1:(dim(prediction_combined)[1])) {
  if (sum(prediction_combined[i,-1] == 0) >= sum(prediction_combined[i,-1] == 1)) {
    pred[i] <- 0
  } else if (sum(prediction_combined[i,-1] == 0) < sum(prediction_combined[i,-1] == 1)) {
    pred[i] <- 1
  }
}

write.table(pred, file = "guidebagging.txt", row.names=FALSE, quote=TRUE)



### compare results
pred1 <- read.table("guidesingletree.txt", header = T)
pred2 <- read.table("guideforest.txt", header = T)
pred3 <- read.table("guidebagging.txt", header = T)

pre <- cbind(pred1, pred2, pred3)
colnames(pre) <- c("pre1","pre2","pre3")

sum(pre$pre1 == pre$pre2)
sum(pre$pre1 == pre$pre3)
sum(pre$pre2 == pre$pre3)
