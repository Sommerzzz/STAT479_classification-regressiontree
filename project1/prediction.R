########## combine train and test data ##########
sink("prediction.txt")
library(readr)
data_imputed = as.data.frame(read_delim(file="data_imputed_3.rdata",delim=" ", quote="\\\"",escape_double=FALSE,
                                        trim_ws=TRUE,guess_max=30000)) 
data_test = as.data.frame(read_delim(file="data_test.rdata",delim=" ", quote="\\\"",escape_double=FALSE,
                                     trim_ws=TRUE,guess_max=30000)) 
## find variables in data_imputed_1.rdata but not in data_test.rdata
exclude_var1 = c()
for (var in colnames(data_imputed)) {
  if (!(var %in% colnames(data_test))) {
    exclude_var1 = append(exclude_var1, var)
  } 
}
## find variables in data_test.rdata but not in data_imputed_1.rdata
exclude_var2 = c()
for (var in colnames(data_test)) {
  if (!(var %in% colnames(data_imputed))) {
    exclude_var2 = append(exclude_var2, var)
  }
}
## add a weight variable
data_imputed$wt = 1
data_test$wt = 0
## combine two data sets
data_test[exclude_var1] = NA
data_imputed[exclude_var2] = NA
data_comp = rbind(data_imputed, data_test)
data_comp[2925,232] <- NA # "\"S35B\""
for (i in 1:ncol(data_comp)) {
  if (is.character(data_comp[,i])) {
    data_comp[,i] = factor(data_comp[,i])
  }
}
write.table(data_comp, "data_comp_3.rdata", row.names = FALSE, col.names = TRUE, quote = FALSE)
## make a description file predsc.txt for data_comp.rdata
data_comp.names = names(data_comp)
k = ncol(data_comp)
role <- rep("n", k)
for(j in 1:k){
  if(class(data_comp[,j]) == "factor") role[j] <- "c"
}
role[data_comp.names %in% exclude_var1] <- "x"
role[data_comp.names %in% exclude_var2] <- "x"
role[data_comp.names %in% "INTRDVX"] <- "d"
role[data_comp.names %in% "FINLWT21"] <- "x"
role[data_comp.names %in% "INTRDVBX"] <- "x"
role[data_comp.names %in% "INTR_VBX"] <- "x"
role[data_comp.names %in% "INTRDVB"] <- "x"
role[data_comp.names %in% "INTRDVB_"] <- "x"
role[data_comp.names %in% "INTRDVX1"] <- "x"
role[data_comp.names %in% "INTRDVX2"] <- "x"
role[data_comp.names %in% "INTRDVX3"] <- "x"
role[data_comp.names %in% "INTRDVX4"] <- "x"
role[data_comp.names %in% "INTRDVX5"] <- "x"
role[data_comp.names %in% "INTRDVXI"] <- "x"
role[data_comp.names %in% "INTRDVXM"] <- "x"
role[data_comp.names %in% "INTRDVX_"] <- "x"
role[data_comp.names %in% "wt"] <- "w"
write("data_comp_3.rdata","predsc_3.txt")
write("NA",file="predsc_3.txt",append=TRUE)
write("2",file="predsc_3.txt",append=TRUE)
write.table(cbind(1:k, names(data_comp), role),file="predsc_3.txt",
            append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)
sink()
########## run guide < preinput.txt ##########
system("./guide < preinput.txt > log.txt")

prefit <- read.table("prefit.txt", header = T)
prefit_train <- prefit[1:2922,]
mse <- sum((prefit_train$predicted - prefit_train$observed)^2)/2922

prefit_8 <- read.table("prefit_8.txt", header = T)
prefit_train_8 <- prefit_8[1:2922,]
mse <- sum((prefit_train_8$predicted - prefit_train_8$observed)^2)/2922
