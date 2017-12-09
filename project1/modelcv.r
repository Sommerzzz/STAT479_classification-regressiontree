sink("log.txt")
########## guide model ##########
# training
data_imputed = as.data.frame(read_delim(file="data_imputed_1.rdata",delim=" ", quote="\\\"",escape_double=FALSE,
                                        trim_ws=TRUE,guess_max=30000)) 
data_test = as.data.frame(read_delim(file="data_test.rdata",delim=" ", quote="\\\"",escape_double=FALSE,
                                     trim_ws=TRUE,guess_max=30000)) 
## exclude variables in data_imputed_1.rdata but not in data_test.rdata
exclude_var = c()
for (var in colnames(data_imputed)) {
  if (!(var %in% colnames(data_test))) {
    exclude_var = append(exclude_var, var)
  } 
}
exclude_var = exclude_var[exclude_var != "INTRDVX"]
## make a description file cvdsc.txt for cv.rdata(data_imputed)
data_imputed.names = names(data_imputed)
k = ncol(data_imputed)
mse <- 0; ncv <- 10
set.seed(3)
cvgroup <- sample.int(ncv,nrow(data_imputed),replace=TRUE) # each obs corresponds to a random int in 1:10
data_imputed$wt <- rep(0,nrow(data_imputed)) # create a weight variable
data_imputed$wt[cvgroup != 1] <- 1 # cvgroup == 1 as training data
write.table(data_imputed, "cv.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
role <- rep("n",(k+1))
for(j in 1:k){
  if(class(data_imputed[,j]) == "factor") role[j] <- "c"
}
role[data_imputed.names %in% "INTRDVX"] <- "d"
role[data_imputed.names %in% "FINLWT21"] <- "x"
role[data_imputed.names %in% "INTRDVBX"] <- "x"
role[data_imputed.names %in% "INTR_VBX"] <- "x"
role[data_imputed.names %in% "INTRDVB"] <- "x"
role[data_imputed.names %in% "INTRDVB_"] <- "x"
role[data_imputed.names %in% "INTRDVX1"] <- "x"
role[data_imputed.names %in% "INTRDVX2"] <- "x"
role[data_imputed.names %in% "INTRDVX3"] <- "x"
role[data_imputed.names %in% "INTRDVX4"] <- "x"
role[data_imputed.names %in% "INTRDVX5"] <- "x"
role[data_imputed.names %in% "INTRDVXI"] <- "x"
role[data_imputed.names %in% "INTRDVXM"] <- "x"
role[data_imputed.names %in% "INTRDVX_"] <- "x"
for (var in exclude_var) {
  role[data_imputed.names %in% var] <- "x"
}
write("cv.rdata","cvdsc.txt")
write("NA",file="cvdsc.txt",append=TRUE)
write("2",file="cvdsc.txt",append=TRUE)
role[(k+1)]="w"
write.table(cbind(1:(k+1),names(data_imputed),role),file="cvdsc.txt",
            append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)
## run cv
mse <- 0; ncv <- 10
cvgroup <- sample.int(ncv,nrow(data_imputed),replace=TRUE)
for(j in 1:ncv){ # first let j = 1 to create a cv.rdata
  data_imputed$wt <- rep(0,nrow(datat_imputed)) # create a weight variable
  data_imputed$wt[cvgroup != j] <- 1 
  write.table(data_imputed,"cv.rdata",col.names=FALSE,row.names=FALSE,quote=TRUE) 
  system("guide < cvinput.txt > log.txt")
  ## change "system" to "shell" for Windows
  cvfit <- read.table("cvfit.txt",header=TRUE) 
  observed <- cvfit$observed[cvfit$train == "n"] 
  predicted <- cvfit$predicted[cvfit$train == "n"] 
  mse <- mse+sum((observed-predicted)^2)
}
mse <- mse/nrow(data_imputed) # mean-squared prediction error
print(mse)
sink()