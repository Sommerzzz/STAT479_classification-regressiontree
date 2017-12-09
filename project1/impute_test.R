library(readr)
data_imputed = as.data.frame(read_delim(file="data_imputed_1.rdata",delim=" ", quote="\\\"",escape_double=FALSE,
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
## combine two data sets and delete variables
data_test[exclude_var1] = NA
data_imputed[exclude_var2] = NA
data_comp = rbind(data_imputed, data_test)
data_comp[2925,232] <- NA # "\"S35B\""
for (i in 1:ncol(data_comp)) {
  if (is.character(data_comp[,i])) {
    data_comp[,i] = factor(data_comp[,i])
  }
}
exclude_var3 = exclude_var1[-22]
mydata = data_comp %>% select(-one_of(exclude_var3)) %>% select(-one_of(exclude_var2))
write.table(mydata, "mydata.rdata", row.names = FALSE, col.names = TRUE, quote = FALSE)
## create description file
z <- read.table("mydata.rdata",header=TRUE)
z.names <- names(z)
role <- rep("n",ncol(z))
con.gp <- NULL # indices of variables with all NAs or are constant 
for(j in 1:ncol(z)){
  if(sum(!is.na(z[,j])) == 0){ # all NAs 
    con.gp <- c(con.gp,j)
  } else {
    if(class(z[,j]) == "factor"){
      if(nlevels(z[,j]) <= 1){
        con.gp <- c(con.gp,j) # only 1 level
      } else { role[j] <- "c" }
    } else { # exclude variables with constant values
      r <- range(z[,j],na.rm=TRUE)
      if(r[2] <= r[1]) con.gp <- c(con.gp,j) # constant 
    }
  }
}
role[con.gp] <- "x" # delete variables with all NA 
role[z.names %in% "INTRDVX"] <- "d" # define response
# delete variables functions of INTRDVX
role[z.names %in% "FINLWT21"] <- "x" 
role[z.names %in% "INTRDVBX"] <- "x"
role[z.names %in% "INTR_VBX"] <- "x" 
role[z.names %in% "INTRDVB"] <- "x" 
role[z.names %in% "INTRDVB_"] <- "x" 
role[z.names %in% "INTRDVX1"] <- "x" 
role[z.names %in% "INTRDVX2"] <- "x" 
role[z.names %in% "INTRDVX3"] <- "x" 
role[z.names %in% "INTRDVX4"] <- "x" 
role[z.names %in% "INTRDVX5"] <- "x" 
role[z.names %in% "INTRDVXI"] <- "x" 
role[z.names %in% "INTRDVXM"] <- "x" 
role[z.names %in% "INTRDVX_"] <- "x"
write("mydata.rdata","mydatadsc.txt")
write("NA",file="mydatadsc.txt",append=TRUE)
write("2",file="mydatadsc.txt",append=TRUE)
write.table(cbind(1:ncol(z),names(z),role),
            file="mydatadsc.txt",append=TRUE,
            row.names=FALSE,col.names=FALSE,quote=FALSE)
## guide impute 
z <- read.table("mydata.rdata",header=TRUE)
dsc <- readLines("mydatadsc.txt")
roles <- NULL
for(j in 4:length(dsc)){  # store original roles of variables
  strng <- strsplit(dsc[j]," +")[[1]]
  roles <- c(roles,strng[3])
}
roles[roles == "d"] <- "x"  # exclude INRDVX from imputation

n <- nrow(z)
z.imputed <- z
for(j in 1:ncol(z)){
  tmproles <- roles
  k <- sum(!is.na(z[,j]))
  if(k > 0 & k < n){ # skip variables without NAs
    tmproles[j] <- "d"   # make this variable "D"
    write(dsc[1],"imputedsc.txt")
    write("NA","imputedsc.txt",append=TRUE)
    write("2","imputedsc.txt",append=TRUE)
    write.table(cbind(1:ncol(z),names(z),tmproles),"imputedsc.txt",
                row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
    if(roles[j] == "n"){ # regression
      system("guide < reg_in_test.txt > log.txt")
    } else if(roles[j] == "c"){  # classification
      system("guide < class_in_test.txt > log.txt")
    }
    fit.info <- read.table("fit_test.txt",header=TRUE)
    gp <- fit.info$train == "n"
    z.imputed[gp,j] <- fit.info$predicted[gp]
  }
}
write.table(z.imputed,"mydata_1.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)


  