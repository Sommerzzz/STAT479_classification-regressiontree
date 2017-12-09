########## Step1: output DTsubset.rdata ##########
library(readr)
z <- as.data.frame(read_delim(file="data_complete.rdata",delim=" ", quote="\\\"",escape_double=FALSE,
                              trim_ws=TRUE,guess_max=30000)) 
### guess_max = number greater than #records in data file
## exclude records with missing INTRDVX values
z <- z[z$INTRDVX_ %in% c("\"D\"", "\"T\""),]
## export data subset
write.table(z, "DTsubset.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)

########## Step2: convert character to factor variables ##########
for (i in 1:ncol(z)) {
  if (is.character(z[,i])) {
    z[,i] = factor(z[,i])
  }
}

########## Step3: create DTsubsetdsc.txt ##########
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
write("DTsubset.rdata","DTsubsetdsc.txt")
write("NA",file="DTsubsetdsc.txt",append=TRUE)
write("2",file="DTsubsetdsc.txt",append=TRUE)
write.table(cbind(1:ncol(z),names(z),role),
            file="DTsubsetdsc.txt",append=TRUE,
            row.names=FALSE,col.names=FALSE,quote=FALSE)

########## Step4: importance scoring ##########
### run guide to give each variable an importance score### 

########## Step5: create data set DTfinal.rdata ##########
### run guide to delete unimportant variables###
## important: delete the commented lines at the top of DTfinal.rdata ##
## content of commented lines ##
# a <- "factor"; n <- "numeric"
# vartype <- c(n,a,n,a,n,n,n,n,n,n,n,a,a,a,n,a,n,a,n,n,a,n,a,n,a,n,a,n,a,n,
#              n,a,n,n,a,n,n,n,a,n,a,n,a,a,a,a,a,a,a,a,a,n,n,n,a,n,a,n,a,a,
#              a,a,n,n,n,n,a,a,a,a,n,a,a,n,a,n,a,n,a,a,n,a,n,n,n,n,n,n,n,n,
#              n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
#              n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
#              n,n,n,n,n,n,a,a,a,a,a,a,a,a,a,a,n,a,n,a,n,n,n,n,n,n,n,n,n,n,
#              n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n,n,n,n,n,n,
#              n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,a,n,n,a,a,a,a,n,n,a,n,a,n,
#              a,a,a,n,n,a,n,a,n,a,n,a,n,n,a,a,a,a,a,a,a,a,n,a,a,n,a,a,a,n,
#              n,n,a,n,a,a,n,a,n,a,n,a,a,a,a,n,n,n,n)
# z <- read.table("DTfinal.rdata",header=TRUE,colClasses=vartype)

########## Step6: create DTfinaldsc.txt ##########
library(readr)
z <- as.data.frame(read_delim(file="DTfinal.rdata",delim=" ",
                              quote="\\\"",escape_double=FALSE,
                              trim_ws=TRUE,guess_max=30000)) 
### convert character variables to factors
for (i in 1:ncol(z)){
  if(is.character(z[,i])) z[,i]=factor(z[,i])
}
role <- rep("n",ncol(z))
con.gp <- NULL # indices of variables with all NAs or are constant 
for(j in 1:ncol(z)){
  if(class(z[,j]) == "factor") role[j] <- "c" 
}
role[names(z) %in% "INTRDVX"] <- "d" 
write("DTfinal.rdata","DTfinaldsc.txt") 
write("NA",file="DTfinaldsc.txt",append=TRUE) 
write("2",file="DTfinaldsc.txt",append=TRUE) 
write.table(cbind(1:ncol(z),names(z),role),file="DTfinaldsc.txt",append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)
