#subset_1 <- fread("subset_1.rdata")
#subset_2 <- fread("subset_2.rdata")
#subset_3 <- fread("subset_3.rdata")
#subset_4 <- fread("subset_4.rdata")
#subset_5 <- fread("subset_5.rdata")
#subset_6 <- fread("subset_6.rdata")
#validation <- fread("validation_set.rdata")

#data_complete <- rbind(subset_1, subset_2, subset_3, subset_4, subset_5, subset_6, validation)

#write.table(data_complete, "data_complete.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)

a <- "factor"; n <- "numeric"
vartype <- c(a,a,a,a,n,a,n,n,n,n,a,n,a,a,a,a,a,a,
             n,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,n,a,
             a,a,a,a,a,n,a,n,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,n,n,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,n,n,a,a,a,a,n,a,a,a,a,
             a,a,a,n,a)
z <- read.table("data_complete.rdata",header=TRUE,colClasses=vartype)

dsc <- readLines("subset_1dsc.txt")
roles <- NULL
for(j in 4:length(dsc)){  # store original roles of variables
        strng <- strsplit(dsc[j]," +")[[1]]
            roles <- c(roles,strng[3])
        }
roles[roles == "d"] <- "x"  # exclude lowbwt from imputation

n <- nrow(z)
z.imputed <- z
for(j in 1:ncol(z)){
  tmproles <- roles
  k <- sum(!is.na(z[,j]))
  if(k > 0 & k < n){ # skip variables without NAs
    tmproles[j] <- "d"   # make this variable "D"
    write("data_complete.rdata","tmpdsc.txt")
    write("NA","tmpdsc.txt",append=TRUE)
    write("2","tmpdsc.txt",append=TRUE)
    write.table(cbind(1:ncol(z),names(z),tmproles),"tmpdsc.txt",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
    if(roles[j] == "n"){ # regression
      system("guide < reg_st_in.txt > log.txt")
    } else if(roles[j] == "c"){  # classification
      system("guide < class_st_in.txt > log.txt")
    }
    fit.info <- read.table("fit_st.txt",header=TRUE)
    gp <- fit.info$train == "n"
    z.imputed[gp,j] <- fit.info$predicted[gp]
  }
}
write.table(z.imputed,"data_st_imputed.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
