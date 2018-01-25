a <- "factor"; n <- "numeric"
vartype <- c(a,a,a,n,a,n,n,n,n,a,n,a,a,a,a,a,a,
             n,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,n,a,
             a,a,a,a,a,n,a,n,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,n,n,a,a,a,a,a,a,a,a,a,a,a,a,
             a,a,a,a,a,a,a,n,n,a,a,a,a,n,a,a,a,a,
             a,a,a,n,a)
z <- read.table("test_sample.rdata",header=TRUE,colClasses=vartype)

dsc <- readLines("subset_1dsc.txt")
roles <- NULL
for(j in 4:length(dsc)){  # store original roles of variables
        strng <- strsplit(dsc[j]," +")[[1]]
            roles <- c(roles,strng[3])
        }
roles <- roles[-1] # exclude lowbwt from imputation

n <- nrow(z)
z.imputed <- z
for(j in 1:ncol(z)){
  tmproles <- roles
  k <- sum(!is.na(z[,j]))
  if(k > 0 & k < n){ # skip variables without NAs
    tmproles[j] <- "d"   # make this variable "D"
    write("test_sample.rdata","tmpdsc_test.txt")
    write("NA","tmpdsc_test.txt",append=TRUE)
    write("2","tmpdsc_test.txt",append=TRUE)
    write.table(cbind(1:ncol(z),names(z),tmproles),"tmpdsc_test.txt",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
    if(roles[j] == "n"){ # regression
      system("guide < reg_st_in_test.txt > log.txt")
    } else if(roles[j] == "c"){  # classification
      system("guide < class_st_in_test.txt > log.txt")
    }
    fit.info <- read.table("fit_st_test.txt",header=TRUE)
    gp <- fit.info$train == "n"
    z.imputed[gp,j] <- fit.info$predicted[gp]
  }
}
write.table(z.imputed,"data_st_imputed_test.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
