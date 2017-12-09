a <- "factor"; n <- "numeric"
vartype <- c(
n,a,n,a,n,n,n,n,n,n,n,a,a,a,n,a,n,a,n,n,a,n,a,n,a,n,a,n,a,n,n,a,n,n,a,
n,n,n,a,n,a,n,a,a,a,a,a,a,a,a,a,n,n,n,a,n,a,n,a,a,a,a,n,n,n,n,a,a,a,a,
n,a,a,n,a,n,a,n,a,a,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,a,a,a,a,a,a,a,a,a,n,a,n,a,n,n,n,n,n,
n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n,n,n,n,n,n,
n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,a,a,n,n,a,a,a,a,n,n,a,n,a,n,a,a,a,n,n,
a,n,a,n,a,n,a,n,n,a,a,a,a,a,a,a,a,n,a,a,n,a,a,a,n,n,n,a,n,a,a,n,a,n,a,
n,a,a,a,a,n,n,n,n)
z <- read.table("DTfinal.rdata",header=TRUE,colClasses=vartype)

dsc <- readLines("DTfinaldsc.txt")
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
        write(dsc[1],"tmpdsc.txt")
        write("NA","tmpdsc.txt",append=TRUE)
        write("2","tmpdsc.txt",append=TRUE)
        write.table(cbind(1:ncol(z),names(z),tmproles),"tmpdsc.txt",
                    row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
        if(roles[j] == "n"){ # regression
            system("guide < reg_in.txt > log.txt")
        } else if(roles[j] == "c"){  # classification
            system("guide < class_in.txt > log.txt")
        }
        fit.info <- read.table("fit.txt",header=TRUE)
        gp <- fit.info$train == "n"
        z.imputed[gp,j] <- fit.info$predicted[gp]
    }
}
write.table(z.imputed,"data_imputed_1.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)

