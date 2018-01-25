library(data.table)
library(dplyr)

load("natl2011.RData")
load("natl2012.RData")
load("natl2013.RData")
load("natl2014.RData")
load("natl2015.RData")
load("natl2016.RData")
load("testsample.RData")

dim(natl2011)
dim(natl2012)
dim(natl2013)
dim(natl2014)
dim(natl2015)
dim(natl2016)
dim(testsample)

########## stratified random subset ##########
natl2011_0 <- natl2011 %>% filter(lowbwt == "0")
natl2011_1 <- natl2011 %>% filter(lowbwt == "1")
natl2012_0 <- natl2012 %>% filter(lowbwt == "0")
natl2012_1 <- natl2012 %>% filter(lowbwt == "1")
natl2013_0 <- natl2013 %>% filter(lowbwt == "0")
natl2013_1 <- natl2013 %>% filter(lowbwt == "1")
natl2014_0 <- natl2014 %>% filter(lowbwt == "0")
natl2014_1 <- natl2014 %>% filter(lowbwt == "1")
natl2015_0 <- natl2015 %>% filter(lowbwt == "0")
natl2015_1 <- natl2015 %>% filter(lowbwt == "1")
natl2016_0 <- natl2016 %>% filter(lowbwt == "0")
natl2016_1 <- natl2016 %>% filter(lowbwt == "1")

set.seed(479)

random_natl2011_0 <- sample_n(natl2011_0, 113160, replace = FALSE)
random_natl2012_0 <- sample_n(natl2012_0, 113160, replace = FALSE)
random_natl2013_0 <- sample_n(natl2013_0, 113160, replace = FALSE)
random_natl2014_0 <- sample_n(natl2014_0, 113160, replace = FALSE)
random_natl2015_0 <- sample_n(natl2015_0, 113160, replace = FALSE)
random_natl2016_0 <- sample_n(natl2016_0, 113160, replace = FALSE)

random_natl2011_1 <- sample_n(natl2011_1, 9840, replace = FALSE)
random_natl2012_1 <- sample_n(natl2012_1, 9840, replace = FALSE)
random_natl2013_1 <- sample_n(natl2013_1, 9840, replace = FALSE)
random_natl2014_1 <- sample_n(natl2014_1, 9840, replace = FALSE)
random_natl2015_1 <- sample_n(natl2015_1, 9840, replace = FALSE)
random_natl2016_1 <- sample_n(natl2016_1, 9840, replace = FALSE)

dim(random_natl2011_0); dim(random_natl2011_1)
dim(random_natl2012_0); dim(random_natl2012_1)
dim(random_natl2013_0); dim(random_natl2013_1)
dim(random_natl2014_0); dim(random_natl2014_1)
dim(random_natl2015_0); dim(random_natl2015_1)
dim(random_natl2016_0); dim(random_natl2016_1)

for (i in 1:6) {
  subset <- rbind(random_natl2011_0[(18400*(i-1)+1):(18400*(i-1)+18400),],
                  random_natl2012_0[(18400*(i-1)+1):(18400*(i-1)+18400),],
                  random_natl2013_0[(18400*(i-1)+1):(18400*(i-1)+18400),],
                  random_natl2014_0[(18400*(i-1)+1):(18400*(i-1)+18400),],
                  random_natl2015_0[(18400*(i-1)+1):(18400*(i-1)+18400),],
                  random_natl2016_0[(18400*(i-1)+1):(18400*(i-1)+18400),],
                  random_natl2011_1[(1600*(i-1)+1):(1600*(i-1)+1600),],
                  random_natl2012_1[(1600*(i-1)+1):(1600*(i-1)+1600),],
                  random_natl2013_1[(1600*(i-1)+1):(1600*(i-1)+1600),],
                  random_natl2014_1[(1600*(i-1)+1):(1600*(i-1)+1600),],
                  random_natl2015_1[(1600*(i-1)+1):(1600*(i-1)+1600),],
                  random_natl2016_1[(1600*(i-1)+1):(1600*(i-1)+1600),])
  write.table(subset, paste("subset_",i,".rdata",sep=""), row.names=FALSE, col.names=TRUE, quote=FALSE)
}

save(subset, file = "subset_.rdata")

validation <- rbind(random_natl2011_0[110401:113160,],
                    random_natl2012_0[110401:113160,],
                    random_natl2013_0[110401:113160,],
                    random_natl2014_0[110401:113160,],
                    random_natl2015_0[110401:113160,],
                    random_natl2016_0[110401:113160,],
                    random_natl2011_1[9601:9840,],
                    random_natl2012_1[9601:9840,],
                    random_natl2013_1[9601:9840,],
                    random_natl2014_1[9601:9840,],
                    random_natl2015_1[9601:9840,],
                    random_natl2016_1[9601:9840,])

write.table(validation, file = "validation_set.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)

save(validation, file = "validation_.rdata")

########## create description file subset[i]dsc.txt ##########
load("subset_.rdata")

subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:6) {
  write(paste("subset_",i,".rdata", sep = ""), paste("subset_",i,"dsc.txt", sep = ""))
  write("NA", file = paste("subset_",i,"dsc.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset_",i,"dsc.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset_",i,"dsc.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

########## GUIDE forest: concatenate subset & validation set ##########
load("validation_.rdata")

subset_1 <- fread("subset_1.rdata")
subset_2 <- fread("subset_2.rdata")
subset_3 <- fread("subset_3.rdata")
subset_4 <- fread("subset_4.rdata")
subset_5 <- fread("subset_5.rdata")
subset_6 <- fread("subset_6.rdata")

validation_x <- validation %>% select(-lowbwt)
validation_x <- validation_x %>% mutate(lowbwt = NA)
validation_x$lowbwt <- as.factor(validation_x$lowbwt)

subset_1_gf <- rbind(subset_1, validation_x)
subset_2_gf <- rbind(subset_2, validation_x)
subset_3_gf <- rbind(subset_3, validation_x)
subset_4_gf <- rbind(subset_4, validation_x)
subset_5_gf <- rbind(subset_5, validation_x)
subset_6_gf <- rbind(subset_6, validation_x)

write.table(subset_1_gf, file = "subset_1_gf.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_2_gf, file = "subset_2_gf.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_3_gf, file = "subset_3_gf.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_4_gf, file = "subset_4_gf.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_5_gf, file = "subset_5_gf.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_6_gf, file = "subset_6_gf.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)

########## GUIDE forest: create description file for validation##########
load("subset_.rdata")

subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:6) {
  write(paste("subset_",i,"_gf.rdata", sep = ""), paste("subset_",i,"dsc_gf.txt", sep = ""))
  write("NA", file = paste("subset_",i,"dsc_gf.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset_",i,"dsc_gf.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset_",i,"dsc_gf.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

########## GUIDE forest: concatenate subset & testsample ##########
load("testsample.RData")

subset_1 <- fread("subset_1.rdata")
subset_2 <- fread("subset_2.rdata")
subset_3 <- fread("subset_3.rdata")
subset_4 <- fread("subset_4.rdata")
subset_5 <- fread("subset_5.rdata")
subset_6 <- fread("subset_6.rdata")

testsample <- testsample %>% mutate(lowbwt = NA)
testsample$lowbwt <- as.factor(testsample$lowbwt)

subset_1_gf_pr <- rbind(subset_1, testsample)
subset_2_gf_pr <- rbind(subset_2, testsample)
subset_3_gf_pr <- rbind(subset_3, testsample)
subset_4_gf_pr <- rbind(subset_4, testsample)
subset_5_gf_pr <- rbind(subset_5, testsample)
subset_6_gf_pr <- rbind(subset_6, testsample)

write.table(subset_1_gf_pr, file = "subset_1_gf_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_2_gf_pr, file = "subset_2_gf_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_3_gf_pr, file = "subset_3_gf_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_4_gf_pr, file = "subset_4_gf_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_5_gf_pr, file = "subset_5_gf_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_6_gf_pr, file = "subset_6_gf_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)

########## GUIDE forest: create description file for prediction##########
load("subset_.rdata")
subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:6) {
  write(paste("subset_",i,"_gf_pr.rdata", sep = ""), paste("subset_",i,"dsc_gf_pr.txt", sep = ""))
  write("NA", file = paste("subset_",i,"dsc_gf_pr.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset_",i,"dsc_gf_pr.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset_",i,"dsc_gf_pr.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

########## GUIDE bagging: concatenate subset & validation set ##########
load("validation_.rdata")

subset_1 <- fread("subset_1.rdata")
subset_2 <- fread("subset_2.rdata")
subset_3 <- fread("subset_3.rdata")
subset_4 <- fread("subset_4.rdata")
subset_5 <- fread("subset_5.rdata")
subset_6 <- fread("subset_6.rdata")

validation_x <- validation %>% select(-lowbwt)
validation_x <- validation_x %>% mutate(lowbwt = NA)
validation_x$lowbwt <- as.factor(validation_x$lowbwt)

subset_1_bg <- rbind(subset_1, validation_x)
subset_2_bg <- rbind(subset_2, validation_x)
subset_3_bg <- rbind(subset_3, validation_x)
subset_4_bg <- rbind(subset_4, validation_x)
subset_5_bg <- rbind(subset_5, validation_x)
subset_6_bg <- rbind(subset_6, validation_x)

write.table(subset_1_bg, file = "subset_1_bg.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_2_bg, file = "subset_2_bg.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_3_bg, file = "subset_3_bg.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_4_bg, file = "subset_4_bg.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_5_bg, file = "subset_5_bg.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_6_bg, file = "subset_6_bg.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)

########## GUIDE bagging: create description file for validation##########
load("subset_.rdata")

subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:6) {
  write(paste("subset_",i,"_bg.rdata", sep = ""), paste("subset_",i,"dsc_bg.txt", sep = ""))
  write("NA", file = paste("subset_",i,"dsc_bg.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset_",i,"dsc_bg.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset_",i,"dsc_bg.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

########## GUIDE bagging: concatenate subset & testsample ##########
load("testsample.RData")

subset_1 <- fread("subset_1.rdata")
subset_2 <- fread("subset_2.rdata")
subset_3 <- fread("subset_3.rdata")
subset_4 <- fread("subset_4.rdata")
subset_5 <- fread("subset_5.rdata")
subset_6 <- fread("subset_6.rdata")

testsample <- testsample %>% mutate(lowbwt = NA)
testsample$lowbwt <- as.factor(testsample$lowbwt)

subset_1_bg_pr <- rbind(subset_1, testsample)
subset_2_bg_pr <- rbind(subset_2, testsample)
subset_3_bg_pr <- rbind(subset_3, testsample)
subset_4_bg_pr <- rbind(subset_4, testsample)
subset_5_bg_pr <- rbind(subset_5, testsample)
subset_6_bg_pr <- rbind(subset_6, testsample)

write.table(subset_1_bg_pr, file = "subset_1_bg_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_2_bg_pr, file = "subset_2_bg_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_3_bg_pr, file = "subset_3_bg_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_4_bg_pr, file = "subset_4_bg_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_5_bg_pr, file = "subset_5_bg_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)
write.table(subset_6_bg_pr, file = "subset_6_bg_pr.rdata", row.name = FALSE, col.names = TRUE, quote = FALSE)

########## GUIDE bagging: create description file for prediction##########
load("subset_.rdata")

subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:6) {
  write(paste("subset_",i,"_bg_pr.rdata", sep = ""), paste("subset_",i,"dsc_bg_pr.txt", sep = ""))
  write("NA", file = paste("subset_",i,"dsc_bg_pr.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset_",i,"dsc_bg_pr.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset_",i,"dsc_bg_pr.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}















########## stratified random subset ##########
natl2011_0 <- natl2011 %>% filter(lowbwt == "0")
natl2011_1 <- natl2011 %>% filter(lowbwt == "1")
natl2012_0 <- natl2012 %>% filter(lowbwt == "0")
natl2012_1 <- natl2012 %>% filter(lowbwt == "1")
natl2013_0 <- natl2013 %>% filter(lowbwt == "0")
natl2013_1 <- natl2013 %>% filter(lowbwt == "1")
natl2014_0 <- natl2014 %>% filter(lowbwt == "0")
natl2014_1 <- natl2014 %>% filter(lowbwt == "1")
natl2015_0 <- natl2015 %>% filter(lowbwt == "0")
natl2015_1 <- natl2015 %>% filter(lowbwt == "1")
natl2016_0 <- natl2016 %>% filter(lowbwt == "0")
natl2016_1 <- natl2016 %>% filter(lowbwt == "1")

set.seed(479)
random_natl2011_0 <- sample_n(natl2011_0, dim(natl2011_0)[1], replace = FALSE)
random_natl2012_0 <- sample_n(natl2012_0, dim(natl2012_0)[1], replace = FALSE)
random_natl2013_0 <- sample_n(natl2013_0, dim(natl2013_0)[1], replace = FALSE)
random_natl2014_0 <- sample_n(natl2014_0, dim(natl2014_0)[1], replace = FALSE)
random_natl2015_0 <- sample_n(natl2015_0, dim(natl2015_0)[1], replace = FALSE)
random_natl2016_0 <- sample_n(natl2016_0, dim(natl2016_0)[1], replace = FALSE)

random_natl2011_1 <- sample_n(natl2011_1, dim(natl2011_1)[1], replace = FALSE)
random_natl2012_1 <- sample_n(natl2012_1, dim(natl2012_1)[1], replace = FALSE)
random_natl2013_1 <- sample_n(natl2013_1, dim(natl2013_1)[1], replace = FALSE)
random_natl2014_1 <- sample_n(natl2014_1, dim(natl2014_1)[1], replace = FALSE)
random_natl2015_1 <- sample_n(natl2015_1, dim(natl2015_1)[1], replace = FALSE)
random_natl2016_1 <- sample_n(natl2016_1, dim(natl2016_1)[1], replace = FALSE)

dim(random_natl2011_0); dim(random_natl2011_1)
dim(random_natl2012_0); dim(random_natl2012_1)
dim(random_natl2013_0); dim(random_natl2013_1)
dim(random_natl2014_0); dim(random_natl2014_1)
dim(random_natl2015_0); dim(random_natl2015_1)
dim(random_natl2016_0); dim(random_natl2016_1)


for (i in 1:43) {
  subset <- rbind(random_natl2011_0[82800*(i-1)+1:82800*(i-1)+82800,],
                  random_natl2012_0[82800*(i-1)+1:82800*(i-1)+82800,],
                  random_natl2013_0[82800*(i-1)+1:82800*(i-1)+82800,],
                  random_natl2014_0[82800*(i-1)+1:82800*(i-1)+82800,],
                  random_natl2015_0[82800*(i-1)+1:82800*(i-1)+82800,],
                  random_natl2016_0[82800*(i-1)+1:82800*(i-1)+82800,],
                  random_natl2011_1[7200*(i-1)+1:7200*(i-1)+7200,],
                  random_natl2012_1[7200*(i-1)+1:7200*(i-1)+7200,],
                  random_natl2013_1[7200*(i-1)+1:7200*(i-1)+7200,],
                  random_natl2014_1[7200*(i-1)+1:7200*(i-1)+7200,],
                  random_natl2015_1[7200*(i-1)+1:7200*(i-1)+7200,],
                  random_natl2016_1[7200*(i-1)+1:7200*(i-1)+7200,])
  write.table(subset, paste("subset",i,".rdata",sep=""), row.names=FALSE, col.names=TRUE, quote=FALSE)
}

save(subset, file = "subset.rdata")

validation <- 
########## create description file subset[i]dsc.txt ##########
load("subset.rdata")
subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:43) {
  write(paste("subset",i,".rdata", sep = ""), paste("subset",i,"dsc.txt", sep = ""))
  write("NA", file = paste("subset",i,"dsc.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset",i,"dsc.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset",i,"dsc.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}
















natldata <- rbind(natl2011, natl2012, natl2013, natl2014, natl2015, natl2016)
write.table(natldata, "natl.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)

write.table(natl2011, "data2011.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2012, "data2012.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2013, "data2013.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2014, "data2014.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2015, "data2015.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2016, "data2016.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)


########## create description file natlxxxximpdsc.txt ##########
natl2016.names <- names(natl2016)
role <- rep("n", ncol(natl2016))

for (j in 1:ncol(natl2016)) {
  if (class(natl2016[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[natl2016.names %in% "lowbwt"] <- "d"

write("data2016.rdata", "natl2016impdsc.txt")
write("NA", file = "natl2016impdsc.txt", append = TRUE)
write("2", file = "natl2016impdsc.txt", append = TRUE)
write.table(cbind(1:ncol(natl2016), names(natl2016), role),
            file = "natl2016impdsc.txt", append = TRUE,
            row.names = FALSE, col.names = FALSE, quote = FALSE)



natl2011sum <- natl2011 %>%
    group_by(lowbwt) %>%
    summarise(count = n())

natl2012sum <- natl2012 %>%
    group_by(lowbwt) %>%
    summarise(count = n())

natl2013sum <- natl2013 %>%
    group_by(lowbwt) %>%
    summarise(count = n())

natl2014sum <- natl2014 %>%
    group_by(lowbwt) %>%
    summarise(count = n())

natl2015sum <- natl2015 %>%
    group_by(lowbwt) %>%
    summarise(count = n())

natl2016sum <- natl2016 %>%
    group_by(lowbwt) %>%
    summarise(count = n())

(320147+316153+315612+319391+321528+322433)/(3636500+3640626+3620694+3675475+3663557+3629136)

random_natl2011 <- sample_n(natl2011, dim(natl2011)[1], replace = FALSE)
random_natl2012 <- sample_n(natl2012, dim(natl2012)[1], replace = FALSE)
random_natl2013 <- sample_n(natl2013, dim(natl2013)[1], replace = FALSE)
random_natl2014 <- sample_n(natl2014, dim(natl2014)[1], replace = FALSE)
random_natl2015 <- sample_n(natl2015, dim(natl2015)[1], replace = FALSE)
random_natl2016 <- sample_n(natl2016, dim(natl2016)[1], replace = FALSE)



for (i in 1:10) {
  subset <- rbind(random_natl2011[100000*(i-1)+1:100000*(i-1)+100000,],
                  random_natl2012[100000*(i-1)+1:100000*(i-1)+100000,],
                  random_natl2013[100000*(i-1)+1:100000*(i-1)+100000,],
                  random_natl2014[100000*(i-1)+1:100000*(i-1)+100000,],
                  random_natl2015[100000*(i-1)+1:100000*(i-1)+100000,],
                  random_natl2016[100000*(i-1)+1:100000*(i-1)+100000,])
  save(subset, file = paste("subset", i, ".RData", sep = ""))
}


########## create description file subset[i]dsc.txt ##########
load("subset1.RData")
subset.names <- names(subset)
role <- rep("n", ncol(subset))

for (j in 1:ncol(subset)) {
  if (class(subset[, j]) == "factor") {
    role[j] <- "c"
  }
}
role[subset.names %in% "lowbwt"] <- "d"

for (i in 1:39) {
  write(paste("subset",i,".RData", sep = ""), paste("subset",i,"dsc.txt", sep = ""))
  write("NA", file = paste("subset",i,"dsc.txt", sep = ""), append = TRUE)
  write("2", file = paste("subset",i,"dsc.txt", sep = ""), append = TRUE)
  write.table(cbind(1:ncol(subset), names(subset), role),
              file = paste("subset",i,"dsc.txt", sep = ""), append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}
natldata <- rbind(natl2011, natl2012, natl2013, natl2014, natl2015, natl2016)
write.table(natldata, "natl.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)

write.table(natl2011, "data2011.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2012, "data2012.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2013, "data2013.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2014, "data2014.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2015, "data2015.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(natl2016, "data2016.rdata", row.names=FALSE, col.names=TRUE, quote=FALSE)

########## guide ##########











