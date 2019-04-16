#
# ###code
# setwd("F:/wcc")
# # data <- readxl::read_excel("20180416 raft vs non-raft-2-181030.xlsx")
# data <- readr::read_csv("data.csv")
# data <- as.data.frame(data)
#
# compound <- data$`compound name`
#
# c_double <- stringr::str_extract_all(string = compound, pattern = "[0-9]{1,2}\\:[0-9]{1,2}")
#
# # raft3 <- data[,c(4:6)]
# # raftA <- data[,c(7:9)]
#
#
# raft <- data[,c(4:9)]
#
# raft <- apply(raft, 1, list)
#
# raft <- lapply(raft, unlist)
#
# raft <- lapply(raft, function(x){
#   temp <- as.data.frame(matrix(x, nrow = 1))
#   colnames(temp) <- names(x)
#   temp
# })
#
#
# data.new <- mapply(function(x,y){
#
#   if(length(x) == 0){
#     temp <- data.frame(NA, y, stringsAsFactors = FALSE)
#   }else{
#     temp <- data.frame(x, y, stringsAsFactors = FALSE)
#   }
#   colnames(temp)[1] <- "c_double"
#   return(list(temp))
# },
# x = c_double,
# y = raft)
#
# data.new <- do.call(what = rbind, args = data.new)
#
#
# data.new <- data.new[which(!is.na(data.new$c_double)),]
#
# c_double <- data.new$c_double
#
# c_number <- as.numeric(unlist(lapply(strsplit(c_double, split = ":"), function(x) x[1])))
# double_number <- as.numeric(unlist(lapply(strsplit(c_double, split = ":"), function(x) x[2])))
#
# data.new1 <- data.frame(c_number, data.new[,-1], stringsAsFactors = FALSE)
# data.new2 <- data.frame(double_number, data.new[,-1], stringsAsFactors = FALSE)
#
#
# c_number_unique <- unique(c_number)
# double_number_unique <- unique(double_number)
#
#
#
# data.new1 <- lapply(c_number_unique, function(x){
#   temp.idx <- which(data.new1$c_number == x)
#   temp.data1 <- data.new1[temp.idx,]
#   temp.data1_2 <- matrix(c(x, colSums(temp.data1[,-1])), nrow = 1)
#   colnames(temp.data1_2) <- colnames(temp.data1)
#   temp.data1_2
# })
#
#
# data.new1 <- do.call(rbind, data.new1)
# data.new1 <- data.new1[order(data.new1[,1]),]
#
#
#
# data.new2 <- lapply(double_number_unique, function(x){
#   temp.idx <- which(data.new2$double_number == x)
#   temp.data2 <- data.new2[temp.idx,]
#   temp.data2_2 <- matrix(c(x, colSums(temp.data2[,-1])), nrow = 1)
#   colnames(temp.data2_2) <- colnames(temp.data2)
#   temp.data2_2
# })
#
#
# data.new2 <- do.call(rbind, data.new2)
# data.new2 <- data.new2[order(data.new2[,1]),]
#
#
#
# write.csv(data.new1, "c_number.data.csv", row.names = FALSE)
# write.csv(data.new2, "double_number.data.csv", row.names = FALSE)
#
#
#
#
#
#
#
# raft3.mean <- apply(raft3, 1, mean)
# raftA.mean <- apply(raftA, 1, mean)
#
#
# raft3.data <- mapply(function(x,y){
#   if(length(y) == 0){
#     temp <- data.frame(NA, x, stringsAsFactors = FALSE)
#   }else{
#     temp <- data.frame(y, x, stringsAsFactors = FALSE)
#   }
#   colnames(temp) <- c("c_double", "mean")
#   return(list(temp))
# },
# x = raft3.mean,
# y = c_double)
#
#
# raft3.data <- do.call(rbind, raft3.data)
#
#
# raftA.data <- mapply(function(x,y){
#   if(length(y) == 0){
#     temp <- data.frame(NA, x, stringsAsFactors = FALSE)
#   }else{
#     temp <- data.frame(y, x, stringsAsFactors = FALSE)
#   }
#   colnames(temp) <- c("c_double", "mean")
#   return(list(temp))
# },
# x = raftA.mean,
# y = c_double)
#
#
# raftA.data <- do.call(rbind, raftA.data)
#
#
# raft3.data <- raft3.data[-which(is.na(raft3.data$c_double)),]
# raftA.data <- raftA.data[-which(is.na(raftA.data$c_double)),]
#
#
# raft3.data2 <- data.frame(do.call(rbind, args = strsplit(raft3.data$c_double, split = ":")),
#                           raft3.data[,2], stringsAsFactors = FALSE)
# raftA.data2 <- data.frame(do.call(rbind, args = strsplit(raftA.data$c_double, split = ":")),
#                           raftA.data[,2], stringsAsFactors = FALSE)
#
# colnames(raft3.data2) <- colnames(raftA.data2) <- c("C_number", "double", "mean")
#
#
# c_number1 <- raft3.data2$mean
# double1 <- raft3.data2$mean
# names(c_number1) <- raft3.data2$C_number
# names(double1) <- raft3.data2$double
#
#
# c_number2 <- raftA.data2$mean
# double2 <- raftA.data2$mean
# names(c_number2) <- raftA.data2$C_number
# names(double2) <- raftA.data2$double
#
# carbon_number1 <- unique(names(c_number1))
#
# group1_c <- sapply(carbon_number1, function(x){
#   sum(c_number1[which(names(c_number1) == x)])
# })
#
# double_number1 <- unique(names(double1))
#
# group1_double <- sapply(double_number1, function(x){
#   sum(double1[which(names(double1) == x)])
# })
#
#
#
# carbon_number2 <- unique(names(c_number2))
#
# group2_c <- sapply(carbon_number2, function(x){
#   sum(c_number2[which(names(c_number2) == x)])
# })
#
# double_number2 <- unique(names(double2))
#
# group2_double <- sapply(double_number2, function(x){
#   sum(double2[which(names(double2) == x)])
# })
#
#
# sum(group1_c[which(as.numeric(names(group1_c)) > 22)])
# sum(group1_c[which(as.numeric(names(group1_c)) > 12 & as.numeric(names(group1_c)) <= 22)])
# sum(group1_c[which(as.numeric(names(group1_c)) <= 12)])
#
# sum(group2_c[which(as.numeric(names(group2_c)) > 22)])
# sum(group2_c[which(as.numeric(names(group2_c)) > 12 & as.numeric(names(group2_c)) <= 22)])
# sum(group2_c[which(as.numeric(names(group2_c)) <= 12)])
#
#
#
# ###把数据输出交给楚楚
# group1_c
# group2_c
#
#
# group1_double
# group2_double
#
#
# temp.data1 <- data.frame(as.numeric(names(group1_c)), group1_c, group2_c, stringsAsFactors = FALSE)
# colnames(temp.data1) <- c("C_number", "raf3", "rafA")
# temp.data1 <- temp.data1[order(temp.data1$C_number),]
# write.csv(temp.data1, "c_number.csv", row.names = FALSE)
#
#
# temp.data2 <- data.frame(as.numeric(names(group1_double)), group1_double, group2_double,
#                          stringsAsFactors = FALSE)
# colnames(temp.data2) <- c("double_number", "raf3", "rafA")
# temp.data2 <- temp.data2[order(temp.data2$double_number),]
# write.csv(temp.data2, "double_number.csv", row.names = FALSE)
#
#
#
#
# ######修改小数点的位数
# setwd("F:/wcc")
# # data <- readxl::read_excel("20180416 raft vs non-raft-2-181030.xlsx")
# data <- readr::read_csv("data.csv")
# data <- as.data.frame(data)
# tags <- data[,c(1:3)]
# class <- data[,"Class", drop = FALSE]
# sample <- data[,-c(1:3)]
# sample <- sample[,-10]
#
# sample[which(sample > 1, arr.ind = TRUE)] <- round(sample[which(sample > 1, arr.ind = TRUE)], 3)
# sample[which(sample < 1, arr.ind = TRUE)] <- signif(sample[which(sample < 1, arr.ind = TRUE)], 3)
#
#
# data.new <- data.frame(tags, sample, class, stringsAsFactors = FALSE)
#
# write.csv(data.new, "data.new.csv", row.names = FALSE)
