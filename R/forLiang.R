setwd("D:/SOLARS/Liang paper")

feature.pos <- readr::read_csv("feature_pos.csv")
feature.neg <- readr::read_csv("feature_neg.csv")
metdata <- readr::read_csv("metaData.csv")


temp <-readRDS("PS123_all_metabolits.rds")


feature.pos <- feature.pos[-c(1,2,3), -1]
colnames(feature.pos) <- c("name", "mz", "rt")


feature.neg <- feature.neg[-c(1,2,3), -1]
colnames(feature.neg) <- c("name", "mz", "rt")



sample <- matrix(data = sample(1:100, 38199*4, replace = TRUE), 
                 nrow = 38199, ncol = 4)

colnames(sample) <- paste("sample", 1:4, sep = "")


feature.pos <- data.frame(feature.pos, sample, stringsAsFactors = FALSE)
feature.neg <- data.frame(feature.neg, sample[1:23958,], stringsAsFactors = FALSE)


feature.pos$rt <- as.numeric(feature.pos$rt) * 60
feature.neg$rt <- as.numeric(feature.neg$rt) * 60

sample.info <- data.frame("sample.name" = colnames(sample),
                          group = rep(c("control", "case"), 2))


write.csv(feature.pos, file = "data.pos.csv", row.names = FALSE)
write.csv(feature.neg, file = "data.neg.csv", row.names = FALSE)
write.csv(sample.info, file = "sample.info.csv", row.names = FALSE)



library(MetDNA)

MetDNA(ms1.data.pos = "data.pos.csv", ms1.data.neg = "data.neg.csv", 
       sample.info.pos = "sample.info.csv", sample.info.neg = "sample.info.csv", 
       mz.tol = 25, pos.path = "D:/SOLARS/Liang paper/MetDNA processing/POS",
      neg.path = "D:/SOLARS/Liang paper/MetDNA processing/NEG", 
       polarity = "negative", column = "rp", threads = 3, group = c("control", "case"),
       pathway.enrichment = FALSE, dn.analysis = FALSE)


setwd("D:\\SOLARS\\Liang paper")
qc.pos <- readr::read_csv("QC")










