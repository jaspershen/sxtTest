# setwd("F:\\test\\SXT\\data analysis")
# plasma.data <- readxl::read_xlsx("QMDiab_metabolomics_Preprocessed.xlsx", sheet = 1)
# plasma.data <- as.data.frame(plasma.data)
# urine.data <- readxl::read_xlsx("QMDiab_metabolomics_Preprocessed.xlsx", sheet = 2)
# urine.data <- as.data.frame(urine.data)
# saliva.data <- readxl::read_xlsx("QMDiab_metabolomics_Preprocessed.xlsx", sheet = 3)
# saliva.data <- as.data.frame(saliva.data)
#
# ##for plasma data
# plasma.pheno <- plasma.data[,c("QMDiab-ID", "AGE", "GENDER", "BMI", "ETH", "T2D")]
# plasma.data <- plasma.data[,-match(c("AGE", "GENDER", "BMI", "ETH", "T2D"), colnames(plasma.data))]
# rownames(plasma.data) <- plasma.data$`QMDiab-ID`
# plasma.data <- plasma.data[,-1]
# plasma.data <- t(plasma.data)
#
# plasma.tags <- readxl::read_xlsx("QMDiab_metabolomics_Preprocessed.xlsx", sheet = 4)
# plasma.tags <- as.data.frame(plasma.tags)
#
#
# ##for urine data
# urine.pheno <- urine.data[,c("QMDiab-ID", "AGE", "GENDER", "BMI", "ETH", "T2D")]
# urine.data <- urine.data[,-match(c("AGE", "GENDER", "BMI", "ETH", "T2D"), colnames(urine.data))]
# rownames(urine.data) <- urine.data$`QMDiab-ID`
# urine.data <- urine.data[,-1]
# urine.data <- t(urine.data)
#
# urine.tags <- readxl::read_xlsx("QMDiab_metabolomics_Preprocessed.xlsx", sheet = 5)
# urine.tags <- as.data.frame(urine.tags)
#
# ##for saliva data
# saliva.pheno <- saliva.data[,c("QMDiab-ID", "AGE", "GENDER", "BMI", "ETH", "T2D")]
# saliva.data <- saliva.data[,-match(c("AGE", "GENDER", "BMI", "ETH", "T2D"), colnames(saliva.data))]
# rownames(saliva.data) <- saliva.data$`QMDiab-ID`
# saliva.data <- saliva.data[,-1]
# saliva.data <- t(saliva.data)
#
# saliva.tags <- readxl::read_xlsx("QMDiab_metabolomics_Preprocessed.xlsx", sheet = 6)
# saliva.tags <- as.data.frame(saliva.tags)
#
# ###
# library(VennDiagram)
# t <- VennDiagram::venn.diagram(x = list("Plasma" = plasma.pheno$`QMDiab-ID`,
#                                    "Urine" = urine.pheno$`QMDiab-ID`,
#                                    "Saliva" = saliva.pheno$`QMDiab-ID`),
#                           filename = NULL, lwd = 3,
#                           category.names = c("Plasma", "Urine", "Saliva"),
#                           col = c("dodgerblue", "firebrick1", "orchid"),
#                           cat.col = c("dodgerblue", "firebrick1", "orchid"),
#                           cat.cex = 1.8,
#                           cex = 1.8)
# grid.draw(t)
#
#
# ####pheno for all sample
# all.QMDiab.ID <- unique(c(plasma.pheno$`QMDiab-ID`, urine.pheno$`QMDiab-ID`, saliva.pheno$`QMDiab-ID`))
#
# all.pheno <- rbind(plasma.pheno, urine.pheno, saliva.pheno)
# all.pheno <- all.pheno[!duplicated(all.pheno),]
#
# all.pheno$T2D[all.pheno$T2D == 0] <- "Control"
# all.pheno$T2D[all.pheno$T2D == "1"] <- "Diabete"
# all.pheno$T2D <- factor(all.pheno$T2D, levels = c("Control", "Diabete"))
#
#
# ###compare the age, gender, BMI, ETH in different groups
# ##age
# control.idx <- which(all.pheno$T2D == "Control")
# case.idx <- which(all.pheno$T2D == "Diabete")
#
# shapiro.test(all.pheno$AGE[control.idx])
# shapiro.test(all.pheno$AGE[case.idx])
# shapiro.test(all.pheno$AGE)
#
# plot(density(all.pheno$AGE))
#
# ###test different in two groups
#
# wilcox.test(x = all.pheno$AGE[control.idx], y = all.pheno$AGE[case.idx])
#
# library(ggplot2)
#
# p <- ggplot(data = all.pheno, aes(x = T2D, y = AGE, col = T2D))+
#   ggplot2::geom_boxplot()+
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.2)+
#   geom_jitter(shape = 16, position = position_jitter(0.2)) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##BMI
# shapiro.test(all.pheno$BMI[control.idx])
# shapiro.test(all.pheno$BMI[case.idx])
# shapiro.test(all.pheno$BMI)
#
# plot(density(all.pheno$BMI))
#
# ###test different in two groups
#
# wilcox.test(x = all.pheno$BMI[control.idx], y = all.pheno$BMI[case.idx])
#
# library(ggplot2)
#
# p <- ggplot(data = all.pheno, aes(x = T2D, y = BMI, col = T2D))+
#   ggplot2::geom_boxplot()+
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.2)+
#   geom_jitter(shape = 16, position = position_jitter(0.2)) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##gender
# chisq.test(table(all.pheno$T2D, all.pheno$GENDER))
#
# temp.data <- table(all.pheno$T2D, all.pheno$GENDER)
# temp.data <- as.data.frame(temp.data)
# colnames(temp.data) <- c("T2D", "GENDER", "Percentage")
# temp.data[c(1,3),3] <- temp.data[c(1,3),3]*100/sum(temp.data[c(1,3),3])
# temp.data[c(2,4),3] <- temp.data[c(2,4),3]*100/sum(temp.data[c(2,4),3])
#
# p <- ggplot(data = temp.data, aes(x = T2D, y = Percentage, fill = GENDER)) +
#   geom_bar(stat="identity")+
#   scale_fill_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##ETH
# chisq.test(table(all.pheno$T2D, all.pheno$ETH))
#
# temp.data <- table(all.pheno$T2D, all.pheno$ETH)
# temp.data <- as.data.frame(temp.data)
# colnames(temp.data) <- c("T2D", "ETH", "Percentage")
# temp.data[c(1,3,5,7),3] <- temp.data[c(1,3,5,7),3]*100/sum(temp.data[c(1,3,5,7),3])
# temp.data[c(2,4,6,8),3] <- temp.data[c(2,4,6,8),3]*100/sum(temp.data[c(2,4,6,8),3])
#
# p <- ggplot(data = temp.data, aes(x = T2D, y = Percentage, fill = ETH)) +
#   geom_bar(stat="identity")+
#   # scale_fill_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ####for each dataset, should be adjusted
#
# ##for plasma data
# plasma.data1 <- apply(plasma.data, 1, list)
# plasma.data1 <- lapply(plasma.data1, unlist)
# age <- plasma.pheno$AGE
# bmi <- plasma.pheno$BMI
# gender <- plasma.pheno$GENDER
# eth <- plasma.pheno$ETH
# plasma.data2 <- lapply(plasma.data1, function(x){
#   lm.reg <- lm(formula = x~age+bmi+eth)
#   lm.reg$residuals
# })
#
# plasma.data2 <- do.call(rbind, plasma.data2)
#
#
# ##for urine data
# urine.data1 <- apply(urine.data, 1, list)
# urine.data1 <- lapply(urine.data1, unlist)
# age <- urine.pheno$AGE
# bmi <- urine.pheno$BMI
# gender <- urine.pheno$GENDER
# eth <- urine.pheno$ETH
# urine.data2 <- lapply(urine.data1, function(x){
#   lm.reg <- lm(formula = x~age+bmi+eth)
#   lm.reg$residuals
# })
#
# urine.data2 <- do.call(rbind, urine.data2)
#
#
# ##for saliva data
# saliva.data1 <- apply(saliva.data, 1, list)
# saliva.data1 <- lapply(saliva.data1, unlist)
# age <- saliva.pheno$AGE
# bmi <- saliva.pheno$BMI
# gender <- saliva.pheno$GENDER
# eth <- saliva.pheno$ETH
# saliva.data2 <- lapply(saliva.data1, function(x){
#   lm.reg <- lm(formula = x~age+bmi+eth)
#   lm.reg$residuals
# })
#
# saliva.data2 <- do.call(rbind, saliva.data2)
#
#
#
# ##use, PCA and HCA to check data
# ##for plasma data
# sample <- sxtScale(sample = plasma.data2,
#                    method = "auto",
#                    center = TRUE)
# pca.object <- prcomp(data.frame(t(sample)),
#                      retx = TRUE,
#                      center = FALSE,
#                      scale = FALSE)
#
#
# pov <- summary(pca.object)$importance[2,]
# sd <- summary(pca.object)$importance[1,]
# cp <- summary(pca.object)$importance[3,]
# pc <- pca.object$x
#
# pc1 <- round(pov[1], 2)
# pc2 <- round(pov[2], 2)
#
# temp.data <- data.frame(rownames(pc), plasma.pheno$T2D,
#                         pc[,c(1:2)], stringsAsFactors = FALSE)
# colnames(temp.data)[1:2] <- c("sample.name", "T2D")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = PC1, y = PC2,
#                                colour = T2D,
#                                shape = T2D)) +
#   ggplot2::geom_point(alpha = 1, size = 2) +
#   ggplot2::labs(x = "PC1",
#                 y = "PC2"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##HCA
# sample.info <- plasma.pheno
# colnames(sample.info)[1] <- "sample.name"
# colnames(sample.info)[6] <- "group"
# sample.info$group[sample.info$group==0] <- "Control"
# sample.info$group[sample.info$group== "1"] <- "Diabete"
# p <- heatMap(sample = plasma.data2, sample.info = sample.info,
#         group = c("Control", "Diabete"), color = c("#56B4E9", "#E69F00"),
#         show_rownames = FALSE, show_colnames = FALSE)
#
# p
#
#
#
#
#
# ##use, PCA and HCA to check data
# ##for urine data
# sample <- sxtScale(sample = urine.data2,
#                    method = "auto",
#                    center = TRUE)
# pca.object <- prcomp(data.frame(t(sample)),
#                      retx = TRUE,
#                      center = FALSE,
#                      scale = FALSE)
#
#
# pov <- summary(pca.object)$importance[2,]
# sd <- summary(pca.object)$importance[1,]
# cp <- summary(pca.object)$importance[3,]
# pc <- pca.object$x
#
# pc1 <- round(pov[1], 2)
# pc2 <- round(pov[2], 2)
#
# temp.data <- data.frame(rownames(pc), urine.pheno$T2D,
#                         pc[,c(1:2)], stringsAsFactors = FALSE)
# colnames(temp.data)[1:2] <- c("sample.name", "T2D")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = PC1, y = PC2,
#                                colour = T2D,
#                                shape = T2D)) +
#   ggplot2::geom_point(alpha = 1, size = 2) +
#   ggplot2::labs(x = "PC1",
#                 y = "PC2"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##HCA
# sample.info <- urine.pheno
# colnames(sample.info)[1] <- "sample.name"
# colnames(sample.info)[6] <- "group"
# sample.info$group[sample.info$group==0] <- "Control"
# sample.info$group[sample.info$group== "1"] <- "Diabete"
# p <- heatMap(sample = urine.data2, sample.info = sample.info,
#              group = c("Control", "Diabete"), color = c("#56B4E9", "#E69F00"),
#              show_rownames = FALSE, show_colnames = FALSE)
#
# p
#
#
#
#
# ##for urine data
# sample <- sxtScale(sample = saliva.data2,
#                    method = "auto",
#                    center = TRUE)
# pca.object <- prcomp(data.frame(t(sample)),
#                      retx = TRUE,
#                      center = FALSE,
#                      scale = FALSE)
#
#
# pov <- summary(pca.object)$importance[2,]
# sd <- summary(pca.object)$importance[1,]
# cp <- summary(pca.object)$importance[3,]
# pc <- pca.object$x
#
# pc1 <- round(pov[1], 2)
# pc2 <- round(pov[2], 2)
#
# temp.data <- data.frame(rownames(pc), saliva.pheno$T2D,
#                         pc[,c(1:2)], stringsAsFactors = FALSE)
# colnames(temp.data)[1:2] <- c("sample.name", "T2D")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = PC1, y = PC2,
#                                colour = T2D,
#                                shape = T2D)) +
#   ggplot2::geom_point(alpha = 1, size = 2) +
#   ggplot2::labs(x = "PC1",
#                 y = "PC2"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##HCA
# sample.info <- saliva.pheno
# colnames(sample.info)[1] <- "sample.name"
# colnames(sample.info)[6] <- "group"
# sample.info$group[sample.info$group==0] <- "Control"
# sample.info$group[sample.info$group== "1"] <- "Diabete"
# p <- heatMap(sample = saliva.data2, sample.info = sample.info,
#              group = c("Control", "Diabete"), color = c("#56B4E9", "#E69F00"),
#              show_rownames = FALSE, show_colnames = FALSE)
#
# p
#
#
#
#
# #####find potential biomarkers
# #####for plasma
# control.idx <- which(plasma.pheno$T2D == 0)
# case.idx <- which(plasma.pheno$T2D == 1)
# plasma.fc <- apply(plasma.data, 1, function(x){
#     median(x[case.idx])/median(x[control.idx])
#   })
#
#
# plasma.p <- apply(plasma.data2, 1, function(x){
#   wilcox.test(x = x[control.idx],
#               y = x[case.idx])$p.value
# })
#
# plasma.p <- p.adjust(p = plasma.p, method = "fdr")
#
# Marker <- rep(NA, length(plasma.p))
# Marker[which(plasma.p < 0.01 & plasma.fc > 1)] <- "Increase"
# Marker[which(plasma.p < 0.01 & plasma.fc < 1)] <- "Decrease"
# Marker[is.na(Marker)] <- "No"
# marker.number <- sum(Marker != "No")
#
# plasma.p <- -log(plasma.p, 10)
# plasma.fc <- log(plasma.fc, 2)
#
# peak.name <- names(plasma.p)
#
# temp.data <- data.frame(peak.name, plasma.fc,
#                         plasma.p, Marker, stringsAsFactors = FALSE)
#
#
#
# p <- ggplot2::ggplot(data = temp.data,
#                      aes(x = plasma.fc, y = plasma.p,
#                          colour = Marker, label = peak.name))+
#   ggplot2::geom_point()+
#   my.theme+
#   ggplot2::geom_hline(yintercept = -log(0.01, 10))+
#   ggplot2::geom_vline(xintercept = log(1, 2))+
#   ggplot2::labs(x = "Fold change (log2, Diabete/Control)",
#                 y = "P-value (log10, FDR)")+
#   annotate(geom = "text", x = plasma.fc[which(Marker != "No")],
#            y = plasma.p[which(Marker != "No")],
#            label = peak.name[which(Marker != "No")]
#            )
#
# p
#
# ###PLS analysis
# library(pls)
# int.Y <- plasma.pheno$T2D
# sample <- sxtScale(sample = plasma.data2, method = "auto")
# pls1 <- pls::plsr(int.Y~t(sample),
#                   scale = FALSE,
#                   validation = "CV",
#                   ncomp = 10,
#                   method = "oscorespls")
# msep <- MSEP(pls1)
# msep<-msep$val[,,]
# plot(x=c(1:9),y=msep[1,2:(9+1)],type="b",col="firebrick1",pch=20,
#      xlab="ncomp",ylab="MSEP",cex.lab=1.3,cex.axis=1.3)
#
# library(plsdepot)
#
# pls2 <- plsreg1(t(sample),int.Y,comps = 3)
#
# library(SXTdummy)
# int.dummy <- SXTdummy(int.Y)
# pls.temp <- plsreg2(t(sample),int.dummy, comps = 3)
# plasma.vip <- pls.temp$VIP
# Q2cum <- pls2$Q2[,5]
# R2cum <- cumsum(pls2$R2)
#
#
# x <- pls2$x.scores[,1]
# y <- pls2$x.scores[,2]
#
# temp.data <- data.frame(names(x), plasma.pheno$T2D,
#                         x, y, stringsAsFactors = FALSE)
# colnames(temp.data)[1:4] <- c("sample.name", "T2D", "t1", "t2")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = t1, y = t2,
#                                colour = T2D,
#                                shape = T2D)) +
#   ggplot2::geom_point(alpha = 1, size = 2) +
#   ggplot2::labs(x = "t1",
#                 y = "t2"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##结合VIP和p value选择marker
# plasma.marker.name <- names(which(plasma.p > 3 & plasma.p > 1))
# # plasma.marker.name <- names(which(plasma.p > 3 & plasma.vip > 1))
#
# ##permutation test
# # info <- vector(mode = "list", length = 2)
# # names(info) <- c("Control", "Diabete")
# # info[[1]] <- plasma.pheno$`QMDiab-ID`[which(plasma.pheno$T2D  == 0)]
# # info[[2]] <- plasma.pheno$`QMDiab-ID`[which(plasma.pheno$T2D  == 1)]
# #
# #
# # data <- t(sample)
#
#
#
#
# #####for urine
# control.idx <- which(urine.pheno$T2D == 0)
# case.idx <- which(urine.pheno$T2D == 1)
# urine.fc <- apply(urine.data, 1, function(x){
#   median(x[case.idx])/median(x[control.idx])
# })
#
#
# urine.p <- apply(urine.data2, 1, function(x){
#   wilcox.test(x = x[control.idx],
#               y = x[case.idx])$p.value
# })
#
# urine.p <- p.adjust(p = urine.p, method = "fdr")
#
# Marker <- rep(NA, length(urine.p))
# Marker[which(urine.p < 0.01 & urine.fc > 1)] <- "Increase"
# Marker[which(urine.p < 0.01 & urine.fc < 1)] <- "Decrease"
# Marker[is.na(Marker)] <- "No"
# marker.number <- sum(Marker != "No")
#
# urine.p <- -log(urine.p, 10)
# urine.fc <- log(urine.fc, 2)
#
# peak.name <- names(urine.p)
#
# temp.data <- data.frame(peak.name, urine.fc,
#                         urine.p, Marker, stringsAsFactors = FALSE)
#
#
#
# p <- ggplot2::ggplot(data = temp.data,
#                      aes(x = urine.fc, y = urine.p,
#                          colour = Marker, label = peak.name))+
#   ggplot2::geom_point()+
#   my.theme+
#   ggplot2::geom_hline(yintercept = -log(0.01, 10))+
#   ggplot2::geom_vline(xintercept = log(1, 2))+
#   ggplot2::labs(x = "Fold change (log2, Diabete/Control)",
#                 y = "P-value (log10, FDR)")+
#   annotate(geom = "text", x = urine.fc[which(Marker != "No")],
#            y = urine.p[which(Marker != "No")],
#            label = peak.name[which(Marker != "No")]
#   )
#
# p
#
# ###PLS analysis
# library(pls)
# int.Y <- urine.pheno$T2D
# sample <- sxtScale(sample = urine.data2, method = "auto")
# pls1 <- pls::plsr(int.Y~t(sample),
#                   scale = FALSE,
#                   validation = "CV",
#                   ncomp = 10,
#                   method = "oscorespls")
# msep <- MSEP(pls1)
# msep<-msep$val[,,]
# plot(x=c(1:9),y=msep[1,2:(9+1)],type="b",col="firebrick1",pch=20,
#      xlab="ncomp",ylab="MSEP",cex.lab=1.3,cex.axis=1.3)
#
# library(plsdepot)
#
# pls2 <- plsreg1(t(sample),int.Y,comps = 3)
#
# library(SXTdummy)
# int.dummy <- SXTdummy(int.Y)
# pls.temp <- plsreg2(t(sample),int.dummy, comps = 3)
# urine.vip <- pls.temp$VIP
# Q2cum <- pls2$Q2[,5]
# R2cum <- cumsum(pls2$R2)
#
#
# x <- pls2$x.scores[,1]
# y <- pls2$x.scores[,2]
#
# temp.data <- data.frame(names(x), urine.pheno$T2D,
#                         x, y, stringsAsFactors = FALSE)
# colnames(temp.data)[1:4] <- c("sample.name", "T2D", "t1", "t2")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = t1, y = t2,
#                                colour = T2D,
#                                shape = T2D)) +
#   ggplot2::geom_point(alpha = 1, size = 2) +
#   ggplot2::labs(x = "t1",
#                 y = "t2"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##结合VIP和p value选择marker
# urine.marker.name <- names(which(urine.p > 3 & urine.p > 1))
#
#
#
#
#
#
# #####for saliva
# control.idx <- which(saliva.pheno$T2D == 0)
# case.idx <- which(saliva.pheno$T2D == 1)
# saliva.fc <- apply(saliva.data, 1, function(x){
#   median(x[case.idx])/median(x[control.idx])
# })
#
#
# saliva.p <- apply(saliva.data2, 1, function(x){
#   wilcox.test(x = x[control.idx],
#               y = x[case.idx])$p.value
# })
#
# saliva.p <- p.adjust(p = saliva.p, method = "fdr")
#
# Marker <- rep(NA, length(saliva.p))
# Marker[which(saliva.p < 0.01 & saliva.fc > 1)] <- "Increase"
# Marker[which(saliva.p < 0.01 & saliva.fc < 1)] <- "Decrease"
# Marker[is.na(Marker)] <- "No"
# marker.number <- sum(Marker != "No")
#
# saliva.p <- -log(saliva.p, 10)
# saliva.fc <- log(saliva.fc, 2)
#
# peak.name <- names(saliva.p)
#
# temp.data <- data.frame(peak.name, saliva.fc,
#                         saliva.p, Marker, stringsAsFactors = FALSE)
#
#
#
# p <- ggplot2::ggplot(data = temp.data,
#                      aes(x = saliva.fc, y = saliva.p,
#                          colour = Marker, label = peak.name))+
#   ggplot2::geom_point()+
#   my.theme+
#   ggplot2::geom_hline(yintercept = -log(0.01, 10))+
#   ggplot2::geom_vline(xintercept = log(1, 2))+
#   ggplot2::labs(x = "Fold change (log2, Diabete/Control)",
#                 y = "P-value (log10, FDR)")+
#   annotate(geom = "text", x = saliva.fc[which(Marker != "No")],
#            y = saliva.p[which(Marker != "No")],
#            label = peak.name[which(Marker != "No")]
#   )
#
# p
#
# ###PLS analysis
# library(pls)
# int.Y <- saliva.pheno$T2D
# sample <- sxtScale(sample = saliva.data2, method = "auto")
# pls1 <- pls::plsr(int.Y~t(sample),
#                   scale = FALSE,
#                   validation = "CV",
#                   ncomp = 10,
#                   method = "oscorespls")
# msep <- MSEP(pls1)
# msep<-msep$val[,,]
# plot(x=c(1:9),y=msep[1,2:(9+1)],type="b",col="firebrick1",pch=20,
#      xlab="ncomp",ylab="MSEP",cex.lab=1.3,cex.axis=1.3)
#
# library(plsdepot)
#
# pls2 <- plsreg1(t(sample),int.Y,comps = 3)
#
# library(SXTdummy)
# int.dummy <- SXTdummy(int.Y)
# pls.temp <- plsreg2(t(sample),int.dummy, comps = 3)
# saliva.vip <- pls.temp$VIP
# Q2cum <- pls2$Q2[,5]
# R2cum <- cumsum(pls2$R2)
#
#
# x <- pls2$x.scores[,1]
# y <- pls2$x.scores[,2]
#
# temp.data <- data.frame(names(x), saliva.pheno$T2D,
#                         x, y, stringsAsFactors = FALSE)
# colnames(temp.data)[1:4] <- c("sample.name", "T2D", "t1", "t2")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = t1, y = t2,
#                                colour = T2D,
#                                shape = T2D)) +
#   ggplot2::geom_point(alpha = 1, size = 2) +
#   ggplot2::labs(x = "t1",
#                 y = "t2"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##结合VIP和p value选择marker
# saliva.marker.name <- names(which(saliva.p > 3 & saliva.p > 1))
#
#
#
# library(VennDiagram)
# t <- VennDiagram::venn.diagram(x = list("Plasma" = plasma.marker.name,
#                                         "Urine" = urine.marker.name,
#                                         "Saliva" = saliva.marker.name),
#                                filename = NULL, lwd = 3,
#                                category.names = c("Plasma", "Urine", "Saliva"),
#                                col = c("dodgerblue", "firebrick1", "orchid"),
#                                cat.col = c("dodgerblue", "firebrick1", "orchid"),
#                                cat.cex = 1.8,
#                                cex = 1.8)
# grid.draw(t)
#
#
#
#
#
# ####1,5-AG应该是一个非常重要的marker，观察该marker在三种数据中的分布
# match("1,5-anhydroglucitol (1,5-AG)", rownames(plasma.data2))
# match("1,5-anhydroglucitol (1,5-AG)", rownames(urine.data2))
# match("1,5-anhydroglucitol (1,5-AG)", rownames(saliva.data2))
#
# temp.data1 <- data.frame(colnames(plasma.data2),plasma.data2[7,],
#                         plasma.pheno$T2D, rep("Plasma", nrow(plasma.pheno)),
#                         stringsAsFactors = FALSE)
#
# temp.data2 <- data.frame(colnames(urine.data2),urine.data2[4,],
#                          urine.pheno$T2D, rep("Urine", nrow(urine.pheno)),
#                          stringsAsFactors = FALSE)
#
# temp.data3 <- data.frame(colnames(saliva.data2),saliva.data2[4,],
#                          saliva.pheno$T2D, rep("Saliva", nrow(saliva.pheno)),
#                          stringsAsFactors = FALSE)
# colnames(temp.data1) <- colnames(temp.data2) <- colnames(temp.data3) <- c("sample.name", "Intensity", "T2D", "Sample")
#
# rownames(temp.data1) <- rownames(temp.data1) <- rownames(temp.data1) <- NULL
# temp.data <- rbind(temp.data1, temp.data2, temp.data3)
#
# colnames(temp.data)[c(1:3)] <- c("sample.name", "Intensity", "T2D")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
# temp.data$Sample <- factor(temp.data$Sample, levels = c("Plasma", "Urine", "Saliva"))
#
# p <- ggplot(data = temp.data, aes(x = Sample, y = Intensity, col = T2D))+
#   ggplot2::geom_boxplot(position=position_dodge(1))+
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.2)+
#   # geom_jitter(shape = 16, position = position_jitter(0.2)) +
#   my.theme
#
# P <- p + geom_dotplot(binaxis='y', stackdir='center',
#                       position=position_dodge(1))
#
#
#
# p + theme(legend.justification = c(0,0), legend.position = c(0,0))
#
#
# ###1,5-anhydroglucitol (1,5-AG) vs saliva vs plasma
# intersect.name <- intersect(temp.data1$sample.name, temp.data3$sample.name)
#
# x <- temp.data1$Intensity[match(intersect.name, temp.data1$sample.name)]
# y <- temp.data3$Intensity[match(intersect.name, temp.data3$sample.name)]
#
# t2d <- temp.data3$T2D[which(temp.data3$sample.name %in% intersect.name)]
#
# temp.data <- data.frame(x, y, t2d, stringsAsFactors = FALSE)
# colnames(temp.data) <- c("Plasma", "Saliva", "T2D")
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
# p <- ggplot(data = temp.data, aes(x = Plasma, y = Saliva, col = T2D))+
#   ggplot2::geom_point()+
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.2)+
#   # geom_jitter(shape = 16, position = position_jitter(0.2)) +
#   my.theme
#
# lm.reg <- lm(formula = y~x)
# p<-p+ggplot2::geom_abline(slope = coefficients(lm.reg)[2], intercept = coefficients(lm.reg)[1])
# cor.test(x,y)
# p + theme(legend.justification = c(1,0), legend.position = c(1,0))
#
#
# ##1,5-anhydroglucitol vs glucose
# intersect.name <- intersect(temp.data1$sample.name, temp.data3$sample.name)
# x <- temp.data3$Intensity[match(intersect.name, temp.data3$sample.name)]
# match("glucose", rownames(plasma.data))
# y <- plasma.data2[181,][match(intersect.name, colnames(plasma.data2))]
#
# t2d <- temp.data3$T2D[which(temp.data3$sample.name %in% intersect.name)]
#
# temp.data <- data.frame(x, y, t2d, stringsAsFactors = FALSE)
# colnames(temp.data) <- c("Saliva", "Plasma", "T2D")
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
#
#
# p <- ggplot(data = temp.data, aes(x = Plasma, y = Saliva, col = T2D))+
#   ggplot2::geom_point()+
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   # xlim(c(-0.8,0.9))+
#   # ylim(c(-0.5, 0.7))+
#   # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.2)+
#   # geom_jitter(shape = 16, position = position_jitter(0.2)) +
#   my.theme
#
# lm.reg <- lm(formula = x~y)
# p<-p+ggplot2::geom_abline(slope = coefficients(lm.reg)[2], intercept = coefficients(lm.reg)[1])
# cor.test(x,y)
# p + theme(legend.justification = c(1,0), legend.position = c(1,0))
#
#
#
# ##仅仅使用尿液中的1,5-anhydroglucitol作为生物标志物，对糖尿病病人进行预测
# match("1,5-anhydroglucitol (1,5-AG)", rownames(saliva.data2))
# y <- saliva.data[4,]
# t2d <- saliva.pheno$T2D
#
#
# ##leave one out 进行预测
# pred.y <- NULL
# for(i in 1:length(y)){
#   cat(i, "")
#   Y <- t2d[-i]
#   x <- y[-i]
#   lm.reg <- lm(formula = Y~x)
#   pred.y[i] <- coefficients(lm.reg)[2]*y[i] + coefficients(lm.reg)[1]
# }
#
# library(pROC)
# roc.object <- pROC::roc(t2d, pred.y, ci = TRUE)
#
# sensitivities <- roc.object$sensitivities
# specificities <- roc.object$specificities
#
# temp.data <- data.frame(specificities, 1 - specificities,
#                         sensitivities, stringsAsFactors = FALSE)
# colnames(temp.data)[2] <- "specificities2"
#
#
# auc <- paste("AUC:",round(roc.object$auc, 3),
#              ";95%CI:", round(roc.object$ci[1], 3),"-",
#              round(roc.object$ci[2], 3), sep = "")
#
# temp.idx <- which.max(sensitivities+specificities)
# sen <- round(sensitivities[temp.idx], 3)
# spe <- round(specificities[temp.idx], 3)
# cutoff <- round(roc.object$thresholds[temp.idx], 3)
#
# point <- paste("Cutoff:", cutoff, sep = "")
#
# plot <- ggplot2::ggplot(data = temp.data, aes(x = 1-specificities, y = sensitivities))+
#   ggplot2::geom_path(size = 1) +
#   my.theme +
#   ggplot2::labs(x = "1-Specificity", y = "Sensitivity")+
#   ggplot2::annotate(geom = "text", x = 0.5, y = 0.25,
#                     label = auc, size = 5)+
#   ggplot2::geom_point(aes(x=1-spe, y=sen), colour="firebrick1", size = 2)+
#   ggplot2::annotate(geom = "text", x = 1 - spe, y = sen, vjust = 1, hjust = -0.2,
#                     label = point, size = 5, colour = "firebrick1")
#
#
# ##从上面可以看到其效果不是太好，因此考虑使用尿液中的marker做建模预测
# intersect.name <- intersect(plasma.marker.name, urine.marker.name)
# match(intersect.name, rownames(urine.data2))
# temp.data <- urine.data2[c(20,129,287, 734),]
#
# ##观察4个marker在plasma和urine中的分布
# intersect.name
#
# ####1,5-AG应该是一个非常重要的marker，观察该marker在三种数据中的分布
# match("X - 19437", rownames(plasma.data2))
# match("X - 19437", rownames(urine.data2))
#
# temp.data1 <- data.frame(colnames(plasma.data2),plasma.data2[500,],
#                          plasma.pheno$T2D, rep("Plasma", nrow(plasma.pheno)),
#                          stringsAsFactors = FALSE)
#
# temp.data2 <- data.frame(colnames(urine.data2),urine.data2[734,],
#                          urine.pheno$T2D, rep("Urine", nrow(urine.pheno)),
#                          stringsAsFactors = FALSE)
#
# colnames(temp.data1) <- colnames(temp.data2)<- c("sample.name", "Intensity", "T2D", "Sample")
#
# rownames(temp.data1) <- rownames(temp.data2)<- NULL
# temp.data <- rbind(temp.data1, temp.data2)
#
# colnames(temp.data)[c(1:3)] <- c("sample.name", "Intensity", "T2D")
#
# temp.data$T2D[temp.data$T2D == 0] <- "Control"
# temp.data$T2D[temp.data$T2D == "1"] <- "Diabete"
# temp.data$T2D <- factor(temp.data$T2D, levels = c("Control", "Diabete"))
# temp.data$Sample <- factor(temp.data$Sample, levels = c("Plasma", "Urine"))
#
# p <- ggplot(data = temp.data, aes(x = Sample, y = Intensity, col = T2D))+
#   ggplot2::geom_boxplot(position=position_dodge(1))+
#   scale_color_manual(values=c("#56B4E9", "#E69F00","#999999"))+
#   # geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.2)+
#   # geom_jitter(shape = 16, position = position_jitter(0.2)) +
#   my.theme
#
# P <- p + geom_dotplot(binaxis='y', stackdir='center',
#                       position=position_dodge(1))
#
#
#
# p + theme(legend.justification = c(0,0), legend.position = c(0,0))
#
# plasma.p[which(names(plasma.p) == "X - 19437")]
# urine.p[which(names(urine.p) == "X - 19437")]
#
#
# ###qudiao X - 19437
# temp.data <- urine.data2[c(20,129,287),]
# ##svm建模
# library(e1071)
# dis.Y <- urine.pheno$T2D
# pred.Y <- NULL
# for(i in 1:length(dis.Y)){
#   cat(i, " ")
#   svr.reg <- svm(t(temp.data[,-i]),dis.Y[-i],scale = F)
#   pred.Y[i] <- predict(svr.reg, newdata = t(temp.data[,i, drop = FALSE]))
# }
#
# library(pROC)
# roc.object <- pROC::roc(dis.Y, pred.Y, ci = TRUE)
#
# sensitivities <- roc.object$sensitivities
# specificities <- roc.object$specificities
#
# temp.data <- data.frame(specificities, 1 - specificities,
#                         sensitivities, stringsAsFactors = FALSE)
# colnames(temp.data)[2] <- "specificities2"
#
#
# auc <- paste("AUC:",round(roc.object$auc, 3),
#              ";95%CI:", round(roc.object$ci[1], 3),"-",
#              round(roc.object$ci[2], 3), sep = "")
#
# temp.idx <- which.max(sensitivities+specificities)
# sen <- round(sensitivities[temp.idx], 3)
# spe <- round(specificities[temp.idx], 3)
# cutoff <- round(roc.object$thresholds[temp.idx], 3)
#
# point <- paste("Cutoff:", cutoff, sep = "")
#
# plot <- ggplot2::ggplot(data = temp.data, aes(x = 1-specificities, y = sensitivities))+
#   ggplot2::geom_path(size = 1) +
#   my.theme +
#   ggplot2::labs(x = "1-Specificity", y = "Sensitivity")+
#   ggplot2::annotate(geom = "text", x = 0.5, y = 0.25,
#                     label = auc, size = 5)+
#   ggplot2::geom_point(aes(x=1-spe, y=sen), colour="firebrick1", size = 2)+
#   ggplot2::annotate(geom = "text", x = 1 - spe, y = sen, vjust = 1, hjust = -0.2,
#                     label = point, size = 5, colour = "firebrick1")
# plot
#
#
#
#
#
#
#
#
#
#
#
#
# ##rf建模
# temp.data <- urine.data2[c(20,129,287),]
# library(randomForest)
# dis.Y <- urine.pheno$T2D
# pred.Y <- NULL
# for(i in 1:length(dis.Y)){
#   cat(i, " ")
#   rf.reg <- randomForest(t(temp.data[,-i]), dis.Y[-i], prox = TRUE,
#                importance = TRUE)
#   pred.Y[i] <- predict(rf.reg, newdata = t(temp.data[,i, drop = FALSE]))
# }
#
# library(pROC)
# roc.object <- pROC::roc(dis.Y, pred.Y, ci = TRUE)
#
# sensitivities <- roc.object$sensitivities
# specificities <- roc.object$specificities
#
# temp.data <- data.frame(specificities, 1 - specificities,
#                         sensitivities, stringsAsFactors = FALSE)
# colnames(temp.data)[2] <- "specificities2"
#
#
# auc <- paste("AUC:",round(roc.object$auc, 3),
#              ";95%CI:", round(roc.object$ci[1], 3),"-",
#              round(roc.object$ci[2], 3), sep = "")
#
# temp.idx <- which.max(sensitivities+specificities)
# sen <- round(sensitivities[temp.idx], 3)
# spe <- round(specificities[temp.idx], 3)
# cutoff <- round(roc.object$thresholds[temp.idx], 3)
#
# point <- paste("Cutoff:", cutoff, sep = "")
#
# plot <- ggplot2::ggplot(data = temp.data, aes(x = 1-specificities, y = sensitivities))+
#   ggplot2::geom_path(size = 1) +
#   my.theme +
#   ggplot2::labs(x = "1-Specificity", y = "Sensitivity")+
#   ggplot2::annotate(geom = "text", x = 0.5, y = 0.25,
#                     label = auc, size = 5)+
#   ggplot2::geom_point(aes(x=1-spe, y=sen), colour="firebrick1", size = 2)+
#   ggplot2::annotate(geom = "text", x = 1 - spe, y = sen, vjust = 1, hjust = -0.2,
#                     label = point, size = 5, colour = "firebrick1")
# plot
#
#
#
# ####对urine和plasma的marker分别做pathway  enrichment分析
# plasma.marker.name
# data("hsa.kegg.pathway", package = "MetDNA")
# kegg.id <- plasma.tags$KEGG[match(plasma.marker.name, plasma.tags$BIOCHEMICAL)]
# kegg.id <- kegg.id[!is.na(kegg.id)]
# result <- mseAnalysis(metabolite.id = kegg.id, species = "hsa")
#
# Enriched <- rep(NA, nrow(result))
# Enriched[which(result$FDR < 0.05)] <- "Yes"
# Enriched[is.na(Enriched)] <- "No"
# temp.data <- data.frame(rownames(result), Enriched, result, stringsAsFactors = FALSE)
# colnames(temp.data)[1] <- "Pathway.name"
# temp.data$FDR <- -log(temp.data$FDR, 10)
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = Overlap, y = FDR,
#                                colour = Enriched,
#                                size = Pathway.length)) +
#   ggplot2::geom_point(alpha = 1) +
#   ggplot2::labs(x = "Overlap",
#                 y = "-log10(P-value, FDR corrected)"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 1.3) +
#   # ggplot2::geom_vline(xintercept = 0) +
#   # scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
# ##urine
# urine.marker.name
# data("hsa.kegg.pathway", package = "MetDNA")
# kegg.id <- urine.tags$KEGG[match(urine.marker.name, urine.tags$BIOCHEMICAL)]
# kegg.id <- kegg.id[!is.na(kegg.id)]
# result <- mseAnalysis(metabolite.id = kegg.id, species = "hsa")
#
# Enriched <- rep(NA, nrow(result))
# Enriched[which(result$FDR < 0.05)] <- "Yes"
# Enriched[is.na(Enriched)] <- "No"
# temp.data <- data.frame(rownames(result), Enriched, result, stringsAsFactors = FALSE)
# colnames(temp.data)[1] <- "Pathway.name"
# temp.data$FDR <- -log(temp.data$FDR, 10)
#
# p <-
#   ggplot2::ggplot(data = temp.data,
#                   ggplot2::aes(x = Overlap, y = FDR,
#                                colour = Enriched,
#                                size = Pathway.length)) +
#   ggplot2::geom_point(alpha = 1) +
#   ggplot2::labs(x = "Overlap",
#                 y = "-log10(P-value, FDR corrected)"
#                 # colour = "Batch"
#   ) +
#   ggplot2::geom_hline(yintercept = 1.3) +
#   # ggplot2::geom_vline(xintercept = 0) +
#   # scale_color_manual(values=c("#56B4E9", "#E69F00","#999999")) +
#   my.theme
#
# p + theme(legend.justification = c(0,1), legend.position = c(0,1))
#
#
#
#
# #####画一个热图，4个marker在三种样品中，不同样品中分布
# intersect(plasma.marker.name, urine.marker.name)
# intersect(plasma.marker.name, saliva.marker.name)
# temp.marker.name <- c(intersect(plasma.marker.name, urine.marker.name),
#                       intersect(plasma.marker.name, saliva.marker.name))
#
# temp.marker.name <- temp.marker.name[-4]
#
# temp.data1 <- plasma.data2[match(temp.marker.name, rownames(plasma.data2)),]
# temp.data2 <- urine.data2[match(temp.marker.name, rownames(urine.data2)),]
# temp.data3 <- saliva.data2[match(temp.marker.name, rownames(saliva.data2)),]
# temp.data3[is.na(temp.data3)] <- 0
#
# sample.info1 <- plasma.pheno
# colnames(sample.info1)[1] <- "sample.name"
# colnames(sample.info1)[6] <- "group"
# sample.info1$group[sample.info1$group==0] <- "Control"
# sample.info1$group[sample.info1$group== "1"] <- "Diabete"
#
# p <- heatMap(sample = temp.data1,
#              sample.info = sample.info1,
#              show_rownames = TRUE,
#              show_colnames = FALSE,
#              cluster_rows = FALSE,
#              group = c("Control", "Diabete"),
#              color = c("#56B4E9", "#E69F00"))
#
#
#
#
# sample.info2 <- urine.pheno
# colnames(sample.info2)[1] <- "sample.name"
# colnames(sample.info2)[6] <- "group"
# sample.info2$group[sample.info2$group==0] <- "Control"
# sample.info2$group[sample.info2$group== "1"] <- "Diabete"
#
# p <- heatMap(sample = temp.data2,
#              sample.info = sample.info2,
#              show_rownames = TRUE,
#              show_colnames = FALSE,
#              cluster_rows = FALSE,
#              group = c("Control", "Diabete"),
#              color = c("#56B4E9", "#E69F00"))
#
#
#
#
# sample.info3 <- urine.pheno
# colnames(sample.info3)[1] <- "sample.name"
# colnames(sample.info3)[6] <- "group"
# sample.info3$group[sample.info3$group==0] <- "Control"
# sample.info3$group[sample.info3$group== "1"] <- "Diabete"
#
# p <- heatMap(sample = temp.data3,
#              sample.info = sample.info3,
#              show_rownames = TRUE,
#              show_colnames = FALSE,
#              cluster_rows = FALSE,
#              group = c("Control", "Diabete"),
#              color = c("#56B4E9", "#E69F00"))
#
#
#
#
# ###
# temp.marker.name <- unique(c(plasma.marker.name, urine.marker.name, saliva.marker.name))
#
# temp.marker.name <- temp.marker.name[-4]
#
# temp.data1 <- plasma.data2[match(temp.marker.name, rownames(plasma.data2)),]
# temp.data2 <- urine.data2[match(temp.marker.name, rownames(urine.data2)),]
# temp.data3 <- saliva.data2[match(temp.marker.name, rownames(saliva.data2)),]
# temp.data1 <- temp.data1[-which(apply(temp.data1, 1, function(x)all(is.na(x)))),]
# temp.data2 <- temp.data2[-which(apply(temp.data2, 1, function(x)all(is.na(x)))),]
# temp.data3 <- temp.data3[-which(apply(temp.data3, 1, function(x)all(is.na(x)))),]
#
#
# intersect.name <- intersect(intersect(colnames(temp.data1), colnames(temp.data2)),
#                            colnames(temp.data3))
#
# temp.data1 <- temp.data1[,match(intersec.name, colnames(temp.data1))]
# temp.data2 <- temp.data2[,match(intersec.name, colnames(temp.data2))]
# temp.data3 <- temp.data3[,match(intersec.name, colnames(temp.data3))]
#
# temp.data1 <- as.data.frame(temp.data1)
# temp.data2 <- as.data.frame(temp.data2)
# temp.data3 <- as.data.frame(temp.data3)
#
#
# temp.pheno <- plasma.pheno[match(intersect.name, plasma.pheno$`QMDiab-ID`),]
# colnames(temp.pheno)[1] <- "id"
# colnames(temp.pheno)[6] <- "group"
#
# temp.tags1 <- plasma.tags[match(rownames(temp.data1), plasma.tags$BIOCHEMICAL),]
# temp.tags2 <- urine.tags[match(rownames(temp.data2), urine.tags$BIOCHEMICAL),]
# temp.tags3 <- saliva.tags[match(rownames(temp.data3), saliva.tags$BIOCHEMICAL),]
#
# temp.tags1$BIOCHEMICAL <- paste(temp.tags1$BIOCHEMICAL, 1, sep = "_")
# temp.tags2$BIOCHEMICAL <- paste(temp.tags2$BIOCHEMICAL, 2, sep = "_")
# temp.tags3$BIOCHEMICAL <- paste(temp.tags3$BIOCHEMICAL, 3, sep = "_")
#
# rownames(temp.data1) <- paste(rownames(temp.data1), 1, sep = "_")
# rownames(temp.data2) <- paste(rownames(temp.data2), 2, sep = "_")
# rownames(temp.data3) <- paste(rownames(temp.data3), 3, sep = "_")
#
#
# temp.data <- rbind(temp.data1, temp.data2, temp.data3)
#
# temp.tags <- rbind(temp.tags1, temp.tags2, temp.tags3)
#
#
#
#
# ####WGCNA分析
# library(WGCNA)
# library(MetaboDiff)
#
#
# head(assay[,1:5])
# head(colData)
# head(rowData[,1:5])
#
# rownames(temp.data) <- paste("met", 1:nrow(temp.data), sep = "")
# rownames(temp.tags) <- paste("met", 1:nrow(temp.data), sep = "")
# temp.data <- as.matrix(temp.data)
# colnames(temp.data) <- paste("pat", 1:ncol(temp.data), sep = "")
# rownames(temp.pheno) <- paste("pat", 1:nrow(temp.pheno), sep = "")
# temp.pheno$GENDER <- factor(temp.pheno$GENDER, levels = c(0,1))
# temp.pheno$ETH <- factor(temp.pheno$ETH, levels = c(1, 2, 3, 4))
# temp.pheno$group <- factor(temp.pheno$group, levels = c(0,1))
# met <- create_mae(assay = temp.data,rowData = temp.tags,colData = temp.pheno)
#
# met <- get_SMPDBanno(met,
#                      column_kegg_id=6,
#                      column_hmdb_id=7,
#                      column_chebi_id=NA)
#
#
#
# na_heatmap(met,
#            group_factor="tumor_normal",
#            label_colors=c("darkseagreen","dodgerblue"))
#
# met = knn_impute(met,cutoff=0.4)
#
#
# outlier_heatmap(met,
#                 group_factor="group",
#                 label_colors=c("darkseagreen","dodgerblue"),
#                 k=2)
#
# met <- normalize_met(met)
#
# source("http://peterhaschke.com/Code/multiplot.R")
# multiplot(
#   pca_plot(met,
#            group_factor="tumor_normal",
#            label_colors=c("darkseagreen","dodgerblue")),
#   tsne_plot(met,
#             group_factor="tumor_normal",
#             label_colors=c("darkseagreen","dodgerblue")),
#   cols=2)
#
# met_example <- diss_matrix(met)
#
# met_example <- identify_modules(met_example,
#                                 min_module_size=5)
#
# WGCNA::plotDendroAndColors(metadata(met_example)$tree,
#                            metadata(met_example)$module_color_vector,
#                            'Module colors',
#                            dendroLabels = FALSE,
#                            hang = 0.03,
#                            addGuide = TRUE,
#                            guideHang = 0.05, main='')
#
#
#
#
# ape::plot.phylo(ape::as.phylo(metadata(met_example)$METree),
#                 type = 'fan',
#                 show.tip.label = FALSE,
#                 main='')
#
#
# ape::tiplabels(frame = 'circle',
#                col='black',
#                text=rep('',length(unique(metadata(met_example)$modules))),
#                bg = WGCNA::labels2colors(0:21))
#
#
#
#
# table(metadata(met_example)$modules)
#
#
# met_example <- name_modules(met_example,
#                             pathway_annotation = "SUB_PATHWAY")
#
#
# ape::plot.phylo(ape::as.phylo(metadata(met_example)$METree), cex=0.9)
#
#
# met_example <- calculate_MS(met_example,
#                             group_factors = c("GENDER"))
#
# lapply(1:11, function(x){
#   temp.tags$BIOCHEMICAL[which(metadata(met_example)$modules == x)]
# })
#
# metadata(met_example)$modules
#
#
#
# ####构建correlation network
# # cor.network <- cor(t(temp.data))
# cor.matrix <- vector(mode = "list", length = nrow(temp.data) - 1)
# for(i in 1:(nrow(temp.data) - 1)){
#   cat(i, " ")
# cor.matrix[[i]] <- lapply((i+1):nrow(temp.data), function(idx){
#   temp.cor <- cor(temp.data[idx], temp.data[i])
#   temp.cor <- data.frame(rownames(temp.cor), colnames(temp.cor),
#                          as.numeric(temp.cor), stringsAsFactors = FALSE)
#   colnames(temp.cor) <- c("met1", "met2", "cor")
#   temp.cor
# })
# }
#
#
# cor.matrix <- lapply(cor.matrix, function(x){
#   do.call(rbind, x)
# })
#
# cor.matrix <- do.call(rbind, cor.matrix)
#
#
#
# ####画一个circos, show the data we have
# library(circlize)
# df <- all.pheno
# df <- df[1:20,]
# df <- rbind(df, df)
# x <- c(rep(1, nrow(df)),rep(2, nrow(df)))
# y <- c(rep(1, nrow(df)),rep(2, nrow(df)))
# df <- data.frame(df, x, y, stringsAsFactors = FALSE)
# df$QMDiab.ID <- as.factor(df$QMDiab.ID)
#
# circos.par("track.height" = 0.15)
# circos.initialize(factors = df$QMDiab.ID, x = df$x)
#
#
# circos.track(factors = df$factors, y = df$y,
#              panel.fun = function(x, y) {
#                circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"),
#                            CELL_META$sector.index)
#                circos.axis(labels.cex = 0.6)
#              }
# )
#
#
#
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#   # xlim = CELL_META$xlim
#   # ylim = CELL_META$ylim
#   # breaks = seq(xlim[1], xlim[2], by = 0.1)
#   # n_breaks = length(breaks)
#   # circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
#   #             breaks[-1], rep(ylim[2], n_breaks - 1),
#   #             col = rand_color(n_breaks), border = NA)
#   circos.rect(0, 0, 1, 1,
#               col = "red", border = NA)
# })
#
#
#
#
#
#
#
# x = rnorm(1600)
# factors = sample(letters[1:16], 1600, replace = TRUE)
# circos.initialize(factors = factors, x = x)
# circos.trackHist(factors = factors, x = x, col = "#999999",
#                  border = "#999999")
# circos.trackHist(factors = factors, x = x, bin.size = 0.1,
#                  col = "#999999", border = "#999999")
# circos.trackHist(factors = factors, x = x, draw.density = TRUE,
#                  col = "#999999", border = "#999999")
#
#
#
#
# my.theme <- ggplot2::theme_bw()+
#   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 13),
#                  axis.title.y = ggplot2::element_text(size = 13)) +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12),
#                  axis.text.y = ggplot2::element_text(size = 12)) +
#   ggplot2::theme(legend.title = ggplot2::element_text(size = 13)) +
#   ggplot2::theme(legend.text = ggplot2::element_text(size = 12))
#
#
#
#
#
#
#
# p <- unname(plasma.p[match(plasma.marker.name, names(plasma.p))])
# fc <- 2^unname(plasma.fc[match(plasma.marker.name, names(plasma.p))])
# info <- plasma.tags[match(plasma.marker.name, plasma.tags$BIOCHEMICAL),]
# plasma.marker.info <- data.frame(info, p, fc, stringsAsFactors = FALSE)
# plasma.marker.info <- plasma.marker.info[,c(2,3,4,11,14,15)]
#
# colnames(plasma.marker.info)[c(5,6)] <- c("P-value", "Fold change")
# rownames(plasma.marker.info) <- NULL
# save(plasma.marker.info, file = "plasma.marker.info")
#
#
#
# p <- unname(urine.p[match(urine.marker.name, names(urine.p))])
# fc <- 2^unname(urine.fc[match(urine.marker.name, names(urine.p))])
# info <- urine.tags[match(urine.marker.name, urine.tags$BIOCHEMICAL),]
# urine.marker.info <- data.frame(info, p, fc, stringsAsFactors = FALSE)
# urine.marker.info <- urine.marker.info[,c(2,3,4,11,14,15)]
#
# colnames(urine.marker.info)[c(5,6)] <- c("P-value", "Fold change")
# rownames(urine.marker.info) <- NULL
# save(urine.marker.info, file = "urine.marker.info")
#
#
# p <- unname(saliva.p[match(saliva.marker.name, names(saliva.p))])
# fc <- 2^unname(saliva.fc[match(saliva.marker.name, names(saliva.p))])
# info <- saliva.tags[match(saliva.marker.name, saliva.tags$BIOCHEMICAL),]
# saliva.marker.info <- data.frame(info, p, fc, stringsAsFactors = FALSE)
# saliva.marker.info <- saliva.marker.info[,c(2,3,4,11,14,15)]
#
# colnames(saliva.marker.info)[c(5,6)] <- c("P-value", "Fold change")
# rownames(saliva.marker.info) <- NULL
# save(saliva.marker.info, file = "saliva.marker.info")
