library(xcms)
library(faahKO)
library(RColorBrewer)
library(pander)
library(magrittr)

## Get the full path to the CDF files
cdfs <- dir(system.file("cdf", package = "faahKO"), full.names = TRUE,
            recursive = TRUE)
## Create a phenodata data.frame
pd <- data.frame(sample_name = sub(basename(cdfs), pattern = ".CDF",
                                   replacement = "", fixed = TRUE),
                 sample_group = c(rep("KO", 6), rep("WT", 6)),
                 stringsAsFactors = FALSE)

raw_data <- readMSData(files = cdfs, pdata = new("NAnnotatedDataFrame", pd),
                       mode = "onDisk")

head(rtime(raw_data))

mzs <- mz(raw_data)

mzs_by_file <- split(mzs, f = fromFile(raw_data))
length(mzs_by_file)


## Get the base peak chromatograms. This reads data from the files.
bpis <- chromatogram(raw_data, aggregationFun = "max")
## Define colors for the two groups
group_colors <- paste0(brewer.pal(3, "Set1")[1:2], "60")
names(group_colors) <- c("KO", "WT")

## Plot all chromatograms.
plot(bpis, col = group_colors[raw_data$sample_group])

bpi_1 <- bpis[1, 1]
head(rtime(bpi_1))
head(intensity(bpi_1))


## Get the total ion current by file
tc <- split(tic(raw_data), f = fromFile(raw_data))
boxplot(tc, col = group_colors[raw_data$sample_group],
        ylab = "intensity", main = "Total ion current")

