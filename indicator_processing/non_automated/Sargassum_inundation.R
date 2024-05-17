
rm(list = ls())
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

# define years  --------------------------------
styear <- 1961
enyear <- terminal_year

# load data -------------------------------------

d <- read.csv("indicator_data/sargassum_innundation_monthly_mean_hu.csv", skip = 2)

d$yr <- substr(d$X, 1, 4)
d$vars <- (d$ulim - d$X.1)^2

tab <- tapply(d$X.1, d$yr, mean, na.rm = T)
sds <- (tapply(d$vars, d$yr, mean))^0.5

# save as indicator object ----------------------
datdata <- 2011:2021
inddats <- data.frame(cbind(tab))
ulidata <- data.frame(cbind(tab + sds))
llidata <- data.frame(cbind(tab - sds))
labs <- c("Annual mean sargassum innundation", "Area (km^2)", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

inddata <- list(labels = indnames, indicators = inddats, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

plotIndicatorTimeSeries(inddata)

ind <- inddata

save(ind, file = "indicator_objects/Sargassum.RData")

