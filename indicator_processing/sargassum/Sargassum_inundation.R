

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data")
d <- read.csv("sargassum_innundation_monthly_mean_hu.csv", skip = 2)

d$yr <- substr(d$X, 1, 4)
d$vars <- (d$ulim - d$X.1)^2

tab <- tapply(d$X.1, d$yr, mean, na.rm = T)
sds <- (tapply(d$vars, d$yr, mean))^0.5

datdata <- 2011:2021
inddats <- data.frame(cbind(tab))
ulidata <- data.frame(cbind(tab + sds))
llidata <- data.frame(cbind(tab - sds))
labs <- c("Annual mean sargassum innundation", "Area (km^2)", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

inddata <- list(labels = indnames, indicators = inddats, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"

#save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects_FINAL/DegreeHeatingWeeks.RData")

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_plots/")
plotIndicatorTimeSeries(inddata, outtype = "png", widadj = 2.5)
