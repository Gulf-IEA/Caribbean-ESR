
### Pollution indicator: Industrially zoned spaces data from Amy Freitag. Amy chatted with EPA and the data represent compliance/attention and not actual time of pollution. More of an indicator of political attention pollution than pollution itself.

dat = read.csv("indicator_data/pollution_sites.csv")
head(dat)

# make sure columns are numeric
dat[] = lapply(dat, as.numeric)

USVI = dat[-1,c(1,4,7,10,13,16)]
head(USVI)
str(USVI)

PR = dat[-1,c(1,3,6,9,12,15)]
head(PR)

USVI$sum = rowSums(USVI[,-1], na.rm=T)
PR$sum = rowSums(PR[,-1], na.rm=T)

PR_ind = PR$sum
USVI_ind = USVI$sum

# save as indicator object ----------------------
datdata <- as.integer(USVI$year)
inddata <- data.frame(PR_ind, USVI_ind)
labs <- c("Pollution sites" , "Number added", "Puerto Rico",
          "Pollution sites" , "Number added", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
save(inddata, file = "indicator_objects/pollution.RData")

plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = TRUE)
