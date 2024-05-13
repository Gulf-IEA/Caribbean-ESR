
### Whole GDP data for PR and USVI from Amy Freitag on 3/5/24 (manually downloaded from FRED https://fred.stlouisfed.org/)
### latest data run through 2021 for USVI and 2022 for PR.

# USVI data: https://fred.stlouisfed.org/series/MKTGDPVIA646NWDB
# PR data: https://fred.stlouisfed.org/series/NYGDPMKTPCDPRI


rm(list = ls())

# load data -------------------------------------

d <- read.csv("indicator_data/GDP.csv")
head(d)

# remove first row with units, convert dollars to billion dollars
d = d[-1,]
PR = (as.numeric(d$Puerto.Rico))/1000000000
USVI = (as.numeric(d$USVI, na.action = na.omit))/1000000000

# save as indicator object ----------------------
datdata <- (min(d$indicator):max(d$indicator))
inddata <- data.frame(PR, USVI)
labs <- c("GDP" , "Billion dollars", "Puerto Rico",
          "GDP" , "Billion dollars", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
save(inddata, file = "indicator_objects/GDP.RData")

plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = TRUE)
