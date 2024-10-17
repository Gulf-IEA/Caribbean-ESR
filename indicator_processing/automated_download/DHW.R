#
# Degree Heating Weeks indicators

rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

options(download.file.method="libcurl")

styear <- 1980
enyear <- terminal_year

# download data and calculate mean -------------------

for (i in 1:2) {

  if (i == 1) {  url <- "https://coralreefwatch.noaa.gov/product/vs/data/puerto_rico.txt"  }
  if (i == 2) {  url <- "https://coralreefwatch.noaa.gov/product/vs/data/usvi.txt"  }

  download.file(url = url, destfile = "dhw.txt")
  d <- read.table("dhw.txt", skip = 21, header = T)
  
  file.remove("dhw.txt")

  d <- d[which(d$YYYY >= styear & d$YYYY <= enyear), ]
  
  head(d)
  d$yrmon <- paste0(d$YYYY, sprintf("%02.f", d$MM))
  # d$yrmon <- d$YYYY
  avs <- tapply(d$DHW_from_90th_HS.1, d$yrmon, mean, na.rm = T)
  uli <- tapply(d$DHW_from_90th_HS.1, d$yrmon, function(x) quantile(x, probs = c(0.975), na.rm = T))
  lli <- tapply(d$DHW_from_90th_HS.1, d$yrmon, function(x) quantile(x, probs = c(0.025), na.rm = T))
  
  if (i == 1) {
    dates <- names(avs)
    dhw <- data.frame(avs, uli, lli)
  }
  if (i == 2) {
    print(paste(table(names(avs) == dates), "<-- check all TRUE"))
    dhw <- data.frame(cbind(dhw, avs, uli, lli))
    names(dhw) <- c("PR", "PRul", "PRll", "USVI", "USVIul", "USVIll")  
  }
}

head(dhw)
matplot(dhw[, 1:3], type = "l")
cor(dhw$PR, dhw$USVI) # correlated at ~98%

# format indicator object ------------------------------
datdata <- dates
inddats <- data.frame(cbind(dhw$PR, dhw$USVI))
ulidata <- data.frame(cbind(dhw$PRul, dhw$USVIul))
llidata <- data.frame(cbind(dhw$PRll, dhw$USVIll))
labs <- c(rep("Average monthly degree heating weeks", 2), 
          rep("degree Celsius-weeks", 2),
          "Puerto Rico", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

ind <- list(labels = indnames, indicators = inddats, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(ind) <- "indicatordata"

# save and plot -----------------------------------

save(ind, file = "indicator_objects/DegreeHeatingWeeks.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, sublabel = T, dateformat = "%Y%m", yposadj = 0.7,
                        sameYscale = T, type = "allLines")

# END ---------------

print("DHW -- SUCCESSFULLY RUN")

