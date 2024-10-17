# fix for dealing with internet timeout error 
# https://stackoverflow.com/questions/68666187/internetopenurl-failed-a-connection-with-the-server-could-not-be-established

rm(list = ls())

plot.new()
dev.off()

library(lubridate)
library(maps)
library(plotTimeSeries)
library(rerddap)

load("indicator_processing/spec_file.RData")

# define years  --------------------------------
styear <- 1982
enyear <- terminal_year

# get ERDDAP info  --------------------------------
sst <- info('ncdcOisst21Agg_LonPM180') # this may work better

# empty data  -------------------------------------------------
dat <- setNames(data.frame(matrix(NA,length(styear:enyear)*12,5)),
                c("year", "mon", 'mean', 'min', 'max'))
m <- 1
n <- 0

# download by year to avoid timeout errors --------------------
for (yr in styear:enyear) { 
  
  ### BDT rERDDAP fix
  sst_grab <- griddap(sst, fields = 'sst', 
                      time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')), 
                      longitude = c(min_lon, max_lon), 
                      latitude = c(min_lat, max_lat))
  
  sst_agg <- aggregate(sst_grab$data$sst, 
                       by = list(year(sst_grab$data$time), month(sst_grab$data$time)), 
                       function(x) c(mean(x, na.rm = T), min(x, na.rm = T), max(x, na.rm = T)))
  
  n <- n + 12
  dat[m:n,] <- data.frame(sst_agg[,1:2], unlist(sst_agg[,3]))
  m <- n + 1

}

# add yearmonth column --------------------------
dat$yrmon <- paste0(dat$mon, "-", dat$year)
dat
head(dat)
tail(dat)

# check outputs and look at correlations ---------------------
table(dat$year)
table(dat$mon)
matplot(dat[3:5], type = "l")
cor(dat[3:5])
plot(dat[3:5])

# format into indicator object ------------------

labs <- c(rep("U.S. Caribbean sea surface temperature", 3), rep("degrees Celsius", 3), 
          "monthly mean", "monthly minimum", "monthly maximum")

indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
inddata <- data.frame(dat[c(3:5)])
datdata <- dat$yrmon

s <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(s) <- "indicatordata"
s

ind <- s

# save and plot ---------------------------------------

save(ind, file = "indicator_objects/Carib_SST.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, 
                        trendAnalysis = T, widadj = 0.5, anom = "mon", type = "allLines") #  outtype = "png", hgtadj = 0.8)



print("SST -- SUCCESSFULLY RUN")



