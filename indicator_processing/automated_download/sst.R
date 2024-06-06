
# fix for dealing with internet timeout error 
# https://stackoverflow.com/questions/68666187/internetopenurl-failed-a-connection-with-the-server-could-not-be-established

rm(list = ls())
dev.off()

library(lubridate)
library(maps)
library(plotTimeSeries)
library(rerddap)

load("indicator_processing/spec_file.RData")

# devtools::install_github("mdsumner/ncdf4")
# library(ncdf4)

# define years  --------------------------------
styear <- 1982
enyear <- terminal_year

# get ERDDAP info  --------------------------------
sst <- info('ncdcOisst21Agg')
sst <- info('ncdcOisst21Agg_LonPM180') # this may work better

# empty data  -------------------------------------------------
# dat <- data.frame(row.names = c("year", "mon", "PR_mean", "PR_min", "PR_max", "VI_mean", "VI_min", "VI_max"))

dat <- setNames(data.frame(matrix(NA,length(styear:enyear)*12,5)),
                c("year", "mon", 'mean', 'min', 'max'))
m <- 1
n <- 0

# download by year to avoid timeout errors --------------------
for (yr in styear:enyear) { 
  
  ### BDT rERDDAP fix
  sst_grab <- griddap(sst, fields = 'sst', 
                      time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')), 
                      longitude = c(360 + min_lon, 360 + max_lon), 
                      latitude = c(min_lat, max_lat))
  
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
  
#   
# # url from ERDDAP for OISST, download and read ----------------
#   url <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst[(", yr, "-01-01T12:00:00Z):1:(", yr, "-12-31T12:00:00Z)][(0.0):1:(0.0)][(17):1:(19)][(292):1:(296)]")
# 
#   #  url_vi <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst[(", yr, "-01-01T12:00:00Z):1:(", yr, "-12-31T12:00:00Z)][(0.0):1:(0.0)][(17):1:(19)][(294.75):1:(296)]")
#   #  url_pr <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst[(", yr, "-01-01T12:00:00Z):1:(", yr, "-12-31T12:00:00Z)][(0.0):1:(0.0)][(17):1:(19)][(292):1:(294.75)]")
#   #  download for entire Caribbean because USVI and PR highly correlated. 
#     download.file(url = url, destfile = "st.csv")
# 
#   labs <- read.table("st.csv", sep = ",", header = T, skip = 0)
#   sst_vi <- read.table("st.csv", sep = ",", header = T, skip = 1)
#   names(sst_vi) <- names(labs)
# 
# # extract month -----------------------------------------------
#   sst_vi$mon <- strftime(sst_vi$time, format = "%m")
# 
# # calculate mean, min, max and concatenate --------------------
#   ind_vi <- as.numeric(tapply(sst_vi$sst, sst_vi$mon, mean, na.rm = T))
#   min_vi <- as.numeric(tapply(sst_vi$sst, sst_vi$mon, min, na.rm = T))
#   max_vi <- as.numeric(tapply(sst_vi$sst, sst_vi$mon, max, na.rm = T))
# 
#   tempdat <- data.frame(yr, unique(sst_vi$mon), ind_vi, min_vi, max_vi, stringsAsFactors = F)
#   dat <- data.frame(rbind(dat, tempdat), stringsAsFactors = F)
}

# file.remove("st.csv")

# add row names and yearmonth column --------------------------
names(dat) <- c("year", "mon", "mean", "min", "max")
dat$yrmon <- paste0(dat$mon, "-", dat$year)
dat
head(dat)
tail(dat)

# check outputs and look at correlations ---------------------
table(dat$year)
table(dat$mon)
matplot(dat[3:5], type = "l") #, col = c(4, 4, 4, 2, 2, 2), lty = c(1, 2, 3, 1, 2, 3))
cor(dat[3:5])
plot(dat[3:5])

# format into indicator object ------------------

labs <- c(rep("U.S. Caribbean sea surface temperature", 3), rep("degrees Celsius", 3), 
          "monthly mean", "monthly minimum", "monthly maximum")
          #          "Puerto Rico - mean", "USVI - mean", "Puerto Rico - monthly minimum", "USVI - monthly minimum", "Puerto Rico - monthly maximum", "USVI - monthly maximum")

indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
inddata <- data.frame(dat[c(3:5)])
datdata <- dat$yrmon

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
s

ind <- s

# save and plot ---------------------------------------

save(ind, file = "indicator_objects/Carib_SST.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, 
                        trendAnalysis = T, widadj = 0.5, anom = "mon", type = "allLines") #  outtype = "png", hgtadj = 0.8)

