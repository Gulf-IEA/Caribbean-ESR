
# fix for dealing with internet timeout error 
# https://stackoverflow.com/questions/68666187/internetopenurl-failed-a-connection-with-the-server-could-not-be-established

rm(list = ls())

#devtools::install_github("mdsumner/ncdf4")
#library(ncdf4)

# empty data  -------------------------------------------------
dat <- data.frame(row.names = c("year", "mon", "PR_mean", "PR_min", "PR_max", "VI_mean", "VI_min", "VI_max"))

# download by year to avoid timeout errors --------------------
for (yr in 1982:2021) { 
  
# url from ERDDAP for OISST, download and read ----------------
  url <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst[(", yr, "-01-01T12:00:00Z):1:(", yr, "-12-31T12:00:00Z)][(0.0):1:(0.0)][(17):1:(19)][(292):1:(296)]")
#  url_vi <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst[(", yr, "-01-01T12:00:00Z):1:(", yr, "-12-31T12:00:00Z)][(0.0):1:(0.0)][(17):1:(19)][(294.75):1:(296)]")
#  url_pr <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst[(", yr, "-01-01T12:00:00Z):1:(", yr, "-12-31T12:00:00Z)][(0.0):1:(0.0)][(17):1:(19)][(292):1:(294.75)]")
  download.file(url = url, destfile = "st.csv")

  labs <- read.table("st.csv", sep = ",", header = T, skip = 0)
  sst_vi <- read.table("st.csv", sep = ",", header = T, skip = 1)
  names(sst_vi) <- names(labs)

# extract month -----------------------------------------------
  sst_vi$mon <- strftime(sst_vi$time, format = "%m")

# calculate mean, min, max and concatenate --------------------
  ind_vi <- as.numeric(tapply(sst_vi$sst, sst_vi$mon, mean, na.rm = T))
  min_vi <- as.numeric(tapply(sst_vi$sst, sst_vi$mon, min, na.rm = T))
  max_vi <- as.numeric(tapply(sst_vi$sst, sst_vi$mon, max, na.rm = T))

  tempdat <- data.frame(yr, unique(sst_vi$mon), ind_vi, min_vi, max_vi, stringsAsFactors = F)
  dat <- data.frame(rbind(dat, tempdat), stringsAsFactors = F)
}

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

#save(s, file = "Carib_SST.RData")

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, 
                        trendAnalysis = T, widadj = 0.5, anom = "mon", type = "allLines", outtype = "png", hgtadj = 0.8)

