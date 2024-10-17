#
# ocean productivity indicator 
#
# download Ocean Color data from ERDDAP 
# monthly time step
# 
# best database as of Mar 1 2024: 
#    https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI60OceanColorMonthly_Lon0360
# can also search ERDDAP https://coastwatch.pfeg.noaa.gov/erddap/index.html
#    for "ESA CCI Ocean Colour Product"
#
############################################################################################

rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

# download data ------------------------------------------------

url <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI60OceanColorMonthly_Lon0360.csv?chlor_a", 
              "[(1997-12-01T00:00:00Z):1:(", terminal_year, "-12-01T00:00:00Z)][(", 
              max_lat, "):1:(", min_lat, ")][(", 360 + min_lon, "):1:(", 360 + max_lon, ")]")
url
              
# old data source: "https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday.csv?chlorophyll[(2003-01-16T00:00:00Z):1:(2021-12-16T00:00:00Z)][(18.5):1:(17.5)][(-67.4):1:(-64.4)]"

options(download.file.method="libcurl")

download.file(url = url, destfile = "chl.csv")
  
labs <- read.table("chl.csv", sep = ",", header = T, skip = 0)
chl <- read.table("chl.csv", sep = ",", header = T, skip = 1)
names(chl) <- names(labs)
head(chl)

file.remove("chl.csv")

# calculate monthly mean and quantiles ------------------------------
ind <- tapply(chl$chlor_a, chl$time, mean, na.rm = T)
uli <- tapply(chl$chlor_a, chl$time, function(x) quantile(x, probs = c(0.975), na.rm = T))
lli <- tapply(chl$chlor_a, chl$time, function(x) quantile(x, probs = c(0.025), na.rm = T))

# look for monthly trend -------------------------------------------
mon <- strftime(names(ind), format = "%m")
boxplot(ind ~ mon)
summary(lm(ind ~ mon))    # significant seasonal trend

# format into indicator object --------------------------------------
datdata <- strftime(names(ind), format = "%m-%Y")
inddata <- data.frame(ind, row.names = datdata)
#ulidata <- data.frame(uli, row.names = datdata)   # data are highly variable -- CIs very wide and not useful
#llidata <- data.frame(lli, row.names = datdata)
labs <- c("U.S. Caribbean primary productivity", "Mean Chlorophyll a Concentration (mg per m^3)", "monthly anomalies")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

# save and plot ------------------------------------------------------

ind <- s
save(ind, file = "indicator_objects/carib_Chl.RData")

plotIndicatorTimeSeries(ind, dateformat = "%m-%Y", sublabel = T, trendAnalysis = T, anom = "mon", 
                        widadj = 0.7, hgtadj = 0.8, type = "allLines")


 # plot spatiotemporal patterns ----------------------------------------

#nc <- nc_open("C:/Users/mandy.karnauskas/Downloads/erdMH1chlamday_a61e_882b_bec3.nc")
#v1 <- nc$var[[1]]
#chl <- ncvar_get(nc, v1)
#lon <- v1$dim[[1]]$vals 
#lat <- v1$dim[[2]]$vals 
#tim <- strftime(as.POSIXct(v1$dim[[3]]$vals, origin = "1970-01-01"), format = "%b %Y")
#nc_close(nc)

#par(mfrow = c(6, 12), mar = c(2, 1, 2, 1), mex = 0.75) 
#cols <- viridis::viridis(9)
#for (i in 1: 72) {
#  image(lon, lat[length(lat):1], chl[, length(lat):1, i], breaks = c(seq(0, 2, 0.25), 7), col = cols, main = tim[i])
#  }

#for (i in 73: 144) {
#  image(lon, lat[length(lat):1], chl[, length(lat):1, i], breaks = c(seq(0, 2, 0.25), 7), col = cols, main = tim[i])
#}

print("primary productivity -- SUCCESSFULLY RUN")

