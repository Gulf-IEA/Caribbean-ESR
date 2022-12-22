
# IRIS Earthquake Browser
# (download .csv file for N. Atlantic basin)

# specification file and libraries -----------------------------
rm(list = ls())
library(maps)
library(plotTimeSeries)

load("spec_file.RData")

# download data directly from site -----------------------------
#options(download.file.method="libcurl")

cat(paste0("https://ds.iris.edu/ieb/index.html?format=text&nodata=404&starttime=1970-01-01&endtime=2025-01-01&minmag=3.5&maxmag=10&mindepth=0&maxdepth=900&orderby=time-desc&src=usgs&limit=1000&maxlat=", 
       max_lat, "&minlat=", min_lat, "&maxlon=", max_lon, "&minlon=", min_lon, "&sbl=1&zm=7&mt=ter"))

# copy link into browser
# ensure all are visible (select 'Maximum earthquakes' if not all visible)
# download as csv and save as "quakes.csv" 

#download.file(url = url, destfile = "../indicator_data/quakes.csv")
dat <- read.csv( "../indicator_data/quakes.csv", header = T)
head(dat)
dim(dat)

# define years and cut columns --------------------------------

min(dat$Year)
max(dat$Year)
hist(dat$Month)
hist(dat$Depth)
hist(dat$Mag)

dat <- dat[which(dat$Year >= 2000), ]
dat <- dat[which(dat$Year <= terminal_year), ]

# check data download -----------------------------------------
map('world', fill = 1, interior=F, col = gray(0.95), add=F, xlim = c(-80, -60), ylim = c(10, 30))
points(dat$Lon, dat$Lat, pch=19, cex=0.5, col = 1)

dev.off()

tot_num <- table(dat$Year)
tot_st <- tapply(dat$Mag, dat$Year, sum, na.rm = T)

barplot(tot_num)
barplot(tot_st)
plot(as.numeric(tot_num), as.numeric(tot_st))

# format indicator object -----------------------------

datdata <- min(dat$Year):max(dat$Year)
inddata <- data.frame(as.numeric(tot_num))
labs <- c("Earthquake activity", "number of events per year", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s)

inddata <- s
save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/earthquakes.RData")

#################################################################################




