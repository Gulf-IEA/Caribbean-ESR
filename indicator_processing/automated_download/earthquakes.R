
# IRIS Earthquake Browser
# (automatically downloads table off of website)

# specification file and libraries -----------------------------
rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)
library(xml2)
library(rvest)

load("indicator_processing/spec_file.RData")


# download data directly from site -----------------------------
#options(download.file.method="libcurl")

#cat(paste0("https://ds.iris.edu/ieb/index.html?format=text&nodata=404&starttime=1970-01-01&endtime=2025-01-01&minmag=3.5&maxmag=10&mindepth=0&maxdepth=900&orderby=time-desc&src=usgs&limit=1000&maxlat=", 
#       max_lat, "&minlat=", min_lat, "&maxlon=", max_lon, "&minlon=", min_lon, "&sbl=1&zm=7&mt=ter"))


url <- paste0("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&st=1970-01-01&et=2025-01-01&minmag=3.5&maxmag=10&mindepth=0&xde=900&orderby=time-desc&src=usgs&limit=5000&", 
        "maxlat=", max_lat, "&minlat=", min_lat, "&maxlon=", max_lon, "&minlon=", min_lon, 
        "&sbl=1&zm=8&mt=ter&title=IEB%20export%3A%201033%20earthquakes%20as%20a%20sortable%20table.&stitle=from%", 
        "201970-01-01%20to%202025-01-01%2C%20with%20magnitudes%20from%203.5%20to%2010%2C%20depths%20from%200%20to", 
        "%20900%20km%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")

page <- read_html(url) #Creates an html document from URL
table <- html_table(page, fill = TRUE) #Parses tables into data frames
table

dat <- data.frame(table[[1]])
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

ind <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(ind) <- "indicatordata"

plotIndicatorTimeSeries(ind)

save(ind, file = "indicator_objects/earthquakes.RData")

#################################################################################

print("earthquakes -- SUCCESSFULLY RUN")


