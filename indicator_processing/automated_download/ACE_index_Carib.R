################  ACE INDEX CALCULATION CODE  ##################################
################  M. Karnauskas August 2012   ##################################
################  updated Feb 15 2022         ##################################

# this code takes hurricane track data downloaded from HURDAT reanalysis and 
# calculates the ACE index (Wang 2011) for the Caribbean
#
# ACE index calculations done according to methods from: 
# GEOPHYSICAL RESEARCH LETTERS, VOL. 38, L19702, doi:10.1029/2011GL049265, 2011

# data source: https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ 
# (download .csv file for N. Atlantic basin)

# specification file and libraries -----------------------------
rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

# define years  --------------------------------
styear <- 1961
enyear <- terminal_year

# download data directly from site -----------------------------
options(download.file.method="libcurl")

url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.NA.list.v04r00.csv"

download.file(url = url, destfile = "ibtracs.csv")
dat <- read.csv("ibtracs.csv", skip = 2, header = F)
datn <- read.csv("ibtracs.csv", skip = 0, header = T)
names(dat) <- names(datn)

file.remove("ibtracs.csv")

# cut columns --------------------------------

head(dat)
dat <- dat[, 1:17]
head(dat)

# check data download -----------------------------------------
map('world', fill = 1, interior=F, col = gray(0.95), add=F)
points(dat$LON, dat$LAT, pch=19, cex=0.5, col = 1)

# subset Caribbean ONLY ---------------------------------------
dat$incl <- NA
dat$incl[which(dat$LON < (max_lon + 1) & dat$LON > (min_lon - 1) & 
               dat$LAT < (max_lat + 1) & dat$LAT > (min_lat - 1))] <- 1

# check study domain ------------------------------------------
points(dat$LON, dat$LAT, pch=19, col=dat$incl+1, cex=0.1)

map('world', fill = 1, interior=F, col = gray(0.95),xlim=c(-80, -60), ylim=c(10, 30))
points(dat$LON, dat$LAT, pch=19, col=dat$incl+1, cex=0.1)

# include only those within specified box ---------------------
d <- dat[which(dat$incl==1), ]
dim(d)
d <- d[which(d$NATURE=="TS"), ]
dim(d)

# standardize time format -------------------------------------
d$tim <- strptime(d$ISO_TIME, format="%Y-%m-%d %H:%M:%S")

# use only obs after 1960 - see Wang paper - not reliable beforehand -------
d <- d[which(d$SEASON >= styear & d$SEASON <= enyear), ]
dim(d)

## remove observations not taken every 6 hours -----------------------------
d$tim2 <- format(d$tim, "%H:%M")
d$Name <- as.factor(as.character(d$NAME))

table(d$tim2)
d2 <- d[-which(d$tim2!="00:00" & d$tim2!="06:00" & d$tim2!="12:00" & d$tim2!="18:00"), ]
dim(d2)

# check for irregular reporting in database --------------------------------
# averages != 6 are usually result of hurricanes turning to tropical storms and back to hurricanes
# thus, time series not always complete for each hurricane. 

d2$uniq <- paste0(d2$NAME, " ", d2$SEASON)
hurlist <- names(table(d2$uniq))
errs <- matrix(NA, nrow=length(hurlist), ncol=2)
hurlist
  
for (i in 1:length(hurlist)) {
    temp <- d2[which(d2$uniq==hurlist[i]), ]
    errs[i, 1] <- hurlist[i]
    m <- difftime(temp$tim[2:(length(temp$tim))], temp$tim[1:(length(temp$tim)-1)], units = "hours")  
    errs[i, 2] <- as.numeric(mean(m))
  }

errs <- data.frame(errs)
errs

nam <- which(errs$X2 != 6)
length(nam)
cols <- rainbow(365)

dev.off()
par(mfrow = c(2, 2), mex = 0.4)
for (i in nam) {
  map('world', add=F, fill=T, col=8,  xlim=c(-72, -62), ylim=c(15, 22))
  ptcol <- as.numeric(strftime(d2$tim[which(d2$uniq == errs$X1[i])], format = "%j"))
  points(d2$LON[which(d2$uniq == errs$X1[i])], d2$LAT[which(d2$uniq == errs$X1[i])], pch = 19, col = cols[ptcol])
  mtext(side = 3, errs$X1[i], cex = 0.5)
  axis(1); axis(2); box()  
  }

# check all trajectories -----------------------------------------
unique(d2$uniq)
nam <- unique(d2$uniq)
length(nam)

dev.off()
par(mfrow = c(8, 8), mex = 0.3)
for (i in 1:length(nam)) {
  map('world', add=F, fill=T, col=8,  xlim=c(-72, -62), ylim=c(15, 22))
  ptcol <- as.numeric(strftime(d2$tim[which(d2$uniq == nam[i])], format = "%j"))
  points(d2$LON[which(d2$uniq == nam[i])], d2$LAT[which(d2$uniq == nam[i])], pch = 19, col = cols[ptcol])
  mtext(side = 3, nam[i], cex = 0.75)
  axis(1); axis(2); box()  
  }

# calculate ACE index --------------------------------------------
# sum of squared max wind intensities (knots) * 10 ^ -4  

d2$sqvel <- d2$WMO_WIND * d2$WMO_WIND
head(d2)

table(dat$SEASON)
table(d2$SEASON)
d2$Season <- factor(d2$SEASON, levels = styear:enyear)

# sum annual ACE index -----------------------------------------
storm_index <- tapply(d2$sqvel, d2$Season, sum, na.rm = T) * 10 ^ (-4)

dev.off()
plot(names(storm_index), storm_index, type="h")
storm_index[is.na(storm_index)] <- 0
plot(names(storm_index), storm_index, type="b")

storm_index

# format indicator object -----------------------------

datdata <- styear:enyear
inddata <- data.frame(storm_index)
labs <- c("Accumulated cyclone energy index", "annual tropical cyclone activity (10^4 kt^2)", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, widadj = 0.5)

ind <- s
save(ind, file = "indicator_objects/ACEindex.RData")

#################################################################################

print("ACE index -- SUCCESSFULLY RUN")






