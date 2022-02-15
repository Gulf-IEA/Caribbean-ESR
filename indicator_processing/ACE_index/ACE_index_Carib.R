################  ACE INDEX CALCULATION CODE  ##################################
################  M. Karnauskas August 2012   ##################################
################  updated Feb 15 2022         ##################################

# this code takes hurricane track data downloaded from HURDAT reanalysis and 
# calculates the ACE index (Wang 2011) for the Caribbean
#
# ACE index calculations done according to methods from: 
# GEOPHYSICAL RESEARCH LETTERS, VOL. 38, L19702, doi:10.1029/2011GL049265, 2011

# data downloaded from: https://www.ncdc.noaa.gov/ibtracs/index.php?name=ib-v4-access 
# (downloaded .csv file for N. Atlantic basin)

rm(list=ls())
library(maps)
setwd("C://Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/ACE_index/")

dat <- read.table("ibtracs.NA.list.v04r00.csv", sep=",", header=T)
head(dat)
dat <- dat[, 1:17]
head(dat)

# check data download -----------------------------------------
map('world', fill = 1, interior=F, col = gray(0.95), add=F)
points(dat$LON, dat$LAT, pch=19, cex=0.5, col = 1)

# subset Caribbean ONLY ---------------------------------------
dat$incl <- NA
dat$incl[which(dat$LON < (-63.5) & dat$LON > (-68.9) & dat$LAT < 19.6 & dat$LAT > 16.7)] <- 1

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
a <- max(which(substr(d$ISO, 1, 2)==18))
x1 <- as.vector(strptime(d$ISO[1:a], format="%Y-%m-%d %H:%M:%S"))
x2 <- as.vector(strptime(d$ISO[(a+1):length(d$ISO)], format="%m/%d/%Y %H:%M"))
d$tim <- c(x1,x2)

# use only obs after 1960 - see Wang paper - not reliable beforehand -------
d <- d[which(d$SEASON>1960), ]
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

par(mfrow = c(7, 8), mex = 0.3)
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
d2$Season <- factor(d2$SEASON, levels=1961:2021)

# sum annual ACE index -----------------------------------------
storm_index <- tapply(d2$sqvel, d2$Season, sum, na.rm = T) * 10 ^ (-4)

dev.off()
plot(names(storm_index), storm_index, type="h", axes = F)
axis(1, at = seq(1960, 2020, 5))
axis(1, at = 1960:2021, lab = rep(NA, length(1960:2021)))
axis(2); box()

storm_index[is.na(storm_index)] <- 0
plot(names(storm_index), storm_index, type="b")

storm_index
m <- matrix(storm_index, ncol=1)
cbind(1961:2021, m)

finmat <- cbind(1961:2021, m)

labs <- c(rep("", 3), "Accumulated cyclone energy index", "annual tropical cyclone activity (10^4 kt^2)", "")

# write indicator as csv file  -------------------------------

write.table(matrix(labs, nrow = 3), file = "ACE_index.csv", append = F, sep=",", col.names = F, row.names = F)
write.table(finmat, file = "ACE_index.csv", append = T, sep=",", col.names = F, row.names = F)

# check output ------------------------------------------------

ace <- conv2indicatordata("ACE_index.csv", default = T)
plotIndicatorTimeSeries(ace)

#################################################################################








