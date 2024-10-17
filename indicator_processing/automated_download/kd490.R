# Turbidity indicator
#
# direct download from ERDDAP
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.html

# specification file and libraries -----------------------------

rm(list = ls())

plot.new()
dev.off()

library(lubridate)
library(maps)
library(plotTimeSeries)
library(rerddap)

load("indicator_processing/spec_file.RData")

# define years  --------------------------------
styear <- 2012
enyear <- terminal_year

# define geographic scope  --------------------------------
pr_coord <- matrix(c(-67.5, -65.2, 17.7, 18.7), 2, 2)
sc_coord <- matrix(c(-64.94, -64.41, 17.59, 17.86), 2, 2)
st_coord <- matrix(c(-65.12, -64.66, 18.20, 18.41), 2, 2)

# get ERDDAP info  --------------------------------
kd490 <- info('nesdisVHNSQkd490Monthly',
              url = 'https://coastwatch.pfeg.noaa.gov/erddap')
# alternative with longer timeseries
# kd490 <- info('pmlEsaCCI60OceanColorDaily',
#               url = 'https://coastwatch.pfeg.noaa.gov/erddap')


# empty data  -------------------------------------------------
pr_dat <- sc_dat <- st_dat <- setNames(data.frame(matrix(NA,length(styear:enyear)*12,3)),
                                       c("year", "mon", 'mean'))
m <- 1
n <- 0

# download by year to avoid timeout errors --------------------
for (yr in styear:enyear) { 
 
  ### BDT rERDDAP fix
  n <- n + 12
  ### Puerto Rico
  pr_kd490_grab <- griddap(kd490, fields = 'kd_490', 
                           time = c(paste0(yr,'-01-15'), paste0(yr,'-12-15')), 
                           longitude = pr_coord[ ,1], 
                           latitude = pr_coord[ ,2])
  
  pr_dat[m:n,] <- aggregate(pr_kd490_grab$data$kd_490, 
                            by = list(year(pr_kd490_grab$data$time), month(pr_kd490_grab$data$time)),
                            mean, na.rm = T)
  
  ### St Croix
  sc_kd490_grab <- griddap(kd490, fields = 'kd_490', 
                           time = c(paste0(yr,'-01-15'), paste0(yr,'-12-15')), 
                           longitude = sc_coord[ ,1], 
                           latitude = sc_coord[ ,2])
  
  sc_dat[m:n,] <- aggregate(sc_kd490_grab$data$kd_490, 
                            by = list(year(sc_kd490_grab$data$time), month(sc_kd490_grab$data$time)),
                            mean, na.rm = T)
  
  ### St Thomas
  st_kd490_grab <- griddap(kd490, fields = 'kd_490', 
                           time = c(paste0(yr,'-01-15'), paste0(yr,'-12-15')), 
                           longitude = st_coord[ ,1], 
                           latitude = st_coord[ ,2])
  
  st_dat[m:n,] <- aggregate(st_kd490_grab$data$kd_490, 
                            by = list(year(st_kd490_grab$data$time), month(st_kd490_grab$data$time)),
                            mean, na.rm = T)
  
  m <- n + 1
}

# create indicator object --------------

datdata <- paste(sprintf('%02d',pr_dat$mon),pr_dat$year,sep='-')
inddata <- data.frame(cbind(pr_dat$mean, st_dat$mean, sc_dat$mean), row.names = datdata)
labs <- c(rep("Turbidity from ocean color data", 3), rep("Diffuse attenuation coefficient\n at 490 nm (m^-1)", 3), 
          "Puerto Rico", "St. Thomas", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

ind <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(ind) <- "indicatordata"

# save and plot ---------------------------------------

save(ind, file = "indicator_objects/turbidity.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, trendAnalysis = F, widadj = 1.5, yposadj = 1.3)
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, trendAnalysis = F, widadj = 1.5, anom = "mon")

print("KD490 turbidity -- SUCCESSFULLY RUN")