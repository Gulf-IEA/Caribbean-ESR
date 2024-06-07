# 
# Turbidity indicator
#
# direct download from ERDDAP
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.html

# specification file and libraries -----------------------------

rm(list = ls())
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
labs <- c(rep("Turbidity from ocean color data", 3), rep("Diffuse attenuation coefficient at 490 nm (m^-1)", 3), 
          "Puerto Rico", "St. Thomas", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

ind <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(ind) <- "indicatordata"

# save and plot ---------------------------------------

save(ind, file = "indicator_objects/turbidity.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, trendAnalysis = F, widadj = 1.5)
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, trendAnalysis = F, widadj = 1.5, anom = "mon")





# terminal_year <- 2013
# # define urls  --------------------------------
# 
# beg <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.csv?kd_490[(2012-01-02T12:00:00Z):1:("
# 
# url_pr <- paste0(beg, terminal_year, "-12-01T12:00:00Z)][(0.0):1:(0.0)][(18.7):1:(17.7)][(-67.5):1:(-65.2)]")
# url_st <- paste0(beg, terminal_year, "-12-01T12:00:00Z)][(0.0):1:(0.0)][(18.41):1:(18.20)][(-65.12):1:(-64.66)]")
# url_sc <- paste0(beg, terminal_year, "-12-01T12:00:00Z)][(0.0):1:(0.0)][(17.86):1:(17.59)][(-64.94):1:(-64.41)]")
# 
# # previously separated St. Thomas and St. John but highly correlated
# #url_sj <- paste0(beg, terminal_year, "-12-01T12:00:00Z)][(0.0):1:(0.0)][(18.37):1:(18.20)][(-64.8):1:(-64.66)]")
# 
# # automated downloads ------------------
# 
# download.file(url = url_pr, destfile = "test_pr.csv")
# download.file(url = url_st, destfile = "test_st.csv")
# #download.file(url = url_sj, destfile = "test_sj.csv")
# download.file(url = url_sc, destfile = "test_sc.csv")
# 
# # read files ----------------------------
# kdlabs <- read.table("test_pr.csv", sep = ",", header = T, skip = 0)
# kd_pr <- read.table("test_pr.csv", sep = ",", header = T, skip = 1)
# kd_st <- read.table("test_st.csv", sep = ",", header = T, skip = 1)
# #kd_sj <- read.table("test_sj.csv", sep = ",", header = T, skip = 1)
# kd_sc <- read.table("test_sc.csv", sep = ",", header = T, skip = 1)
# 
# # remove .csv files ---------------------
# file.remove("test_pr.csv")
# file.remove("test_st.csv")
# #file.remove("test_sj.csv")
# file.remove("test_sc.csv")
# 
# # assign labels to data frames ----------
# names(kd_pr) <- names(kdlabs)
# names(kd_st) <- names(kdlabs)
# #names(kd_sj) <- names(kdlabs)
# names(kd_sc) <- names(kdlabs)
# 
# head(kd_pr)
# head(kd_st)
# #head(kd_sj)
# head(kd_sc)
# 
# # calculate means -----------------------
# ind_pr <- tapply(kd_pr$kd_490, kd_pr$time, mean, na.rm = T)
# ind_st <- tapply(kd_st$kd_490, kd_st$time, mean, na.rm = T)
# #ind_sj <- tapply(kd_sj$kd_490, kd_sj$time, mean, na.rm = T)
# ind_sc <- tapply(kd_sc$kd_490, kd_sc$time, mean, na.rm = T)
# 
# # check names are the same --------------
# summary(names(ind_pr) == names(ind_st))
# #summary(names(ind_pr) == names(ind_sj))
# summary(names(ind_pr) == names(ind_sc))
# 
# #uli <- tapply(kd$kd_490, kd$time, function(x) quantile(x, probs = c(0.95), na.rm = T))
# #lli <- tapply(kd$kd_490, kd$time, function(x) quantile(x, probs = c(0.05), na.rm = T))
# 
# # create indicator object --------------
# 
# datdata <- strftime(names(ind_pr), format = "%m-%Y")
# inddata <- data.frame(cbind(ind_pr, ind_st, ind_sc), row.names = datdata)
# labs <- c(rep("Turbidity from ocean color data", 3), rep("Diffuse attenuation coefficient at 490 nm (kd^2)", 3), 
#           "Puerto Rico", "St. Thomas", "St. Croix")
# indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
# 
# ind <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
# class(ind) <- "indicatordata"
# 
# # save and plot ---------------------------------------
# 
# save(ind, file = "indicator_objects/turbidity.RData")
# 
# plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, trendAnalysis = F, widadj = 1.5)
# plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, trendAnalysis = F, widadj = 1.5, anom = "mon")
# 


