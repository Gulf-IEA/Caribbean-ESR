

url_pr <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.csv?kd_490[(2012-01-02T12:00:00Z):1:(2021-12-01T12:00:00Z)][(0.0):1:(0.0)][(18.7):1:(17.7)][(-67.5):1:(-65.2)]"
url_st <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.csv?kd_490[(2012-01-02T12:00:00Z):1:(2021-12-01T12:00:00Z)][(0.0):1:(0.0)][(18.41):1:(18.14)][(-65.12):1:(-64.79)]"
url_sj <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.csv?kd_490[(2012-01-02T12:00:00Z):1:(2021-12-01T12:00:00Z)][(0.0):1:(0.0)][(18.37):1:(18.20)][(-64.8):1:(-64.66)]"
url_sc <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQkd490Monthly.csv?kd_490[(2012-01-02T12:00:00Z):1:(2021-12-01T12:00:00Z)][(0.0):1:(0.0)][(17.86):1:(17.59)][(-64.94):1:(-64.41)]"

download.file(url = url_pr, destfile = "test_pr.csv")
download.file(url = url_st, destfile = "test_st.csv")
download.file(url = url_sj, destfile = "test_sj.csv")
download.file(url = url_sc, destfile = "test_sc.csv")

kdlabs <- read.table("test_pr.csv", sep = ",", header = T, skip = 0)
kd_pr <- read.table("test_pr.csv", sep = ",", header = T, skip = 1)
kd_st <- read.table("test_st.csv", sep = ",", header = T, skip = 1)
kd_sj <- read.table("test_sj.csv", sep = ",", header = T, skip = 1)
kd_sc <- read.table("test_sc.csv", sep = ",", header = T, skip = 1)

names(kd_pr) <- names(kdlabs)
names(kd_st) <- names(kdlabs)
names(kd_sj) <- names(kdlabs)
names(kd_sc) <- names(kdlabs)

head(kd_pr)
head(kd_st)
head(kd_sj)
head(kd_sc)

ind_pr <- tapply(kd_pr$kd_490, kd_pr$time, mean, na.rm = T)
ind_st <- tapply(kd_st$kd_490, kd_st$time, mean, na.rm = T)
ind_sj <- tapply(kd_sj$kd_490, kd_sj$time, mean, na.rm = T)
ind_sc <- tapply(kd_sc$kd_490, kd_sc$time, mean, na.rm = T)

summary(names(ind_pr) == names(ind_st))
summary(names(ind_pr) == names(ind_sj))
summary(names(ind_pr) == names(ind_sc))

#uli <- tapply(kd$kd_490, kd$time, function(x) quantile(x, probs = c(0.95), na.rm = T))
#lli <- tapply(kd$kd_490, kd$time, function(x) quantile(x, probs = c(0.05), na.rm = T))

datdata <- strftime(names(ind_pr), format = "%m-%Y")
inddata <- data.frame(cbind(ind_pr, ind_st, ind_sj, ind_sc), row.names = datdata)
labs <- c(rep("Turbidity from ocean color data", 4), rep("Diffuse attenuation coefficient at 490 nm (kd^2)", 4), 
          "Puerto Rico", "St. Thomas", "St. John", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1:4, dateformat = "%m-%Y", sublabel = T, trendAnalysis = T, widadj = 1.5, outtype = "png")

