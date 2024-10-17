
rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)

# load data -------------------------------------

d <- read.csv("indicator_data/inputsToBeUpdatedAnnually/sargassum_innundation_monthly_mean_hu.csv", skip = 2)

d$yr <- substr(d$X, 1, 4)
d$vars <- (d$ulim - d$X.1)^2

tab <- tapply(d$X.1, d$yr, mean, na.rm = T)
sds <- (tapply(d$vars, d$yr, mean))^0.5

# save as indicator object ----------------------
datdata <- 2011:2021
inddats <- data.frame(cbind(tab))
ulidata <- data.frame(cbind(tab + sds))
llidata <- data.frame(cbind(tab - sds))
labs <- c("Annual mean sargassum innundation", "Area (km^2)", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))

inddata <- list(labels = indnames, indicators = inddats, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

plotIndicatorTimeSeries(inddata)

ind <- inddata

save(ind, file = "indicator_objects/Sargassum.RData")


print("sargassum -- SUCCESSFULLY RUN")

# #################    INCOMPLETE  ########################
# # need to look up algorithm to convert AFAI units to Area of inundation 
# # check Chuanmin write-up and find methods
# #
# # Sargassum inundation indicator
# #
# # direct download from ERDDAP
# # https://cwcgom.aoml.noaa.gov/erddap/griddap/noaa_aoml_atlantic_oceanwatch_AFAI_7D.graph
# 
# # specification file and libraries -----------------------------
# 
# rm(list = ls())
# dev.off()
# 
# library(lubridate)
# library(maps)
# library(plotTimeSeries)
# library(rerddap)
# 
# load("indicator_processing/spec_file.RData")
# 
# # define years  --------------------------------
# styear <- 2012
# enyear <- terminal_year
# 
# # get ERDDAP info  --------------------------------
# afai <- info('noaa_aoml_atlantic_oceanwatch_AFAI_7D',
#              url = 'https://cwcgom.aoml.noaa.gov/erddap')
# afai
# 
# # download data --------------------
# 
# yr1 <- as.numeric(substr(afai$alldata$NC_GLOBAL$value[35], 1, 4)) + 1
# yr2 <- as.numeric(substr(afai$alldata$NC_GLOBAL$value[34], 1, 4)) - 1
# yr1
# yr2
# 
# res <- griddap(afai, fields = 'AFAI', 
#                time = c(paste0(yr1, '-01-01'), paste0(yr2, '-12-31')), 
#                longitude = c(min_lon, max_lon), 
#                latitude = c(min_lat, max_lat))
# 
# hist(res$data$AFAI)
# quantile(res$data$AFAI, na.rm = T, probs = 0.95)
# 
# res$data$lim <- 0
# res$data$lim[which(res$data$AFAI > 0.00063)] <- 1
# 
# res$data$year <- substr(res$data$time, 1, 4)
# res$data$tim <- substr(res$data$time, 1, 7)
# 
# head(res$data)
# 
# mon_afai <- tapply(res$data$lim, res$data$tim, sum, na.rm = T)
# yr_afai <- tapply(res$data$lim, res$data$year, sum, na.rm = T)
# 
# plot(mon_afai, type = "l")
# plot(2017:2023, yr_afai, type = "l")
# 
# # create indicator object --------------
# 
# 

