# Number of amendments / regulations indicator

# Last updated 6/19/2024 by Carissa Gervasi

# rm(list = ls())
# dev.off()
# 
# library(dplyr)
# 
# dat = read.csv("indicator_data/intermediateFiles/regulations.csv")
# 
# 
# head(dat)
# 
# dat$year2 = as.numeric(dat$year2)
# dat$amendments = as.numeric(dat$amendments)
# 
# styear = min(dat$year2)
# enyear = max(dat$year2)
# 
# 
# # Create a dataframe with the full date range 
# full_dates <- data.frame(
#   year2 = styear:enyear)
# 
# # Merge the two dataframes, keeping all dates and filling NAs where there are no matches
# dat <- full_dates %>%
#   left_join(dat, by = "year2")
# 
# 
# 
# # save as indicator object ----------------------
# datdata <- styear:enyear
# inddata <- data.frame(dat$amendments)
# labs <- c("Number of new regulations in effect" , "Sum of regulations", "")
# indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
# inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
# class(inddata) <- "indicatordata"
# 
# # plot and save ----------------------------------
# 
# ind <- inddata
# plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1)
# 
# save(ind, file = "indicator_objects/regulations.RData")
# 
# 



###############################  using FR_Section



rm(list = ls())

plot.new()
dev.off()

library(dplyr)

dat = read.csv("indicator_data/inputsToBeUpdatedAnnually/FRsection.csv")


head(dat)

dat$year2 = as.numeric(dat$year2)
dat$amendments = as.numeric(dat$amendments)

styear = min(dat$year2)
enyear = max(dat$year2)


# Create a dataframe with the full date range 
full_dates <- data.frame(
  year2 = styear:enyear)

# Merge the two dataframes, keeping all dates and filling NAs where there are no matches
dat <- full_dates %>%
  left_join(dat, by = "year2")



# save as indicator object ----------------------
datdata <- styear:enyear
inddata <- data.frame(dat$amendments)
labs <- c("Number of new management actions" , "Sum of regulations", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1, type = "allLines")

save(ind, file = "indicator_objects/FRsection.RData")


print("regulations -- SUCCESSFULLY RUN")

