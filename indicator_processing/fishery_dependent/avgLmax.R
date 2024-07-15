## Average Lmax per year by island

rm(list = ls())
dev.off()

library(plotTimeSeries)
library(dplyr)

load("indicator_data/fish-dep-indicators/Lmax_PR.RData")
PR_dat = as.data.frame(findat)

load("indicator_data/fish-dep-indicators/Lmax_STT.RData")
STT_dat = as.data.frame(findat)

load("indicator_data/fish-dep-indicators/Lmax_STX.RData")
STX_dat = as.data.frame(findat)


styear = min(PR_dat$V1, STT_dat$yrs, STX_dat$yrs)
enyear = max(PR_dat$V1, STT_dat$yrs, STX_dat$yrs)


yrs = styear:enyear
PR = PR_dat$lmax

STT_dat2 = data.frame(yrs = yrs)
STT_dat2 = STT_dat2 %>% 
  left_join(STT_dat, by = "yrs")

STT = STT_dat2$lmax


STX_dat2 = data.frame(yrs = yrs)
STX_dat2 = STX_dat2 %>% 
  left_join(STX_dat, by = "yrs")

STX = STX_dat2$lmax




# save as indicator object ----------------------
datdata <- yrs
inddata <- data.frame(cbind(PR, STT, STX))
labs <- c("Average Lmax" , "length (cm)", "Puerto Rico",
          "Average Lmax" , "length (cm)", "St. Thomas and St. John",
          "Average Lmax" , "length (cm)", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1, sameYscale = TRUE)

save(ind, file = "indicator_objects/avgLmax.RData")

###############################  END  #############################