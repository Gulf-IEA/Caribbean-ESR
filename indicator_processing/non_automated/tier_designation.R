# Code to create the governance indicator for the ABC control rule tier of each stock/complex

# The indicator is the percent of species in the FMPs that are managed under Tier 3 control rules. 

# All stocks/complexes have been Tier 4 up until 2023, when spiny lobster were designated as Tier 3.

rm(list = ls())

plot.new()
dev.off()

# Tally number of stocks from Maria's table:
PR_stocks = 34
STTSTJ_stocks = 20
STX_stocks = 20

PR_perc = (1/PR_stocks)*100
USVI_perc = (1/STX_stocks)*100

PR = c(0,0,0,0,0,0,0,0,0,0,0,0,PR_perc)
USVI = c(0,0,0,0,0,0,0,0,0,0,0,0,USVI_perc)

# save as indicator object ----------------------
datdata <- 2011:2023
inddata <- data.frame(cbind(PR, USVI))
labs <- c("Stocks/complexes with ACLs derived from stock assessments" , "Percent", "Puerto Rico",
          "Stocks/complexes with ACLs derived from stock assessments" , "Percent", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
ind <- inddata
plotIndicatorTimeSeries(ind, plotrownum = 2, coltoplot = 1:2, sublabel = TRUE, dateformat = "%Y%b", trendAnalysis = T)

save(ind, file = "indicator_objects/tier3.RData")

print("tier1 stocks -- SUCCESSFULLY RUN")
