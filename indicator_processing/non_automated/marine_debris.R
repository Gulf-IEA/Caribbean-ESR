#
# marine debris data from UN SDG TrackerGoal 14: Life below water
# 

# specification file and libraries -----------------------------
rm(list = ls())
dev.off()

library(maps)
library(plotTimeSeries)
library(xml2)
library(rvest)

load("indicator_processing/spec_file.RData")

# download data directly from site -----------------------------

#url <- "https://sdg-tracker.org/ca8b6a47-9731-4fd8-8369-1c69c25485d6"


url <- "https://ourworldindata.org/grapher/beach-litter?tab=table&time=2015..2020&country=~PRI"

# this does not work -- need to fix 
page <- read_html(url) #Creates an html document from URL

table <- html_table(page, fill = TRUE) #Parses tables into data frames
table
# automated download does not work ############

dat <- read.csv("indicator_data/beach-litter.csv", skip = 0, header = T)

d <- dat[which(dat$Entity == "Puerto Rico" | dat$Entity == "United States Virgin Islands"), ]
d$Entity <- droplevels(d$Entity)

tab <- tapply(d$X14.1.1...Beach.litter.per.square.kilometer..Number....EN_MAR_BEALITSQ, list(d$Year, d$Entity), sum)

# format indicator object -----------------------------

datdata <- as.integer(rownames(tab))
inddata <- data.frame(tab/10^6)
labs <- c("Marine debris", "Millions of items per km^2", "Puerto Rico", 
          "Marine debris", "Millions of items per km^2", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

# save and plot -----------------------------------------
plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T)

ind <- s

save(ind, file = "indicator_objects/marine_debris.RData")


