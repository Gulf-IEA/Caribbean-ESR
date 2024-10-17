####################################################################
#
#  processing reconstructed recreational catch from Sea Around Us
#  M. Karnauskas 2/8/2024
#  
#  Advanced data search at: https://www.seaaroundus.org/data/#/search
#  select PR and USVI EEZs, Dimension == Fishing sector, Measure == Tonnage
#  Download data and save locally
# 
####################################################################

# specification file and libraries -----------------------------
rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

styear <- 1995

# load data -------------------------------------
d <- read.csv("indicator_data/inputsToBeUpdatedAnnually/SAU/SAU EEZ 630,850 v50-1.csv")
head(d)

# look at fields ------------------------------

apply(d[, 1:15], 2, table, useNA = "always")
d <- d[which(d$year >= styear), ]

# calculate total rec catch for PR -------------

table(d$fishing_sector, useNA = "always")
drec <- d[which(d$fishing_sector == "Recreational"), ]
dim(drec)
drec

apply(drec, 2, table, useNA = "always")

par(mar = c(15, 5, 1, 1))
barplot(sort(tapply(drec$tonnes, drec$common_name, sum, na.rm = T)), las = 2, horiz = F)

tapply(drec$uncertainty_score, drec$year, mean)
boxplot(drec$uncertainty_score ~ drec$year)

barplot(tapply(drec$tonnes, list(drec$area_name, drec$reporting_status), sum, na.rm = T), beside = T)
barplot(tapply(drec$tonnes, list(drec$area_name, drec$scientific_name), sum, na.rm = T), beside = T, las = 2)
barplot(tapply(drec$tonnes, list(drec$area_name, drec$common_name), sum, na.rm = T), beside = T, las = 2)
barplot(tapply(drec$tonnes, list(drec$area_name, drec$functional_group), sum, na.rm = T), beside = T, las = 2)

tot <- tapply(drec$tonnes, list(drec$year, drec$area_name), sum, na.rm = T) * 2204.62

# save as indicator object ----------------------

datdata <- as.integer(rownames(tot))
inddats <- data.frame(tot/ 10^6)
labs <- c("Total recreational catch", "millions of pounds", "Puerto Rico", 
          "Total recreational catch", "millions of pounds", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

inddata <- list(labels = indnames, indicators = inddats, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"

plotIndicatorTimeSeries(inddata, coltoplot = 1:2, plotrownum = 2, sublabel = T, sameYscale = F)

# plot and save ----------------------------------
ind <- inddata
save(ind, file = "indicator_objects/total_rec_catch.RData")

print("recreational catch -- SUCCESSFULLY RUN")
