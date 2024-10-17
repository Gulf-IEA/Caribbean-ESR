##########################################################################
# Ocean acificiation indicator
# Sent by F. Gomez on 02/19/2023
# Notes from Fabian: 
#
# derived surface omega series from the MOM-Topaz hindcast 0.10deg resolution model
# *txt files have this structure: column1: year, column2=month, and column3=carbon system variable
# The mean magnitude of the simulated omega trends, around 9e-3 year-1, is consistent with observed trends in the Subtropical North Atlantic 
# 
# Question: What is causing the acceleration after 2008?  
# Answer: made a Taylor decomposition to figure out what was driving that acceleration in the simulated ??Ar anomaly. 
# Interannual ??Ar changes were mostly driven by the balance between dic and alkalinity. 
# There was a positive trend from the 1980s until mid 2000s, which contributed to moderate the ??Ar decline. 
# But that trend vanished in the last 15 years or so, which probably has to do with this accelerated decline in ??Ar in the last decade. 
##########################################################################

rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

dat <- read.table("indicator_data/inputsToBeUpdatedAnnually/surface_omega_series.txt", skip = 0, header = F)

head(dat)
tail(dat)

dat$dates <- paste0(month.abb[dat$V2], dat$V1)

# format indicator object -----------------------------

datdata <- dat$dates
inddata <- data.frame(dat$V3)
labs <- c("Ocean acidification", "Surface aragonite saturation", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

# save and plot -----------------------------------------
plotIndicatorTimeSeries(s)

ind <- s

save(ind, file = "indicator_objects/OA.RData")


print("OA -- SUCCESSFULLY RUN")

