# code for calculating total landings or revenues
# uses logbook data for PR and USVI 

rm(list = ls())

# input data for Puerto Rico ---------------------------
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")
dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20_wSC_2005cor.csv")

table(dat$YEAR_LANDED)

# check multiplier adjustments -------------------------

summary(dat$xADJ == dat$POUNDS_LANDED * 1/dat$CORRECTION_FACTOR)

# define start and end years ---------------------------
styear <- 1990
enyear <- 2020

d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]

# check field codes -----------------

table(d$YEAR_LANDED, useNA = "always")
table(d$CORRECTION_FACTOR, d$YEAR_LANDED)

# look at top landings -----------------------

sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_COMMON_NAME, sum, na.rm = T))
sort(table(d$ITIS_COMMON_NAME[grep("LOBSTER", d$ITIS_COMMON_NAME)]))
sort(table(d$ITIS_COMMON_NAME[grep("CONCH", d$ITIS_COMMON_NAME)]))

d$sppgrp <- "other"
d$sppgrp[grep("LOBSTER", d$ITIS_COMMON_NAME)] <- "lobster"
#d$sppgrp[grep("LOBSTERS,SPINY", d$ITIS_COMMON_NAME)] <- "lobster"  # to compare directly to only spiny lobster
d$sppgrp[grep("CONCH", d$ITIS_COMMON_NAME)] <- "conch"

table(d$ITIS_COMMON_NAME, d$sppgrp)

# check for rule of 3 ------------------------

table(d$sppgrp, d$YEAR_LANDED)
table(table(d$sppgrp, d$YEAR_LANDED) < 3)

# sum landings by  year ----------------------

totland_pr <- tapply(d$ADJUSTED_POUNDS, list(d$YEAR_LANDED, d$sppgrp), sum, na.rm = T) / 10^3
dim(totland_pr)
totland_pr
matplot(1990:2020, totland_pr, type = "l", lty = 1, lwd = 2)

rm(list = ls()[-match(c("totland_pr", "styear", "enyear"), ls())])

# calculate for STT --------------------------------------

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STT_landings.csv")
head(dat)

# take a look at data fields ----------------------------

table(dat$TRIP_YEAR)
d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]

# check field codes -----------------

table(d$TRIP_YEAR, useNA = "always")

# look at top landings -----------------------

sort(tapply(d$POUNDS_LANDED, d$SPECIES_NM, sum, na.rm = T))
sort(table(d$SPECIES_NM[grep("LOBSTER", d$SPECIES_NM)]))
sort(table(d$SPECIES_NM[grep("CONCH", d$SPECIES_NM)]))

d$sppgrp <- "other"
d$sppgrp[grep("LOBSTER", d$SPECIES_NM)] <- "lobster"
d$sppgrp[grep("CONCH", d$SPECIES_NM)] <- "conch"

table(d$SPECIES_NM, d$sppgrp)

# check for rule of 3 ------------------------

table(d$TRIP_YEAR, d$sppgrp)
table(table(d$TRIP_YEAR, d$sppgrp) <= 3)

# sum landings by  year ----------------------

totland_st <- tapply(d$POUNDS_LANDED, list(d$TRIP_YEAR, d$sppgrp), sum, na.rm = T) / 10^3
totland_st[3, 1] <- NA  # fix confidentiality issue
dim(totland_st)
totland_st
totland_st[is.na(totland_st)] <- 0
matplot(totland_st, type = "l")

# calculate for STX --------------------------------------

rm(list = ls()[-match(c("totland_st", "totland_pr", "styear", "enyear"), ls())])

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STX_072011_present_LANDINGS_trip_2021-03-11.csv")
head(dat)

# take a look at data fields ----------------------------

table(dat$LANDING_AREA_NAME)
table(dat$TRIP_YEAR)
d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
d$TRIP_YEAR <- factor(d$TRIP_YEAR, levels = c(styear: enyear))
table(d$TRIP_YEAR)

# check field codes -----------------

table(d$TRIP_YEAR, useNA = "always")

# look at top landings -----------------------

sort(tapply(d$POUNDS_LANDED, d$SPECIES_NM, sum, na.rm = T))
sort(table(d$SPECIES_NM[grep("LOBSTER", d$SPECIES_NM)]))
sort(table(d$SPECIES_NM[grep("CONCH", d$SPECIES_NM)]))

d$sppgrp <- "other"
d$sppgrp[grep("LOBSTER", d$SPECIES_NM)] <- "lobster"
d$sppgrp[grep("CONCH", d$SPECIES_NM)] <- "conch"

table(d$SPECIES_NM, d$sppgrp)

# check for rule of 3 ------------------------

table(d$TRIP_YEAR, d$sppgrp)
table(table(d$TRIP_YEAR, d$sppgrp) <= 3)

# sum landings by  year ----------------------

totland_sx <- tapply(d$POUNDS_LANDED, list(d$TRIP_YEAR, d$sppgrp), sum, na.rm = T) / 10^3
dim(totland_sx)
totland_sx
#totland_sx[is.na(totland_sx)] <- 0
matplot(totland_sx, type = "l")

# summarize and plot ---------------------

ls()[grep("totland", ls())]

datdata <- styear:enyear
inddata <- data.frame(cbind(totland_pr[, 2], totland_st[, 2], totland_sx[, 2], 
                            totland_pr[, 1], totland_st[, 1], totland_sx[, 1],
                            totland_pr[, 3], totland_st[, 3], totland_sx[, 3]))
labs <- c("Lobster landings", "thousands of pounds", "Puerto Rico", 
          "Lobster landings", "thousands of pounds", "St. Thomas and St. John", 
          "Lobster landings", "thousands of pounds", "St. Croix", 
          "Conch landings", "thousands of pounds", "Puerto Rico",
          "Conch landings", "thousands of pounds", "St. Thomas and St. John",
          "Conch landings", "thousands of pounds", "St. Croix", 
          "Landings of all other species", "thousands of pounds", "Puerto Rico", 
          "Landings of all other species",  "thousands of pounds", "St. Thomas and St. John", 
          "Landings of all other species",  "thousands of pounds", "St. Croix")

indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_plots/")

plotIndicatorTimeSeries(s, coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, sublabel = T, sameYscale = F, 
                        widadj = 0.8, hgtadj = 0.7, trendAnalysis = F)   # outtype = "png")

#inddata <- s
#save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/landings.RData")


