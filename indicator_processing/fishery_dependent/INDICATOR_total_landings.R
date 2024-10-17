# M. Karnauskas 10/19/2023
# code for calculating total landings or revenues
# uses logbook data for PR and USVI 

# specification file and libraries -----------------------------
rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/MOST_RECENT/"

# define start and end years ---------------------------
styear <- 2000
enyear <- 2022

# input data for Puerto Rico ---------------------------

dat <- read.csv(paste0(confpath, "wrkeithly_pr_com_data_2000_2022_20240625_C.csv"))

table(dat$YEAR_LANDED)

# check multiplier adjustments -------------------------

dat$xADJ <- dat$POUNDS_LANDED * 1/dat$CORRECTION_FACTOR
summary(dat$xADJ == dat$POUNDS_LANDED * 1/dat$CORRECTION_FACTOR)
hist(dat$xADJ - (dat$POUNDS_LANDED * 1/dat$CORRECTION_FACTOR))
max(abs(dat$xADJ - (dat$POUNDS_LANDED * 1/dat$CORRECTION_FACTOR)), na.rm = T)

# subset years------------------------------------------
d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]

# check field codes ---------------------------------

table(d$YEAR_LANDED, useNA = "always")
table(d$CORRECTION_FACTOR, d$YEAR_LANDED)

# remove land crab trips -------------------

sort(table(d$ITIS_COMMON_NAME[grep("CRAB", d$ITIS_COMMON_NAME)]))
sort(table(d$ERDMAN_GEAR_NAME[grep("CRAB,BLUE", d$ITIS_COMMON_NAME)]))
sort(table(d$FAMILY[grep("CRAB,BLUE", d$ITIS_COMMON_NAME)]))
sort(table(d$ERDMAN_GEAR_NAME[which(d$FAMILY == "LAND CRABS")]))
par(mar = c(12, 4, 1, 1))
barplot(sort(table(d$ERDMAN_GEAR_NAME[which(d$FAMILY == "LAND CRABS")])), las = 2)
sort(table(d$ITIS_COMMON_NAME[which(d$ERDMAN_GEAR_NAME == "BY HAND")]))  # don't remove, contains conch as well
# filtering by LAND CRAB TRAP gear takes out vast majority (96%) of land crab trips

d[which(d$ERDMAN_GEAR_NAME == "LAND CRAB TRAP"), ]
dim(d)
d <- d[which(d$ERDMAN_GEAR_NAME != "LAND CRAB TRAP"), ]
dim(d)

# look at top landings -------------------------------

sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_COMMON_NAME, sum, na.rm = T))
sort(table(d$ITIS_COMMON_NAME[grep("LOBSTER", d$ITIS_COMMON_NAME)]))
sort(table(d$ITIS_COMMON_NAME[grep("CONCH", d$ITIS_COMMON_NAME)]))

sort(table(d$ITIS_COMMON_NAME[grep("DOLPHIN", d$ITIS_COMMON_NAME)]))

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
matplot(styear:enyear, totland_pr, type = "l", lty = 1, lwd = 2)

rm(list = ls()[-match(c("totland_pr", "styear", "enyear", "confpath"), ls())])

#################     END PR    ########################

# calculate for STT --------------------------------------

dat <- read.csv(paste0(confpath, "STT_2024.csv"))

head(dat)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)
tab <- table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# take out incomplete years -----------------------------

table(dat$TRIP_YEAR)
lis <- as.numeric(names(which(apply(tab, 1, min) == 0)))
lis
dat <- dat[!(dat$TRIP_YEAR %in% lis), ]
table(dat$TRIP_YEAR)

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
table(table(d$TRIP_YEAR, d$sppgrp) <= 3 & table(d$TRIP_YEAR, d$sppgrp) > 0)

# sum landings by  year ----------------------

totland_st <- tapply(d$POUNDS_LANDED, list(d$TRIP_YEAR, d$sppgrp), sum, na.rm = T) / 10^3
dim(totland_st)
totland_st
totland_st[21, 1] <- NA  # fix confidentiality issue
totland_st
#totland_st[is.na(totland_st)] <- 0
matplot(totland_st, type = "l")

#################     END STT    ########################

# calculate for STX --------------------------------------

rm(list = ls()[-match(c("totland_st", "totland_pr", "styear", "enyear", "confpath"), ls())])

dat <- read.csv(paste0(confpath, "STX_2024.csv"))
head(dat)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)
tab <- table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# take out incomplete years -----------------------------

table(dat$TRIP_YEAR)
lis <- as.numeric(names(which(apply(tab, 1, min) == 0)))
lis
dat <- dat[!(dat$TRIP_YEAR %in% lis), ]
table(dat$TRIP_YEAR)

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
table(table(d$TRIP_YEAR, d$sppgrp) <= 3 & table(d$TRIP_YEAR, d$sppgrp) > 0)

# sum landings by  year ----------------------

totland_sx <- tapply(d$POUNDS_LANDED, list(d$TRIP_YEAR, d$sppgrp), sum, na.rm = T) / 10^3
dim(totland_sx)
totland_sx
#totland_sx[is.na(totland_sx)] <- 0
matplot(totland_sx, type = "l")

#################     END STX    ########################


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

ind <- s

plotIndicatorTimeSeries(ind, coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, sublabel = T, sameYscale = F, 
                        widadj = 0.8, hgtadj = 0.7, trendAnalysis = T) #, outtype = "png")

save(ind, file = "indicator_objects/total_landings.RData")

#####################  END   ##########################

print("total landings -- SUCCESSFULLY RUN")

