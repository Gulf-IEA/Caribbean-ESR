# code for calculating Gini coefficient based on landings or revenues
# uses logbook data for PR and USVI 

rm(list = ls())

# Gini function ----------------------------------------

calcGini <- function(vec) {
  v <- sort(vec)
  N <- length(vec)
  gini <- sum(sapply(1:N, function(x) ((2 * x - N - 1) * v[x]))) / (N^2 * mean(vec))
  return(gini)
}

# specification file and libraries -----------------------------

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/MOST_RECENT/"

# define start and end years ---------------------------
styear <- 2010
enyear <- 2022

# input data for Puerto Rico ---------------------------

dat <- read.csv(paste0(confpath, "wrkeithly_pr_com_data_2000_2022_20240501_C.csv"))

d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]

# check field codes related to permits -----------------

table(d$YEAR_LANDED, useNA = "always")
table(d$PR_ID_CODE_ED, useNA = "always")
table(d$PR_ID_CODE_ED, d$YEAR_LANDED)

# remove bad price values ------------------------------
#Notes from Juan: 
# individual catch/revenues assignments to a single vessel are likely to be better after 2012 when reporting improved 
# If you are using PR data (another) caveat you  may want to delete those observations with prices higher than say $12-15
# per pound, except those prices for land crabs ("jueyes"), which can fetch up $60 per dozen.

hist(d$PRICE_PER_LB)
sort(table(d$ITIS_COMMON_NAME[which(d$PRICE_PER_LB > 15)]))
length(which(d$ITIS_COMMON_NAME != "CRAB,BLUE LAND" & d$PRICE_PER_LB > 15))
d[(which(d$ITIS_COMMON_NAME != "CRAB,BLUE LAND" & d$PRICE_PER_LB > 15)), ]
table(d$ITIS_COMMON_NAME[(which(d$ITIS_COMMON_NAME != "CRAB,BLUE LAND" & d$PRICE_PER_LB > 15))])
hist(d$PRICE_PER_LB[(which(d$ITIS_COMMON_NAME != "CRAB,BLUE LAND" & d$PRICE_PER_LB > 15))])

d$PRICE_PER_LB[(which(d$ITIS_COMMON_NAME != "CRAB,BLUE LAND" & d$PRICE_PER_LB > 15))] <- NA
hist(d$PRICE_PER_LB[(which(d$ITIS_COMMON_NAME == "CRAB,BLUE LAND"))])
max(d$PRICE_PER_LB[(which(d$ITIS_COMMON_NAME == "CRAB,BLUE LAND"))])

# calculate revenue, sum by permit and year ------------

#d$REV <- d$POUNDS_LANDED * d$PRICE_PER_LB
d$REV <- d$ADJUSTED_POUNDS * d$PRICE_PER_LB
totrev <- tapply(d$REV, list(d$PR_ID_CODE_ED, d$YEAR_LANDED), sum, na.rm = T)
dim(totrev)
totrev[is.na(totrev)] <- 0
totrev
rowSums(totrev, na.rm = T)
which(rowSums(totrev, na.rm = T) == 0)
totrev <- totrev[-which(rowSums(totrev, na.rm = T) == 0), ]
dim(totrev)

# sum landings by permit and year ----------------------

totland <- tapply(d$ADJUSTED_POUNDS, list(d$PR_ID_CODE_ED, d$YEAR_LANDED), sum, na.rm = T)
dim(totland)
totland[is.na(totland)] <- 0
totland
rowSums(totland, na.rm = T)
which(rowSums(totland, na.rm = T) == 0)
#totland <- totland[-which(rowSums(totland, na.rm = T) == 0), ]
dim(totland)

# calculate gini index --------------------------------

par(mfrow = c(2, 1), mar = c(3, 5, 1, 1))
gini_rev_pr <- apply(totrev, 2, calcGini)
plot(names(gini_rev_pr), gini_rev_pr, type = "b")

gini_land_pr <- apply(totland, 2, calcGini)
plot(names(gini_land_pr), gini_land_pr, type = "b", col = 2)

rm(list = ls()[-match(c("gini_land_pr", "gini_rev_pr", "styear", "enyear", "calcGini", "confpath"), ls())])

######################  END PR  ##############################

# calculate for STT  --------------------------------------

dat <- read.csv(paste0(confpath, "STT_2024.csv"))

table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
d$TRIP_YEAR <- factor(d$TRIP_YEAR, levels = c(styear: enyear))
table(d$TRIP_YEAR)


# take a look at data fields ----------------------------

table(d$TRIP_ID, useNA = "always")
table(d$TRIP_YEAR, useNA = "always")
table(d$VESSEL_CD, useNA = "always")
table(d$FISHER_PERMIT, useNA = "always")
table(d$FISHER_FIRST_NAME, useNA = "always")
table(d$FISHER_LAST_NAME, useNA = "always")
table(d$CHARTER_TRIP, useNA = "always")

# remove bad price values ------------------------------

hist(d$PRICE)
which(d$PRICE > 15)
d$SPECIES_NM[which(d$PRICE > 15)]
hist(d$PRICE[which(d$PRICE > 15)])
d$PRICE[which(d$PRICE > 15)] <- NA

# calculate revenue, sum by permit and year ------------

d$REV <- d$POUNDS_LANDED * d$PRICE
totrev <- tapply(d$REV, list(d$FISHER_PERMIT, d$TRIP_YEAR), sum, na.rm = T)
dim(totrev)
totrev[is.na(totrev)] <- 0
totrev
rowSums(totrev, na.rm = T)
which(rowSums(totrev, na.rm = T) == 0)
totrev <- totrev[-which(rowSums(totrev, na.rm = T) == 0), ]
dim(totrev)

# sum landings by permit and year ----------------------

totland <- tapply(d$POUNDS_LANDED, list(d$FISHER_PERMIT, d$TRIP_YEAR), sum, na.rm = T)
dim(totland)
totland[is.na(totland)] <- 0
totland
rowSums(totland, na.rm = T)
which(rowSums(totland, na.rm = T) == 0)
#totland <- totland[-which(rowSums(totland, na.rm = T) == 0), ]
dim(totland)

# calculate gini index --------------------------------

gini_rev_stt <- apply(totrev, 2, calcGini)
plot(names(gini_rev_stt), gini_rev_stt, type = "b")

gini_land_stt <- apply(totland, 2, calcGini)
plot(names(gini_land_stt), gini_land_stt, col = 2)

plot(gini_land_stt, gini_rev_stt)
cor(gini_land_stt, gini_rev_stt)

###########################  END STT  ############################

rm(list = ls()[-match(c("gini_land_pr", "gini_rev_pr", "gini_land_stt", "gini_rev_stt", "styear", "enyear", "calcGini", "confpath"), ls())])

# calculate for STX  --------------------------------------

dat <- read.csv(paste0(confpath, "STX_2024.csv"))

table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
d$TRIP_YEAR <- factor(d$TRIP_YEAR, levels = c(styear: enyear))
table(d$TRIP_YEAR)
head(d)

# take a look at data fields ----------------------------

table(d$TRIP_ID, useNA = "always")
table(d$TRIP_YEAR, useNA = "always")
table(d$VESSEL_CD, useNA = "always")
table(d$FISHER_PERMIT, useNA = "always")
table(d$FISHER_FIRST_NAME, useNA = "always")
table(d$FISHER_LAST_NAME, useNA = "always")
table(d$CHARTER_TRIP, useNA = "always")

#d <- d[which(d$CHARTER_TRIP == "Y"), ]

# remove bad price values ------------------------------

hist(d$PRICE)
which(d$PRICE > 15)

# calculate revenue, sum by permit and year ------------

d$REV <- d$POUNDS_LANDED * d$PRICE
totrev <- tapply(d$REV, list(d$FISHER_PERMIT, d$TRIP_YEAR), sum, na.rm = T)
dim(totrev)
totrev[is.na(totrev)] <- 0
totrev
rowSums(totrev, na.rm = T)
which(rowSums(totrev, na.rm = T) == 0)
#totrev <- totrev[-which(rowSums(totrev, na.rm = T) == 0), ]
dim(totrev)

# sum landings by permit and year ----------------------

totland <- tapply(d$POUNDS_LANDED, list(d$FISHER_PERMIT, d$TRIP_YEAR), sum, na.rm = T)
dim(totland)
totland[is.na(totland)] <- 0
totland
rowSums(totland, na.rm = T)
which(rowSums(totland, na.rm = T) == 0)
#totland <- totland[-which(rowSums(totland, na.rm = T) == 0), ]
dim(totland)

dev.off()

# calculate gini index --------------------------------

par(mfrow = c(2, 1))
gini_rev_stx <- apply(totrev, 2, calcGini)
plot(names(gini_rev_stx), gini_rev_stx, type = "b")

gini_land_stx <- apply(totland, 2, calcGini)
plot(names(gini_land_stx), gini_land_stx, col = 2, type = "b")

plot(gini_land_stx, gini_rev_stx)
cor(gini_land_stx, gini_rev_stx)

###########################  END STX  ############################

# format indicator object -----------------------------

ls()[grep("gini", ls())]

datdata <- styear:enyear
inddata <- data.frame(gini_rev_pr, gini_rev_stt, gini_land_stx)
labs <- c("Inequality in revenues" , "Gini index", "Puerto Rico", 
          "Inequality in revenues" , "Gini index", "St. Thomas and St. John",
          "Inequality in revenues" , "Gini index", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

ind <- s 

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = T, 
                        widadj = 1.3, hgtadj = 1, trendAnalysis = T)

save(ind, file = "indicator_objects/gini.RData")


# hurricane Maria - 2017 - STX and PR
# hurricane Irma - 2017 - STT
