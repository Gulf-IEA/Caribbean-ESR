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

# input data for Puerto Rico ---------------------------
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")
dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20.csv")

# define start and end years ---------------------------
styear <- 2012
enyear <- 2020

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
table(d$ITIS_COMMON_NAME[which(d$PRICE_PER_LB > 15)])
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

# calculate for STT and STX --------------------------------------

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STT_landings.csv")
d <- dat[which(dat$TRIP_YEAR >= 2012 & dat$TRIP_YEAR <= 2020), ]

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

gini_rev_vi <- apply(totrev, 2, calcGini)
plot(names(gini_rev_vi), gini_rev_vi, type = "b", ylim = c(0.7, 1))

gini_land_vi <- apply(totland, 2, calcGini)
lines(names(gini_land_vi), gini_land_vi, col = 2)

# format indicator object -----------------------------

ls()[grep("gini", ls())]

datdata <- styear:enyear
inddata <- data.frame(gini_rev_pr, gini_rev_vi)
labs <- c("Inequality in revenues" , "Gini index", "Puerto Rico", 
          "Inequality in revenues" , "Gini index", "St. Thomas and St. John")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_plots/")
plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T, sameYscale = T, 
                        widadj = 1.3, hgtadj = 1, outtype = "png")

inddata <- s
save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/gini.RData")



