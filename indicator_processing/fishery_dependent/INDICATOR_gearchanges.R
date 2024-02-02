####################################################################

# M. Karnauskas 12/22/2023
# code for calculating trip-level statistics, gear changes
# uses logbook data for PR and USVI 

##########  NEED TO CHECK ON TRIP IDENTIFIERS  #####################

rm(list = ls())
library(pals)
library(dplyr)

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STT_landings.csv")

# define start and end years ---------------------------
styear <- 1990
enyear <- 2020
table(dat$TRIP_YEAR)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# subset years------------------------------------------

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
table(d$TRIP_YEAR)

# take a look at data fields ----------------------------

table(d$TRIP_ID, useNA = "always")
table(d$TRIP_YEAR, useNA = "always")
table(d$TRIP_MONTH, useNA = "always")
table(d$TRIP_DAY, useNA = "always")
table(d$VESSEL_CD, useNA = "always")
table(d$FISHER_PERMIT, useNA = "always")
table(d$FISHER_FIRST_NAME, useNA = "always")
table(d$FISHER_LAST_NAME, useNA = "always")
table(d$CHARTER_TRIP, useNA = "always")
table(d$NUM_PARTNERS_OR_HELPERS, useNA = "always")
table(d$IS_CATCH_SPLIT, useNA = "always")
table(d$ISLAND, useNA = "always")
table(d$LANDING_AREA_NAME, useNA = "always")
table(d$AREA_CD1, useNA = "always")
table(d$AREA_CD2, useNA = "always")
table(d$HOURS_FISHED, useNA = "always")
table(d$DEPTH, useNA = "always")
table(d$TOTAL_TRAPS_HAULED, useNA = "always")
table(d$TOTAL_TRAPS_IN_WATER, useNA = "always")
table(d$FAD_CD, useNA = "always")
table(d$GEAR_TYPE_NM, useNA = "always")
table(d$GEAR1_NAME, useNA = "always")
table(d$SPECIES_CD, useNA = "always")
table(d$SPECIES_NM, useNA = "always")
table(d$FORM_TYPE, useNA = "always")
table(d$TRIP_YEAR, d$FORM_TYPE, useNA = "always")
table(d$TRIP_MAY_BE_DUPLICATE, useNA = "always")

# check that TRIP IDs are unique ------------

dim(d)
d <- d[-which(d$TRIP_MAY_BE_DUPLICATE == "Y"), ]
dim(d)
d <- d[-which(d$CHARTER_TRIP == "Y"), ]
dim(d)

d$TRIP_ID <- as.character(d$TRIP_ID)

d$id1 <- paste(d$TRIP_YEAR, d$TRIP_MONTH, d$TRIP_DAY, d$VESSEL_CD, sep = "_")
d$id2 <- paste(d$TRIP_YEAR, d$TRIP_MONTH, d$TRIP_DAY, d$FISHER_PERMIT, sep = "_")
n_distinct(d$id1)
n_distinct(d$id2)
n_distinct(d$TRIP_ID)

n_distinct(d$TRIP_ID) - n_distinct(d$id1)

id2 <- as.numeric(as.factor(d$id1))
n_distinct(id2)

mn <- tapply(id2, d$TRIP_ID, mean)
table(mn - round(mn))

table(tapply(d$TRIP_YEAR, d$TRIP_ID, sd, na.rm = T))

# look at gear names -----------------------------------
table(d$GEAR_NM, d$GEAR_TYPE_NM)
table(d$GEAR_TYPE_NM)

#d[which(d$TRIP_ID == "CCL12380"), ]

# calculate main gear, year, region for each trip ---------------------

tab <- tapply(d$POUNDS_LANDED, list(d$TRIP_ID, d$GEAR_TYPE_NM), sum, na.rm = T)
tabc <- tab
tabc[!is.na(tabc)] <- 1
hist(rowSums(tabc, na.rm = T))
table(rowSums(tabc, na.rm = T))
tab[is.na(tab)] <- 0
co <- apply(tab, 1, which.max)
gears <- colnames(tabc)[co]

head(tab, 20)        # check to see if correct
gears[1:20]

years <- tapply(d$TRIP_YEAR, d$TRIP_ID, mean, na.rm = T)

tab2 <- table(d$TRIP_ID, d$LANDING_AREA_NAME)
co <- apply(tab2, 1, which.max)
areas <- colnames(tab2)[co]

table(rownames(tab) == names(years))
table(rownames(tab) == rownames(tab2))

fin <- data.frame(rownames(tab), years, gears, areas)
names(fin) <- c("id", "year", "gear", "area")
head(fin)

apply(fin, 2, table, useNA = "always")  # check no NAs


# plot results -----------------------

mat <- table(fin$year, fin$gear)
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(tab))

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = 34, y = 0.9), legend.text = colnames(tab))

byc <- rowSums(mat[, c(4, 6)]) / rowSums(mat)   # proportion of trips with non-selective gears
plot(names(byc), byc, type = "l")

par(mfrow = c(2,1), mar = c(5, 4, 2, 1))
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright", bty = "n"), legend.text = colnames(tab), las = 2, 
        main = "Number of trips per year by gear type")
plot(as.numeric(names(byc)), byc, type = "l", las = 2, xlab = "year", ylab = "proportion", main = "Proportion of trips with nets and traps")
points(as.numeric(names(byc)), byc, pch = 20, cex = 1.2)

par(mfrow = c(2,2), mar = c(20, 3, 1, 0))
barplot(table(fin$year), las = 2)
barplot(table(fin$gear), las = 2)
barplot(table(fin$area), las = 2)

spp <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$GEAR_TYPE_NM), sum, na.rm = T)
par(mfrow = c(3, 2), mar = c(15, 4, 1, 0))
for (i in 1:6)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
  main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

spp <- tapply(d$POUNDS_LANDED[which(d$TRIP_YEAR > 2010)], 
         list(d$SPECIES_NM[which(d$TRIP_YEAR > 2010)], d$GEAR_TYPE_NM[which(d$TRIP_YEAR > 2010)]), sum, na.rm = T)
par(mfrow = c(2, 2), mar = c(15, 3, 1, 0))
for (i in 3:6)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
          main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

dev.off()
pc <- prcomp(mat, scale = T)
summary(pc)
plot(pc)
biplot(pc)


############################  END STT #################################


########################### START STX  ################################

rm(list = ls())

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STX_072011_present_LANDINGS_trip_2021-03-11.csv")

# define start and end years ---------------------------
styear <- 1990
enyear <- 2020
table(dat$TRIP_YEAR)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# subset years------------------------------------------

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
table(d$TRIP_YEAR)

# take a look at data fields ----------------------------

table(d$TRIP_ID, useNA = "always")
table(d$TRIP_YEAR, useNA = "always")
table(d$TRIP_MONTH, useNA = "always")
table(d$TRIP_DAY, useNA = "always")
table(d$VESSEL_CD, useNA = "always")
table(d$FISHER_PERMIT, useNA = "always")
table(d$FISHER_FIRST_NAME, useNA = "always")
table(d$FISHER_LAST_NAME, useNA = "always")
table(d$CHARTER_TRIP, useNA = "always")
table(d$NUM_PARTNERS_OR_HELPERS, useNA = "always")
table(d$IS_CATCH_SPLIT, useNA = "always")
table(d$ISLAND, useNA = "always")
table(d$LANDING_AREA_NAME, useNA = "always")
table(d$AREA_CD1, useNA = "always")
table(d$AREA_CD2, useNA = "always")
table(d$HOURS_FISHED, useNA = "always")
table(d$DEPTH, useNA = "always")
table(d$TOTAL_TRAPS_HAULED, useNA = "always")
table(d$TOTAL_TRAPS_IN_WATER, useNA = "always")
table(d$FAD_CD, useNA = "always")
table(d$GEAR_TYPE_NM, useNA = "always")
table(d$GEAR_NM, useNA = "always")
table(d$GEAR1_NAME, useNA = "always")
table(d$SPECIES_CD, useNA = "always")
table(d$SPECIES_NM, useNA = "always")
table(d$FORM_TYPE, useNA = "always")
table(d$TRIP_YEAR, d$FORM_TYPE, useNA = "always")
table(d$TRIP_MAY_BE_DUPLICATE, useNA = "always")

# check that TRIP IDs are unique ------------

dim(d)
#d <- d[-which(d$TRIP_MAY_BE_DUPLICATE == "Y"), ]
#dim(d)
d <- d[-which(d$CHARTER_TRIP == "Y"), ]
dim(d)

d$TRIP_ID <- as.character(d$TRIP_ID)

d$id1 <- paste(d$TRIP_YEAR, d$TRIP_MONTH, d$TRIP_DAY, d$VESSEL_CD, sep = "_")
d$id2 <- paste(d$TRIP_YEAR, d$TRIP_MONTH, d$TRIP_DAY, d$FISHER_PERMIT, sep = "_")
n_distinct(d$id1)
n_distinct(d$id2)
n_distinct(d$TRIP_ID)

n_distinct(d$TRIP_ID) - n_distinct(d$id1)

id2 <- as.numeric(as.factor(d$id1))
n_distinct(id2)

mn <- tapply(id2, d$TRIP_ID, mean)
table(mn - round(mn))

table(tapply(d$TRIP_YEAR, d$TRIP_ID, sd, na.rm = T))

# look at gear names -----------------------------------
table(d$GEAR_NM, d$GEAR_TYPE_NM)
table(d$GEAR_TYPE_NM)

#d[which(d$TRIP_ID == "CCL12380"), ]

# calculate main gear, year, region for each trip ---------------------

tab <- tapply(d$POUNDS_LANDED, list(d$TRIP_ID, d$GEAR_TYPE_NM), sum, na.rm = T)
tabc <- tab
tabc[!is.na(tabc)] <- 1
hist(rowSums(tabc, na.rm = T))
table(rowSums(tabc, na.rm = T))
tab[is.na(tab)] <- 0
co <- apply(tab, 1, which.max)
gears <- colnames(tabc)[co]

head(tab, 20)        # check to see if correct
gears[1:20]

years <- tapply(d$TRIP_YEAR, d$TRIP_ID, mean, na.rm = T)

tab2 <- table(d$TRIP_ID, d$LANDING_AREA_NAME)
co <- apply(tab2, 1, which.max)
areas <- colnames(tab2)[co]

table(rownames(tab) == names(years))
table(rownames(tab) == rownames(tab2))

fin <- data.frame(rownames(tab), years, gears, areas)
names(fin) <- c("id", "year", "gear", "area")
head(fin)

apply(fin, 2, table, useNA = "always")  # check no NAs


# plot results -----------------------

mat <- table(fin$year, fin$gear)
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(tab))

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = 10, y = 0.8), legend.text = colnames(tab))

mat[, c(2, 4)]
byc <- rowSums(mat[, c(2, 4)]) / rowSums(mat)   # proportion of trips with non-selective gears
plot(names(byc), byc, type = "l")

par(mfrow = c(2,1), mar = c(5, 4, 2, 1))
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright", bty = "n"), legend.text = colnames(tab), las = 2, 
        main = "Number of trips per year by gear type")
plot(as.numeric(names(byc)), byc, type = "l", las = 2, xlab = "year", ylab = "proportion", main = "Proportion of trips with nets and traps")
points(as.numeric(names(byc)), byc, pch = 20, cex = 1.2)

par(mfrow = c(2,2), mar = c(20, 3, 1, 0))
barplot(table(fin$year), las = 2)
barplot(table(fin$gear), las = 2)
barplot(table(fin$area), las = 2)

spp <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$GEAR_TYPE_NM), sum, na.rm = T)
par(mfrow = c(2, 2), mar = c(15, 4, 1, 0))
for (i in 1:6)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
          main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

pc <- prcomp(mat, scale = T)
summary(pc)
plot(pc)
biplot(pc)

############################  END STX #################################


###########################  START PR  #################################
rm(list = ls())

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20_wSC_2005cor.csv")

# define start and end years ---------------------------
styear <- 1990
enyear <- 2020

# subset years------------------------------------------

d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]
table(d$YEAR_LANDED)
head(d)

# take a look at data fields ----------------------------
table(d$VESSEL, useNA = "always")
table(d$YEAR_LANDED, useNA = "always")
table(d$MONTH_LANDED, useNA = "always")
table(d$DAY_LANDED, useNA = "always")
table(d$FISHING_CENTER_ED, useNA = "always")
table(d$FISHING_CENTER_NAME, useNA = "always")
table(d$MUNICIPALITY, useNA = "always")
table(d$AREA_FISHED1, useNA = "always")
table(d$AREA_FISHED2, useNA = "always")
table(d$AREA_FISHED3, useNA = "always")
table(d$AREA_FISHED4, useNA = "always")
table(d$FIN_GEAR_CODE, useNA = "always")
table(d$FIN_GEAR_NAME, useNA = "always")
table(d$PR_ID_CODE_ED, useNA = "always")
table(d$NUMBER_OF_TRIPS_ED, useNA = "always")
table(d$GEAR_QTY_ED, useNA = "always")
table(d$GEAR_HOURS_ED, useNA = "always")
hist(d$MAXIMUM_DEPTH_ED)
hist(d$MINIMUM_DEPTH_ED)
table(d$SPECIES_ITIS, useNA = "always")
table(d$ITIS_COMMON_NAME, useNA = "always")
table(d$ITIS_SCIENTIFIC_NAME, useNA = "always")
hist(d$POUNDS_LANDED)
hist(d$ADJUSTED_POUNDS)
hist(d$VALUE_IN_DOLLARS)
hist(d$PRICE_PER_LB)
table(d$DISTANCE, useNA = "always")
table(d$DISTANCE_DESCRIBE, useNA = "always")
table(d$TRIP_TICKET_NUMBER_ED, useNA = "always")

table(d$YEAR_LANDED, is.na(d$TRIP_TICKET_NUMBER_ED))  # no NAs after 2000
table(d$YEAR_LANDED, d$TRIP_TICKET_NUMBER_ED == "")   # no empties after 2002

#d <- dat[which(dat$YEAR_LANDED >= 2003), ]
table(d$YEAR_LANDED)
table(is.na(d$TRIP_TICKET_NUMBER_ED))
table(d$TRIP_TICKET_NUMBER_ED == "")

# check that TRIP IDs are unique ------------

d$TRIP_TICKET_NUMBER_ED <- as.character(d$TRIP_TICKET_NUMBER_ED)

id1 <- paste(d$YEAR_LANDED, d$MONTH_LANDED, d$DAY_LANDED, d$PR_ID_CODE_ED, sep = "_")
n_distinct(id1)
n_distinct(d$TRIP_TICKET_NUMBER_ED)

n_distinct(d$TRIP_TICKET_NUMBER_ED) - n_distinct(id1)

id2 <- as.numeric(as.factor(id1))
n_distinct(id2)

mn <- tapply(id2, d$TRIP_TICKET_NUMBER_ED, mean)
table(mn - round(mn))

table(tapply(d$YEAR_LANDED, d$TRIP_TICKET_NUMBER_ED, sd, na.rm = T))

d$ID <- id1  # select way to identify individual trips

# look at gear names -----------------------------------

table(d$FIN_GEAR_NAME)
par(mar = c(15, 2, 2, 2))
barplot(table(d$FIN_GEAR_NAME), las = 2)
d$GEAR <- NA

d$GEAR[grep("SEINE", d$FIN_GEAR_NAME)] <- "NETS"
d$GEAR[grep("NET", d$FIN_GEAR_NAME)] <- "NETS"
d$GEAR[grep("LINE", d$FIN_GEAR_NAME)] <- "HOOK AND LINE"
d$GEAR[grep("ROD AND REEL", d$FIN_GEAR_NAME)] <- "HOOK AND LINE"
d$GEAR[grep("TRAPS", d$FIN_GEAR_NAME)] <- "TRAPS"
d$GEAR[grep("DIVING", d$FIN_GEAR_NAME)] <- "DIVING"
d$GEAR[grep("BY HAND", d$FIN_GEAR_NAME)] <- "DIVING"
d$GEAR[grep("SPEARS", d$FIN_GEAR_NAME)] <- "DIVING"
d$GEAR[grep("TRAP", d$FIN_GEAR_NAME)] <- "TRAPS"
d$GEAR[grep("BOTTOM", d$FIN_GEAR_NAME)] <- "BOTTOM LONG LINE"
d$GEAR[grep("LONG LINE", d$FIN_GEAR_NAME)] <- "BOTTOM LONG LINE"
d$GEAR[grep("COMBINED GEARS", d$FIN_GEAR_NAME)] <- "TRAPS"  # assumed traps based on composition; only 25 obs

table(d$FIN_GEAR_NAME, d$GEAR)
which(is.na(d$GEAR))

table(d$GEAR)

WEST <- c("AGUADA", "AGUADILLA", "ANASCO", "CABO ROJO", "LAJAS", "MAYAGUEZ", "RINCON")
EAST <- c("CEIBA", "CULEBRA", "FAJARDO", "HUMACAO", "LUQUILLO", "MAUNABO", "NAGUABO", "RIO GRANDE", "VIEQUES", "YABUCOA")         
NORTH <- c("ARECIBO", "BARCELONETA", "CAMUY", "CAROLINA", "CATANO", "DORADO", "GUAYNABO", "HATILLO", 
           "ISABELA", "LOIZA", "MANATI", "QUEBRADILLAS", "SAN JUAN", "TOA BAJA", "VEGA ALTA", "VEGA BAJA")
SOUTH <- c("AIBONITO", "ARROYO", "GUANICA", "GUAYAMA", "GUAYANILLA", "JUANA DIAZ", "PATILLAS", "PENUELAS", 
           "PONCE", "SALINAS", "SANTA ISABEL", "VILLALBA")

d$REGION <- NA
d$REGION[which(d$MUNICIPALITY %in% WEST)] <- "WEST"
d$REGION[which(d$MUNICIPALITY %in% EAST)] <- "EAST"
d$REGION[which(d$MUNICIPALITY %in% NORTH)] <- "NORTH"
d$REGION[which(d$MUNICIPALITY %in% SOUTH)] <- "SOUTH"

# calculate main gear, year, region for each trip ---------------------

tab <- tapply(d$ADJUSTED_POUNDS, list(d$ID, d$GEAR), sum, na.rm = T)
tabc <- tab
tabc[!is.na(tabc)] <- 1
hist(rowSums(tabc, na.rm = T))
table(rowSums(tabc, na.rm = T))
tab[is.na(tab)] <- 0
co <- apply(tab, 1, which.max)
gears <- colnames(tabc)[co]

head(tab, 20)        # check to see if correct
gears[1:20]

years <- tapply(d$YEAR_LANDED, d$ID, mean, na.rm = T)
table(years, useNA = "always")

tab2 <- table(d$ID, d$REGION)
co <- apply(tab2, 1, which.max)
areas <- colnames(tab2)[co]

table(rownames(tab) == names(years))
table(rownames(tab) == rownames(tab2))

fin <- data.frame(rownames(tab), years, gears, areas)
names(fin) <- c("id", "year", "gear", "area")
head(fin)

apply(fin, 2, table, useNA = "always")  # check no NAs


# plot results -----------------------

dev.off()

mat <- table(fin$year, fin$gear)
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat))

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = "topleft"), legend.text = colnames(matp))

mat[, c(4, 5)]
byc <- rowSums(mat[, c(4, 5)]) / rowSums(mat)   # proportion of trips with non-selective gears
plot(names(byc), byc, type = "l")

par(mfrow = c(2,1), mar = c(5, 4, 2, 1))
barplot(t(mat), col = cols25(5), args.legend = c(x = "topright", bty = "n", pt.cex = 0.8), 
        legend.text = colnames(mat), las = 2, 
        main = "Number of trips per year by gear type", ylim = c(0, 50000))
plot(as.numeric(names(byc)), byc, type = "l", las = 2, xlab = "year", ylab = "proportion", main = "Proportion of trips with nets and traps")
points(as.numeric(names(byc)), byc, pch = 20, cex = 1.2)

par(mfrow = c(2,2), mar = c(20, 3, 1, 0))
barplot(table(fin$year), las = 2)
barplot(table(fin$gear), las = 2)
barplot(table(fin$area), las = 2)

spp <- tapply(d$ADJUSTED_POUNDS, list(d$ITIS_COMMON_NAME, d$GEAR), sum, na.rm = T)
par(mfrow = c(3, 2), mar = c(15, 4, 1, 0))
for (i in 1:5)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
          main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

dev.off()
pc <- prcomp(mat, scale = T)
summary(pc)
plot(pc)
biplot(pc)

mata <- table(fin$area, fin$gear)
barplot(t(mata), col = cols25(7), args.legend = c(x = "topleft"), legend.text = colnames(mata))

pc <- prcomp(mata, scale = T)
summary(pc)
plot(pc)
biplot(pc)

lis <- c("NORTH", "EAST", "SOUTH", "WEST")
nams <- c("BLL", "DIVING", "LINE", "NETS", "TRAPS")

par(mfrow = c(4, 2), mar = c(3, 3, 1, 1) + 1)
for(i in 1:4)  { 
  fin1 <- fin[which(fin$area == lis[i]), ]
  mat <- table(fin1$year, fin1$gear)
  if (i == 2) { barplot(t(mat), col = cols25(7), args.legend = c(x = "topright", bty = "n"), legend.text = colnames(mat), main = lis[i]) }
  if (i != 2) { barplot(t(mat), col = cols25(7), main = lis[i]) }
  mat1 <- as.data.frame((cbind(as.numeric(rownames(mat)), mat)))
  pc <- plotOrdScores(mat1, main = lis[i])
  text(pc$rotation[, 1]*2, pc$rotation[, 2]*2, nams, col = gray(0.5))
  print(pc$x)
  }

############################  END PR  #################################




