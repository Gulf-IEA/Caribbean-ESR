####################################################################

# M. Karnauskas 12/22/2023
# code for calculating trip-level statistics, gear changes
# uses logbook data for PR and USVI 

##########  NEED TO CHECK ON TRIP IDENTIFIERS  #####################


# specification file and libraries -----------------------------

rm(list = ls())

plot.new()
dev.off()

library(pals)
library(dplyr)
library(vegan)
library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")

# define start and end years ---------------------------
styear <- 1990
enyear <- 2022

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/MOST_RECENT/"

dat <- read.csv(paste0(confpath, "STT_2024.csv"))

table(dat$TRIP_YEAR)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# take out incomplete years -----------------------------

tab <- table(dat$TRIP_YEAR, dat$TRIP_MONTH)
table(dat$TRIP_YEAR)
lis <- as.numeric(names(which(apply(tab, 1, min) == 0)))
lis
dat <- dat[!(dat$TRIP_YEAR %in% lis), ]
table(dat$TRIP_YEAR)

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

head(tab2, 20)        # check to see if correct
areas[1:20]

table(rownames(tab) == names(years))
table(rownames(tab) == rownames(tab2))

fin <- data.frame(rownames(tab), years, gears, areas)
names(fin) <- c("id", "year", "gear", "area")
head(fin)

fin$gear[which(fin$gear == "SPEAR OR HAND")] <- "DIVING"
fin$gear <- droplevels(factor(fin$gear))

apply(fin, 2, table, useNA = "always")  # check no NAs

# plot results -----------------------

par(mfrow = c(2, 1))
mat <- table(fin$year, fin$gear)
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat))
matplot(mat, type = "b", pch = 19, col = cols25(7))

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = 34, y = 0.9), legend.text = colnames(mat))
matplot(matp, type = "b", pch = 19, col = cols25(7))

head(mat)
ind <- which(colnames(mat) == "NETS" | colnames(mat) == "TRAPS")
mat[, ind]
byc <- rowSums(mat[, ind], na.rm = T)/ rowSums(mat, na.rm = T)   # proportion of trips with non-selective gears - nets and traps
plot(names(byc), byc, type = "b")

matp
ind2 <- which(colnames(matp) == "DIVING")
plot(names(byc), matp[, ind2], type = "b")
div <- matp[, ind2]

save(div, file ="indicator_data/intermediateFiles/trip_types/prop_trips_diving_STT.RData")
save(byc, file ="indicator_data/intermediateFiles/trip_types/prop_trips_nonselective_STT.RData")

# plot trips and percent bycatch gears -------------------

png(filename = "indicator_plots/gearTypes_STT.png", 
    units="in", width = 6, height = 3, pointsize=12, res=72*2)

par(mar = c(3, 4, 2, 1))
barplot(t(mat), col = cols25(7), args.legend = list(x = "topright", bty = "n", y.intersp = 0.8), legend.text = colnames(mat), las = 2, 
        main = "Number of trips per year by gear type\nSt. Thomas and St. John", ylim = c(0, 8000))
abline(h=0)
#plot(as.numeric(names(byc)), byc, type = "l", las = 2, xlab = "", ylab = "proportion", main = "Proportion of trips with nets and traps", axes = F)
#axis(1); axis(2, las = 2); box()
#points(as.numeric(names(byc)), byc, pch = 20, cex = 1.2)

dev.off()


par(mfrow = c(2,2), mar = c(20, 3, 1, 0))
barplot(table(fin$year), las = 2)
barplot(table(fin$gear), las = 2)
barplot(table(fin$area), las = 2)

spp <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$GEAR_TYPE_NM), sum, na.rm = T)
par(mfrow = c(3, 2), mar = c(15, 4, 1, 0))
for (i in 1:4)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
  main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

spp <- tapply(d$POUNDS_LANDED[which(d$TRIP_YEAR > 2010)], 
         list(d$SPECIES_NM[which(d$TRIP_YEAR > 2010)], d$GEAR_TYPE_NM[which(d$TRIP_YEAR > 2010)]), sum, na.rm = T)
par(mfrow = c(2, 2), mar = c(15, 3, 1, 0))
for (i in 1:4)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
          main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

dev.off()

pc <- prcomp(mat, scale = T)
summary(pc)
plot(pc)
biplot(pc)
plot(pc$x[, 1], pc$x[, 2], type = "l")
text(pc$x[, 1], pc$x[, 2], rownames(pc$x))
# shift in 2010, likely due to change in reporting form

# trends by area -----------------

fin <- fin[which(fin$year >= 2011), ]
fin$area <- as.character(fin$area)

table(fin$area, useNA = "always")

fin$area[grep("CHARLOTTE AMALIE", fin$area)] <- "CHARLOTTE AMALIE"
fin$area[grep("SAINT THOMAS HARBOR", fin$area)] <- "CHARLOTTE AMALIE"
fin$area[grep("EAST END", fin$area)] <- "EAST END"
fin$area[grep("WEST END", fin$area)] <- "WEST END"
fin$area[grep("RED HOOK", fin$area)] <- "RED HOOK"
fin$area[grep("SECRET HARBOR", fin$area)] <- "NAZARETH"
fin$area[which(fin$area == "")] <- "UNK"

table(fin$area, useNA = "always")

table(fin$area, fin$year)

fin$gear <- droplevels(fin$gear)

mat <- table(fin$area, fin$gear)
mat

par(mar = c(20, 3, 1, 1))
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat), las = 2)

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat), las = 2)

dev.off()

pc <- prcomp(matp, scale = T)
summary(pc)
plot(pc)
biplot(pc)

z <- metaMDS(comm = matp, autotransform = FALSE, distance = "bray",
             engine = "monoMDS", k = 2, weakties = TRUE, model = "global",
             maxit = 300, try = 40, trymax = 100)
print(z)
gof <- goodness(object = z)
plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))
plot(z$diss, z$dist)
stressplot(object = z, lwd = 5)

z$points[6, 2] <- z$points[6, 2]/3
#adj <- which(z$points[, 2] > 0 & z$points[, 2] < 0.1)
#z$points[adj, 2] <- z$points[adj, 2]*1.3

png(filename = "indicator_plots/NMDSgear_STT.png", 
    units="in", width = 5, height = 5, pointsize=12, res=72*2)
par(mar = c(2, 2, 3, 2))

plot(z$points[, 1], z$points[, 2], col = 0, axes = F, xlab = "", ylab = "", xlim = c(-1.3, 1.7), ylim= c(-0.4, 0.52), 
     main = "Ordination of gear type usage by landing sites\nSt. Thomas and St. John")
text(z$points[, 1], z$points[, 2], rownames(z$points), cex = 0.65, col = gray(0.7))
box()
text(z$species[, 1]/2.4, z$species[, 2]/2.4, rownames(z$species), col = 1, font = 2)

dev.off()

############################  END STT #################################


########################### START STX  ################################

rm(list = ls()[-match(c("styear", "enyear", "confpath"), ls())])

dat <- read.csv(paste0(confpath, "STX_2024.csv"))

table(dat$TRIP_YEAR)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# take out incomplete years -----------------------------

tab <- table(dat$TRIP_YEAR, dat$TRIP_MONTH)
table(dat$TRIP_YEAR)
lis <- as.numeric(names(which(apply(tab, 1, min) == 0)))
lis
dat <- dat[!(dat$TRIP_YEAR %in% lis), ]
table(dat$TRIP_YEAR)

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

head(tab2, 20)        # check to see if correct
areas[1:20]

table(rownames(tab) == names(years))
table(rownames(tab) == rownames(tab2))

fin <- data.frame(rownames(tab), years, gears, areas)
names(fin) <- c("id", "year", "gear", "area")
head(fin)

fin$gear[which(fin$gear == "SPEAR OR HAND")] <- "DIVING"
fin$gear <- droplevels(factor(fin$gear))

apply(fin, 2, table, useNA = "always")  # check no NAs

# plot results -----------------------

par(mfrow = c(2, 1))
mat <- table(fin$year, fin$gear)
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat))
matplot(mat, type = "b", pch = 19, col = cols25(7))

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = 34, y = 0.9), legend.text = colnames(mat))
matplot(matp, type = "b", pch = 19, col = cols25(7))

head(mat)
ind <- which(colnames(mat) == "NETS" | colnames(mat) == "TRAPS")
mat[, ind]
byc <- rowSums(mat[, ind], na.rm = T)/ rowSums(mat, na.rm = T)   # proportion of trips with non-selective gears - nets and traps
plot(names(byc), byc, type = "b")

matp
ind2 <- which(colnames(matp) == "DIVING")
plot(names(byc), matp[, ind2], type = "b")
div <- matp[, ind2]

save(div, file ="indicator_data/intermediateFiles/trip_types/prop_trips_diving_STX.RData")
save(byc, file ="indicator_data/intermediateFiles/trip_types/prop_trips_nonselective_STX.RData")

# plot trips and percent bycatch gears -------------------

cols <- cols25(7)
cols <- cols[c(2:5)]

png(filename = "indicator_plots/gearTypes_STX.png", 
    units="in", width = 4.75, height = 3, pointsize=12, res=72*2)

par(mar = c(3, 4, 2, 1))
barplot(t(mat), col = cols, args.legend = list(x = "topright", bty = "n", y.intersp = 0.9), legend.text = colnames(mat), las = 2, 
        main = "Number of trips per year by gear type\nSt. Croix")
abline(h=0)
#plot(as.numeric(names(byc)), byc, type = "l", las = 2, xlab = "", ylab = "proportion", main = "Proportion of trips with nets and traps", axes = F)
#axis(1); axis(2, las = 2); box()
#points(as.numeric(names(byc)), byc, pch = 20, cex = 1.2)
dev.off()

par(mfrow = c(2,2), mar = c(20, 3, 1, 0))
barplot(table(fin$year), las = 2)
barplot(table(fin$gear), las = 2)
barplot(table(fin$area), las = 2)

spp <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$GEAR_TYPE_NM), sum, na.rm = T)
par(mfrow = c(3, 2), mar = c(15, 4, 1, 0))
for (i in 1:4)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
          main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

spp <- tapply(d$POUNDS_LANDED[which(d$TRIP_YEAR > 2010)], 
              list(d$SPECIES_NM[which(d$TRIP_YEAR > 2010)], d$GEAR_TYPE_NM[which(d$TRIP_YEAR > 2010)]), sum, na.rm = T)
par(mfrow = c(2, 2), mar = c(15, 3, 1, 0))

for (i in 1:4)  { 
  barplot(sort(spp[, i], decreasing = T)[1:10], las = 2, 
          main = colnames(spp)[i])
  legend("topright", legend = NA, title = paste("N species =", table(is.na(spp[, i]))[1], "    "), bty = "n")
}

dev.off()

pc <- prcomp(mat, scale = T)
summary(pc)
plot(pc)
biplot(pc)
plot(pc$x[, 1], pc$x[, 2], type = "l")
text(pc$x[, 1], pc$x[, 2], rownames(pc$x))

# trends by area -----------------

fin$area <- as.character(fin$area)

table(fin$area, useNA = "always")

fin$area[grep("ALTONA", fin$area)] <- "ALTONA"
fin$area[grep("CHRISTIANSTED", fin$area)] <- "CHRISTIANSTED"
fin$area[grep("RICHMOND", fin$area)] <- "CHRISTIANSTED"
fin$area[grep("EAST END", fin$area)] <- "EAST END"
fin$area[grep("FREDERIKSTED", fin$area)] <- "FREDERIKSTED"
fin$area[grep("GALLOWS BAY", fin$area)] <- "GALLOWS BAY"
fin$area[grep("KRAUSE LAGOON", fin$area)] <- "KRAUSE LAGOON"
fin$area[grep("SALT RIVER", fin$area)] <- "SALT RIVER"
fin$area[grep("TAGUE", fin$area)] <- "TAGUE BAY"
fin$area[grep("LONG POINT", fin$area)] <- "SANDY POINT"
fin$area[grep("SAINT CROIX", fin$area)] <- "UNK"
fin$area[which(fin$area == "")] <- "UNK"

table(fin$area, useNA = "always")

mat <- table(fin$area, fin$gear)
mat

par(mar = c(20, 3, 1, 1))
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat), las = 2)

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat), las = 2)

dev.off()

pc <- prcomp(matp, scale = T)
summary(pc)
plot(pc)
biplot(pc, xlim = c(-0.5, 0.5), ylim= c(-0.5, 0.5))

# NMDS to group landing sites by gear ----------------------------------

z <- metaMDS(comm = matp, autotransform = FALSE, distance = "bray",
             engine = "monoMDS", k = 2, weakties = TRUE, model = "global",
             maxit = 300, try = 40, trymax = 100)
print(z)
z$stress
gof <- goodness(object = z)
plot(z, display = "sites", type = "none")
points(z, display = "sites", cex = 2*gof/mean(gof))
plot(z$diss, z$dist)
stressplot(object = z, lwd = 5)

png(filename = "indicator_plots/NMDSgear_STX.png", 
    units="in", width = 5, height = 5, pointsize=12, res=72*2)
par(mar = c(2, 2, 3, 2))

z$points[1, 2] <- z$points[1, 2] * 1.1
z$points[9, 2] <- z$points[9, 2] * 0.95
plot(z$points[, 1], z$points[, 2], col = 0, axes = F, xlab = "", ylab = "", xlim = c(-1.35, 1.42), 
     main = "Ordination of gear type usage by landing sites\nSt. Croix")
text(z$points[, 1], z$points[, 2], rownames(z$points), cex = 0.75, col = gray(0.8))
box()
text(z$species[, 1], z$species[, 2], rownames(z$species), col = 1, font = 2)

dev.off()

############################  END STX #################################


###########################  START PR  #################################
rm(list = ls()[-match(c("styear", "enyear", "confpath"), ls())])

dat <- read.csv(paste0(confpath, "wrkeithly_pr_com_data_2000_2022_20240625_C.csv"))

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
table(d$ERDMAN_GEAR_NAME, useNA = "always")
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

# remove land crab trips -------------------

sort(table(d$ITIS_COMMON_NAME[grep("CRAB", d$ITIS_COMMON_NAME)]))
sort(table(d$ERDMAN_GEAR_NAME[grep("CRAB,BLUE", d$ITIS_COMMON_NAME)]))
sort(table(d$FAMILY[grep("CRAB,BLUE", d$ITIS_COMMON_NAME)]))
sort(table(d$ERDMAN_GEAR_NAME[which(d$FAMILY == "LAND CRABS")]))
# filtering by LAND CRAB TRAP gear takes out vast majority (96%) of land crab trips

d[which(d$ERDMAN_GEAR_NAME == "LAND CRAB TRAP"), ]
dim(d)
d <- d[which(d$ERDMAN_GEAR_NAME != "LAND CRAB TRAP"), ]
dim(d)

# develop trip ID ---------------------------
table(d$YEAR_LANDED, is.na(d$TRIP_TICKET_NUMBER_ED))  # no NAs after 2000
table(d$YEAR_LANDED, d$TRIP_TICKET_NUMBER_ED == "")   # no empties after 2002

d <- dat[which(dat$YEAR_LANDED >= 2003), ]
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

#table(tapply(d$YEAR_LANDED, d$TRIP_TICKET_NUMBER_ED, sd, na.rm = T))

d$ID <- id1  # select way to identify individual trips

# look at gear names -----------------------------------

d$FIN_GEAR_NAME <- d$ERDMAN_GEAR_NAME  # new data 2024 relabel

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
d$GEAR[grep("SPEAR", d$FIN_GEAR_NAME)] <- "DIVING"
d$GEAR[grep("SNARE", d$FIN_GEAR_NAME)] <- "DIVING"
d$GEAR[grep("TRAP", d$FIN_GEAR_NAME)] <- "TRAPS"
d$GEAR[grep("POT", d$FIN_GEAR_NAME)] <- "TRAPS"
d$GEAR[grep("BOTTOM", d$FIN_GEAR_NAME)] <- "BOTTOM LONG LINE"
d$GEAR[grep("LONG LINE", d$FIN_GEAR_NAME)] <- "BOTTOM LONG LINE"
d$GEAR[grep("OTHER", d$FIN_GEAR_NAME)] <- "DIVING"
d$GEAR[grep("SILK", d$FIN_GEAR_NAME)] <- "NETS"
d$GEAR[grep("COMBINED GEARS", d$FIN_GEAR_NAME)] <- "TRAPS"  # assumed traps based on composition; only 25 obs

table(d$FIN_GEAR_NAME, d$GEAR, useNA = "always")
which(is.na(d$GEAR))

table(d$GEAR)

#WEST <- c("AGUADA", "AGUADILLA", "ANASCO", "CABO ROJO", "LAJAS", "MAYAGUEZ", "RINCON")
#EAST <- c("CEIBA", "CULEBRA", "FAJARDO", "HUMACAO", "LUQUILLO", "MAUNABO", "NAGUABO", "RIO GRANDE", "VIEQUES", "YABUCOA")         
#NORTH <- c("ARECIBO", "BARCELONETA", "CAMUY", "CAROLINA", "CATANO", "DORADO", "GUAYNABO", "HATILLO", 
#           "ISABELA", "LOIZA", "MANATI", "QUEBRADILLAS", "SAN JUAN", "TOA BAJA", "VEGA ALTA", "VEGA BAJA")
#SOUTH <- c("AIBONITO", "ARROYO", "GUANICA", "GUAYAMA", "GUAYANILLA", "JUANA DIAZ", "PATILLAS", "PENUELAS", 
#           "PONCE", "SALINAS", "SANTA ISABEL", "VILLALBA")
#
#d$REGION <- NA
#d$REGION[which(d$MUNICIPALITY %in% WEST)] <- "WEST"
#d$REGION[which(d$MUNICIPALITY %in% EAST)] <- "EAST"
#d$REGION[which(d$MUNICIPALITY %in% NORTH)] <- "NORTH"
#d$REGION[which(d$MUNICIPALITY %in% SOUTH)] <- "SOUTH"

table(d$COAST, useNA = "always")

d$REGION <- d$COAST

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

head(tab2, 20)        # check to see if correct
areas[1:20]

#tab3 <- table(d$ID, d$MUNICIPALITY)
#co <- apply(tab3, 1, which.max)
#areas2 <- colnames(tab3)[co]
#head(tab3, 20)        # check to see if correct
#areas2[1:20]

table(rownames(tab) == names(years))
table(rownames(tab) == rownames(tab2))

fin <- data.frame(rownames(tab), years, gears, areas) #, areas2)
names(fin) <- c("id", "year", "gear", "area") # , "municipality")
head(fin)

apply(fin, 2, table, useNA = "always")  # check no NAs

# plot results -----------------------

dev.off()

mat <- table(fin$year, fin$gear)
barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat))

matp <- t(apply(mat, 1, function(x) x/sum(x)))
barplot(t(matp), col = cols25(7), args.legend = c(x = "topleft"), legend.text = colnames(matp))

mat

ind <- which(colnames(mat) == "NETS" | colnames(mat) == "TRAPS")
mat[, ind]
byc <- rowSums(mat[, ind], na.rm = T)/ rowSums(mat, na.rm = T)   # proportion of trips with non-selective gears - nets and traps
plot(names(byc), byc, type = "b")

matp
ind2 <- which(colnames(matp) == "DIVING")
plot(names(byc), matp[, ind2], type = "b")
div <- matp[, ind2]

save(div, file ="indicator_data/intermediateFiles/trip_types/prop_trips_diving_PR.RData")
save(byc, file ="indicator_data/intermediateFiles/trip_types/prop_trips_nonselective_PR.RData")

# plot trips and percent bycatch gears -------------------

mat
lis <- c("BOTTOM LONG LINE", "HOOK AND LINE", "NETS", "DIVING", "TRAPS")
mat <- mat[, match(colnames(mat), lis)]
cols <- cols25(7)
cols <- cols[c(7, 2:5)]

png(filename = "indicator_plots/gearTypes_PR.png", 
    units="in", width = 5.5, height = 3, pointsize=12, res=72*2)

par(mar = c(3, 4, 2, 1))
barplot(t(mat), col = cols, args.legend = list(x = "topright", bty = "n", x.intersp = 0.5, ncol = 2), legend.text = colnames(mat), las = 2, 
        main = "Number of trips per year by gear type\nPuerto Rico", ylim = c(0, 44000))
abline(h=0)
#plot(as.numeric(names(byc)), byc, type = "l", las = 2, xlab = "", ylab = "proportion", main = "Proportion of trips with nets and traps", axes = F)
#axis(1); axis(2, las = 2); box()
#points(as.numeric(names(byc)), byc, pch = 20, cex = 1.2)

dev.off()

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

# trends by area -----------------
# ###########  THIS DOES NOT WORK WITH 2024 DATA PULL -- NEED TO FIX 
# 
# fin$municipality <- as.character(fin$municipality)
# table(fin$municipality, useNA = "always")
# 
# fin$municipality[grep("AIBONITO", fin$municipality)] <- "UNK"
# fin$municipality[grep("VILLALBA", fin$municipality)] <- "UNK"
# fin$municipality[grep("ISLAND OF PUERTO RICO", fin$municipality)] <- "UNK"
# fin$municipality[grep("MISSIN", fin$municipality)] <- "UNK"
# fin$municipality[grep("ISLAND OF PUERTO RICO", fin$municipality)] <- "UNK"
# fin$municipality[grep("GUAYNABO", fin$municipality)] <- "SAN JUAN"
# fin$municipality[grep("TOA BAJA", fin$municipality)] <- "SAN JUAN"
# fin$municipality[grep("HATILLO", fin$municipality)] <- "ARECIBO"
# fin$municipality[is.na(fin$municipality)] <- "UNK"
# 
# table(fin$municipality, useNA = "always")
# 
# fin2 <- fin  # [which(fin$year >= 2011), ]
# mat <- table(fin2$municipality, fin2$gear)
# mat
# 
# par(mar = c(10, 3, 1, 1))
# barplot(t(mat), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat), las = 2)
# 
# matp <- t(apply(mat, 1, function(x) x/sum(x)))
# barplot(t(matp), col = cols25(7), args.legend = c(x = "topright"), legend.text = colnames(mat), las = 2)
# 
# pc <- prcomp(matp, scale = T)
# summary(pc)
# plot(pc)
# biplot(pc)
# 
# windows()
# 
# # NMDS to group landing sites by gear ----------------------------------
# 
# z <- metaMDS(comm = matp, autotransform = FALSE, distance = "bray",
#              engine = "monoMDS", k = 3, weakties = TRUE, model = "global",
#              maxit = 300, try = 40, trymax = 100)
# print(z)
# z$stress
# gof <- goodness(object = z)
# plot(z, display = "sites", type = "none")
# points(z, display = "sites", cex = 2*gof/mean(gof))
# plot(z$diss, z$dist)
# stressplot(object = z, lwd = 5)
# 
# lis <- rownames(z$points)
# lis[which(lis %in% WEST)] <- 4
# lis[which(lis %in% EAST)] <- 2
# lis[which(lis %in% NORTH)] <- 1
# lis[which(lis %in% SOUTH)]<- 3
# lis <- as.numeric(lis)
# lis[is.na(lis)] <- 7
# 
# png(filename = "indicator_plots/NMDSgear_PR.png", 
#     units="in", width = 5, height = 5, pointsize=12, res=72*2)
# par(mar = c(2, 2, 3, 2))
# 
# plot(z$points[, 1], z$points[, 2], col = 0, axes = F, xlab = "", ylab = "", xlim = c(-1.1, 1), 
#      main = "Ordination of gear type usage by landing sites\nPuerto Rico")
# text(z$points[, 1], z$points[, 2], rownames(z$points), cex = 0.75, col = lis + 1)
# box()
# z$species[1, 2] <- z$species[1, 2] * 0.95
# z$species[3, 2] <- z$species[3, 2] * 1.2
# text(z$species[, 1], z$species[, 2], rownames(z$species), col = 1, cex = 1.1, font = 2)
# legend("bottomleft", c("NORTH", "EAST", "SOUTH", "WEST"), text.col = 2:5, cex = 1.05, bty = "n")
# 
# dev.off()

############################  END PR  #################################


#lis <- c("NORTH", "EAST", "SOUTH", "WEST")
#nams <- c("BLL", "DIVING", "LINE", "NETS", "TRAPS")

#par(mfrow = c(4, 2), mar = c(3, 3, 1, 1) + 1)
#for(i in 1:4)  { 
#  fin1 <- fin[which(fin$area == lis[i]), ]
#  mat <- table(fin1$year, fin1$gear)
#  if (i == 2) { barplot(t(mat), col = cols25(7), args.legend = c(x = "topright", bty = "n"), legend.text = colnames(mat), main = lis[i]) }
#  if (i != 2) { barplot(t(mat), col = cols25(7), main = lis[i]) }
#  mat1 <- as.data.frame((cbind(as.numeric(rownames(mat)), mat)))
#  pc <- plotOrdScores(mat1, main = lis[i])
#  text(pc$rotation[, 1]*2, pc$rotation[, 2]*2, nams, col = gray(0.5))
#  print(pc$x)
#}

###################  SYNTHESIZE GEAR INDICATORS   ########################

rm(list = ls())

load("indicator_data/intermediateFiles/trip_types/prop_trips_diving_STT.RData")
divSTT <- div
load("indicator_data/intermediateFiles/trip_types/prop_trips_diving_STX.RData")
divSTX <- div
load("indicator_data/intermediateFiles/trip_types/prop_trips_diving_PR.RData")
divPR <- div

# extract years ----------------------

#names(divSTT) == names(divSTX)
#names(divSTX) == names(divPR)
yrs <- (min(c(names(divSTX), names(divSTT), names(divPR)))) : (max(c(names(divSTX), names(divSTT), names(divPR))))
yrs

# put data together in single matrix ----------------------

mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 4))
rownames(mat) <- yrs
mat[match(names(divPR), yrs), 1] <- divPR
mat[match(names(divSTT), yrs), 2] <- divSTT
mat[match(names(divSTX), yrs), 3] <- divSTX
mat[, 4] <- yrs
mat
mat <- mat[which(rownames(mat) == 2000):nrow(mat), ]
names(mat) <- c("PR", "STT", "STX", "yrs")
mat

# format indicator object --------------------

datdata <- mat$yrs
inddata <- data.frame(mat[, 1:3])
labs <- c("Proportion of diving trips", "proportion", "Puerto Rico",
          "Proportion of diving trips", "proportion", "St. Thomas and St. John", 
          "Proportion of diving trips", "proportion", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
ind <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(ind) <- "indicatordata"

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F, 
                        widadj = 1, hgtadj = 1, trendAnalysis = T)

save(ind, file = "indicator_objects/prop_diving_trips.RData")

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F, 
     widadj = 1, hgtadj = 0.6, trendAnalysis = T, outtype = "png")
                        

# bycatch gears ----------------------------

rm(list = ls())

load("indicator_data/intermediateFiles/trip_types/prop_trips_nonselective_STT.RData")
divSTT <- byc
load("indicator_data/intermediateFiles/trip_types/prop_trips_nonselective_STX.RData")
divSTX <- byc
load("indicator_data/intermediateFiles/trip_types/prop_trips_nonselective_PR.RData")
divPR <- byc

# extract years ----------------------

#names(divSTT) == names(divSTX)
#names(divSTX) == names(divPR)
yrs <- (min(c(names(divSTX), names(divSTT), names(divPR)))) : (max(c(names(divSTX), names(divSTT), names(divPR))))
yrs

# put data together in single matrix ----------------------

mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 4))
rownames(mat) <- yrs
mat[match(names(divPR), yrs), 1] <- divPR
mat[match(names(divSTT), yrs), 2] <- divSTT
mat[match(names(divSTX), yrs), 3] <- divSTX
mat[, 4] <- yrs
mat
mat <- mat[which(rownames(mat) == 2000):nrow(mat), ]
names(mat) <- c("PR", "STT", "STX", "yrs")
mat

# format indicator object --------------------

datdata <- mat$yrs
inddata <- data.frame(mat[, 1:3])
labs <- c("Proportion of trips using non-selective gears", "proportion", "Puerto Rico",
          "Proportion of trips using non-selective gears", "proportion", "St. Thomas and St. John", 
          "Proportion of trips using non-selective gears", "proportion", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
ind <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(ind) <- "indicatordata"

plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F, 
                        widadj = 1, hgtadj = 1, trendAnalysis = T)

save(ind, file = "indicator_objects/prop_trips_bycatch.RData")

# plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F, 
#                        widadj = 1, hgtadj = 0.6, trendAnalysis = T, outtype = "png")

##########################   END   ############################

print("gear changes -- SUCCESSFULLY RUN")

