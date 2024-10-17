#
# code for calculating "disturbance indicator" based on changes in seasonality of landings
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

d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]

# look at seasonal cycles ---------------------------------------

an <- tapply(d$ADJUSTED_POUNDS, list(d$MONTH_LANDED, d$YEAR_LANDED), sum, na.rm = T)
an
dim(an)
matplot(an, type = "l", col = rainbow(dim(an)[2]))

an2 <- an
for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
pc <- c(letters[1:26], LETTERS[1:12])
matplot(an2, col = rainbow(dim(an)[2]), type = "b", pch = pc)
an2

av <- apply(an2, 1, mean, na.rm = T)
er <- apply(an2, 1, sd, na.rm = T)*1.96

# identify anomalous years and highlight -----------------------
anomlis <- c(2017, 2020)
cols <- rep("#00000060", dim(an)[2])
cols[which(colnames(an2) == 2017)] <- 3
cols[which(colnames(an2) == 2020)] <- 4
lwds <- rep(1, dim(an)[2])
lwds[which(colnames(an2) == 2017)] <- 3
lwds[which(colnames(an2) == 2020)] <- 3

matplot(an2, col = cols, type = "l", lty = 1, lwd = lwds, axes = F, ylab = "proportion of landings", 
        main = "Distribution of landings throughout the year")
axis(1, at = 1:12, lab = month.abb, las = 2)
axis(2, las = 2); box()
text(3, 0.065, col = 4, "2020", cex = 1.2, font = 2)
text(8, 0.025, col = 3, "2017", cex = 1.2, font = 2)
lines(1:12, av, lwd = 3)
#polygon(c(1:12, 12:1), c((av + er), (av - er)[12:1]), col = "#00000020", border = NA)
#legend(10, 0.17, colnames(an)[which(pc %in% anomlis)], lty = 1, lwd = 2, col = 3:4, bty = "n")
legend("topright", c("average seasonality", "individual years"), lwd = c(3, 1), lty = c(1, 1), col = c(1, 8), pt.cex = c(0, 2), bty = "n", cex= 1.1)

# calculate disturbance indicator ---------------------------

dst <- rep(NA, ncol(an2))
for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
plot(styear:enyear, dst, type = "b")

# now for separate species ----------------------------------

tab <- sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_COMMON_NAME, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab[1:20], las = 2)
lis <- names(tab)[1:10]
#lis <- lis[-which(lis == "FISHES,BONY,UNSPECIFIED")]

# recalculate index for individual spp --------------
dstfin <- c()
par(mfrow = c(2, 5), mex = 0.5)

for (j in 1:10) {
  d1 <- d[which(d$ITIS_COMMON_NAME == lis[j]), ]
  d1$YEAR_LANDED <- factor(d1$YEAR_LANDED, levels = styear:enyear)
  an <- tapply(d1$ADJUSTED_POUNDS, list(d1$MONTH_LANDED, d1$YEAR_LANDED), sum, na.rm = T)
  an2 <- an
  for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
  matplot(an2, type = "b", pch = pc, main = lis[j])

  dst <- rep(NA, ncol(an2))
  for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
  dstfin <- cbind(dstfin, dst) 
}

selec <- c(1, 3, 5, 7, 8, 9)   # snappers, groupers subject to seasonal closure
lis[selec]

# look at correlations across spp --------------------

dstfin[which(is.na(dstfin))] <- 0
colnames(dstfin) <- lis
cor(dstfin)
plot(data.frame(dstfin))
dev.off()

matplot(styear: enyear, dstfin, type = "b")
rownames(dstfin) <- styear: enyear
dstfin

avdst <- rowMeans(dstfin[, selec])   
sddst <- apply(dstfin[, selec], 1, sd)
plot(styear: enyear, avdst, type = "b")

# save data for PR -----------------
datdata <- styear: enyear
inddata <- data.frame(avdst)
llidata <- data.frame(avdst - sddst)
ulidata <- data.frame(avdst + sddst)

final_PR <- data.frame(datdata, inddata, llidata, ulidata)
names(final_PR) <- c("year", "ind", "lli", "uli")


# for STT --------------------------------------------------------

rm(list = ls()[-match(c("final_PR", "styear", "enyear", "confpath"), ls())])

# calculate for STT  --------------------------------------

dat <- read.csv(paste0(confpath, "STT_2024.csv"))

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
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
d$TRIP_YEAR <- factor(d$TRIP_YEAR, levels = c(styear: enyear))
table(d$TRIP_YEAR)


names(d)[which(names(d) == "TRIP_YEAR")] <- "YEAR_LANDED"
names(d)[which(names(d) == "TRIP_MONTH")] <- "MONTH_LANDED"
names(d)[which(names(d) == "SPECIES_NM")] <- "ITIS_COMMON_NAME"
table(d$YEAR_LANDED)

# look at seasonal cycles ---------------------------------------

an <- tapply(d$POUNDS_LANDED, list(d$MONTH_LANDED, d$YEAR_LANDED), sum, na.rm = T)
dim(an)
matplot(an, type = "l", col = rainbow(dim(an)[2]))

an2 <- an
for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
pc <- c(letters[1:26], LETTERS[1:12])
matplot(an2, col = rainbow(dim(an)[2]), type = "b", pch = pc)

av <- apply(an2, 1, mean, na.rm = T)
er <- apply(an2, 1, sd, na.rm = T)*1.96

# identify anomalous years and highlight -----------------------
anomlis <- c(2017, 2020)
cols <- rep("#00000060", dim(an)[2])
cols[which(colnames(an2) == 2017)] <- 3
cols[which(colnames(an2) == 2020)] <- 4
lwds <- rep(1, dim(an)[2])
lwds[which(colnames(an2) == 2017)] <- 3
lwds[which(colnames(an2) == 2020)] <- 3

matplot(an2, col = cols, type = "l", lty = 1, lwd = lwds, axes = F, ylab = "proportion of landings", 
        main = "Distribution of landings throughout the year")
axis(1, at = 1:12, lab = month.abb[c(7:12, 1:6)])
axis(2, las = 2); box()
text(2, 0.11, col = 4, "2020", cex = 1.2, font = 2)
text(4, 0.025, col = 3, "2017", cex = 1.2, font = 2)
#lines(1:12, av, lwd = 3)
#polygon(c(1:12, 12:1), c((av + er), (av - er)[12:1]), col = "#00000020", border = NA)
#legend(10, 0.17, colnames(an)[which(pc %in% anomlis)], lty = 1, lwd = 2, col = 3:4, bty = "n")
#legend(7, 0.17, c("average", "95% C.I."), lwd = c(3, 0), pch = c(0, 15), col = c(1, 8), pt.cex = c(0, 2), bty = "n")

# calculate disturbance indicator ---------------------------

dst <- rep(NA, ncol(an2))
for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
plot(styear: enyear, dst, type = "b")

# now for separate species ----------------------------------

ref <- read.csv("indicator_processing/fishery_dependent/spp_ref_STT_manualEdit.csv")
head(ref)
head(d)

db <- merge(d, ref, by.x = "ITIS_COMMON_NAME", by.y = "COMname", all.x = TRUE)
dim(d)
dim(db)
head(db)

which(is.na(db$famcode))
table(db$famcode, useNA = "always")

#tab <- sort(tapply(d$POUNDS_LANDED, d$ITIS_COMMON_NAME, sum, na.rm = T), decreasing = T)
tab <- sort(tapply(db$POUNDS_LANDED, db$famcode, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab[1:20], las = 2)
lis <- names(tab)[1:10]
#lis <- lis[-which(lis == "UNID")]

# recalculate index for individual spp --------------
dstfin <- c()
par(mfrow = c(2, 5), mex = 0.5)

for (j in 1:10) {
  d1 <- db[which(db$famcode == lis[j]), ]
  d1$YEAR_LANDED <- factor(d1$YEAR_LANDED, levels = styear:enyear)
  an <- tapply(d1$POUNDS_LANDED, list(d1$MONTH_LANDED, d1$YEAR_LANDED), sum, na.rm = T)
  an2 <- an
  for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
  matplot(an2, type = "b", pch = pc, main = lis[j])
  
  dst <- rep(NA, ncol(an2))
  for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
  dstfin <- cbind(dstfin, dst) 
}

selec <- c(1, 3, 5, 6)
lis[selec]

# look at correlations across spp --------------------

dstfin[which(is.na(dstfin))] <- 0
colnames(dstfin) <- lis
cor(dstfin)
plot(data.frame(dstfin))
dev.off()

matplot(styear: enyear, dstfin, type = "b")
rownames(dstfin) <- styear:enyear
dstfin   # looks only reasonable starting in 2000

dstfin[which(styear: enyear < 2000), ] <- NA
dstfin[which(styear:enyear == 2022), ] <- NA
dstfin 

avdst <- rowMeans(dstfin[, selec])   # snappers, groupers subject to seasonal closure
sddst <- apply(dstfin[, selec], 1, sd)
plot(styear: enyear, avdst, type = "b")

# save data for STT -----------------
inddata <- data.frame(avdst)
llidata <- data.frame(avdst - sddst)
ulidata <- data.frame(avdst + sddst)

final_ST <- data.frame(inddata, llidata, ulidata)
names(final_ST) <- c("ind", "lli", "uli")

#####################  END STT  ######################

# for STX --------------------------------------------------------

rm(list = ls()[-match(c("final_PR", "final_ST", "styear", "enyear", "confpath"), ls())])

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

names(d)[which(names(d) == "TRIP_YEAR")] <- "YEAR_LANDED"
names(d)[which(names(d) == "TRIP_MONTH")] <- "MONTH_LANDED"
names(d)[which(names(d) == "SPECIES_NM")] <- "ITIS_COMMON_NAME"
table(d$YEAR_LANDED)

# look at seasonal cycles ---------------------------------------

an <- tapply(d$POUNDS_LANDED, list(d$MONTH_LANDED, d$YEAR_LANDED), sum, na.rm = T)
dim(an)
matplot(an, type = "l", col = rainbow(dim(an)[2]))

an2 <- an
for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
pc <- c(letters[1:26], LETTERS[1:12])
matplot(an2, col = rainbow(dim(an)[2]), type = "b", pch = pc)

av <- apply(an2, 1, mean, na.rm = T)
er <- apply(an2, 1, sd, na.rm = T)*1.96

# identify anomalous years and highlight -----------------------
anomlis <- c(2017, 2019)
cols <- rep("#00000060", dim(an)[2])
cols[which(colnames(an2) == 2017)] <- 3
cols[which(colnames(an2) == 2019)] <- 4
lwds <- rep(1, dim(an)[2])
lwds[which(colnames(an2) == 2017)] <- 3
lwds[which(colnames(an2) == 2019)] <- 3

matplot(an2, col = cols, type = "l", lty = 1, lwd = lwds, axes = F, ylab = "proportion of landings", 
        main = "Distribution of landings throughout the year")
axis(1, at = 1:12, lab = month.abb[c(7:12, 1:6)])
axis(2, las = 2); box()
text(2, 0.11, col = 4, "2019", cex = 1.2, font = 2)
text(8, 0.025, col = 3, "2017", cex = 1.2, font = 2)

# calculate disturbance indicator ---------------------------

dst <- rep(NA, ncol(an2))
for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
plot(styear: enyear, dst, type = "b")

# now for separate species ----------------------------------

ref <- read.csv("indicator_processing/fishery_dependent/spp_ref_STX_manualEdit.csv")
head(ref)
head(d)

db <- merge(d, ref, by.x = "ITIS_COMMON_NAME", by.y = "COMname", all.x = TRUE)
dim(d)
dim(db)
head(db)
which(is.na(db$famcode))
db$ITIS_COMMON_NAME[which(is.na(db$famcode))]
table(db$famcode, useNA = "always")

#tab <- sort(tapply(d$POUNDS_LANDED, d$ITIS_COMMON_NAME, sum, na.rm = T), decreasing = T)
tab <- sort(tapply(db$POUNDS_LANDED, db$famcode, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab[1:20], las = 2)
lis <- names(tab)[1:10]
#lis <- lis[-which(lis == "UNID")]

# recalculate index for individual spp --------------
dstfin <- c()
par(mfrow = c(2, 5), mex = 0.5)

for (j in 1:10) {
  d1 <- db[which(db$famcode == lis[j]), ]
  d1$YEAR_LANDED <- factor(d1$YEAR_LANDED, levels = styear:enyear)
  an <- tapply(d1$POUNDS_LANDED, list(d1$MONTH_LANDED, d1$YEAR_LANDED), sum, na.rm = T)
  an2 <- an
  for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
  matplot(an2, type = "b", pch = pc, main = lis[j])
  
  dst <- rep(NA, ncol(an2))
  for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
  dstfin <- cbind(dstfin, dst) 
}

selec <- c(2, 4, 5, 8)
lis[selec]

# look at correlations across spp --------------------

dstfin[which(is.na(dstfin))] <- 0
colnames(dstfin) <- lis
cor(dstfin)
plot(data.frame(dstfin))
dev.off()

matplot(styear:enyear, dstfin, type = "b")
rownames(dstfin) <- styear:enyear
dstfin   # only have data starting in 2011

dstfin[which(styear:enyear < 2011), ] <- NA
dstfin[which(styear:enyear == 2022), ] <- NA

avdst <- rowMeans(dstfin[, selec])   #
sddst <- apply(dstfin[, selec], 1, sd)
plot(styear: enyear, avdst, type = "b")


# format as indicator object ---------------------

datdata <- final_PR$year
inddata <- data.frame(final_PR$ind)
llidata <- data.frame(final_PR$lli)
ulidata <- data.frame(final_PR$uli)

inddata[, 2] <- final_ST$ind
llidata[, 2] <- final_ST$lli
ulidata[, 2] <- final_ST$uli

inddata[, 3] <- avdst
llidata[, 3] <- avdst - sddst
ulidata[, 3] <- avdst + sddst

labs <- c("Disturbance indicator", "difference from mean landings", "Puerto Rico", 
          "Disturbance indicator", "difference from mean landings", "St. Thomas and St. John", 
          "Disturbance indicator", "difference from mean landings", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

# plot and save -------------------------------------
plotIndicatorTimeSeries(s, sublabel = T, coltoplot = 1:3, plotrownum = 3, yposadj = 1.2, type = "allLines", 
                        sameYscale = F, trendAnalysis = F)

ind <- s
save(ind, file = "indicator_objects/disturbance.RData")

##########################  END  #############################

print("disturbance -- SUCCESSFULLY RUN")
