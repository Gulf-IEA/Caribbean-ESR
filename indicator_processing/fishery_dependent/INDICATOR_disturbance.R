
rm(list = ls())
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")

# for PUERTO RICO -----------------------------------------------
d <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/PR_landings_83_20.csv")

# look at seasonal cycles ---------------------------------------

an <- tapply(d$POUNDS_LANDED, list(d$MONTH_LANDED, d$YEAR_LANDED), sum, na.rm = T)
dim(an)
matplot(an, type = "l", col = rainbow(38))
yrs <- as.numeric(colnames(an))

an2 <- an
for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
pc <- c(letters[1:26], LETTERS[1:12])
matplot(an2, col = rainbow(38), type = "b", pch = pc)

av <- apply(an2, 1, mean)
er <- apply(an2, 1, sd)*1.96

# identify anomalous years and highlight -----------------------
anomlis <- c(2017, 2020)
cols <- rep("#00000060", 38)
cols[which(colnames(an2) == 2017)] <- 3
cols[which(colnames(an2) == 2020)] <- 4
lwds <- rep(1, 38)
lwds[which(colnames(an2) == 2017)] <- 3
lwds[which(colnames(an2) == 2020)] <- 3

matplot(an2, col = cols, type = "l", lty = 1, lwd = lwds, axes = F, ylab = "proportion of landings", 
        main = "Distribution of landings throughout the year")
axis(1, at = 1:12, lab = month.abb, las = 2)
axis(2, las = 2); box()
text(3, 0.15, col = 4, "2020", cex = 1.2, font = 2)
text(8, 0.025, col = 3, "2017", cex = 1.2, font = 2)
lines(1:12, av, lwd = 3)
#polygon(c(1:12, 12:1), c((av + er), (av - er)[12:1]), col = "#00000020", border = NA)
#legend(10, 0.17, colnames(an)[which(pc %in% anomlis)], lty = 1, lwd = 2, col = 3:4, bty = "n")
legend("topright", c("average seasonality", "individual years"), lwd = c(3, 1), lty = c(1, 1), col = c(1, 8), pt.cex = c(0, 2), bty = "n", cex= 1.1)

# calculate disturbance indicator ---------------------------

dst <- rep(NA, ncol(an2))
for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
plot(yrs, dst, type = "b")

# now for separate species ----------------------------------

tab <- sort(tapply(d$POUNDS_LANDED, d$ITIS_COMMON_NAME, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab[1:20], las = 2)
lis <- names(tab)[1:9]
lis <- lis[-which(lis == "FISHES,BONY,UNSPECIFIED")]

# recalculate index for individual spp --------------
dstfin <- c()
par(mfrow = c(2, 4), mex = 0.5)

for (j in 1:8) {
  d1 <- d[which(d$ITIS_COMMON_NAME == lis[j]), ]
  an <- tapply(d1$POUNDS_LANDED, list(d1$MONTH_LANDED, d1$YEAR_LANDED), sum, na.rm = T)
  an2 <- an
  for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
  matplot(an2, type = "b", pch = pc, main = lis[j])

  dst <- rep(NA, ncol(an2))
  for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
  dstfin <- cbind(dstfin, dst) 
}

# look at correlations across spp --------------------

dstfin[which(is.na(dstfin))] <- 0
colnames(dstfin) <- lis
cor(dstfin)
plot(data.frame(dstfin))
matplot(yrs, dstfin, type = "b")

selec <- c(1, 4, 5, 6)
lis[selec]
avdst <- rowMeans(dstfin[, selec])   # snappers, groupers subject to seasonal closure
sddst <- apply(dstfin[, selec], 1, sd)
plot(yrs, avdst, type = "b")

# format as indicator object and plot -----------------
datdata <- min(yrs): max(yrs)
inddata <- data.frame(avdst)
llidata <- data.frame(avdst - sddst)
ulidata <- data.frame(avdst + sddst)

final_PR <- data.frame(datdata, inddata, llidata, ulidata)
names(final_PR) <- c("year", "ind", "lli", "uli")


# for STT --------------------------------------------------------
rm(list = ls()[which(ls() != "final_PR")])
d <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STT_landings.csv")

names(d)[which(names(d) == "TRIP_YEAR")] <- "YEAR_LANDED"
names(d)[which(names(d) == "TRIP_MONTH")] <- "MONTH_LANDED"
names(d)[which(names(d) == "SPECIES_NM")] <- "ITIS_COMMON_NAME"
table(d$YEAR_LANDED)

d <- d[which(d$YEAR_LANDED >= 2000), ]
d <- d[which(d$YEAR_LANDED <= 2020), ]

# look at seasonal cycles ---------------------------------------

an <- tapply(d$POUNDS_LANDED, list(d$MONTH_LANDED, d$YEAR_LANDED), sum, na.rm = T)
dim(an)
matplot(an, type = "l", col = rainbow(38))
yrs <- as.numeric(colnames(an))

an2 <- an
for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
pc <- c(letters[1:26], LETTERS[1:12])
matplot(an2, col = rainbow(38), type = "b", pch = pc)

av <- apply(an2, 1, mean)
er <- apply(an2, 1, sd)*1.96

# identify anomalous years and highlight -----------------------
anomlis <- c(2017, 2020)
cols <- rep("#00000060", 38)
cols[which(colnames(an2) == 2017)] <- 3
cols[which(colnames(an2) == 2020)] <- 4
lwds <- rep(1, 38)
lwds[which(colnames(an2) == 2017)] <- 3
lwds[which(colnames(an2) == 2020)] <- 3

matplot(an2, col = cols, type = "l", lty = 1, lwd = lwds, axes = F, ylab = "proportion of landings", 
        main = "Distribution of landings throughout the year")
axis(1, at = 1:12, lab = month.abb)
axis(2, las = 2); box()
text(3, 0.15, col = 4, "2020", cex = 1.2, font = 2)
text(8, 0.025, col = 3, "2017", cex = 1.2, font = 2)
#lines(1:12, av, lwd = 3)
#polygon(c(1:12, 12:1), c((av + er), (av - er)[12:1]), col = "#00000020", border = NA)
#legend(10, 0.17, colnames(an)[which(pc %in% anomlis)], lty = 1, lwd = 2, col = 3:4, bty = "n")
#legend(7, 0.17, c("average", "95% C.I."), lwd = c(3, 0), pch = c(0, 15), col = c(1, 8), pt.cex = c(0, 2), bty = "n")

# calculate disturbance indicator ---------------------------

dst <- rep(NA, ncol(an2))
for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
plot(yrs, dst, type = "b")

# now for separate species ----------------------------------

ref <- read.csv("spp_ref_STT_manualEdit.csv")
head(ref)
head(d)

db <- merge(d, ref, by.x = "ITIS_COMMON_NAME", by.y = "COMname", all.x = TRUE)
dim(d)
dim(db)

tab <- sort(tapply(db$POUNDS_LANDED, db$famcode, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab[1:20], las = 2)
lis <- names(tab)[1:8]
#lis <- lis[-which(lis == "FISHES,BONY,UNSPECIFIED")]

# recalculate index for individual spp --------------
dstfin <- c()
par(mfrow = c(2, 4), mex = 0.5)

for (j in 1:8) {
  d1 <- db[which(db$famcode == lis[j]), ]
  an <- tapply(d1$POUNDS_LANDED, list(d1$MONTH_LANDED, d1$YEAR_LANDED), sum, na.rm = T)
  an2 <- an
  for (i in 1:ncol(an)) { an2[, i] <- an[, i] / sum(an[, i]) }
  matplot(an2, type = "b", pch = pc, main = lis[j])
  
  dst <- rep(NA, ncol(an2))
  for (i in 1:ncol(an2)) {  dst[i] <- sum((an2[, i] - apply(an2, 1, mean, na.rm = T))^2)   }
  dstfin <- cbind(dstfin, dst) 
}

# look at correlations across spp --------------------

dstfin[which(is.na(dstfin))] <- 0
colnames(dstfin) <- lis
cor(dstfin)
plot(data.frame(dstfin))
matplot(yrs, dstfin, type = "b")

selec <- c(1, 3, 5, 6)
lis[selec]
avdst <- rowMeans(dstfin[, selec])   # snappers, groupers subject to seasonal closure
sddst <- apply(dstfin[, selec], 1, sd)
plot(yrs, avdst, type = "b")

# format as indicator object ---------------------

yrind <- which(final_PR$year %in% yrs)

datdata <- final_PR$year
inddata <- data.frame(final_PR$ind)
llidata <- data.frame(final_PR$lli)
ulidata <- data.frame(final_PR$uli)

inddata[yrind, 2] <- avdst
llidata[yrind, 2] <- avdst - sddst
ulidata[yrind, 2] <- avdst + sddst

labs <- c("Disturbance indicator", "difference from mean landings", "Puerto Rico", 
          "Disturbance indicator", "difference from mean landings", "St. Thomas and St. John")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

# plot and save -------------------------------------
plotIndicatorTimeSeries(s, sublabel = T, coltoplot = 1:2, plotrownum = 2, yposadj = 1.2, type = "allLines")

inddata <- s
save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/disturbance.RData")





