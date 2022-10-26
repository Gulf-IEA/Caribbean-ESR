
rm(list = ls())

library(pals)
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")

d <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STT_landings.csv")

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
table(d$TOTAL_TRAPS_IN_WATER, useNA = "always")
table(d$FAD_CD, useNA = "always")
table(d$GEAR_TYPE_NM, useNA = "always")
table(d$GEAR1_NAME, useNA = "always")
table(d$SPECIES_CD, useNA = "always")
table(d$SPECIES_NM, useNA = "always")
table(d$TRIP_MAY_BE_DUPLICATE, useNA = "always")

# look at gear trends ---------------------------------
tapply(d$POUNDS_LANDED, d$GEAR_TYPE_NM, sum, na.rm = T)
par(mar = c(5, 15, 2, 2))
barplot(tapply(d$POUNDS_LANDED, d$GEAR_TYPE_NM, sum, na.rm = T), horiz = T, las = 2)

d$gear <- "other" 
d$gear[which(d$GEAR_TYPE_NM == "TRAPS")] <- "traps"

par(mar = c(4, 4, 1, 1))
tab <- tapply(d$POUNDS_LANDED, list(d$gear, d$TRIP_YEAR), sum, na.rm = T)
barplot(tab, col = c(2, gray(0.1), gray(0.5), gray(0.8)), args.legend = c(x = "topright"), legend.text = rownames(tab))

tab2 <- tab
for (i in 1:ncol(tab)) {   tab2[, i] <- tab[, i] / sum(tab[, i])   }

barplot(tab2, col = c(2, gray(0.2), gray(0.4), gray(0.6)))

numtrap <- tab[2,] / (10^6)
pertrap <- 1 - tab2[1, ]

# format into indicator object --------------------------------------
#datdata <- 1974:2021
#inddata <- data.frame(cbind(numtrap, pertrap))
#labs <- c("Number of traps", "millions of traps" , "St. Thomas", "Proportion of effort from traps", "proportion", "St. Thomas")
#indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

#s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
#class(s) <- "indicatordata"

#plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T) # outtype = "png")

# look at main species landed --------------------------------
tab <- sort(tapply(d$POUNDS_LANDED, d$SPECIES_NM, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab, las = 2)
barplot(tab[1:100], las = 2)
barplot(tab[1:50], las = 2)

tab <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1)+1)
matplot(1974:2021, t(tab[1:10, ]), type = "l", col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
legend("topright", rownames(tab)[1:10], col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
abline(v = 1999)   # 2000 is data-rich period

d <- d[which(d$TRIP_YEAR >= 2010), ]  # cut out because very little data beforehand

yrs <- 2010:2021

tab <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1)+1)
matplot(yrs, t(tab[1:10, ]), type = "l", col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
legend("topright", rownames(tab)[1:10], col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)

tab2 <- tab[1:10, ]
for (i in 1:ncol(tab)) {  tab2[, i] <- tab[1:10, i] / sum(tab[1:10, i], na.rm = T)   }
barplot(tab2, col = glasbey(10), xlim = c(0, 56), legend.text = rownames(tab2), args.legend = c(x = "right"))


# pull in reference file and merge ------------------------------------

ref <- read.csv("spp_ref_STT_manualEdit.csv")
head(ref)
head(d)

hist(ref$Lmax)
table(cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000)))
ref$Lmax_cat <- cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000))

db <- merge(d, ref, by.x = "SPECIES_NM", by.y = "COMname", all.x = TRUE)
dim(d)
dim(db)

tab <- tapply(db$POUNDS_LANDED, list(db$famcode, db$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]
head(tab, 15)

nsp <- 15
cols <- glasbey(nsp)

par(mar = c(4, 4, 1, 1)+1)
matplot(yrs, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)

tab2 <- tab[1:(nsp-1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"

tab3 <- tab2
for (i in 1:ncol(tab2)) {  tab3[, i] <- tab2[, i] / sum(tab2[, i], na.rm = T)   }

cols[which(rownames(tab3) == "UNID")] <- "white"
barplot(tab3, col = cols, xlim = c(0, 18), legend.text = rownames(tab2), args.legend = c(x = "right"), las = 2)

#png(filename="PR_proportion_landings.png", units="in", width=8, height=5, pointsize=12, res=72*10)
#barplot(tab3[, 23:38], col = cols, xlim = c(0, 23), legend.text = rownames(tab2), args.legend = c(x = "right", bty = "n"), las = 2, space = 0, border = NA, 
#        xlab = "", ylab = "proportion of catch", main = "Puerto Rico landings composition by year                                ")
#abline(h = 0)
dev.off()

#table(db$SPECIES_NM == db$COMname)
table(db$SPECIES_NM[which(is.na(db$Lmax))])

length(which(db$ITIS_COMMON_NAME == "FISHES,BONY,UNSPECIFIED"))
table(db$PD, useNA = "always")
table(db$Lmax, useNA = "always")

# remove invertebrates and unidentified fish ----------------
dbf <- db[which(db$PD != "invert"), ]
dbf <- dbf[which(dbf$SPECIES_NM != "FISHES,BONY,UNSPECIFIED"), ]
dbf <- dbf[which(dbf$SPECIES_NM != "OTHER SPECIES"), ]
table(dbf$SPECIES_NM[which(is.na(dbf$Lmax))])

head(dbf)
dim(dbf)
table(dbf$PD, useNA = "always")
table(dbf$Lmax, useNA = "always")

# calculate P:D ratio ---------------------------------------

dbf$PD2 <- "demersal"
dbf$PD2[grep("pelagic", dbf$PD)] <- "pelagic"
table(dbf$PD, dbf$PD2)

pd <- tapply(dbf$POUNDS_LANDED, list(dbf$TRIP_YEAR, dbf$PD2), sum, na.rm = T)
pdrat <- pd[, 2] / pd[, 1]
plot(yrs, pdrat, type = "b")

# make indicator object and plot P:D ratio ------------------
datdata <- yrs
inddata <- data.frame(cbind(pd[, 2]/10^6, pd[, 1]/10^6, pdrat))
labs <- c("Total pelagic landings", "millions of pounds" , "St. Thomas", 
          "Total demersal landings", "millions of pounds", "St. Thomas", 
          "Pelagic:demersal ratio", "ratio of landings", "St. Thomas")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, sublabel = T, trendAnalysis = F) #, outtype = "png")

# combine with PR ------------------------

load("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data/PDRatioPR.RData")
pdrat
pdrat2

pdrat <- c(rep(NA, 10), pdrat)
pdrat2 <- c(pdrat2, NA)

datdata <- 2000:2021
inddata <- data.frame(cbind(pdrat2, pdrat))
labs <- c("Pelagic to demersal ratio", "ratio of landings", "Puerto Rico", 
          "Pelagic to demersal ratio", "ratio of landings", "St. Thomas and St. John")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_plots/")
plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T, sameYscale = T)
plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T, sameYscale = T, hgtadj = 0.8, outtype = "png")


# Lmax calculations -----------------------------------------

tab <- tapply(dbf$POUNDS_LANDED, list(dbf$SPECIES_NM, dbf$TRIP_YEAR), sum, na.rm = T)
tab[is.na(tab)] <- 0

splis <- data.frame(rownames(tab))
names(splis) <- "COMname"

splisref <- merge(splis, ref, by = "COMname", all.x = TRUE)
dim(splis)
dim(splisref)
table(rownames(tab) == splisref$COMname)

llis <- levels(splisref$Lmax_cat)
Lmax1 <- colSums(tab[which(splisref$Lmax_cat == llis[1]), ])
Lmax2 <- colSums(tab[which(splisref$Lmax_cat == llis[2]), ])
Lmax3 <- colSums(tab[which(splisref$Lmax_cat == llis[3]), ])
Lmax4 <- colSums(tab[which(splisref$Lmax_cat == llis[4]), ])
Lmax5 <- colSums(tab[which(splisref$Lmax_cat == llis[5]), ])
#Lmax6 <- colSums(tab[which(splisref$Lmax_cat == llis[6]), ])

Lmaxcl <- cbind(Lmax1, Lmax2, Lmax3, Lmax4, Lmax5) #, Lmax6)

Lmaxcl2 <- Lmaxcl
for (i in 1:nrow(Lmaxcl)) {  Lmaxcl2[i, ] <- Lmaxcl[i, ] / sum(Lmaxcl[i, ], na.rm = T)   }

matplot(Lmaxcl)
barplot(t(Lmaxcl2), col = 1:6, las = 2, main = "Distribution of catch in Lmax size classes                      ", 
        ylab = "proportion", xlim = c(1, 30), 
        legend = c("<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm"), args.legend = c(x = "right", bty = "n"))

# format indicator objects and plot ---------------------------------------
datdata <- yrs
inddata <- data.frame(Lmaxcl/10^6)
labs <- c(rep("Total landings in Lmax class", 5), 
          rep("millions of pounds", 5),
          "<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
plotIndicatorTimeSeries(s, coltoplot = 1:5, plotrownum = 5, sublabel = T, widadj = 1.5) #, outtype = "png")

inddata <- data.frame(Lmaxcl2)
labs <- c(rep("Proportion of landings in Lmax class",5), 
          rep("proportion", 5),
          "<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
plotIndicatorTimeSeries(s, coltoplot = 1:5, plotrownum = 5, sublabel = T, widadj = 1.5, hgtadj = 0.6) # outtype = "png", sameYscale = F)

# understand what is driving the trends -------------------------------------------

splisref$COMname[which(splisref$Lmax_cat == "(60,100]")]
splisref$COMname[which(splisref$Lmax_cat == "(100,200]")]

sort(table(splisref$famcode[which(splisref$Lmax_cat == "(60,100]")]))
sort(table(splisref$famcode[which(splisref$Lmax_cat == "(100,200]")]))

splisref$recLand <- rowSums(tab)

plate <- splisref[which(splisref$Lmax_cat == "(60,100]"), ]
head(plate[order(plate$recLand, decreasing = T), ], 15)

big <- splisref[which(splisref$Lmax_cat == "(100,200]"), ]
head(big[order(big$recLand, decreasing = T), ], 15)

# calculate average Lmax indicator -----------------------------------------------

lmax <- rep(NA, ncol(tab))
for (i in 1:ncol(tab))  {  lmax[i] <- weighted.mean(splisref$Lmax, tab[, i])  }

plot(yrs, lmax)
#plot(2000:2020, lmax[18:38], type = "b", las = 2)
axis(1, 2000:2021)
out <- lm(lmax ~ yrs)
summary(out)
abline(out)

plot(yrs, tapply(dbf$Lmax, dbf$TRIP_YEAR, mean, na.rm = T))

datdata <- yrs
inddata <- data.frame(lmax)
labs <- c("Average maximum length of catch", "length (cm)" , "Puerto Rico")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T) # outtype = "png")

# figure out what is going on in 2018 with spike in pelagics -------------------

plot(1983:2020, pdrat, type = "b", las = 2)
matplot(1983:2020, pd, axes = F, type = "b")
axis(1, at = 1983:2020, las = 2)

tabp <- tab[grep("pelagic", splisref$PD), ]
tabp <- tabp[order(rowSums(tabp), decreasing = T), ]

matplot(1983:2020, t(tabp[1:20, ]), type = "l", col = glasbey(10), lwd = 2, lty = 1)
legend("topright", rownames(tabp)[1:20], col = glasbey(10), lwd = 2, lty = 1)

tabd <- tab[-grep("pelagic", splisref$PD), ]
tabd <- tabd[order(rowSums(tabd), decreasing = T), ]

matplot(1983:2020, t(tabd[1:20, ]), type = "b", col = glasbey(10), lwd = 2, lty = 1, pch = 19, las = 2)
legend("topright", rownames(tabd)[1:20], col = glasbey(10), lwd = 2, lty = 1)


