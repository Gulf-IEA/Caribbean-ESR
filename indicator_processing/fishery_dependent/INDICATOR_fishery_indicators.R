
rm(list = ls())

library(pals)
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")

d <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/PR_landings_83_20.csv")

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
hist(d$VALUE_IN_DOLLARS)
hist(d$PRICE_PER_LB)
table(d$DISTANCE, useNA = "always")
table(d$DISTANCE_DESCRIBE, useNA = "always")
#table(d$TRIP_TICKET_NUMBER_ED, useNA = "always")

# look at gear trends ---------------------------------
tapply(d$POUNDS_LANDED, d$FIN_GEAR_NAME, sum, na.rm = T)
par(mar = c(5, 15, 2, 2))
barplot(tapply(d$POUNDS_LANDED, d$FIN_GEAR_NAME, sum, na.rm = T), horiz = T, las = 2)

d$gear <- "other" 
d$gear[grep("POTS", d$FIN_GEAR_NAME)] <- d$FIN_GEAR_NAME[grep("POTS", d$FIN_GEAR_NAME)]

par(mar = c(4, 4, 1, 1))
tab <- tapply(d$POUNDS_LANDED, list(d$gear, d$YEAR_LANDED), sum, na.rm = T)
barplot(tab, col = c(2, gray(0.1), gray(0.5), gray(0.8)), args.legend = c(x = "topright"), legend.text = rownames(tab))

tab2 <- tab
for (i in 1:ncol(tab)) {   tab2[, i] <- tab[, i] / sum(tab[, i])   }

barplot(tab2, col = c(2, gray(0.2), gray(0.4), gray(0.6)))

numtrap <- colSums(tab[2:4, ]) / (10^6)
pertrap <- 1 - tab2[1, ]

# format into indicator object --------------------------------------
datdata <- 1983:2020
inddata <- data.frame(cbind(numtrap, pertrap))
labs <- c("Number of traps", "millions of traps" , "Puerto Rico", "Proportion of effort from traps", "proportion", "Puerto Rico")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

#plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T, outtype = "png")

# look at main species landed --------------------------------
tab <- sort(tapply(d$POUNDS_LANDED, d$ITIS_COMMON_NAME, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab, las = 2)
barplot(tab[1:100], las = 2)
barplot(tab[1:50], las = 2)

tab <- tapply(d$POUNDS_LANDED, list(d$ITIS_COMMON_NAME, d$YEAR_LANDED), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1)+1)
matplot(1983:2020, t(tab[1:10, ]), type = "l", col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
legend("topright", rownames(tab)[1:10], col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)

tab2 <- tab[1:10, ]
for (i in 1:ncol(tab)) {  tab2[, i] <- tab[1:10, i] / sum(tab[1:10, i], na.rm = T)   }

barplot(tab2, col = glasbey(10), xlim = c(0, 56), legend.text = rownames(tab2), args.legend = c(x = "right"))

rownames(tab)[4]
plot(1983:2020, tab[4, ]/colSums(tab, na.rm = T), main = "proportion of unidentified landings by year", type = "b")
abline(h = 0.02)
abline(v = 2005) # 2000 and on is 'data rich' period
abline(h = 0.1)
abline(v = 2000)

# pull in reference file and merge ------------------------------------

ref <- read.csv("spp_ref_manualEdit.csv")
head(ref)
head(d)

hist(ref$Lmax)
table(cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000)))
ref$Lmax_cat <- cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000))

db <- merge(d, ref, by.x = "ITIS_SCIENTIFIC_NAME", by.y = "SCIname", all.x = TRUE)
dim(d)
dim(db)

tab <- tapply(db$POUNDS_LANDED, list(db$famcode, db$YEAR_LANDED), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]
head(tab, 15)

nsp <- 15
cols <- glasbey(nsp)

par(mar = c(4, 4, 1, 1)+1)
matplot(1983:2020, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)

tab2 <- tab[1:(nsp-1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"

tab3 <- tab2
for (i in 1:ncol(tab2)) {  tab3[, i] <- tab2[, i] / sum(tab2[, i], na.rm = T)   }

cols[which(rownames(tab3) == "UNID")] <- "white"
barplot(tab3, col = cols, xlim = c(0, 56), legend.text = rownames(tab2), args.legend = c(x = "right"), las = 2)

png(filename="PR_proportion_landings.png", units="in", width=8, height=5, pointsize=12, res=72*10)
barplot(tab3[, 23:38], col = cols, xlim = c(0, 23), legend.text = rownames(tab2), args.legend = c(x = "right", bty = "n"), las = 2, space = 0, border = NA, 
        xlab = "", ylab = "proportion of catch", main = "Puerto Rico landings composition by year                                ")
abline(h = 0)
dev.off()

table(db$ITIS_COMMON_NAME == db$COMname)
length(which(db$ITIS_COMMON_NAME == "FISHES,BONY,UNSPECIFIED"))
table(db$PD, useNA = "always")
table(db$Lmax, useNA = "always")

# remove invertebrates and unidentified fish ----------------
dbf <- db[which(db$PD != "invert"), ]
dbf <- dbf[which(dbf$ITIS_COMMON_NAME != "FISHES,BONY,UNSPECIFIED"), ]
dim(dbf)
table(dbf$PD, useNA = "always")
table(dbf$Lmax, useNA = "always")

# calculate P:D ratio ---------------------------------------

dbf$PD2 <- "demersal"
dbf$PD2[grep("pelagic", dbf$PD)] <- "pelagic"
table(dbf$PD, dbf$PD2)

pd <- tapply(dbf$POUNDS_LANDED, list(dbf$YEAR_LANDED, dbf$PD2), sum, na.rm = T)
pdrat <- pd[, 2] / pd[, 1]
plot(1983:2020, pdrat, type = "b")

pd2 <- pd[18:38, ]
pdrat2 <- pd2[, 2] / pd2[, 1]

# make indicator object and plot P:D ratio ------------------
datdata <- 2000:2020
inddata <- data.frame(cbind(pd2[, 2]/10^6, pd2[, 1]/10^6, pdrat2))
labs <- c("Total pelagic landings", "millions of pounds" , "Puerto Rico", 
          "Total demersal landings", "millions of pounds", "Puerto Rico", 
          "Pelagic:demersal ratio", "ratio of landings", "Puerto Rico")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

#plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, sublabel = T, outtype = "png")


# Lmax calculations -----------------------------------------

tab <- tapply(dbf$POUNDS_LANDED, list(dbf$ITIS_SCIENTIFIC_NAME, dbf$YEAR_LANDED), sum, na.rm = T)
tab[is.na(tab)] <- 0

splis <- data.frame(rownames(tab))
names(splis) <- "SCIname"

splisref <- merge(splis, ref, by = "SCIname", all.x = TRUE)
table(rownames(tab) == splisref$SCIname)

llis <- levels(splisref$Lmax_cat)
Lmax1 <- colSums(tab[which(splisref$Lmax_cat == llis[1]), ])
Lmax2 <- colSums(tab[which(splisref$Lmax_cat == llis[2]), ])
Lmax3 <- colSums(tab[which(splisref$Lmax_cat == llis[3]), ])
Lmax4 <- colSums(tab[which(splisref$Lmax_cat == llis[4]), ])
Lmax5 <- colSums(tab[which(splisref$Lmax_cat == llis[5]), ])
#Lmax6 <- colSums(tab[which(splisref$Lmax_cat == llis[6]), ])

Lmaxcl <- cbind(Lmax1, Lmax2, Lmax3, Lmax4, Lmax5) #, Lmax6)
Lmaxcl <- Lmaxcl[18:38, ]

Lmaxcl2 <- Lmaxcl
for (i in 1:nrow(Lmaxcl)) {  Lmaxcl2[i, ] <- Lmaxcl[i, ] / sum(Lmaxcl[i, ], na.rm = T)   }

matplot(Lmaxcl)
barplot(t(Lmaxcl2), col = 1:6, las = 2, main = "Distribution of catch in Lmax size classes                      ", 
        ylab = "proportion", xlim = c(1, 24), 
legend = c("<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm"), args.legend = c(x = "right", bty = "n"))

# format indicator objects and plot ---------------------------------------
datdata <- 2000:2020
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

splisref$recLand <- rowSums(tab[, 18:38])

plate <- splisref[which(splisref$Lmax_cat == "(60,100]"), ]
head(plate[order(plate$recLand, decreasing = T), ], 15)

big <- splisref[which(splisref$Lmax_cat == "(100,200]"), ]
head(big[order(big$recLand, decreasing = T), ], 15)

# calculate average Lmax indicator -----------------------------------------------

lmax <- rep(NA, ncol(tab))
for (i in 1:ncol(tab))  {  lmax[i] <- weighted.mean(splisref$Lmax, tab[, i])  }

plot(1983:2020, lmax)
plot(2000:2020, lmax[18:38], type = "b", las = 2)
axis(1, 2000:2020)
out <- lm(lmax[18:38] ~ c(2000:2020))
summary(out)
abline(out)

plot(1983:2020, tapply(dbf$Lmax, dbf$YEAR_LANDED, mean, na.rm = T))

datdata <- 2000:2020
inddata <- data.frame(lmax[18:38])
labs <- c("Average maximum length of catch", "length (cm)" , "Puerto Rico")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
 
plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T, outtype = "png")

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


