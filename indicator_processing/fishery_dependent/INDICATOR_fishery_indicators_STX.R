# M. Karnauskas 10/19/2023
# code for calculating pelagic:demersal ratio and Lmax indicators
# uses logbook data for PR and USVI 

rm(list = ls())
library(pals)

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/STX_072011_present_LANDINGS_trip_2021-03-11.csv")

# define start and end years ---------------------------
styear <- 2011
enyear <- 2020
table(dat$TRIP_YEAR)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12

# subset years------------------------------------------

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
table(d$TRIP_YEAR)

# look at main species landed --------------------------------
tab <- sort(tapply(d$POUNDS_LANDED, d$SPECIES_NM, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab, las = 2)
barplot(tab[1:50], las = 2)
barplot(tab[1:25], las = 2)

tab <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1)+1)
matplot(styear:enyear, t(tab[1:10, ]), type = "l", col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
legend("topright", rownames(tab)[1:10], col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
#abline(v = 1999)   

#styear <- 2010
#d <- d[which(d$TRIP_YEAR >= styear), ]  # cut out because very little data beforehand

yrs <- styear:enyear

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
which(is.na(db$SPECIES_NM))

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
labs <- c("Total pelagic landings", "millions of pounds" , "St. Croix", 
          "Total demersal landings", "millions of pounds", "St. Croix", 
          "Pelagic:demersal ratio", "ratio of landings", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, sublabel = T, trendAnalysis = F) #, outtype = "png")

############################  END PD ratio  #######################################

dev.off()

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
plotIndicatorTimeSeries(s, coltoplot = 1:5, plotrownum = 5, sublabel = T, widadj = 1.5, trendAnalysis = F) #, outtype = "png")

inddata <- data.frame(Lmaxcl2)
labs <- c(rep("Proportion of landings in Lmax class",5), 
          rep("proportion", 5),
          "<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
plotIndicatorTimeSeries(s, coltoplot = 1:5, plotrownum = 5, sublabel = T, widadj = 1.5, hgtadj = 0.6, trendAnalysis = F) # outtype = "png", sameYscale = F)

# understand what is driving the trends -------------------------------------------

splisref$COMname[which(splisref$Lmax_cat == "(0,40]")]
splisref$COMname[which(splisref$Lmax_cat == "(100,200]")]

sort(table(splisref$famcode[which(splisref$Lmax_cat == "(0,40]")]))
sort(table(splisref$famcode[which(splisref$Lmax_cat == "(100,200]")]))

splisref$recLand <- rowSums(tab)

small <- splisref[which(splisref$Lmax_cat == "(0,40]"), ]
head(small[order(small$recLand, decreasing = T), ], 15)

big <- splisref[which(splisref$Lmax_cat == "(100,200]"), ]
head(big[order(big$recLand, decreasing = T), ], 15)

dev.off()

# calculate average Lmax indicator -----------------------------------------------

lmax <- rep(NA, ncol(tab))
for (i in 1:ncol(tab))  {  lmax[i] <- weighted.mean(splisref$Lmax, tab[, i])  }

plot(yrs, lmax)
plot(yrs, lmax, type = "b", las = 2)
axis(1, yrs)
out <- lm(lmax ~ yrs)
summary(out)
abline(out)

plot(yrs, tapply(dbf$Lmax, dbf$TRIP_YEAR, mean, na.rm = T), type = "l")

# look at what is driving PD ratio  -------------------

plot(yrs, pdrat, type = "b", las = 2)
matplot(yrs, pd, axes = F, type = "b")
axis(1, at = yrs, las = 2)

tabp <- tab[grep("pelagic", splisref$PD), ]
tabp <- tabp[order(rowSums(tabp), decreasing = T), ]

matplot(yrs, t(tabp), type = "l", col = glasbey(10), lwd = 2, lty = 1)
legend("topleft", rownames(tabp), col = glasbey(10), lwd = 2, lty = 1, cex = 0.7)

tabd <- tab[-grep("pelagic", splisref$PD), ]
tabd <- tabd[order(rowSums(tabd), decreasing = T), ]

matplot(yrs, t(tabd[1:20, ]), type = "b", col = glasbey(10), lwd = 2, lty = 1, pch = 19, las = 2)
legend("topright", rownames(tabd)[1:20], col = glasbey(10), lwd = 2, lty = 1)


