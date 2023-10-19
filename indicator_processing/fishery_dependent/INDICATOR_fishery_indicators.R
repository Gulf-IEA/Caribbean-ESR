# M. Karnauskas 10/19/2023
# code for calculating pelagic:demersal ratio and Lmax indicators
# uses logbook data for PR and USVI 

rm(list = ls())

library(pals)

# define start and end years ---------------------------
styear <- 1990
enyear <- 2020

# input data for Puerto Rico ---------------------------
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")
dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20_wSC_2005cor.csv")

table(dat$YEAR_LANDED)

# subset years------------------------------------------

d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]
table(d$YEAR_LANDED)

# look at main species landed --------------------------------
tab <- sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_COMMON_NAME, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab, las = 2)
barplot(tab[1:50], las = 2)
barplot(tab[1:25], las = 2)

tab <- tapply(d$ADJUSTED_POUNDS, list(d$ITIS_COMMON_NAME, d$YEAR_LANDED), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1)+1)
matplot(styear:enyear, t(tab[1:10, ]), type = "l", col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)
legend("topright", rownames(tab)[1:10], col = 1:10, lty = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), lwd = 2)

tab2 <- tab[1:10, ]
for (i in 1:ncol(tab)) {  tab2[, i] <- tab[1:10, i] / sum(tab[1:10, i], na.rm = T)   }

barplot(tab2, col = glasbey(10), xlim = c(0, 56), legend.text = rownames(tab2), args.legend = c(x = "right"), las = 2)

j <- grep("FISHES,BONY,UNSPEC", rownames(tab)); rownames(tab)[j]
plot(styear:enyear, tab[j, ]/colSums(tab, na.rm = T), main = "proportion of unidentified landings by year", type = "b")
abline(h = 0.02)
abline(v = 2005) # 2000 and on is 'data rich' period
abline(h = 0.1)
abline(v = 2000)

# redo starting year cutoff based on data avail --------------------------
styear <- 2000
d <- d[which(d$YEAR_LANDED >= styear), ]

# add missing scientific names

d$ITIS_SCIENTIFIC_NAME <- as.character(d$ITIS_SCIENTIFIC_NAME)
lis <- as.data.frame(unique(d$ITIS_SCIENTIFIC_NAME))
names(lis) = "sci"
lis$com <- NA
head(lis)
lis <- lis[1:(nrow(lis)-1), ]
lis

for (i in 1:nrow(lis))  { 
  n <- sort(table(d$ITIS_COMMON_NAME[which(d$ITIS_SCIENTIFIC_NAME == lis$sci[i])]), decreasing = T)
  lis$com[i] <- names(n[1])    }

table(d$YEAR_LANDED[which(is.na(d$ITIS_SCIENTIFIC_NAME))])

mis <- which(is.na(d$ITIS_SCIENTIFIC_NAME))
missci <- as.character(lis$sci[match(d$ITIS_COMMON_NAME[mis], lis$com)])
d$ITIS_SCIENTIFIC_NAME[which(is.na(d$ITIS_SCIENTIFIC_NAME))] <- missci
dim(table(d$ITIS_SCIENTIFIC_NAME))

mis <- which(is.na(d$ITIS_SCIENTIFIC_NAME))
d[mis, ]   # still some missing but not going to fix until new data pull

# pull in reference file and merge ------------------------------------

ref <- read.csv("spp_ref_manualEdit.csv")
head(ref)
head(d)
tail(d)

hist(ref$Lmax)
table(cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000)))
ref$Lmax_cat <- cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000))

db <- merge(d, ref, by.x = "ITIS_SCIENTIFIC_NAME", by.y = "SCIname", all.x = TRUE)
dim(ref)
dim(d)
dim(db)
head(db)

tab <- tapply(db$ADJUSTED_POUNDS, list(db$famcode, db$YEAR_LANDED), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]
head(tab, 15)

nsp <- 15
cols <- glasbey(nsp)

# plot top species groups --------------------------------

par(mar = c(4, 4, 1, 1)+1)
matplot(styear:enyear, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)

tab2 <- tab[1:(nsp-1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"
tab2

tab3 <- tab2
for (i in 1:ncol(tab2)) {  tab3[, i] <- tab2[, i] / sum(tab2[, i], na.rm = T)   }

cols[which(rownames(tab3) == "UNID")] <- "white"
barplot(tab3, col = cols, xlim = c(0, 56), legend.text = rownames(tab2), args.legend = c(x = "right"), las = 2)

barplot(tab3[, which(colnames(tab3) == styear):ncol(tab3)], col = cols, xlim = c(0, 23), legend.text = rownames(tab2), args.legend = c(x = "right", bty = "n"), las = 2, space = 0, border = NA, 
        xlab = "", ylab = "proportion of catch", main = "Puerto Rico landings composition by year                                ")
abline(h = 0)

#png(filename="PR_proportion_landings.png", units="in", width=8, height=5, pointsize=12, res=72*10)

#dev.off()


table(as.character(db$ITIS_COMMON_NAME) == as.character(db$COMname))
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

pd <- tapply(dbf$ADJUSTED_POUNDS, list(dbf$YEAR_LANDED, dbf$PD2), sum, na.rm = T)
pd
matplot(pd)
pdrat <- pd[, 2] / pd[, 1]
plot(styear:enyear, pdrat, type = "b")

save(pdrat, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data/PDRatioPR.RData")

# make indicator object and plot P:D ratio ------------------
datdata <- styear:enyear
inddata <- data.frame(cbind(pd[, 2]/10^6, pd[, 1]/10^6, pdrat))
labs <- c("Total pelagic landings", "millions of pounds" , "Puerto Rico", 
          "Total demersal landings", "millions of pounds", "Puerto Rico", 
          "Pelagic:demersal ratio", "ratio of landings", "Puerto Rico")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1:3,plotrownum = 3, sublabel = T, widadj = 1.2, trendAnalysis = F)

dev.off()

#######################  END P:D ratio  ################################

# Lmax calculations -----------------------------------------

tab <- tapply(dbf$ADJUSTED_POUNDS, list(dbf$ITIS_SCIENTIFIC_NAME, dbf$YEAR_LANDED), sum, na.rm = T)
tab[is.na(tab)] <- 0
tab

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

Lmaxcl2 <- Lmaxcl
for (i in 1:nrow(Lmaxcl)) {  Lmaxcl2[i, ] <- Lmaxcl[i, ] / sum(Lmaxcl[i, ], na.rm = T)   }

matplot(Lmaxcl)
barplot(t(Lmaxcl2), col = 1:6, las = 2, main = "Distribution of catch in Lmax size classes                      ", 
        ylab = "proportion", xlim = c(1, 24), 
legend = c("<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm"), args.legend = c(x = "right", bty = "n"))

# format indicator objects and plot ---------------------------------------
datdata <- styear:enyear
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

plot(styear:enyear, lmax)
plot(styear:enyear, lmax, type = "b", las = 2)
axis(1, styear:enyear)
out <- lm(lmax ~ c(styear:enyear))
summary(out)
abline(out)

plot(styear:enyear, tapply(dbf$Lmax, dbf$YEAR_LANDED, mean, na.rm = T))

datdata <- styear:enyear
inddata <- data.frame(lmax)
labs <- c("Average maximum length of catch", "length (cm)" , "Puerto Rico")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
 
plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T) #, outtype = "png")

dev.off()

# figure out what is going on in 2018 with spike in pelagics -------------------

plot(styear:enyear, pdrat, type = "b", las = 2, pch = 19)
matplot(styear:enyear, pd, axes = F, type = "b", pch = 19)
axis(1, at = styear:enyear, las = 2)

tabp <- tab[grep("pelagic", splisref$PD), ]
tabp <- tabp[order(rowSums(tabp), decreasing = T), ]

matplot(styear:enyear, t(tabp[1:10, ]), type = "l", col = glasbey(10), lwd = 2, lty = 1)
legend("topright", rownames(tabp)[1:10], col = glasbey(10), lwd = 2, lty = 1)
abline(v = c(2008, 2019))

tabd <- tab[-grep("pelagic", splisref$PD), ]
tabd <- tabd[order(rowSums(tabd), decreasing = T), ]

matplot(styear:enyear, t(tabd[1:10, ]), type = "b", col = glasbey(10), lwd = 2, lty = 1, pch = 19, las = 2)
legend("topright", rownames(tabd)[1:10], col = glasbey(10), lwd = 2, lty = 1)
abline(v = c(2008, 2019))

