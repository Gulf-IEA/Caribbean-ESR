# M. Karnauskas 10/19/2023
# code for calculating pelagic:demersal ratio and Lmax indicators
# uses logbook data for PR and USVI 

# specification file and libraries -----------------------------
rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)
library(pals)
library(modi)

load("indicator_processing/spec_file.RData")

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/MOST_RECENT/"

# define start and end years ---------------------------
styear <- 2000
enyear <- 2022

# input data for Puerto Rico ---------------------------

dat <- read.csv(paste0(confpath, "wrkeithly_pr_com_data_2000_2022_20240625_C.csv"))

dat[which(is.na(dat$ITIS_SCIENTIFIC_NAME)), ]
summary(is.na(dat$ITIS_SCIENTIFIC_NAME))
dim(dat)
dat <- dat[which(!is.na(dat$ITIS_SCIENTIFIC_NAME)), ]
dim(dat)

table(dat$YEAR_LANDED)

# subset years------------------------------------------

d <- dat[which(dat$YEAR_LANDED >= styear & dat$YEAR_LANDED <= enyear), ]
table(d$YEAR_LANDED, useNA = "always")

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

tab2 <- apply(tab[1:10, ], 2, function(x) x/sum(x))

# remove land crab trips -------------------

sort(table(d$ITIS_COMMON_NAME[grep("CRAB", d$ITIS_COMMON_NAME)]))
sort(table(d$ERDMAN_GEAR_NAME[grep("CRAB,BLUE", d$ITIS_COMMON_NAME)]))
sort(table(d$FAMILY[grep("CRAB,BLUE", d$ITIS_COMMON_NAME)]))
sort(table(d$ERDMAN_GEAR_NAME[which(d$FAMILY == "LAND CRABS")]))
par(mar = c(12, 4, 1, 1))
barplot(sort(table(d$ERDMAN_GEAR_NAME[which(d$FAMILY == "LAND CRABS")])), las = 2)
sort(table(d$ITIS_COMMON_NAME[which(d$ERDMAN_GEAR_NAME == "BY HAND")]))  # don't remove, contains conch as well
# filtering by LAND CRAB TRAP gear takes out vast majority (96%) of land crab trips

d[which(d$ERDMAN_GEAR_NAME == "LAND CRAB TRAP"), ]
dim(d)
d <- d[which(d$ERDMAN_GEAR_NAME != "LAND CRAB TRAP"), ]
dim(d)

# note - is this spp composition plot possibly useful for the report??? ----------------------
barplot(tab2, col = glasbey(10), xlim = c(0, 56), legend.text = rownames(tab2), args.legend = c(x = "right"), las = 2)

j <- grep("FISHES,BONY,UNSPEC", rownames(tab)); rownames(tab)[j]
plot(styear:enyear, tab[j, ]/colSums(tab, na.rm = T), main = "proportion of unidentified landings by year", type = "b")
abline(h = 0.02)
abline(v = 2005) # 2005 and on is 'data rich' period
abline(h = 0.1)
abline(v = 2000)

# redo starting year cutoff based on data avail --------------------------
styear <- 2005
d <- d[which(d$YEAR_LANDED >= styear), ]

# add missing scientific names

d$ITIS_SCIENTIFIC_NAME <- as.character(d$ITIS_SCIENTIFIC_NAME)
lis <- as.data.frame(unique(d$ITIS_SCIENTIFIC_NAME))
names(lis) = "sci"
lis$com <- NA
head(lis)
#lis <- lis[1:(nrow(lis)-1), ]
lis

for (i in 1:nrow(lis))  { 
  n <- sort(table(d$ITIS_COMMON_NAME[which(d$ITIS_SCIENTIFIC_NAME == lis$sci[i])]), decreasing = T)
  lis$com[i] <- names(n[1])    }

lis

table(d$YEAR_LANDED[which(is.na(d$ITIS_SCIENTIFIC_NAME))])

mis <- which(is.na(d$ITIS_SCIENTIFIC_NAME))
d[mis, ]
missci <- as.character(lis$sci[match(d$ITIS_COMMON_NAME[mis], lis$com)])
d$ITIS_SCIENTIFIC_NAME[which(is.na(d$ITIS_SCIENTIFIC_NAME))] <- missci
dim(table(d$ITIS_SCIENTIFIC_NAME))

mis <- which(is.na(d$ITIS_SCIENTIFIC_NAME))
d[mis, ]   # still some missing but not going to fix until new data pull
unique(d$ERDMAN_COMMON_NAME[mis])

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

table(is.na(d$PRICE), d$ITIS_COMMON_NAME)

# pull in reference file and merge ------------------------------------

ref <- read.csv("indicator_processing/fishery_dependent/spp_ref_manualEdit.csv")
head(ref)
head(d)
tail(d)

ref$SCIname <- str_replace(ref$SCIname, "\xa0", " ")

hist(ref$Lmax)
table(cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000)))
ref$Lmax_cat <- cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000))

ref$SCIname <- toupper(ref$SCIname)

ref$SCIname[30]
d$ITIS_SCIENTIFIC_NAME[which(d$ITIS_SCIENTIFIC_NAME == "ACANTHOSTRACION QUADRICORNIS")] <- ref$SCIname[30]
ref$SCIname[196]
d$ITIS_SCIENTIFIC_NAME[which(d$ITIS_SCIENTIFIC_NAME == "SCARUS TAENIOPTERUS")] <- ref$SCIname[196]
ref$SCIname[197]
d$ITIS_SCIENTIFIC_NAME[which(d$ITIS_SCIENTIFIC_NAME == "SPARISOMA RUBRIPINNE")] <- ref$SCIname[197]
ref$SCIname[257]
d$ITIS_SCIENTIFIC_NAME[which(d$ITIS_SCIENTIFIC_NAME == "PRISTIPOMOIDES AQUILONARIS")] <- ref$SCIname[257]
ref$SCIname[27]
d$ITIS_SCIENTIFIC_NAME[which(d$ITIS_SCIENTIFIC_NAME == "LACTOPHRYS BICAUDALIS")] <- ref$SCIname[27]

# merge trip ticket data with reference file to get spp info --------------------------

db <- merge(d, ref, by.x = "ITIS_SCIENTIFIC_NAME", by.y = "SCIname", all.x = TRUE)
dim(ref)
dim(d)
dim(db)
head(db)

unique(db$ITIS_SCIENTIFIC_NAME[which(is.na(db$COMname))])
unique(db$ITIS_COMMON_NAME[which(is.na(db$COMname))])

# insert missing prices -------------------------------

par(mar = c(8, 1, 1, 1)+2)
barplot(sort(tapply(db$PRICE_PER_LB, db$famcode, mean, na.rm = T), decreasing = T), las = 2, 
        main = "average price by family")

table(is.na(db$PRICE_PER_LB), db$ITIS_COMMON_NAME)
table(is.na(db$PRICE_PER_LB), db$famcode)
table(is.na(db$PRICE_PER_LB))

for (i in unique(db$famcode))  {
  m <- mean(db$PRICE_PER_LB[which(db$famcode == i)], na.rm = T)
  db$PRICE_PER_LB[which(db$famcode == i & is.na(db$PRICE_PER_LB))] <- m
}

table(is.na(db$PRICE_PER_LB), db$famcode)
table(is.na(db$PRICE_PER_LB))
barplot(sort(tapply(db$PRICE_PER_LB, db$famcode, mean, na.rm = T), decreasing = T), las = 2, 
        main = "average price by family")

db$REV <- db$ADJUSTED_POUNDS * db$PRICE_PER_LB

# plot % landings and % revenue ----------------------

nsp <- 10
cols <- glasbey(nsp)

# by landings 

tab <- tapply(db$ADJUSTED_POUNDS, list(db$famcode, db$YEAR_LANDED), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]
tab

par(mar = c(4, 4, 1, 1)+1)
matplot(styear:enyear, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)

tab2 <- tab[1:(nsp - 1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"

tab3 <- apply(tab2, 2, function(x) x/sum(x))
#cols[which(rownames(tab3) == "UNID")] <- "white"
barplot(tab3, col = cols, xlim = c(0,ncol(tab3)*1.8), legend.text = rownames(tab3), args.legend = c(x = "right"), las = 2)

# by revenue ---------------------------------

tab <- tapply(db$REV, list(db$famcode, db$YEAR_LANDED), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

matplot(styear:enyear, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2, ncol = 2)

tab2 <- tab[1:(nsp - 1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"

tabr <- apply(tab2, 2, function(x) x/sum(x))

# plot of proportion revenue by year and species group ------------------------

colgd <- read.csv("indicator_processing/fishery_dependent/cols.csv", header = F) 

barplot(tabr, col = as.character(colgd$V2[match(rownames(tabr), colgd$V1)]), 
        xlim = c(0, ncol(tabr)*1.8), legend.text = rownames(tabr), args.legend = c(x = "right"), las = 2)

plot(tab3, tabr)

png(filename="indicator_plots/per_landings_PR.png", 
    units="in", width=7, height=4.5, pointsize=12, res=72*10)

barplot(tabr, col = as.character(colgd$V2[match(rownames(tabr), colgd$V1)]),
        xlim = c(0, ncol(tabr)*1.9), legend.text = rownames(tabr), 
        args.legend = c(x = "right", bty = "n", title = "Puerto Rico", border = NA), 
        las = 2, border = NA, axes = F, xlab = "", ylab = "percent of total revenue")
axis(2, at = seq(0, 1, 0.2), lab = paste0(seq(0, 100, 20), "%"), las = 2)
abline(h = 0)

dev.off()

# end plot 

# calcuate P:D ratio and Lmax  -------------------------------------

table(as.character(db$ITIS_COMMON_NAME) == as.character(db$COMname))
which(as.character(db$ITIS_COMMON_NAME) != as.character(db$COMname))
db[which(as.character(db$ITIS_COMMON_NAME) != as.character(db$COMname)), c(31, 42)]

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
pdrat <- pd[, 2] / pd[, 1]  # pdrat is pelagic divided by benthic
plot(styear:enyear, pdrat, type = "b")

save(pdrat, file = "indicator_data/intermediateFiles/fish-dep-indicators/PDRatioPR.RData")

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

# figure out what is going on in 2018 with spike in pelagics -------------------

# plot(styear:enyear, pdrat, type = "b", las = 2, pch = 19)
# matplot(styear:enyear, pd, axes = F, type = "b", pch = 19)
# axis(1, at = styear:enyear, las = 2)
# 
# tabp <- tab[grep("pelagic", splisref$PD), ]
# tabp <- tabp[order(rowSums(tabp), decreasing = T), ]
# 
# matplot(styear:enyear, t(tabp[1:10, ]), type = "l", col = glasbey(10), lwd = 2, lty = 1)
# legend("topright", rownames(tabp)[1:10], col = glasbey(10), lwd = 2, lty = 1)
# abline(v = c(2008, 2019))  # driven ~50% by dolphinfish catch
# 
# tabd <- tab[-grep("pelagic", splisref$PD), ]
# tabd <- tabd[order(rowSums(tabd), decreasing = T), ]
# 
# matplot(styear:enyear, t(tabd[1:10, ]), type = "b", col = glasbey(10), lwd = 2, lty = 1, pch = 19, las = 2)
# legend("topright", rownames(tabd)[1:10], col = glasbey(10), lwd = 2, lty = 1)
# abline(v = c(2008, 2019))
# 

#######################  END P:D ratio  ################################

# Lmax calculations -----------------------------------------

# based on feedback from seminar -- calculate Lmax based on demersals only

table(dbf$PD, useNA = "always")
table(dbf$PD2, useNA = "always")
dbf <- dbf[which(dbf$PD2 == "demersal"), ]
dim(dbf)
head(dbf)

tab <- tapply(dbf$ADJUSTED_POUNDS, list(dbf$ITIS_SCIENTIFIC_NAME, dbf$YEAR_LANDED), sum, na.rm = T)
tab[is.na(tab)] <- 0
tab

splis <- data.frame(rownames(tab))
names(splis) <- "SCIname"

splisref <- merge(splis, ref, by = "SCIname", all.x = TRUE)
table(rownames(tab) == splisref$SCIname)
splisref

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
        ylab = "proportion", xlim = c(1, 28), 
legend = c("<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm"), args.legend = c(x = "right", bty = "n"))

# format indicator objects and plot by total catch ----------------------------
datdata <- styear:enyear
inddata <- data.frame(Lmaxcl/10^6)
labs <- c(rep("Total landings in Lmax class", 5), 
          rep("millions of pounds", 5),
          "<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
plotIndicatorTimeSeries(s, coltoplot = 1:5, plotrownum = 5, sublabel = T, widadj = 1.5, trendAnalysis = F) #, outtype = "png")

# plot based on proportion of catch in each size class -------------------------
inddata <- data.frame(Lmaxcl2)
labs <- c(rep("Proportion of landings in Lmax class",5), 
          rep("proportion", 5),
          "<40 cm", "40-60 cm", "60-100 cm", "100-200 cm", ">200 cm")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"
plotIndicatorTimeSeries(s, coltoplot = 1:5, plotrownum = 5, sublabel = T, widadj = 1.5, hgtadj = 0.6, trendAnalysis = F) # outtype = "png", sameYscale = F)

ind <- s

save(ind, file = "indicator_objects/PR_Lmax_classes.RData")

# understand what is driving the trends -------------------------------------------

splisref$COMname[which(splisref$Lmax_cat == "(60,100]")]
splisref$COMname[which(splisref$Lmax_cat == "(100,200]")]

sort(table(splisref$famcode[which(splisref$Lmax_cat == "(60,100]")]))
sort(table(splisref$famcode[which(splisref$Lmax_cat == "(100,200]")]))

splisref$recLand <- rowSums(tab)

sm <- splisref[which(splisref$Lmax_cat == "(40,60]"), ]
head(sm[order(sm$recLand, decreasing = T), ], 15)

plate <- splisref[which(splisref$Lmax_cat == "(60,100]"), ]
head(plate[order(plate$recLand, decreasing = T), ], 15)
# the 60-100cm category is mostly driven by deepwater snapper species, also yellowtail, hogfish and red hind

big <- splisref[which(splisref$Lmax_cat == "(100,200]"), ]
head(big[order(big$recLand, decreasing = T), ], 15)
# the 100-200 group is driven by mackerels, large rare parrotfishes, tunas and snook.  Also some large groupers

xtra <- splisref[which(splisref$Lmax_cat == "(200,2e+03]"), ]
head(xtra[order(xtra$recLand, decreasing = T), ], 15)
# 200+ group is very heavily driven by dolphinfish.  also some sharks


# calculate average Lmax indicator -----------------------------------------------

# formula from https://www.sciencedirect.com/science/article/pii/135223109400210C
weighted.var.se <- function(x, w, na.rm=FALSE)   #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}

lmax <- rep(NA, ncol(tab))
lmax_sem <- rep(NA, ncol(tab))
for (i in 1:ncol(tab))  {  
  lmax[i] <- weighted.mean(splisref$Lmax, tab[, i], na.rm = TRUE)  
  lmax_sem[i] <- sqrt(weighted.var.se(splisref$Lmax, tab[, i], na.rm = TRUE))
  }

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
s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = inddata + lmax_sem, llim = inddata - lmax_sem)
class(s) <- "indicatordata"
 
plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T) #, outtype = "png")
#plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T, outtype = "png", widadj = 1.3, hgtadj = 0.6)

dev.off()

findat <- data.frame(cbind(styear:enyear, lmax))

save(findat, file = "indicator_data/intermediateFiles/fish-dep-indicators/Lmax_PR.RData")

#############  END  ######################

print("PR indicators -- SUCCESSFULLY RUN")
