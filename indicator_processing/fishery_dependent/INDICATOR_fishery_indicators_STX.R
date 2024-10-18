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

load("indicator_processing/spec_file.RData")

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/MOST_RECENT/"

# define start and end years ---------------------------
styear <- 2000
enyear <- 2022

dat <- read.csv(paste0(confpath, "STX_2024.csv"))

head(dat)

# define start and end years ---------------------------

table(dat$TRIP_YEAR)
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# adjust year to fishing year (Jul 1 - Jun 30) -------------

aa <- which(dat$TRIP_MONTH < 7)
dat$TRIP_YEAR[aa] <- dat$TRIP_YEAR[aa] - 1
dat$TRIP_MONTH[aa] <- dat$TRIP_MONTH[aa] + 12
table(dat$TRIP_YEAR, dat$TRIP_MONTH)

# take out incomplete years -----------------------------

tab <- table(dat$TRIP_YEAR, dat$TRIP_MONTH)
lis <- as.numeric(names(which(apply(tab, 1, min) == 0)))
lis
dat <- dat[!(dat$TRIP_YEAR %in% lis), ]
table(dat$TRIP_YEAR)

# subset years------------------------------------------

d <- dat[which(dat$TRIP_YEAR >= styear & dat$TRIP_YEAR <= enyear), ]
table(d$TRIP_YEAR)
table(d$TRIP_YEAR, d$TRIP_MONTH)

styear <- min(d$TRIP_YEAR)
enyear <- max(d$TRIP_YEAR)

# look at main species landed --------------------------------
tab <- sort(tapply(d$POUNDS_LANDED, d$SPECIES_NM, sum, na.rm = T), decreasing = T)
par(mar = c(15, 5, 2, 2))
barplot(tab, las = 2)
barplot(tab[1:50], las = 2)
barplot(tab[1:25], las = 2)

tab <- tapply(d$POUNDS_LANDED, list(d$SPECIES_NM, d$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1) + 1)
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

tab2 <- apply(tab[1:10, ], 2, function(x) x/sum(x))
barplot(tab2, col = glasbey(10), xlim = c(0, ncol(tab2)*2), legend.text = rownames(tab2), args.legend = c(x = "right"))

# remove land crab trips -------------------

sort(table(d$SPECIES_NM[grep("CRAB", d$SPECIES_NM)]))
sort(table(d$GEAR_NM[grep("CRAB", d$SPECIES_NM)]))
d$GEAR_NM[grep("CRAB", d$GEAR_NM)]
# no land crab traps in USVI?  

# remove bad price values and calculate revenue  ------------------------------

hist(d$PRICE)
which(d$PRICE > 15)
#d$SPECIES_NM[which(d$PRICE > 15)]
#hist(d$PRICE[which(d$PRICE > 15)])
#d$PRICE[which(d$PRICE > 15)] <- NA

table(d$SPECIES_NM, is.na(d$PRICE))

# pull in reference file and merge ------------------------------------

ref <- read.csv("indicator_processing/fishery_dependent/spp_ref_STT_manualEdit.csv")
head(ref)
head(d)

hist(ref$Lmax)
table(cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000)))
ref$Lmax_cat <- cut(ref$Lmax, breaks = c(0, 40, 60, 100, 200, 2000))

db <- merge(d, ref, by.x = "SPECIES_NM", by.y = "COMname", all.x = TRUE)
dim(d)
dim(db)
which(is.na(db$SPECIES_NM))
table(as.character(db$ITIS_SCIENTIFICNAME) == as.character(db$SCIname))
which(as.character(db$ITIS_SCIENTIFICNAME) != as.character(db$SCIname))
head(db)

# insert missing prices -------------------------------

par(mar = c(8, 1, 1, 1)+2)
barplot(sort(tapply(db$PRICE, db$famcode, mean, na.rm = T), decreasing = T), las = 2, 
        main = "average price by family")

table(db$SPECIES_NM, is.na(db$PRICE))
table(db$famcode, is.na(db$PRICE))
db$TRIP_YEAR[which(db$famcode == "lobsters" & is.na(db$PRICE))]

for (i in unique(db$famcode))  {
  m <- mean(db$PRICE[which(db$famcode == i)], na.rm = T)
  db$PRICE[which(db$famcode == i & is.na(db$PRICE))] <- m
}

db$PRICE[which(db$famcode == "lobsters" & is.na(db$PRICE))] 

table(db$famcode, is.na(db$PRICE))
barplot(sort(tapply(db$PRICE, db$famcode, mean, na.rm = T), decreasing = T), las = 2, 
        main = "average price by family")

db$REV <- db$POUNDS_LANDED * db$PRICE

# plot % landings and % revenue ----------------------

nsp <- 10
cols <- glasbey(nsp)

# by landings 

tab <- tapply(db$POUNDS_LANDED, list(db$famcode, db$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

par(mar = c(4, 4, 1, 1)+1)
matplot(yrs, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)

tab2 <- tab[1:(nsp - 1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"

tab3 <- apply(tab2, 2, function(x) x/sum(x))
#cols[which(rownames(tab3) == "UNID")] <- "white"
barplot(tab3, col = cols, xlim = c(0, 18), legend.text = rownames(tab3), args.legend = c(x = "right"), las = 2)

# by revenue 

tab <- tapply(db$REV, list(db$famcode, db$TRIP_YEAR), sum, na.rm = T)
tab <- tab[order(rowSums(tab, na.rm = T), decreasing = T), ]

matplot(yrs, t(tab[1:nsp, ]), type = "l", col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)
legend("topright", rownames(tab)[1:nsp], col = cols, lty = rep(1:3, (nsp/3)), lwd = 2)

tab2 <- tab[1:(nsp - 1), ]
tab2 <- rbind(tab2, colSums(tab[nsp:nrow(tab), ], na.rm = T))
rownames(tab2)[nsp] <- "other"

tabr <- apply(tab2, 2, function(x) x/sum(x))

colgd <- read.csv("indicator_processing/fishery_dependent/cols.csv", header = F) 

barplot(tabr, col = as.character(colgd$V2[match(rownames(tabr), colgd$V1)]), 
        xlim = c(0, ncol(tabr)*1.8), legend.text = rownames(tabr), args.legend = c(x = "right"), las = 2)

plot(tab3, tabr)

png(filename="indicator_plots/per_landings_STX.png", 
    units="in", width=7, height=4.5, pointsize=12, res=72*10)

barplot(tabr, col = as.character(colgd$V2[match(rownames(tabr), colgd$V1)]), 
        xlim = c(0, ncol(tabr)*1.9), legend.text = rownames(tabr), 
        args.legend = c(x = "right", bty = "n", title = "St. Croix", border = NA), 
        las = 2, border = NA, axes = F, xlab = "", ylab = "percent of total revenue")
axis(2, at = seq(0, 1, 0.2), lab = paste0(seq(0, 100, 20), "%"), las = 2)
abline(h = 0)

dev.off()

# calcuate P:D ratio and Lmax  -----------------------------------

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
pdrat <- pd[, 2] / pd[, 1]    # pdrat is pelagic divided by benthic
plot(yrs, pdrat, type = "b")

save(pdrat, file = "indicator_data/intermediateFiles/fish-dep-indicators/PDRatioSTX.RData")


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

# based on feedback from seminar -- calculate Lmax based on demersals only

table(dbf$PD, useNA = "always")
table(dbf$PD2, useNA = "always")
dbf <- dbf[which(dbf$PD2 == "demersal"), ]
dim(dbf)
head(dbf)

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

# format indicator objects and plot by total catch ----------------------------
datdata <- yrs
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

save(ind, file = "indicator_objects/STX_Lmax_classes.RData")

# understand what is driving the trends -------------------------------------------

splisref$COMname[which(splisref$Lmax_cat == "(0,40]")]
splisref$COMname[which(splisref$Lmax_cat == "(100,200]")]

sort(table(splisref$famcode[which(splisref$Lmax_cat == "(0,40]")]))
sort(table(splisref$famcode[which(splisref$Lmax_cat == "(100,200]")]))

splisref$recLand <- rowSums(tab)

small <- splisref[which(splisref$Lmax_cat == "(0,40]"), ]
head(small[order(small$recLand, decreasing = T), ], 15)
#driven by redband parrotfish and princess parrotfish.  also herrings and surgeonfishes

plate <- splisref[which(splisref$Lmax_cat == "(40,60]"), ]
head(plate[order(plate$recLand, decreasing = T), ], 15)
# mainly stoplight and queen parrotfishes.  Also blackfin and silk snapper and red hind

big <- splisref[which(splisref$Lmax_cat == "(100,200]"), ]
head(big[order(big$recLand, decreasing = T), ], 15)
# tunas (little tunny) and king mackerel

dev.off()

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

plot(yrs, lmax)
plot(yrs, lmax, type = "b", las = 2)
axis(1, yrs)
out <- lm(lmax ~ yrs)
summary(out)
abline(out)

plot(yrs, tapply(dbf$Lmax, dbf$TRIP_YEAR, mean, na.rm = T), type = "l")

datdata <- styear:enyear
inddata <- data.frame(lmax)
labs <- c("Average maximum length of catch", "length (cm)" , "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = inddata + lmax_sem, llim = inddata - lmax_sem)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T)
plotIndicatorTimeSeries(s, coltoplot = 1, sublabel = T, outtype = "png", widadj = 1.65, hgtadj = 0.6)

dev.off()

findat <- data.frame(cbind(yrs, lmax))

save(findat, file = "indicator_data/intermediateFiles/fish-dep-indicators/Lmax_STX.RData")


# look at what is driving PD ratio  -------------------

# plot(yrs, pdrat, type = "b", las = 2)
# matplot(yrs, pd, axes = F, type = "b")
# axis(1, at = yrs, las = 2)
# 
# tabp <- tab[grep("pelagic", splisref$PD), ]
# tabp <- tabp[order(rowSums(tabp), decreasing = T), ]
# 
# matplot(yrs, t(tabp), type = "l", col = glasbey(10), lwd = 2, lty = 1)
# legend("topleft", rownames(tabp), col = glasbey(10), lwd = 2, lty = 1, cex = 0.7)
# 
# tabd <- tab[-grep("pelagic", splisref$PD), ]
# tabd <- tabd[order(rowSums(tabd), decreasing = T), ]
# 
# matplot(yrs, t(tabd[1:20, ]), type = "b", col = glasbey(10), lwd = 2, lty = 1, pch = 19, las = 2)
# legend("topright", rownames(tabd)[1:20], col = glasbey(10), lwd = 2, lty = 1)

print("STX indicators -- SUCCESSFULLY RUN")
