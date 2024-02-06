###############################################################################
# TCRMP data analysis
# M. Karnauskas 06/27/2023
#
# data access at: https://sites.google.com/site/usvitcrmp/available-data
# benthic data downloaded from: https://docs.google.com/spreadsheets/d/1If0CsxbG469kv6U_GyZ0t9ewAtlm03zx/edit#gid=697263468
# fish data downloaded from: https://docs.google.com/spreadsheets/d/1-FvhfMAFs5hNEqmYm0EJd2P23BBYeV7_/edit#gid=926142160
#
# last data download: Feb 6, 2024
################################################################################
#
# download excel from google doc and convert to .csv file; save locally
# 2 tabs from fish database need to be saved as separate csvs: FishMetadata, FishData
#
# Fish data: 24 historical sites and 32 additional sites as of 2012
# 10 replicate belt transects and three replicate roving dive surveys per site
# 
#################################################################################

# fish data analysis

rm(list = ls())

# get top landed species -----------------
# with more time - use logbook data from STT and STX - but need scientific names

d <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20.csv")
d <- d[which(d$YEAR_LANDED >= 2003 & d$YEAR_LANDED <= 2020), ]
table(d$YEAR_LANDED, useNA = "always")
d$ITIS_SCIENTIFIC_NAME <- droplevels(d$ITIS_SCIENTIFIC_NAME)

sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T))
par(mar = c(10, 5, 1, 1))
barplot(sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T), decreasing = T)[1:50], las = 2)
quantile(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T), probs = c(0.05, 0.1, 0.2, 0.5, 0.9))
quantile(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T), probs = c(0.5))  

# create species list ------------------------------

splis <- names(which(sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T)) > 1068.57))
splis[-grep(" ", splis)]
splis <- splis[grep(" ", splis)]
splis
splis <- splis[1:79]
splis

# read in fish data -------------------------------

spp <- read.csv("FishMetadata.csv", stringsAsFactors = F)
head(spp)

splis2 <- spp$ScientificName[which(spp$Commercial == "Y")]

# compare TRCRMP commercial spp list with logbook ---------
splis2 %in% splis
splis2 <- splis2[-which(splis2 %in% splis)]  # should be 5 species missing from landings list
splis2

table(splis %in% spp$ScientificName)
splis[which(splis %in% spp$ScientificName)]
sort(splis[-which(splis %in% spp$ScientificName)]) # these are mostly pelagic or invertebrate species, ok

splis <- c(splis, splis2, "Carangoides bartholomaei", "Haemulon plumierii", "Trachinotus blochii", "Pristipomoides aquilonaris", "Mugil cephalus")
splis
length(splis)

# read in site metadata ------------------------------

met <- read.csv("SiteMetadata.csv", stringsAsFactors = F)
head(met)
met <- met[1:8]  # get rid of extra NA columns

hist(met$YearAdded)
table(met$YearAdded)  # use only pre-2005 sites? 

table(met$Island, met$Location)

# run separately for STX and STT/STJ
sitelis <- met$Location[which(met$YearAdded < 2005)]
#sitelis <- met$Location[which(met$YearAdded < 2005 & met$Island == "STX")]; island <- "STX"  # for STX
#sitelis <- met$Location[which(met$YearAdded < 2005 & met$Island != "STX")]; island <- "STT" # for STT/STJ

splis
sitelis

# read in fish data -----------------------------------

fish <- read.csv("FishData.csv", stringsAsFactors = F)
head(fish)
dim(fish)

yrs <- sort(unique(fish$SampleYear))

apply(fish[1:10], 2, table)

barplot(sort(table(fish$CommonName), decreasing = T)[1:30], las = 2)  # most common spp

fish <- fish[which(fish$Metric == "Abundance"), ]  # use only abundance data
fish <- fish[which(fish$Period == "Annual"), ]     # use only regular annual sampling

# subset for selected sites ----------------------------------
fish <- fish[which(fish$Location %in% sitelis), ]
apply(fish[1:10], 2, table)
dim(fish)

# run separately for fished and not fished
#fish <- fish[-which(fish$ScientificName %in% splis), ]; grp <- "oth" # unfished
fish <- fish[which(fish$ScientificName %in% splis), ]; grp <- "com"  # fished

apply(fish[1:10], 2, table)
dim(fish)

# look at frequency of sampling across years -----------------
table(fish$Location, fish$SampleYear)
matplot(2003:2021, table(fish$SampleYear, fish$Location), type = "l")

# reformat size data into different object --------------------
names(fish)[11:21]

v1 <- rep(sitelis, length(yrs))
v2 <- sort(rep(yrs, length(sitelis)))
mat <- matrix(data = NA, nrow = length(v1), ncol = length(names(fish)[11:21]))
mat <- data.frame(v1, v2, mat)
names(mat) <- c("site", "yr", "X0.10", names(fish)[13:21], "total")
head(mat)

# summarize by site and year ----------------------------------
# DOUBLE CHECK THIS 
i <- 0
for (j in sitelis) { 
  for (y in yrs)     {  
    f1 <- fish[which(fish$SampleYear == y & fish$Location == j), ]
    su <- colSums(f1[11:21])
    su2 <- su[1] + su[2]
    i <- i + 1
    mat[i, 3:12] <- c(su2, su[3:length(su)])
    mat[i, 13] <- sum(f1$Total)  }
} 
dim(mat)
mat <- mat[-which(rowSums(mat[3:ncol(mat)]) == 0), ]
dim(mat)
which(rowSums(mat[3:ncol(mat)]) == 0)

# mat is new data object with one site per year ------------
table(mat$site, mat$yr)

#for (i in 1:nrow(mat)) {  mat[i, 3:ncol(mat)] <- mat[i, 3:ncol(mat)] / sum(mat[i, 3:ncol(mat)]/ 100)  }

barplot(as.matrix(log(mat[3:12] + 1)), beside = T, col = rainbow(length(yrs)), names.arg = names(mat)[3:12])

plot((colSums(mat[3:12])), type = "h")
plot(log(colSums(mat[3:12])), type = "h")

# look at size spectra and subset classes that are selected for ------------------

if (grp == "com")  {  classes <- 2:5  }
if (grp != "com")  {  classes <- 1:5  }

head(mat[, classes + 2], 100)

# calculate slope of the size spectrum for each site and year --------------------

mat$slope <- NA
mat$SE <- NA

### CHECK IF NEED TO STANDARDIZE TO PROPORTIONS??

par(mfrow = c(8, 5))
for (i in 1:nrow(mat)) { 
  ve <- as.numeric(log(mat[i, classes + 2]))
  plot(ve, type = "h", axes = F)
  axis(1, at = 1:10, lab = names(mat)[3:12], las = 2)
  mtext(side = 3, text = paste(mat$site[i], mat$yr[i]))
  
  ve[which(ve == "-Inf")] <- NA
  out <- lm(ve ~ classes)
  
  if (sum(!is.na(ve)) > 2) { abline(out, col = 2) 
    mat$slope[i] <- summary(out)$coef[2, 1]
    mat$SE[i] <- summary(out)$coef[2, 2]   }
}

mat <- merge(mat, met, by.x = "site", by.y = "Location")
mat
mat$yr <- as.factor(mat$yr)

dev.off()

plot(log(mat$total) ~ mat$site)
plot(log(mat$total) ~ mat$yr)

boxplot(mat$slope ~ mat$site)
boxplot(mat$slope ~ mat$yr)

metric <- mat$slope

ind <- tapply(metric, mat$yr, mean, na.rm = T)
indse <- tapply(metric, mat$yr, sd, na.rm = T)

out <- lmer(metric ~ (1 | mat$site) + mat$yr)
out <- lm(metric ~ mat$yr + mat$site + 0)
summary(out)
anova(out)

stind <- summary(out)$coef[1:19, 1]
stindse <- summary(out)$coef[1:19, 2]

par(mfrow = c(2, 1))

plot(as.numeric(names(ind)), ind, ylim = c(max(ind) + 1, min(ind)-1))
lines(as.numeric(names(ind)), ind + indse, lty = 2)
lines(as.numeric(names(ind)), ind - indse, lty = 2)

points(as.numeric(names(ind)), stind, col = 2)
lines(as.numeric(names(ind)), stind + stindse, lty = 2, col = 2)
lines(as.numeric(names(ind)), stind - stindse, lty = 2, col = 2)

cor(ind, stind)
fin <- data.frame(cbind(ind, indse))

#filenam <- paste0("slopeSizeSpectrum_", grp, ".RData")
#save(fin, file = filenam)

fin$slope <- NA
fin$SE <- NA

par(mfrow = c(5, 4))
for (i in 1:length(yrs))   {
  m2 <- mat[which(mat$yr == yrs[i]), ]
  ve <- log(colSums(m2[, classes + 2]))
  plot(ve, type = "h", axes = F, xlab = "", ylab = "")
  axis(1, at = 1:10, lab = names(mat)[3:12], las = 2)
  mtext(side = 3, text = yrs[i])
  #  ve[which(ve == "-Inf")] <- NA
  out <- lm(ve ~ classes)
  
  if (sum(!is.na(ve)) > 2) { abline(out, col = 2) 
    fin$slope[i] <- summary(out)$coef[2, 1]
    fin$SE[i] <- summary(out)$coef[2, 2]  }
}

fin
fin$year <- as.numeric(rownames(fin))

plot(fin$ind, fin$slope)
cor(fin$ind, fin$slope)

plot(fin$year, fin$ind, ylim = c(max(ind) + 1, min(ind)-1))
lines(fin$year, fin$ind + fin$indse, lty = 2)
lines(fin$year, fin$ind - fin$indse, lty = 2)

points(fin$year, fin$slope, col = 2)
lines(fin$year, fin$slope + fin$SE, lty = 2, col = 2)
lines(fin$year, fin$slope - fin$SE, lty = 2, col = 2)



# density ------------------------

metric <- log(mat$total)

ind <- tapply(metric, mat$yr, mean, na.rm = T)
indse <- tapply(metric, mat$yr, sd, na.rm = T)

out <- lm(metric ~ mat$yr + mat$Island + mat$ReefComplex + mat$Depth)
summary(out)
anova(out)

out <- lm(metric ~ mat$yr + mat$Island + mat$ReefComplex + mat$Depth + 0)
summary(out)
anova(out)

stind <- summary(out)$coef[1:19, 1]
stindse <- summary(out)$coef[1:19, 2]

plot(as.numeric(names(ind)), ind, ylim = c(max(ind) + 1, min(ind)-1))
lines(as.numeric(names(ind)), ind + indse, lty = 2)
lines(as.numeric(names(ind)), ind - indse, lty = 2)

points(as.numeric(names(ind)), stind, col = 2)
lines(as.numeric(names(ind)), stind + stindse, lty = 2, col = 2)
lines(as.numeric(names(ind)), stind - stindse, lty = 2, col = 2)

cor(ind, stind)

