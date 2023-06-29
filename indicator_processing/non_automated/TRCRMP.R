###############################################################################
# TCRMP data analysis
# M. Karnauskas 06/27/2023
#
# data access at: https://sites.google.com/site/usvitcrmp/available-data
# benthic data downloaded from: https://docs.google.com/spreadsheets/d/1If0CsxbG469kv6U_GyZ0t9ewAtlm03zx/edit#gid=697263468
# fish data downloaded from: https://docs.google.com/spreadsheets/d/1-FvhfMAFs5hNEqmYm0EJd2P23BBYeV7_/edit#gid=926142160
#
# last data download: June 27, 2023
################################################################################

# download excel from google doc and convert to .csv file; save locally
# 4 tabs from benthic database need to be saved as separate csvs: SiteMetadata, BenthicCodes, BenthicData, BenthicSummary
# 2 tabs from fish database need to be saved as separate csvs: FishMetadata, FishData
#
# Benthic data: permanent transects surveyed annually, six 10-m long video transects, substrate determined for 20 randomly allocated points
#
# Fish data: 24 historical sites and 32 additional sites as of 2012
# 10 replicate belt transects and three replicate roving dive surveys per site
# 
#################################################################################

rm(list = ls())
if(!require(lme4)){install.packages("lme4")}
library(lme4)

setwd("../../Desktop/Caribbean-ESR/indicator_data/TCRMP")

met <- read.csv("SiteMetadata.csv")
hist(met$YearAdded)
table(met$YearAdded)  # use only pre-2005 sites? 

maps::map("world", xlim = c(-68, -63), ylim = c(16, 19))
points(met$Longitude, met$Latitude, col = as.numeric(met$Island), pch = 19)
table(met$Island)
points(met$Longitude, met$Latitude, col = as.numeric(met$ReefComplex), pch = 19)
table(met$ReefComplex)

cds <- read.csv("BenthicCodes.csv", stringsAsFactors = F)
cds$Cat2 <- cds$Category
cds$Meaning[grep("spp.", cds$Meaning)]
cds$Cat2[grep("spp.", cds$Meaning)] <- "X"

cds$Meaning[which(cds$Cat2 == "Coral")]
coralspplis <- cds$Code[which(cds$Cat2 == "Coral")]
coralspplis
length(coralspplis)        # should be 53

ben <- read.csv("BenthicData.csv", stringsAsFactors = F)
head(ben)
which(names(ben) %in% coralspplis)
corals <- ben[which(names(ben) %in% coralspplis)]
names(corals) == coralspplis
summary(names(corals) == coralspplis)

corals[corals > 0] <- 1
ben$corspprich <- rowSums(corals)
hist(ben$corspprich)

bensum <- read.csv("BenthicSummary.csv", stringsAsFactors = F)
dim(ben)
dim(bensum)
head(ben)
head(bensum)

ben <- ben[which(ben$Period == "Annual"), ]
bensum <- bensum[which(bensum$Period == "Annual"), ]

ben$ID <- paste0(ben$SampleYear, ben$SampleMonth, ben$Location, ben$Transect)
bensum$ID <- paste0(bensum$SampleYear, bensum$SampleMonth, bensum$Location, bensum$Transect)
length(unique(ben$ID))
length(unique(bensum$ID))
dim(ben)
dim(bensum)

bennew <- merge(ben, bensum, by = "ID")
dim(bennew)
head(bennew)

ben2 <- bennew[c(2:5, 8, 10, 130, 137)]
names(ben2) <- c("SampleYear", "SampleMonth", "Period", "Location", "AnalysisBy", "Transect", "corspprich", "CoralCov")


apply(ben2, 2, table, useNA = "always")

ben2$AnalysisBy[grep("Allen-Requa", ben2$AnalysisBy)] <- "Laurie Allen-Requa"
ben2$AnalysisBy[grep("Celest", ben2$AnalysisBy)] <- "Celest Mosher"

ben2$Island <- met$Island[match(ben2$Location, met$Location)]
ben2$Depth  <- met$Depth[match(ben2$Location, met$Location)]
ben2$ReefCm <- met$ReefComplex[match(ben2$Location, met$Location)]
ben2$YearAd <- met$YearAdded[match(ben2$Location, met$Location)]

# modular analysis - change variable here! 

#varint <- "sprich"
varint <- "percov"

if (varint == "sprich")  {  ben2$var <- ben2$corspprich  }
if (varint == "percov")  {  ben2$var <- ben2$CoralCov    }

barplot(tapply(ben2$var, ben2$SampleYear, mean, na.rm = T))
barplot(tapply(ben2$var, ben2$Location, mean, na.rm = T), las = 2)
barplot(tapply(ben2$var, ben2$Island, mean, na.rm = T))
barplot(tapply(ben2$var, ben2$ReefCm, mean, na.rm = T))
barplot(tapply(ben2$var, ben2$AnalysisBy, mean, na.rm = T), las = 2)

ind <- tapply(ben2$var, ben2$SampleYear, mean, na.rm = T)
indse <- tapply(ben2$var, ben2$SampleYear, sd, na.rm = T)/ sqrt(table(ben2$SampleYear))

ben2$SampleYear <- as.factor(ben2$SampleYear)
ben2$Transect <- as.factor(ben2$Transect)

hist(ben2$var)

#if (varint == "percov")  {  ben2$var <- sqrt(sqrt(ben2$CoralCov))    }

out <- lm(var ~ SampleYear + Location + AnalysisBy, data = ben2)
summary(out)
anova(out) 

out <- lm(var ~ SampleYear + ReefCm + AnalysisBy, data = ben2)
summary(out)
anova(out)

out <- lm(var ~ SampleYear + ReefCm + Island, data = ben2)
summary(out)
anova(out)

out1 <- lmer(var ~ SampleYear + (1 | Location) + (1 | AnalysisBy) + 0, data = ben2)
summary(out1)
anova(out1)
summary(out1)$coef[1:21, 1]

mod <- summary(out1)$coef[1:21, 1]
modse <- summary(out1)$coef[1:21, 2]

plot(2001:2021, ind, ylim = c(min(ind) - 3, max(ind + 3)))
lines(2001:2021, mod, col = 2)
lines(2001:2021, mod + modse, col = 2, lty = 2)
lines(2001:2021, mod - modse, col = 2, lty = 2)
lines(2001:2021, ind + indse)
lines(2001:2021, ind - indse)
cor(ind, summary(out1)$coefficients[1:21, 1])

if (varint == "sprich")  {  save(out1, file = "coralspprich_USVI.RData")  }
if (varint == "percov")  {  save(out1, file = "percoralcov_USVI.RData")   }

############################################################################
# fish data analysis

rm(list = ls())

# get top landed species -----------------
d <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20.csv")
d <- d[which(d$YEAR_LANDED >= 2003 & d$YEAR_LANDED <= 2020), ]
table(d$YEAR_LANDED, useNA = "always")
d$ITIS_SCIENTIFIC_NAME <- droplevels(d$ITIS_SCIENTIFIC_NAME)

sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T))
par(mar = c(10, 5, 1, 1))
barplot(sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T), decreasing = T)[1:50], las = 2)
quantile(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T), probs = c(0.05, 0.1, 0.2, 0.5, 0.9))
quantile(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T), probs = c(0.5))  

splis <- names(which(sort(tapply(d$ADJUSTED_POUNDS, d$ITIS_SCIENTIFIC_NAME, sum, na.rm = T)) > 1068.57))
splis[-grep(" ", splis)]
splis <- splis[grep(" ", splis)]
splis
splis <- splis[1:79]
splis

spp <- read.csv("FishMetadata.csv", stringsAsFactors = F)
splis2 <- spp$ScientificName[which(spp$Commercial == "Y")]

splis2 %in% splis
splis2 <- splis2[-which(splis2 %in% splis)]  # should be 5 species missing from landings list
splis2

table(splis %in% spp$ScientificName)
splis[which(splis %in% spp$ScientificName)]
sort(splis[-which(splis %in% spp$ScientificName)]) # these are mostly pelagic or invertebrate species, ok

splis <- c(splis, splis2, "Carangoides bartholomaei", "Haemulon plumierii", "Trachinotus blochii", "Pristipomoides aquilonaris", "Mugil cephalus")
splis
length(splis)

met <- read.csv("SiteMetadata.csv", stringsAsFactors = F)
met <- met[1:8]

hist(met$YearAdded)
table(met$YearAdded)  # use only pre-2005 sites? 

table(met$Island, met$Location)

# run separately for STX and STT/STJ
sitelis <- met$Location[which(met$YearAdded < 2005)]
#sitelis <- met$Location[which(met$YearAdded < 2005 & met$Island == "STX")]; island <- "STX"  # for STX
#sitelis <- met$Location[which(met$YearAdded < 2005 & met$Island != "STX")]; island <- "STT" # for STT/STJ

splis
sitelis

fish <- read.csv("FishData.csv", stringsAsFactors = F)
head(fish)
dim(fish)

yrs <- sort(unique(fish$SampleYear))

apply(fish[1:10], 2, table)

barplot(sort(table(fish$CommonName), decreasing = T)[1:30], las = 2)

fish <- fish[which(fish$Metric == "Abundance"), ]
fish <- fish[which(fish$Period == "Annual"), ]

fish <- fish[which(fish$Location %in% sitelis), ]
apply(fish[1:10], 2, table)
dim(fish)

# run separately for fished and not fished
# fish <- fish[-which(fish$ScientificName %in% splis), ]; grp <- "oth" # unfished
fish <- fish[which(fish$ScientificName %in% splis), ]; grp <- "com"  # fished

apply(fish[1:10], 2, table)
dim(fish)

table(fish$Location, fish$SampleYear)
matplot(2003:2021, table(fish$SampleYear, fish$Location), type = "l")

names(fish)[11:21]

v1 <- rep(sitelis, length(yrs))
v2 <- sort(rep(yrs, length(sitelis)))
mat <- matrix(data = NA, nrow = length(v1), ncol = length(names(fish)[11:21]))
mat <- data.frame(v1, v2, mat)
names(mat) <- c("site", "yr", "X0.10", names(fish)[13:21], "total")
head(mat)

# summarize by site and year
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
mat
mat <- mat[-which(rowSums(mat[3:ncol(mat)]) == 0), ]
mat

table(mat$site, mat$yr)

#for (i in 1:nrow(mat)) {  mat[i, 3:ncol(mat)] <- mat[i, 3:ncol(mat)] / sum(mat[i, 3:ncol(mat)]/ 100)  }

barplot(as.matrix(log(mat[3:12] + 1)), beside = T, col = rainbow(length(yrs)), names.arg = names(mat)[3:12])

plot((colSums(mat[3:12])), type = "h")
plot(log(colSums(mat[3:12])), type = "h")

if (grp == "com")  {  classes <- 2:5  }
if (grp != "com")  {  classes <- 1:5  }

head(mat[, classes + 2], 100)

mat$slope <- NA
mat$SE <- NA

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
fin <- cbind(ind, indse)

#filenam <- paste0("slopeSizeSpectrum_", grp, ".RData")
#save(fin, file = filenam)

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
