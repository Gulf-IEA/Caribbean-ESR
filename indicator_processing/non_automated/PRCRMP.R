###############################################################################
# PRCRMP data analysis
# M. Karnauskas 06/29/2023
#
# data access at: https://www.coris.noaa.gov/search/catalog/main/home.page
# or direct access thru ncei: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0204647
# automatic download via HTTP: https://www.nodc.noaa.gov/archive/arc0147/0204647/4.4/data/0-data/
#
# last data download: June 29, 2023
################################################################################
#
# download .csv files through http directly 
# download site classification database, benthic sessile data, fish-invert abundance data, fish-invert size-frequency data
# 
# CORRECT THIS################################
# Benthic data: permanent transects surveyed annually, six 10-m long video transects, substrate determined for 20 randomly allocated points
#
# Fish data: 24 historical sites and 32 additional sites as of 2012
# 10 replicate belt transects and three replicate roving dive surveys per site
# 
#################################################################################
#
# methodology notes: 
# compared method of subsetting only sites that are monitored regularly in sampling years
# with only using all sites but standardizing as a function of site (random effect)
# results were similar (98.5% correlation); used standardization method for longer time series
#################################################################################

rm(list = ls())
if(!require(lme4)){install.packages("lme4")}
library(lme4)

url <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/4.4/data/0-data/PRCRMP_Site_Classification_Database_(1-24-2022).csv"
met <- read.csv(url, stringsAsFactors = F)

urlb <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/4.4/data/0-data/PRCRMP_Benthic-sessile_data_1999-2021_(updated_12-6-2021).csv"
ben <- read.csv(urlb, stringsAsFactors = F)

head(met)
head(ben)

dim(met)
dim(ben)

tail(ben$YEAR, 20)
ben <- ben[-which(is.na(ben$YEAR)), ]

apply(met, 2, table, useNA = "always")

ben$LOCATION[which(ben$LOCATION == "Mayagüez")] <- "Mayaguez"
ben$DEPTH.ZONE[grep("mediate", ben$DEPTH.ZONE)] <- "intermediate"
ben$DEPTH.ZONE[grep("photic", tolower(ben$DEPTH.ZONE))] <- "mesophotic"
ben$DEPTH.ZONE[grep("ery", ben$DEPTH.ZONE)] <- "very shal"
ben$DEPTH.ZONE[grep("hallow", ben$DEPTH.ZONE)] <- "shallow"

hist(met$Baseline.Year)
table(met$Baseline.Year)  # use only pre-2005 sites? 
summary(met$X..Surveyed.Years >= 12)

met[59, ]  # Lat is 18.07169; 18.34732  Lon is -67.93698;-67.26997
met$Longitude <- as.numeric(met$Longitude)
met$Latitude <- as.numeric(met$Latitude)
met$Longitude[59] <- -67.93698
met$Latitude[59] <- 18.07169

maps::map("world", xlim = c(-69, -64), ylim = c(17, 19))
points(met$Longitude, met$Latitude, col = (as.numeric(as.factor(met$Location))), pch = 19)

met$loc <- paste0(-round(met$Longitude, 2), "_", round(met$Latitude, 2))

#met[which(met$X..Surveyed.Years >= 12), ]
#met <- met[which(met$X..Surveyed.Years >= 12), ]

maps::map("world", xlim = c(-69, -64), ylim = c(17, 19))
points(met$T1_Lon, met$T1_Lat, col = as.numeric(as.factor(met$Geographic.Zone)), pch = 19)
table(met$Geographic.Zone)
points(met$T1_Lon, met$T1_Lat, col = as.numeric(as.factor(met$Region)), pch = 19)
table(met$Region)
points(met$T1_Lon, met$T1_Lat, col = as.numeric(as.factor(met$Habitat.Type)), pch = 19)
table(met$Habitat.Type)
points(met$T1_Lon, met$T1_Lat, col = as.numeric(as.factor(met$Insular.Shelf.Zone)), pch = 19)
table(met$Insular.Shelf.Zone)
points(met$T1_Lon, met$T1_Lat, col = as.numeric(as.factor(met$Reef.Zonation)), pch = 19)
table(met$Reef.Zonation)
points(met$T1_Lon, met$T1_Lat, col = as.numeric(as.factor(met$Depth.Zone)), pch = 19)
table(met$Depth.Zone)

#sitelis <- met$Site.Name
#sitelis
#ben <- ben[ben$SITE.NAME %in% sitelis, ]

sort(unique(met$Site.Name)) == sort(unique(ben$SITE.NAME))

table(ben$SITE.NAME, ben$YEAR)   # start with 2004 and combine 2018/19? 
ben$SITE.NAME[which(ben$SITE.NAME == "Maria Langa 5m ")] <- "Maria Langa 5m"
ben$SITE.NAME[which(ben$SITE.NAME == "Windward Reef ")] <- "Windward Reef"

sort(unique(met$Site.Name)) == sort(unique(ben$SITE.NAME))

# Stony Corals (total) == Substrate cover percentage by all Stony Coral species, including hyrocorals (Millepora)
ben$Stony.Corals..total. <- as.numeric(ben$Stony.Corals..total.)

which(names(ben) == "Acropora.cervicornis")
which(names(ben) == "Stylaster.roseus")
coralspplis <- names(ben)[which(names(ben) == "Acropora.cervicornis"):which(names(ben) == "Stylaster.roseus")]
coralspplis[grep("spp.", coralspplis)]
coralspplis <- coralspplis[-grep("spp.", coralspplis)] 
coralspplis[grep("complex", coralspplis)]
coralspplis <- coralspplis[-grep("complex", coralspplis)]
coralspplis

which(names(ben) %in% coralspplis)
corals <- ben[which(names(ben) %in% coralspplis)]
names(corals) == coralspplis
summary(names(corals) == coralspplis)

corals[corals > 0] <- 1
for (i in 1:nrow(corals)) {  corals[i, which(corals[i, ] == "")] <- 0  }
ben$corspprich <- NA
for (i in 1:nrow(corals)) {  ben$corspprich[i] <- sum(as.numeric(corals[i, ]))   }
hist(ben$corspprich)

#ben2 <- ben[which(ben$YEAR > 2001), ]          # if using only consistent sites thru time
#ben2$YEAR[which(ben2$YEAR == 2018)] <- 2019

ben2 <- merge(ben, met, by.x = "SITE.NAME", by.y = "Site.Name")
table(ben2$SITE.NAME, ben2$YEAR)
table(ben2$loc, ben2$YEAR)

yrs <- sort(unique(ben2$YEAR))

# modular analysis - change variable here! 

varint <- "sprich"
#varint <- "percov"

if (varint == "sprich")  {  ben2$var <- ben2$corspprich  }
if (varint == "percov")  {  ben2$var <- ben2$Stony.Corals..total.    }

hist(ben2$var)

par(mar = c(15, 5, 1, 1))
barplot(tapply(ben2$var, ben2$REGION, mean, na.rm = T), las = 2)
barplot(tapply(ben2$var, ben2$LOCATION, mean, na.rm = T), las = 2)
barplot(tapply(ben2$var, ben2$SITE.NAME, mean, na.rm = T), las = 2, cex.names = 0.7)
barplot(tapply(ben2$var, ben2$loc, mean, na.rm = T), las = 2, cex.names = 0.7)
barplot(tapply(ben2$var, ben2$Depth.Zone, mean, na.rm = T), las = 2)
barplot(tapply(ben2$var, ben2$Reef.Zonation, mean, na.rm = T), las = 2)  #
barplot(tapply(ben2$var, ben2$Habitat.Type, mean, na.rm = T), las = 2)
barplot(tapply(ben2$var, ben2$Insular.Shelf.Zone, mean, na.rm = T), las = 2) # 
barplot(tapply(ben2$var, ben2$Coral.Biotope, mean, na.rm = T), las = 2) # 

ind <- tapply(ben2$var, ben2$YEAR, mean, na.rm = T)
indse <- tapply(ben2$var, ben2$YEAR, sd, na.rm = T)/ sqrt(table(ben2$YEAR))

ben2$YEAR <- as.factor(ben2$YEAR)

out <- lm(var ~ YEAR + LOCATION + Depth.Zone + Reef.Zonation + Insular.Shelf.Zone + Coral.Biotope + Habitat.Type, data = ben2)
summary(out)
anova(out) 

out <- lm(var ~ YEAR + SITE.NAME, data = ben2)
summary(out)
anova(out)

out1 <- lmer(var ~ YEAR + (1 | loc) + 0, data = ben2)
summary(out1)
anova(out1)
summary(out1)$coef[1: length(yrs), 1]

mod <- summary(out1)$coef[1:length(yrs), 1]
modse <- summary(out1)$coef[1:length(yrs), 2]

plot(yrs, ind, ylim = c(min(ind) - 1, max(ind + 1)))
lines(yrs, ind + indse, lty = 2, col = 1)
lines(yrs, ind - indse, lty = 2, col = 1)

points(yrs, mod, col = 2)
lines(yrs, mod + modse, col = 2, lty = 2)
lines(yrs, mod - modse, col = 2, lty = 2)

cor(ind, mod)

if (varint == "sprich")  {  save(out1, file = "coralspprich_PR.RData")  }
if (varint == "percov")  {  save(out1, file = "percoralcov_PR.RData")   }


############################################################################
# fish data analysis

rm(list = ls())

urlf <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/4.4/data/0-data/PRCRMP_Fish-Invert Abundance_data_1999-2021_(updated_12-6-2021).csv"
fish <- read.csv(urlf, stringsAsFactors = F)

urlfi <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/4.4/data/0-data/PRCRMP_Fish-Invert Size-Freq_data_1999-2021_(updated_12-6-2021).csv"
siz <- read.csv(urlfi, stringsAsFactors = F)

tail(fish$YEAR, 20)
fish <- fish[-which(is.na(fish$YEAR)), ]

fish$LOCATION[which(fish$LOCATION == "Mayagüez")] <- "Mayaguez"
fish$DEPTH.ZONE[grep("mediate", fish$DEPTH.ZONE)] <- "intermediate"
fish$DEPTH.ZONE[grep("photic", tolower(fish$DEPTH.ZONE))] <- "mesophotic"
fish$DEPTH.ZONE[grep("ery", fish$DEPTH.ZONE)] <- "very shal"
fish$DEPTH.ZONE[grep("hallow", fish$DEPTH.ZONE)] <- "shallow"

apply(fish[1:10], 2, table)

num <- NA
for (i in 1:ncol(fish)) { num[i] <- length(which(fish[, i] == ""))  } 
table(num)
names(fish)[num > 12]  # these are all inverts - remove
invlis <- names(fish)[num > 12] 

fish <- fish[-which(names(fish) %in% invlis)]

num <- NA
for (i in 1:nrow(fish)) { num[i] <- length(which(fish[i, ] == ""))  }
table(num)
dim(fish); length(num)
fish <- fish[which(num < 200), ]
dim(fish)
fish <- fish[-which(names(fish) == "X")]

siz <- siz[-which(is.na(siz$YEAR)), ]

siz$LOCATION[which(siz$LOCATION == "Mayagüez")] <- "Mayaguez"
siz$DEPTH.ZONE[grep("mediate", siz$DEPTH.ZONE)] <- "intermediate"
siz$DEPTH.ZONE[grep("photic", tolower(siz$DEPTH.ZONE))] <- "mesophotic"
siz$DEPTH.ZONE[grep("ery", siz$DEPTH.ZONE)] <- "very shal"
siz$DEPTH.ZONE[grep("hallow", siz$DEPTH.ZONE)] <- "shallow"

apply(siz[1:10], 2, table)

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
splis <- splis[1:79]
splis
splis <- c(splis, "Carangoides bartholomaei", "Haemulon plumierii", "Trachinotus blochii", "Pristipomoides aquilonaris", "Mugil cephalus", "Epinephelus adscensionis",  "Epinephelus spp.")

splis2 <- as.vector(sapply(splis, function(x) gsub(" ", ".", x)))

splis2[which(splis2 %in% names(fish))]
sort(splis2[-which(splis2 %in% names(fish))])

# run separately for STX and STT/STJ
#sitelis <- met$Location[which(met$YearAdded < 2005)]
#sitelis <- met$Location[which(met$YearAdded < 2005 & met$Island == "STX")]; island <- "STX"  # for STX
#sitelis <- met$Location[which(met$YearAdded < 2005 & met$Island != "STX")]; island <- "STT" # for STT/STJ

splis2 <- splis2[which(splis2 %in% names(fish))]
splis2

yrs <- sort(unique(fish$YEAR))

apply(fish[1:10], 2, table)

#fish <- fish[which(fish$Location %in% sitelis), ]

# run separately for fished and not fished
fish2 <- fish[which(names(fish) == "Abudefduf.saxatilis"): (ncol(fish)-1)];  grp <- "oth" # unfished
#fish2 <- fish[which(names(fish) %in% splis2)]; grp <- "com"  # fished

fish$dens <- NA
for (i in 1:nrow(fish2)) {  fish$dens[i] <- sum(as.numeric(fish2[i, ]))   }
hist(fish$dens)
table(fish$dens)

table(fish$SITE.NAME, fish$YEAR)

# modular analysis - change variable here! 

varint <- "density"
#varint <- "percov"

if (varint == "density")  {  fish$var <- log(fish$dens+0.001)  }
#if (varint == "percov")  {  fish$var <-    }

hist(fish$var)

barplot(tapply(fish$var, fish$REGION, mean, na.rm = T), las = 2)
barplot(tapply(fish$var, fish$LOCATION, mean, na.rm = T), las = 2)
barplot(tapply(fish$var, fish$SITE.NAME, mean, na.rm = T), las = 2)
barplot(tapply(fish$var, fish$DEPTH.ZONE, mean, na.rm = T), las = 2)

ind <- as.numeric(tapply(fish$var, fish$YEAR, mean, na.rm = T))
indse <- tapply(fish$var, fish$YEAR, sd, na.rm = T)/ sqrt(table(fish$YEAR))

fish$YEAR <- as.factor(fish$YEAR)

out <- lm(var ~ YEAR + REGION + LOCATION + DEPTH.ZONE, data = fish)
summary(out)
anova(out) 

out <- lm(var ~ YEAR + SITE.NAME, data = fish)
summary(out)
anova(out)

out1 <- lmer(var ~ YEAR + (1 | SITE.NAME) + 0, data = fish)
summary(out1)
anova(out1)
summary(out1)$coef[1: length(yrs), 1]

mod <- summary(out1)$coef[1:length(yrs), 1]
modse <- summary(out1)$coef[1:length(yrs), 2]

plot(yrs, ind, ylim = c(min(ind) - 1, max(ind + 1)))
lines(yrs, ind + indse, lty = 2, col = 1)
lines(yrs, ind - indse, lty = 2, col = 1)

points(yrs, mod, col = 2)
lines(yrs, mod + modse, col = 2, lty = 2)
lines(yrs, mod - modse, col = 2, lty = 2)

cor(ind, mod)

#if (varint == "sprich")  {  save(out1, file = "coralspprich_PR.RData")  }
#if (varint == "percov")  {  save(out1, file = "percoralcov_PR.RData")   }

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

