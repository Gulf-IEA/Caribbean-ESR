###############################################################################
# PRCRMP data analysis
# M. Karnauskas 06/29/2023
#
# data access at: https://www.coris.noaa.gov/search/catalog/main/home.page
# or direct access thru ncei: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0204647
# automatic download via HTTP: https://www.nodc.noaa.gov/archive/arc0147/0204647/
#
# last data download: Feb 6, 2024
################################################################################
#
# download .csv files through http directly 
# download site classification database, benthic sessile data
# 
# CORRECT THIS################################
# Benthic data: permanent transects surveyed annually, six 10-m long video transects, substrate determined for 20 randomly allocated points
#
#################################################################################
#
# Methodology notes: 
# Compared method of 1) subsetting sites that are monitored regularly in sampling years only 
# with 2) using all sites but standardizing as a function of site (random effect).  
# Results were similar (98.5% correlation); used standardization method for longer time series
#################################################################################

rm(list = ls())
#if(!require(lme4)){install.packages("lme4")}
library(lme4)

directory <- rprojroot::find_rstudio_root_file()

setwd(directory)
setwd("indicator_data/PRCRMP")
dir()

# download data -------------------------------

url <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/5.5/data/0-data/PRCRMP_Site_Classification_Database_(1-24-2022).csv"
met <- read.csv(url, stringsAsFactors = F)

urlb <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/4.4/data/0-data/PRCRMP_Benthic-sessile_data_1999-2021_(updated_12-6-2021).csv"
ben <- read.csv(urlb, stringsAsFactors = F)

met <- read.csv("PRCRMP_Site_Classification_Database_(11-25-2023).csv", stringsAsFactors = F)
ben <- read.csv("PRCRMP_Benthic-sessile_data_1999-2023_(updated_11-30-2023).csv", stringsAsFactors = F)

head(met)
head(ben)

dim(met)
dim(ben)

tail(ben$YEAR, 20)
ben <- ben[-which(is.na(ben$YEAR)), ]

apply(met, 2, table, useNA = "always")

# check labeling and reclassify --------------------------------
table(ben$LOCATION, useNA = "always")
table(ben$DEPTH.ZONE, useNA = "always")

ben$LOCATION[which(ben$LOCATION == "Mayagüez")] <- "Mayaguez"
ben$DEPTH.ZONE[grep("mediate", ben$DEPTH.ZONE)] <- "intermediate"
ben$DEPTH.ZONE[grep("photic", tolower(ben$DEPTH.ZONE))] <- "mesophotic"
ben$DEPTH.ZONE[grep("ery", ben$DEPTH.ZONE)] <- "very shal"
ben$DEPTH.ZONE[grep("hallow", ben$DEPTH.ZONE)] <- "shallow"
table(ben$LOCATION, useNA = "always")
table(ben$DEPTH.ZONE, useNA = "always")

# look at timeline of data collection -----------------------------
hist(met$Baseline.Year)
table(met$Baseline.Year)  # use only pre-2005 sites? 
summary(met$X..Surveyed.Years >= 12)

# correct one erroneous site in database - manual!  ---------------
met[44, ]  # Lat is 18.07169; 18.34732  Lon is -67.93698;-67.26997
met$Longitude <- as.numeric((met$Longitude))  # should get one NA coerced
met$Latitude <- as.numeric((met$Latitude))
met$Longitude[44] <- -67.93698
met$Latitude[44] <- 18.07169

maps::map("world", xlim = c(-69, -64), ylim = c(17, 19))
points(met$Longitude, met$Latitude, col = (as.numeric(as.factor(met$Location))), pch = 19)

met$loc <- paste0(-round(met$Longitude, 2), "_", round(met$Latitude, 2))  # location identifier

#met[which(met$X..Surveyed.Years >= 12), ]
#met <- met[which(met$X..Surveyed.Years >= 12), ]

# look at data distribution ------------------------------------
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

# check that site names match ----------------------------------

sort(unique(met$Site.Name)) == sort(unique(ben$SITE.NAME))
cbind(sort(unique(met$Site.Name)), sort(unique(ben$SITE.NAME)))
ben$SITE.NAME[which(ben$SITE.NAME == "Maria Langa 5m ")] <- "Maria Langa 5m"
ben$SITE.NAME[which(ben$SITE.NAME == "Windward Reef ")] <- "Windward Reef"
ben$SITE.NAME[which(ben$SITE.NAME == "Berbería")] <- "Berberia"
ben$SITE.NAME[which(ben$SITE.NAME == "Cana Gorda")] <- "Caña Gorda"
sort(unique(met$Site.Name)) == sort(unique(ben$SITE.NAME))
table(sort(unique(met$Site.Name)) == sort(unique(ben$SITE.NAME)))

table(ben$SITE.NAME, ben$YEAR)   # start with 2004 and combine 2018/19? 

# Stony Corals (total) == Substrate cover percentage by all Stony Coral species, including hyrocorals (Millepora)
ben$Stony.Corals..total. <- as.numeric(ben$Stony.Corals..total.)

# refine coral species list, take out complexes ------------

which(names(ben) == "Acropora.cervicornis")
which(names(ben) == "Stylaster.roseus")
coralspplis <- names(ben)[which(names(ben) == "Acropora.cervicornis"):which(names(ben) == "Stylaster.roseus")]
coralspplis
coralspplis[grep("spp.", coralspplis)]
coralspplis <- coralspplis[-grep("spp.", coralspplis)] 
coralspplis[grep("complex", coralspplis)]
coralspplis <- coralspplis[-grep("complex", coralspplis)]
coralspplis

which(names(ben) %in% coralspplis)
corals <- ben[which(names(ben) %in% coralspplis)]
names(corals) == coralspplis
summary(names(corals) == coralspplis)
corals <- data.frame(corals, stringsAsFactors = F)
corals

# replace erroneous cells with zeros ------------------------------

for (i in 1:nrow(corals)) {  corals[i, which(corals[i, ] == "")] <- 0  }
for (i in 1:ncol(corals)) {  corals[, i] <- as.numeric(corals[, i])    }
corals
corals[1:5, 1:5]
corals[corals > 0]
corals[corals > 0] <- 1

# calculate coral species richness ---------------------------------

ben$corspprich <- NA
for (i in 1:nrow(corals)) {  ben$corspprich[i] <- sum(as.numeric(corals[i, ]))   }
hist(ben$corspprich)

#ben2 <- ben[which(ben$YEAR > 2001), ]          # if using only consistent sites thru time
#ben2$YEAR[which(ben2$YEAR == 2018)] <- 2019

# merge site reference info with database --------------------------

ben2 <- merge(ben, met, by.x = "SITE.NAME", by.y = "Site.Name")
dim(ben)
dim(met)
dim(ben2)
table(ben2$SITE.NAME, ben2$YEAR)
table(ben2$loc, ben2$YEAR)

yrs <- sort(unique(ben2$YEAR))

# modular analysis - two variables -----------------------------------
# calculate spp richness and % cover, using GLM to standardize for site random effect

for (i in 1:2)  {

if (i == 1)  { varint <- "sprich"  } else {  varint <- "percov"  } 

if (varint == "sprich")  {  ben2$var <- ben2$corspprich  }
if (varint == "percov")  {  ben2$var <- ben2$Stony.Corals..total.    }

hist(ben2$var)

par(mar = c(10, 4, 3, 1))
barplot(tapply(ben2$var, ben2$REGION, mean, na.rm = T), las = 2, main = varint)
barplot(tapply(ben2$var, ben2$LOCATION, mean, na.rm = T), las = 2, main = varint)
barplot(tapply(ben2$var, ben2$SITE.NAME, mean, na.rm = T), las = 2, cex.names = 0.7, main = varint)
barplot(tapply(ben2$var, ben2$loc, mean, na.rm = T), las = 2, cex.names = 0.7, main = varint)
barplot(tapply(ben2$var, ben2$Depth.Zone, mean, na.rm = T), las = 2, main = varint)
barplot(tapply(ben2$var, ben2$Reef.Zonation, mean, na.rm = T), las = 2, main = varint)  #
barplot(tapply(ben2$var, ben2$Habitat.Type, mean, na.rm = T), las = 2, main = varint)
barplot(tapply(ben2$var, ben2$Insular.Shelf.Zone, mean, na.rm = T), las = 2, main = varint) # 
barplot(tapply(ben2$var, ben2$Coral.Biotope, mean, na.rm = T), las = 2, main = varint) # 

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

par(mar = c(4, 4, 3, 1))
plot(yrs, ind, ylim = c(min(ind) * 0.6, max(ind) * 1.1), main = varint)
lines(yrs, ind + indse, lty = 2, col = 1)
lines(yrs, ind - indse, lty = 2, col = 1)

points(yrs, mod, col = 2)
lines(yrs, mod + modse, col = 2, lty = 2)
lines(yrs, mod - modse, col = 2, lty = 2)

cor(ind, mod)

if (varint == "sprich")  {  save(out1, file = "coralspprich_PR.RData")  }
if (varint == "percov")  {  save(out1, file = "percoralcov_PR.RData")   }

}

#####################  END  ############################


