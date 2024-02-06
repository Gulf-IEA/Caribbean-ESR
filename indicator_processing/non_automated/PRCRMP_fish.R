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
# download site classification database, fish-invert abundance data, fish-invert size-frequency data
# 
# CORRECT THIS################################
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
############################################################################
# fish data analysis

rm(list = ls())

# download data -------------------------------

urlf <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/5.5/data/0-data/PRCRMP_Fish-Invert_Abundance_data_1999-2023_(updated_11-30-2023).csv"
fish <- read.csv(urlf, stringsAsFactors = F)

urlfi <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/5.5/data/0-data/PRCRMP_Fish-Invert_Size-Freq._data_2004-2023_(updated_11-30-2023).csv"
siz <- read.csv(urlfi, stringsAsFactors = F)


fish <- read.csv("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data/PRCRMP/PRCRMP_Fish-Invert_Abundance_data_1999-2023_(updated_11-30-2023).csv", 
            stringsAsFactors = F)

siz <- read.csv("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data/PRCRMP/PRCRMP_Fish-Invert_Size-Freq._data_2004-2023_(updated_11-30-2023).csv", 
            stringsAsFactors = F)

met <- read.csv("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data/PRCRMP/PRCRMP_Site_Classification_Database_(11-25-2023).csv", 
                stringsAsFactors = F)
sitelis <- met$Site.Name
sitelis

# start analysis -----------------------------

tail(fish$YEAR, 20)
#fish <- fish[-which(is.na(fish$YEAR)), ]

table(fish$LOCATION)
table(fish$DEPTH.ZONE)

#fish$LOCATION[which(fish$LOCATION == "Mayagüez")] <- "Mayaguez"
fish$DEPTH.ZONE[grep("mediate", fish$DEPTH.ZONE)] <- "intermediate"
fish$DEPTH.ZONE[grep("photic", tolower(fish$DEPTH.ZONE))] <- "mesophotic"
fish$DEPTH.ZONE[grep("ery", fish$DEPTH.ZONE)] <- "very shal"
fish$DEPTH.ZONE[grep("hallow", fish$DEPTH.ZONE)] <- "shallow"

apply(fish[1:10], 2, table)

num <- NA
for (i in 1:ncol(fish)) { num[i] <- length(which(fish[, i] == ""))  } 
table(num)

names(fish)[num > 12]  # these are all inverts - remove - only worked for v. 4.4 
#invlis <- names(fish)[num > 12] 

invlis <- c("Ancylomenes.pedersoni", "Anomura.spp.", "Astrophyton.muricatum", "Carpilius.corallinus", "Coralliophila.salebrosa",
    "Coralliophila.spp.", "Ctenoides.scaber", "Cyphoma.gibbosum", "Maguimithrax.spinosissimus", "Diadema.antillarum", "Echinometra.lucunter",
    "Echinometra.spp.", "Echinometra.viridis", "Eucidaris.tribuloides", "Gastropoda.spp.", "Grapsus.spp.", "Hermodice.carunculata", 
    "Holothuria..Halodeima..mexicana", "Isostichopus.badionotus", "Aliger.gigas", "Octopus.vulgaris", "Ophioderma.spp.", "Ophiothrix..Acanthophiothrix..suensonii",
    "Paguristes.spp.", "Pagurus.spp.", "Panulirus.argus", "Panulirus.guttatus", "Percnon.gibbesi", "Periclimenes.spp.", "Pteria.columbus",
    "Scyllarides.spp.", "Stenopus.hispidus", "Stenorhynchus.seticornis", "Stramonita.rustica", "Tripneustes.ventricosus")

which(names(fish) %in% invlis)
dim(fish)
fish <- fish[-which(names(fish) %in% invlis)]
dim(fish)

# correction for data v.4.4 - no longer necessary? ------

which(fish == "")
#num <- NA
#for (i in 1:nrow(fish)) { num[i] <- length(which(fish[i, ] == ""))  }
#table(num)
#dim(fish); length(num)
#fish <- fish[which(num < 200), ]
#dim(fish)
#fish <- fish[-which(names(fish) == "X")]

table(is.na(siz$YEAR))
#siz <- siz[-which(is.na(siz$YEAR)), ]

# check labels and correct ------------------------------------------
table(siz$LOCATION)
table(siz$DEPTH.ZONE)
siz$LOCATION[which(siz$LOCATION == "Mayagüez")] <- "Mayaguez"
siz$DEPTH.ZONE[grep("mediate", siz$DEPTH.ZONE)] <- "intermediate"
siz$DEPTH.ZONE[grep("photic", tolower(siz$DEPTH.ZONE))] <- "mesophotic"
siz$DEPTH.ZONE[grep("ery", siz$DEPTH.ZONE)] <- "very shal"
siz$DEPTH.ZONE[grep("hallow", siz$DEPTH.ZONE)] <- "shallow"
table(siz$DEPTH.ZONE, useNA = "always")
table(siz$LOCATION, useNA = "always")

apply(siz[1:10], 2, table, useNA = "always")

# get top landed species from logbook ------------------------------------

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
splis[grep(" ", splis)]
splis[-grep(" ", splis)]
splis <- splis[grep(" ", splis)]
splis
splis <- splis[1:79]
splis
splis <- c(splis, "Carangoides bartholomaei", "Haemulon plumierii", "Trachinotus blochii", "Pristipomoides aquilonaris", 
           "Mugil cephalus", "Epinephelus adscensionis",  "Epinephelus spp.")

splis2 <- as.vector(sapply(splis, function(x) gsub(" ", ".", x)))

splis2[which(splis2 %in% names(fish))]         # these species are in the PRCRMP database
sort(splis2[-which(splis2 %in% names(fish))])  # these species are not in PRCRMP

splis2 <- splis2[which(splis2 %in% names(fish))]
splis2

yrs <- sort(unique(fish$YEAR))

apply(fish[1:10], 2, table)

names(fish)
names(fish)[which(names(fish) == "Abudefduf.saxatilis"): (ncol(fish))]

# run separately for fished and not fished -------------------------------

#fish2 <- fish[which(names(fish) == "Abudefduf.saxatilis"): (ncol(fish))];  grp <- "oth" # unfished
fish2 <- fish[which(names(fish) %in% splis2)]; grp <- "com"  # fished

names(fish2)

# calculate total fish density by site ---------------------------------
fish$dens <- NA
for (i in 1:nrow(fish2)) {  fish$dens[i] <- sum(as.numeric(fish2[i, ]))   }
hist(fish$dens)
table(fish$dens, useNA = "always")

table(fish$SITE.NAME, fish$YEAR)

# analysis of average fish density ------------------------------------

fish$var <- log(fish$dens+0.001)  

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
summary(out1)$coef[, 1]

mod <- summary(out1)$coef[, 1]
modse <- summary(out1)$coef[, 2]

plot(yrs, (ind), ylim = c(min(ind) - 1, max(ind + 1)))
lines(yrs, (ind + indse), lty = 2, col = 1)
lines(yrs, (ind - indse), lty = 2, col = 1)
points(yrs, mod, col = 2)
lines(yrs, mod + modse, col = 2, lty = 2)
lines(yrs, mod - modse, col = 2, lty = 2)

cor(ind, mod)

save(out1, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data/PRCRMP/fish_density_PR.RData")

############ NOT FINALIZED BELOW 
############ COPIED OVER FROM TRCRMP BUT NEED TO MODIFY FOR PR DATA
# size-based indicators -------------------------------------

# set up empty matrix to put fish length data in ------------
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

