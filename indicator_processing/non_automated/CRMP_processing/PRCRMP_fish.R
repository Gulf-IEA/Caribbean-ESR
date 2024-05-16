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

directory <- rprojroot::find_rstudio_root_file()

setwd(directory)
setwd("indicator_data/PRCRMP")
dir()


# download data -------------------------------

urlf <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/5.5/data/0-data/PRCRMP_Fish-Invert_Abundance_data_1999-2023_(updated_11-30-2023).csv"
fish <- read.csv(urlf, stringsAsFactors = F)

urlfi <- "https://www.nodc.noaa.gov/archive/arc0147/0204647/5.5/data/0-data/PRCRMP_Fish-Invert_Size-Freq._data_2004-2023_(updated_11-30-2023).csv"
siz <- read.csv(urlfi, stringsAsFactors = F)


fish <- read.csv("PRCRMP_Fish-Invert_Abundance_data_1999-2023_(updated_11-30-2023).csv", stringsAsFactors = F)

siz <- read.csv("PRCRMP_Fish-Invert_Size-Freq._data_2004-2023_(updated_11-30-2023).csv", stringsAsFactors = F)

met <- read.csv("PRCRMP_Site_Classification_Database_(11-25-2023).csv", stringsAsFactors = F)

sitelis <- met$Site.Name
sitelis

# start analysis -----------------------------

tail(fish$YEAR, 20)
#fish <- fish[-which(is.na(fish$YEAR)), ]

table(fish$LOCATION, useNA = "always")
table(fish$DEPTH.ZONE, useNA = "always")

fish$LOCATION[which(fish$LOCATION == "Mayagüez")] <- "Mayaguez"
fish$DEPTH.ZONE[grep("mediate", fish$DEPTH.ZONE)] <- "intermediate"
fish$DEPTH.ZONE[grep("photic", tolower(fish$DEPTH.ZONE))] <- "mesophotic"
fish$DEPTH.ZONE[grep("ery", fish$DEPTH.ZONE)] <- "very shal"
fish$DEPTH.ZONE[grep("hallow", fish$DEPTH.ZONE)] <- "shallow"

apply(fish[1:10], 2, table, useNA = "always")

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

table(siz$LOCATION, useNA = "always")
table(siz$DEPTH.ZONE, useNA = "always")
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

apply(fish[1:10], 2, table, useNA = "always")

names(fish)
names(fish)[which(names(fish) == "Abudefduf.saxatilis"): (ncol(fish))]
table(fish$TRANSECT, useNA = "always")

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
image(table(fish$YEAR, fish$SITE.NAME))

# analysis of average fish density ------------------------------------

fish$var <- log(fish$dens+0.1)  

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

# functions for converting lognormal mean and SE to normal space -------------
lnorm.mean <- function(x1, x1e) {  exp(x1 + 0.5 * x1e^2)   }
lnorm.se   <- function(x1, x1e) {  ((exp(x1e^2)-1)*exp(2 * x1 + x1e^2))^0.5  } 

ind_norm   <- lnorm.mean(mod, modse)                # convert lognormal mean and SE to normal space
indse_norm <- lnorm.se(mod, modse)
  
plot(yrs, (ind_norm), ylim = c(0, 3))
lines(yrs, (ind_norm + indse_norm), lty = 2, col = 1)
lines(yrs, (ind_norm - indse_norm), lty = 2, col = 1)

points(yrs, tapply(fish$dens, fish$YEAR, mean, na.rm = T), col = 2)

fin <- data.frame(cbind(yrs, ind_norm, indse_norm))
fin

save(fin, file = "fish_density_PR.RData")

###########################  END  ###############################






##############   CODE BELOW IS ABORTED   #######################
# CODE FUNCTIONS BUT NOT SUFFICIENT DATA TO RUN ANALYSIS
# size-based indicators -------------------------------------

head(siz)
splis2

# get extract columns for commercial species -----------------
cols <- c()
for (i in splis2) { 
  co <- grep(i, names(siz))
  if (length(co) > 0)  { cols <- c(cols, co) }
  }

cols <- sort(cols)
names(siz)[cols]
dim(siz)
siz <- siz[, c(1:7, cols)]
dim(siz)
head(siz)

yrs <- yrs[which(yrs >= min(siz$YEAR))]

# reformat size data into different object --------------------

#v1 <- rep(sitelis, length(yrs))
#v2 <- sort(rep(yrs, length(sitelis)))
v2 <- yrs
v1 <- NA
mat <- matrix(data = NA, nrow = length(v1), ncol = 12)
mat <- data.frame(v1, v2, mat)
names(mat) <- c("site", "yr", paste0("c", 1:12))
head(mat)

head(siz)
names(siz)[grep(".c10", names(siz))] <- gsub(".c10", ".cX10", names(siz)[grep(".c10", names(siz))])
names(siz)[grep(".c11", names(siz))] <- gsub(".c11", ".cX11", names(siz)[grep(".c11", names(siz))])
names(siz)[grep(".c12", names(siz))] <- gsub(".c12", ".cX12", names(siz)[grep(".c12", names(siz))])
names(siz)[grep(".c13", names(siz))] <- gsub(".c13", ".cX13", names(siz)[grep(".c13", names(siz))])
names(siz)[grep(".c14", names(siz))] <- gsub(".c14", ".cX14", names(siz)[grep(".c14", names(siz))])
names(siz)[grep(".c15", names(siz))] <- gsub(".c15", ".cX15", names(siz)[grep(".c15", names(siz))])
names(siz)[grep(".c16", names(siz))] <- gsub(".c16", ".cX16", names(siz)[grep(".c16", names(siz))])
names(siz)[grep(".c17", names(siz))] <- gsub(".c17", ".cX17", names(siz)[grep(".c17", names(siz))])
names(siz)[grep(".c18", names(siz))] <- gsub(".c18", ".cX18", names(siz)[grep(".c18", names(siz))])
names(siz)[grep(".c19", names(siz))] <- gsub(".c19", ".cX19", names(siz)[grep(".c19", names(siz))])
names(siz)[grep(".c20", names(siz))] <- gsub(".c20", ".cX20", names(siz)[grep(".c20", names(siz))])
names(siz)[grep(".c30", names(siz))] <- gsub(".c30", ".cX30", names(siz)[grep(".c30", names(siz))])

names(siz)[grep(".c20", names(siz))]
names(siz)[grep(".c30", names(siz))]

# summarize by site and year ----------------------------------

i <- 1
# for (j in sitelis) { 
  for (y in yrs)     {  
    f1 <- siz[which(siz$YEAR == y), ] # & siz$SITE.NAME == j), ]
    if (nrow(f1) > 0) { 
    mat[i, 3]  <- sum(f1[, grep(".c1", names(siz))], na.rm = T)
    mat[i, 4]  <- sum(f1[, grep(".c2", names(siz))], na.rm = T)
    mat[i, 5]  <- sum(f1[, grep(".c3", names(siz))], na.rm = T)
    mat[i, 6]  <- sum(f1[, grep(".c4", names(siz))], na.rm = T)
    mat[i, 7]  <- sum(f1[, grep(".c5", names(siz))], na.rm = T)
    mat[i, 8]  <- sum(f1[, grep(".c6", names(siz))], na.rm = T)
    mat[i, 9]  <- sum(f1[, grep(".c7", names(siz))], na.rm = T)
    mat[i, 10] <- sum(f1[, grep(".c8", names(siz))], na.rm = T)
    mat[i, 11] <- sum(f1[, grep(".c9", names(siz))], na.rm = T)
    mat[i, 12] <- sum(f1[, grep(".cX10", names(siz))], na.rm = T)
    mat[i, 13] <- sum(f1[, grep(".cX11", names(siz))], na.rm = T)
    mat[i, 14] <- sum(f1[, grep(".cX12", names(siz))], na.rm = T)
    }
    i <- i + 1
  #  }
} 

head(mat)
dim(mat)
#mat <- mat[-which(rowSums(mat[3:ncol(mat)]) == 0), ]
#mat <- mat[-which(is.na(rowSums(mat[3:ncol(mat)]))), ]
dim(mat)
which(rowSums(mat[3:ncol(mat)]) == 0)

# mat is new data object with one site per year ------------

table(mat$site, mat$yr)

plot((colSums(mat[3:14])), type = "h", main = "count data")
plot(log(colSums(mat[3:14])), type = "h", main = "count data")

barplot(as.matrix(log(mat[3:14] + 1)), beside = T, col = rainbow(length(yrs)), names.arg = names(mat)[3:14])

#for (i in 1:nrow(mat)) {       # converting to proportions results in exactly the same estimates
#  mat[i, 3:(ncol(mat)-1)] <- mat[i, 3:(ncol(mat)-1)] / sum(mat[i, 3:(ncol(mat)-1)]) * 100  }

# look at size spectra and subset classes that are selected for ------------------

classes <- 3:10  

head(mat[, classes + 2], 30)

# calculate slope of the size spectrum for each site and year --------------------

mat$slope <- NA
mat$SE <- NA

par(mfrow = c(4, 5), mex = 0.5)
for (i in 1:nrow(mat)) { 
  ve <- as.numeric(log(mat[i, classes + 2]))
  plot(ve, type = "h", axes = F)
  axis(1, at = 1:8, lab = names(mat[i, classes + 2]), las = 2)
  mtext(side = 3, text = paste(mat$yr[i]))
  
  ve[which(ve == "-Inf")] <- NA
  out <- lm(ve ~ classes)
  
  if (sum(!is.na(ve)) > 2) { abline(out, col = 2) 
    mat$slope[i] <- summary(out)$coef[2, 1]
    mat$SE[i] <- summary(out)$coef[2, 2]   }
}

# merge slope estimates with data file and view ---------------------

#mat <- merge(mat, met, by.x = "site", by.y = "Location")
mat
mat$yr <- as.factor(mat$yr)

dev.off()

plot(mat$yr, mat$slope)

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

