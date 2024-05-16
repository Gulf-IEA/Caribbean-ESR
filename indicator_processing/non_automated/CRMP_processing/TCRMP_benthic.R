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

# download excel from google doc and convert to .csv file; save locally
# 4 tabs from benthic database need to be saved as separate csvs: SiteMetadata, BenthicCodes, BenthicData, BenthicSummary
#
# Benthic data: permanent transects surveyed annually, six 10-m long video transects, 
# substrate determined for 20 randomly allocated points
# 
#################################################################################

rm(list = ls())
if(!require(lme4)){install.packages("lme4")}
library(lme4)

directory <- rprojroot::find_rstudio_root_file()

setwd(directory)
setwd("indicator_data/TCRMP")
dir()

# read in site data -------------------------------------

met <- read.csv("SiteMetadata.csv")
apply(met, 2, table, useNA = "always")

hist(met$YearAdded)
table(met$YearAdded)  # use only pre-2005 sites? 

maps::map("world", xlim = c(-68, -63), ylim = c(16, 19))
points(met$Longitude, met$Latitude, col = as.numeric(met$Island), pch = 19)
table(met$Island)
points(met$Longitude, met$Latitude, col = as.numeric(met$ReefComplex), pch = 19)
table(met$ReefComplex)

# read in benthic code fields ---------------------------

cds <- read.csv("BenthicCodes.csv", stringsAsFactors = F)
cds$Cat2 <- cds$Category
cds$Meaning[grep("spp.", cds$Meaning)]
cds$Cat2[grep("spp.", cds$Meaning)] <- "X"

# create list of coral species ---------------------------

cds$Meaning
cds$Meaning[which(cds$Cat2 == "Coral")]
coralspplis <- cds$Code[which(cds$Cat2 == "Coral")]
coralspplis
length(coralspplis)        # should be 53

# read in benthic cover data ----------------------------

ben <- read.csv("BenthicData.csv", stringsAsFactors = F)
head(ben)

which(names(ben) %in% coralspplis)
corals <- ben[which(names(ben) %in% coralspplis)]
names(corals) == coralspplis
summary(names(corals) == coralspplis)

# calculate coral species richness ----------------------
corals[corals > 0]
corals[corals > 0] <- 1
ben$corspprich <- rowSums(corals)
hist(ben$corspprich)

# read in benthic summary data --------------------------

bensum <- read.csv("BenthicSummary.csv", stringsAsFactors = F)
dim(ben)
dim(bensum)
head(ben)
head(bensum)

table(ben$Period)
ben <- ben[which(ben$Period == "Annual"), ]
bensum <- bensum[which(bensum$Period == "Annual"), ]

# create unique sample ID, for merging databases ------------------------

ben$ID <- paste0(ben$SampleYear, ben$SampleMonth, ben$Location, ben$Transect)
bensum$ID <- paste0(bensum$SampleYear, bensum$SampleMonth, bensum$Location, bensum$Transect)
length(unique(ben$ID))
length(unique(bensum$ID))
dim(ben)
dim(bensum)

bennew <- merge(ben, bensum, by = "ID")
dim(bennew)
head(bennew)

# create summary dataset for further analysis -----------------------------
names(bennew[c(2:5, 8, 10, 130, 137)])
ben2 <- bennew[c(2:5, 8, 10, 130, 137)]
names(ben2) <- c("SampleYear", "SampleMonth", "Period", "Location", "AnalysisBy", "Transect", "corspprich", "CoralCov")

apply(ben2, 2, table, useNA = "always")

# clean factors and add more factors from site reference database ------------

ben2$AnalysisBy[grep("Allen-Requa", ben2$AnalysisBy)] <- "Laurie Allen-Requa"
ben2$AnalysisBy[grep("Celest", ben2$AnalysisBy)] <- "Celest Mosher"

ben2$Island <- met$Island[match(ben2$Location, met$Location)]
ben2$Depth  <- met$Depth[match(ben2$Location, met$Location)]
ben2$ReefCm <- met$ReefComplex[match(ben2$Location, met$Location)]
ben2$YearAd <- met$YearAdded[match(ben2$Location, met$Location)]

head(ben2)

# modular analysis - two variables -----------------------------------
# calculate spp richness and % cover, using GLM to standardize for site random effect

for (i in 1:2)  {
  
  if (i == 1)  { varint <- "sprich"  } else {  varint <- "percov"  } 
  
  if (varint == "sprich")  {  ben2$var <- ben2$corspprich  }
  if (varint == "percov")  {  ben2$var <- ben2$CoralCov    }

barplot(tapply(ben2$var, ben2$SampleYear, mean, na.rm = T), main = varint)
barplot(tapply(ben2$var, ben2$Location, mean, na.rm = T), las = 2, main = varint)
barplot(tapply(ben2$var, ben2$Island, mean, na.rm = T), main = varint)
barplot(tapply(ben2$var, ben2$ReefCm, mean, na.rm = T), main = varint)
barplot(tapply(ben2$var, ben2$AnalysisBy, mean, na.rm = T), las = 2, main = varint)

ind <- tapply(ben2$var, ben2$SampleYear, mean, na.rm = T)
indse <- tapply(ben2$var, ben2$SampleYear, sd, na.rm = T)/ sqrt(table(ben2$SampleYear))

ben2$SampleYear <- as.factor(ben2$SampleYear)
ben2$Transect <- as.factor(ben2$Transect)

hist(ben2$var, main = varint)

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
summary(out1)$coef[, 1]

mod <- summary(out1)$coef[, 1]
modse <- summary(out1)$coef[, 2]

plot(2001:2021, ind, ylim = c(min(ind) - 2, max(ind + 2)), main = varint)
lines(2001:2021, ind + indse)
lines(2001:2021, ind - indse)
points(2001:2021, mod, col = 2)
lines(2001:2021, mod + modse, col = 2, lty = 2)
lines(2001:2021, mod - modse, col = 2, lty = 2)

cor(ind, mod)

if (varint == "sprich")  {  save(out1, file = "coralspprich_USVI.RData")  }
if (varint == "percov")  {  save(out1, file = "percoralcov_USVI.RData")   }
}

##############################  END ###############################