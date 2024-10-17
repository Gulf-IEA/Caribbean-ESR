
## compile TCRMP and PRCRMP into data plots
## M. Karnauskas 2/8/2024
#
# run all codes in 'Caribbean-ESR\indicator_processing\non_automated\CRMP_processing' first
# PRCRMP_benthic.R, TCRMP_benthic.R, PRCRMP_fish.R and TCRMP_fish.R 
# these output data objects to Caribbean-ESR\indicator_data\PRCRMP and Caribbean-ESR\indicator_data\TCRMP
# 

rm(list = ls())

plot.new()
dev.off()

#directory <- rprojroot::find_rstudio_root_file()
#setwd(directory)
#dir()

library(plotTimeSeries)

# load coral indicator data -------------------------

load("indicator_data/intermediateFiles/PRCRMP/coralspprich_PR.RData")
PRsr <- out1
load("indicator_data/intermediateFiles/PRCRMP/percoralcov_PR.RData") 
PRcc <- out1
load("indicator_data/intermediateFiles/TCRMP/coralspprich_USVI.RData")
VIsr <- out1
load("indicator_data/intermediateFiles/TCRMP/percoralcov_USVI.RData") 
VIcc <- out1

# extract years from linear mixed model ----------------------

y1 <- as.numeric(gsub("YEAR", "", rownames(summary(PRsr)$coef)))
y2 <- as.numeric(gsub("YEAR", "", rownames(summary(PRcc)$coef)))
y3 <- as.numeric(gsub("SampleYear", "", rownames(summary(VIsr)$coef)))
y4 <- as.numeric(gsub("SampleYear", "", rownames(summary(VIcc)$coef)))

y1 == y2
y3 == y4
yrs <- (min(c(y1, y2, y3, y4))) : (max(c(y1, y2, y3, y4)))

# extract coefficients from linear mixed model ----------------------

mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 4))
rownames(mat) <- yrs
mat[match(y1, yrs), 1] <- summary(PRsr)$coef[, 1] 
mat[match(y2, yrs), 2] <- summary(PRcc)$coef[, 1] 
mat[match(y3, yrs), 3] <- summary(VIsr)$coef[, 1] 
mat[match(y4, yrs), 4] <- summary(VIcc)$coef[, 1] 

matse <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 4))
rownames(matse) <- yrs
matse[match(y1, yrs), 1] <- summary(PRsr)$coef[, 2] 
matse[match(y2, yrs), 2] <- summary(PRcc)$coef[, 2] 
matse[match(y3, yrs), 3] <- summary(VIsr)$coef[, 2] 
matse[match(y4, yrs), 4] <- summary(VIcc)$coef[, 2] 

# format indicator object --------------------

datdata <- yrs
inddata <- data.frame(mat)
ulidata <- data.frame(mat + matse)
llidata <- data.frame(mat - matse)
labs <- c("Coral species richness" , "average number per transect", "Puerto Rico",
          "Percent coral cover" , "percent cover", "Puerto Rico",
          "Coral species richness" , "average number per transect", "USVI", 
          "Percent coral cover" , "percent cover", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1:4, plotrownum = 2, sublabel = T, sameYscale = F, 
                        widadj = 1, hgtadj = 1, trendAnalysis = T, type = "allLines", CItype = "band")

ind <- s

save(ind, file = "indicator_objects/coral_spprichness_cover.RData")



## compile TCRMP and PRCRMP into data plots
## M. Karnauskas 2/8/2024
#
# run PRCRMP_benthic.R, TCRMP_benthic.R, PRCRMP_fish.R and TCRMP_fish.R first
# 

rm(list = ls())

# load fish indicator data -------------------------

load("indicator_data/intermediateFiles/PRCRMP/fish_density_PR.RData")
PR <- fin
load("indicator_data/intermediateFiles/TCRMP/fish_density_USVI.RData") 
VI <- findens
load("indicator_data/intermediateFiles/TCRMP/slopeSizeSpec_USVI.RData") 
sl <- fin

# extract years ----------------------

PR$yrs == VI$yrs
PR$yrs == sl$yrs
yrs <- (min(c(PR$yrs, VI$yrs, sl$yrs))) : (max(c(PR$yrs, VI$yrs, sl$yrs)))

# extract coefficients from linear mixed model ----------------------

mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 3))
rownames(mat) <- yrs
mat[match(PR$yrs, yrs), 1] <- PR$ind_norm
mat[match(VI$yrs, yrs), 2] <- VI$ind_norm
mat[match(sl$yrs, yrs), 3] <- sl$stind

matse <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 3))
rownames(matse) <- yrs
matse[match(PR$yrs, yrs), 1] <- PR$indse_norm
matse[match(VI$yrs, yrs), 2] <- VI$indse_norm
matse[match(sl$yrs, yrs), 3] <- sl$stindse

# format indicator object --------------------

datdata <- yrs
inddata <- data.frame(mat)
ulidata <- data.frame(mat + matse)
llidata <- data.frame(mat - matse)
labs <- c("Commercial fish density" , "average number per transect", "Puerto Rico",
          "Commercial fish density" , "average number per transect", "USVI", 
          "Slope of the size spectrum" , "slope of log distribution", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, sublabel = T, sameYscale = F, 
                        widadj = 1, hgtadj = 1, trendAnalysis = T, type = "allLines", CItype = "band")

ind <- s

save(ind, file = "indicator_objects/fish_density.RData")


#########################  END  ############################

print("CRMP fishery-dependent -- SUCCESSFULLY RUN")