# Code to manipulate reef visual census data from https://github.com/jeremiaheb/rvc. Last updated 3/1/2024 by Carissa Gervasi
# The indicator of interest is the abundance of economically important fish.
# Identified target species for the Caribbean region: yellowtail snapper, queen triggerfish, red hind, redband parrotfish, stoplight parrotfish, mutton snapper.

install.packages('devtools')
devtools::install_github('jeremiaheb/rvc')
library(rvc)

# The data portal has more information on the data: https://grunt.sefsc.noaa.gov/rvc_analysis20/

# Species of interest:

# OCY CHRY --> yellowtail snapper
# LUT ANAL  --> mutton snapper*
# BAL VETU  --> queen triggerfish
# EPI GUTT  --> red hind
# SPA AURO  --> redband parrotfish
# SPA VIRI  --> stoplight parrotfish

# Regions of interest:

# PRICO  --> Puerto Rico
# STTSTJ  --> St. Thomas & St. John
# STX  --> St. Croix

# NOTE: as of 3/1/2024 calibrations have not been completed for all species. This means the data on the portal only go back to 2016. To get the full time series (2001 onward) we needed to get the calibrated data from Jeremiah Blondeau (jeremiah.blondeau@noaa.gov). Species specific calibrations were done. The calibrated_species.csv file is a list of all the species that have thus far been calibrated. All species of interest have been calibrated except for mutton snapper. For mutton snapper we can only use data from 2017 onward.

calibrated = read.csv("indicator_data/RVC/calibrated_species.csv")

prico = readRDS("indicator_data/RVC/prico_2001_2021_calibrated.rds")
sttstj = readRDS("indicator_data/RVC/sttstj_2001_2021_calibrated.rds")
stx = readRDS("indicator_data/RVC/stx_2001_2021_calibrated.rds")

## Make a list of species
## You can use full scientific names, common names, or
## species codes (first 3 letters of genus, and first 4 of species)
## Only scientific names are case-sensitive
spcs = c("OCY CHRY", "LUT ANAL", "BAL VETU", "EPI GUTT", "SPA AURO", "SPA VIRI")


## Code to bring in data from the server if needed for updating
## Download desired Regions data from server 
#carib = getRvcData(years = 2016:2021, regions = c("PRICO","STTSTJ","STX"))


# Extract the time series for each species and each area

regions = c("PRICO","STTSTJ","STX")
for(j in regions) {
  # call data for the current region
  dat = switch(j, 
                "PRICO" = prico,
                "STTSTJ" = sttstj,
                "STX" = stx)
  
  ## Calculate statistics for entire sampling domain
  ddens = getDomainDensity(dat, species = spcs)
  
  # Convert SPECIES_CD to factor to ensure correct ordering in plots
  ddens$SPECIES_CD <- factor(ddens$SPECIES_CD)
  
  species <- unique(ddens$SPECIES_CD)
  for (i in species) {
    # Subset data for the current species
    species_data <- filter(ddens, SPECIES_CD == i)
  
    # format indicator object -----------------------------
  
    datdata <- species_data$YEAR
    inddata <- data.frame(species_data$density)
    labs <- c(j, paste("Density", i), "")
    indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
    s <- list(labels = indnames, indicators = inddata, datelist = datdata) 
    class(s) <- "indicatordata"
   
    # save and plot -----------------------------------------
    plotIndicatorTimeSeries(s, trendAnalysis = F)
  
    inddata <- s
    #save(inddata, file = paste("indicator_objects/RUVdensity_", j, "_", i, ".RData", sep = ""))
  }
}

####### SAVE FILES TO INDICATOR_OBJECTS ONCE ALL PROBLEMS ARE FIXED


### Left to troubleshoot:

# Year is not showing up in the plots. Need to figure out what to label the plots. Need to take out mutton snapper and do that one separately because the data are not calibrated.

