# Code to manipulate reef visual census data from https://github.com/jeremiaheb/rvc. Last updated 3/1/2024 by Carissa Gervasi
# The indicator of interest is the abundance of economically important fish.
# Identified target species for the Caribbean region: yellowtail snapper, queen triggerfish, red hind, redband parrotfish, stoplight parrotfish, mutton snapper.

#install.packages('devtools')
#devtools::install_github('jeremiaheb/rvc')
library(rvc)

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()  # MK - set root path so that all links below work with master file 

# first process automated downloads --------------

setwd(directory)

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
    
    # If the species is mutton snapper (LUT ANAL), change density values to NA before 2017
    if (i == "LUT ANAL") {
      species_data$density[species_data$YEAR < 2017] <- NA
    }
  
    # format indicator object -----------------------------
  
    datdata <- species_data$YEAR
    inddata <- data.frame(species_data$density)
    s <- cbind(datdata, inddata) 
   
    # save -----------------------------------------
  
    save(s, file = paste("indicator_data/RVC/RUVdensity_", j, "_", i, ".RData", sep = ""))
  }
}

# Set the directory path where the .RData files are located
folder_path <- "indicator_data/RVC"

# List all the files in the directory
files <- list.files(path = folder_path, pattern = "\\.RData$", full.names = TRUE)

# Load all .RData files
for (file in files) {
  # Replace spaces with underscores in object names
  object_names <- gsub(" ", "_", gsub(".RData", "", basename(file)))
  
  # Load the file
  loaded_objects <- load(file)
  
  # Assign loaded objects to global environment with modified names
  for (i in seq_along(loaded_objects)) {
    assign(object_names[i], get(loaded_objects[i]), envir = .GlobalEnv)
  }
}



# format indicator object -----------------------------

# OCY CHRY --> yellowtail snapper
# LUT ANAL  --> mutton snapper*
# BAL VETU  --> queen triggerfish
# EPI GUTT  --> red hind
# SPA AURO  --> redband parrotfish
# SPA VIRI  --> stoplight parrotfish

# Puerto Rico
datdata <- as.integer(RUVdensity_PRICO_BAL_VETU$datdata)
inddata <- data.frame(cbind(RUVdensity_PRICO_BAL_VETU$species_data.density, RUVdensity_PRICO_EPI_GUTT$species_data.density, RUVdensity_PRICO_LUT_ANAL$species_data.density, RUVdensity_PRICO_OCY_CHRY$species_data.density, RUVdensity_PRICO_SPA_AURO$species_data.density, RUVdensity_PRICO_SPA_VIRI$species_data.density))
labs <- c("Puerto Rico" , "Density", "queen triggerfish",
          "Puerto Rico" , "Density", "red hind",
          "Puerto Rico" , "Density", "mutton snapper*",
          "Puerto Rico" , "Density", "yellowtail snapper",
          "Puerto Rico" , "Density", "redband parrotfish",
          "Puerto Rico" , "Density", "stoplight parrotfish")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"
ind <- inddata
save(ind, file = "indicator_objects/RVC_PR.RData")

# St. Thomas & St. John
datdata <- as.integer(RUVdensity_PRICO_BAL_VETU$datdata)
inddata <- data.frame(cbind(RUVdensity_STTSTJ_BAL_VETU$species_data.density, RUVdensity_STTSTJ_EPI_GUTT$species_data.density, RUVdensity_STTSTJ_LUT_ANAL$species_data.density, RUVdensity_STTSTJ_OCY_CHRY$species_data.density, RUVdensity_STTSTJ_SPA_AURO$species_data.density, RUVdensity_STTSTJ_SPA_VIRI$species_data.density))
labs <- c("St. Thomas & St. John" , "Density", "queen triggerfish",
          "St. Thomas & St. John" , "Density", "red hind",
          "St. Thomas & St. John" , "Density", "mutton snapper*",
          "St. Thomas & St. John" , "Density", "yellowtail snapper",
          "St. Thomas & St. John" , "Density", "redband parrotfish",
          "St. Thomas & St. John" , "Density", "stoplight parrotfish")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"
ind <- inddata
save(ind, file = "indicator_objects/RVC_STSJ.RData")


# St. Croix
datdata <- as.integer(RUVdensity_PRICO_BAL_VETU$datdata)
inddata <- data.frame(cbind(RUVdensity_STX_BAL_VETU$species_data.density, RUVdensity_STX_EPI_GUTT$species_data.density, RUVdensity_STX_LUT_ANAL$species_data.density, RUVdensity_STX_OCY_CHRY$species_data.density, RUVdensity_STX_SPA_AURO$species_data.density, RUVdensity_STX_SPA_VIRI$species_data.density))
labs <- c("St. Croix" , "Density", "queen triggerfish",
          "St. Croix" , "Density", "red hind",
          "St. Croix" , "Density", "mutton snapper*",
          "St. Croix" , "Density", "yellowtail snapper",
          "St. Croix" , "Density", "redband parrotfish",
          "St. Croix" , "Density", "stoplight parrotfish")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"
ind <- inddata
save(ind, file = "indicator_objects/RVC_STX.RData")


load("indicator_objects/RVC_PR.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("indicator_objects/RVC_STSJ.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("indicator_objects/RVC_STX.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)



### Left to troubleshoot:

# Need to take out mutton snapper and do that one separately because the data are not calibrated. Need to chop it so only use data from 2017 onward.

