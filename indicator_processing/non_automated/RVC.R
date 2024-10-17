# Code to manipulate reef visual census data from https://github.com/jeremiaheb/rvc. Last updated 3/1/2024 by Carissa Gervasi
# The indicator of interest is the abundance of economically important fish.
# Identified target species for the Caribbean region: yellowtail snapper, queen triggerfish, red hind, redband parrotfish, stoplight parrotfish, mutton snapper.

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

#This is the list of calibrated species as of 2024. 
calibrated = read.csv("indicator_data/intermediateFiles/RVC/calibrated_species.csv")



rm(list = ls())

plot.new()
dev.off()


#install.packages('devtools')
#devtools::install_github('jeremiaheb/rvc')
library(rvc)

styear = 2001
enyear = 2023 #to update the plots when new data are added to the online portal, just update this end year and the code will automatically update.



# Define the regions and corresponding file paths
regions <- c("prico", "sttstj", "stx")


# Run this code if you need to pull more data from the server
##########

data_2001_2021_paths <- c("indicator_data/intermediateFiles/RVC/prico_2001_2021_calibrated.rds", 
                          "indicator_data/intermediateFiles/RVC/sttstj_2001_2021_calibrated.rds", 
                          "indicator_data/intermediateFiles/RVC/stx_2001_2021_calibrated.rds")

# Function to combine data for a given region
combine_data <- function(region, data_2001_2021_path) {
  # Load the 2001-2021 data
  data_2001_2021 <- readRDS(data_2001_2021_path)
  
  # Load the new data using the getRvcData function
  data_2023 <- getRvcData(years = 2023:enyear, regions = toupper(region))
  
  # Initialize an empty list to store combined data
  combined_data <- list()
  
  # Iterate over the list elements and combine them
  for (element in names(data_2001_2021)) {
    if (element %in% names(data_2023)) {
      # Combine data frames from both lists
      combined_data[[element]] <- bind_rows(data_2001_2021[[element]], data_2023[[element]])
    } else {
      # If the element is not in the 2023 data, just use the 2001-2021 data
      combined_data[[element]] <- data_2001_2021[[element]]
    }
  }
  
  # Add any elements that are only in the 2023 data
  for (element in names(data_2023)) {
    if (!element %in% names(data_2001_2021)) {
      combined_data[[element]] <- data_2023[[element]]
    }
  }
  
  return(combined_data)
}

# Loop over regions to combine data and save the results
for (i in seq_along(regions)) {
  region <- regions[i]
  data_2001_2021_path <- data_2001_2021_paths[i]
  
  combined_data <- combine_data(region, data_2001_2021_path)
  
  # Save the combined dataset
  saveRDS(combined_data, paste0("indicator_data/intermediateFiles/RVC/combined_", region, "_2001_2023.rds"))
}

#########
# Start here if you don't need to pull new data from the server

prico = readRDS("indicator_data/intermediateFiles/RVC/combined_prico_2001_2023.rds")
sttstj = readRDS("indicator_data/intermediateFiles/RVC/combined_sttstj_2001_2023.rds")
stx = readRDS("indicator_data/intermediateFiles/RVC/combined_stx_2001_2023.rds")

## Make a list of species
## You can use full scientific names, common names, or
## species codes (first 3 letters of genus, and first 4 of species)
## Only scientific names are case-sensitive
spcs = c("OCY CHRY", "LUT ANAL", "BAL VETU", "EPI GUTT", "SPA AURO", "SPA VIRI")


#test = getDomainDensity(prico, species = "OCY CHRY")


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
    inddata <- data.frame(density = species_data$density)
    vardata <- data.frame(var = species_data$var)
    ulidata <- data.frame(uli = species_data$density + sqrt(species_data$var))
    llidata <- data.frame(lli = species_data$density - sqrt(species_data$var))
    s <- cbind(datdata, inddata, vardata, ulidata, llidata)
   
    # save -----------------------------------------
  
    save(s, file = paste("indicator_data/intermediateFiles/RVC/RUVdensity_", j, "_", i, ".RData", sep = ""))
  }
}

# Set the directory path where the .RData files are located
folder_path <- "indicator_data/intermediateFiles/RVC"

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
inddata <- data.frame(cbind(RUVdensity_PRICO_BAL_VETU$density,
                            RUVdensity_PRICO_EPI_GUTT$density,
                            RUVdensity_PRICO_LUT_ANAL$density,
                            RUVdensity_PRICO_OCY_CHRY$density,
                            RUVdensity_PRICO_SPA_AURO$density,
                            RUVdensity_PRICO_SPA_VIRI$density))
ulidata <- data.frame(cbind(RUVdensity_PRICO_BAL_VETU$uli,
                            RUVdensity_PRICO_EPI_GUTT$uli,
                            RUVdensity_PRICO_LUT_ANAL$uli,
                            RUVdensity_PRICO_OCY_CHRY$uli,
                            RUVdensity_PRICO_SPA_AURO$uli,
                            RUVdensity_PRICO_SPA_VIRI$uli))
llidata <- data.frame(cbind(RUVdensity_PRICO_BAL_VETU$lli,
                            RUVdensity_PRICO_EPI_GUTT$lli,
                            RUVdensity_PRICO_LUT_ANAL$lli,
                            RUVdensity_PRICO_OCY_CHRY$lli,
                            RUVdensity_PRICO_SPA_AURO$lli,
                            RUVdensity_PRICO_SPA_VIRI$lli))
labs <- c("Puerto Rico" , "Density", "queen triggerfish",
          "Puerto Rico" , "Density", "red hind",
          "Puerto Rico" , "Density", "mutton snapper*",
          "Puerto Rico" , "Density", "yellowtail snapper",
          "Puerto Rico" , "Density", "redband parrotfish",
          "Puerto Rico" , "Density", "stoplight parrotfish")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"
ind <- inddata
save(ind, file = "indicator_objects/RVC_PR.RData")

# St. Thomas & St. John
datdata <- as.integer(RUVdensity_PRICO_BAL_VETU$datdata)
inddata <- data.frame(cbind(RUVdensity_STTSTJ_BAL_VETU$density,
                            RUVdensity_STTSTJ_EPI_GUTT$density,
                            RUVdensity_STTSTJ_LUT_ANAL$density,
                            RUVdensity_STTSTJ_OCY_CHRY$density,
                            RUVdensity_STTSTJ_SPA_AURO$density,
                            RUVdensity_STTSTJ_SPA_VIRI$density))
ulidata <- data.frame(cbind(RUVdensity_STTSTJ_BAL_VETU$uli,
                            RUVdensity_STTSTJ_EPI_GUTT$uli,
                            RUVdensity_STTSTJ_LUT_ANAL$uli,
                            RUVdensity_STTSTJ_OCY_CHRY$uli,
                            RUVdensity_STTSTJ_SPA_AURO$uli,
                            RUVdensity_STTSTJ_SPA_VIRI$uli))
llidata <- data.frame(cbind(RUVdensity_STTSTJ_BAL_VETU$lli,
                            RUVdensity_STTSTJ_EPI_GUTT$lli,
                            RUVdensity_STTSTJ_LUT_ANAL$lli,
                            RUVdensity_STTSTJ_OCY_CHRY$lli,
                            RUVdensity_STTSTJ_SPA_AURO$lli,
                            RUVdensity_STTSTJ_SPA_VIRI$lli))
labs <- c("St. Thomas & St. John" , "Density", "queen triggerfish",
          "St. Thomas & St. John" , "Density", "red hind",
          "St. Thomas & St. John" , "Density", "mutton snapper*",
          "St. Thomas & St. John" , "Density", "yellowtail snapper",
          "St. Thomas & St. John" , "Density", "redband parrotfish",
          "St. Thomas & St. John" , "Density", "stoplight parrotfish")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"
ind <- inddata
save(ind, file = "indicator_objects/RVC_STSJ.RData")


# St. Croix
datdata <- as.integer(RUVdensity_PRICO_BAL_VETU$datdata)
inddata <- data.frame(cbind(RUVdensity_STX_BAL_VETU$density,
                            RUVdensity_STX_EPI_GUTT$density,
                            RUVdensity_STX_LUT_ANAL$density,
                            RUVdensity_STX_OCY_CHRY$density,
                            RUVdensity_STX_SPA_AURO$density,
                            RUVdensity_STX_SPA_VIRI$density))
ulidata <- data.frame(cbind(RUVdensity_STX_BAL_VETU$uli,
                            RUVdensity_STX_EPI_GUTT$uli,
                            RUVdensity_STX_LUT_ANAL$uli,
                            RUVdensity_STX_OCY_CHRY$uli,
                            RUVdensity_STX_SPA_AURO$uli,
                            RUVdensity_STX_SPA_VIRI$uli))
llidata <- data.frame(cbind(RUVdensity_STX_BAL_VETU$lli,
                            RUVdensity_STX_EPI_GUTT$lli,
                            RUVdensity_STX_LUT_ANAL$lli,
                            RUVdensity_STX_OCY_CHRY$lli,
                            RUVdensity_STX_SPA_AURO$lli,
                            RUVdensity_STX_SPA_VIRI$lli))
labs <- c("St. Croix" , "Density", "queen triggerfish",
          "St. Croix" , "Density", "red hind",
          "St. Croix" , "Density", "mutton snapper*",
          "St. Croix" , "Density", "yellowtail snapper",
          "St. Croix" , "Density", "redband parrotfish",
          "St. Croix" , "Density", "stoplight parrotfish")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"
ind <- inddata
save(ind, file = "indicator_objects/RVC_STX.RData")


load("indicator_objects/RVC_PR.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("indicator_objects/RVC_STSJ.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("indicator_objects/RVC_STX.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

print("RVC abundances -- SUCCESSFULLY RUN")



