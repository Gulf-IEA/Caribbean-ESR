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

# NOTE: as of 3/1/2024 calibrations have not been completed for all species. This means the data on the portal only go back to 2016. To get the full time series (2001 onward) we needed to get the calibrated data from Jeremiah. Species specific calibrations were done. The calibrated_species.csv file is a list of all the species that have thus far been calibrated. All species of interest have been calibrated except for mutton snapper. For mutton snapper we can only use data from 2017 onward.

calibrated = read.csv("indicator_data/RVC/calibrated_species.csv")

prico = readRDS("indicator_data/RVC/prico_2001_2021_calibrated.rds")
sttstj = readRDS("indicator_data/RVC/sttstj_2001_2021_calibrated.rds")
stx = readRDS("indicator_data/RVC/stx_2001_2021_calibrated.rds")

## Make a list of species
## You can use full scientific names, common names, or
## species codes (first 3 letters of genus, and first 4 of species)
## Only scientific names are case-sensitive
spcs = c("OCY CHRY", "LUT ANAL", "BAL VETU", "EPI GUTT", "SPA AURO", "SPA VIRI")



## Download desired Regions data from server (for these 3 regions the data appear to only go back to 2016)
carib = getRvcData(years = 2001:2021, regions = c("PRICO","STTSTJ","STX"))



## Calculate statistics for entire sampling domain
ddens = getDomainDensity(prico, species = spcs)
dabun = getDomainAbundance(prico, species = spcs)
docc = getDomainOccurrence(prico, species = spcs)

