# 
# script to update ALL indicators for report 
# M. Karnauskas Mar 3 2024

# load libraries ---------------------------------
rm(list = ls())

packages = c("data.table", "dplyr", "ggplot2", "ggrepel", "httr", "jsonlite", "lubridate", 
             "maps", "pals", "pdftools", "plotTimeSeries", "readr", "readxl", "rerddap", "rvest", 
              "stringr", "tidyr", "vegan", "xml2", "modi", "devtools")

## Now load or install & load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

devtools::install_github('jeremiaheb/rvc')
library(rvc)
install_github("MandyKarnauskas-NOAA/plotTimeSeries")
library(plotTimeSeries)

search()

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()

# first process automated downloads --------------

# run all scripts in folder ---------------------

# fully automated - pull from internet ----------

setwd(directory)
dir("indicator_processing/automated_download/")

source("indicator_processing/automated_download/sst.R")                   # sea surface temperature
source("indicator_processing/automated_download/DHW.R")                   # degree heating weeks
source("indicator_processing/automated_download/ACE_index_Carib.R")       # hurricane energy index
source("indicator_processing/automated_download/earthquakes.R")           # earthquakes
source("indicator_processing/automated_download/kd490.R")                 # turbidity from Kd490   # THIS DOWNLOAD CAN BE FINICKY
source("indicator_processing/automated_download/chl_caribbean.R")         # primary productivity
source("indicator_processing/automated_download/Unemployment.R")          # unemployment rate
source("indicator_processing/automated_download/GDP.R")                   # Gross Domestic Product
source("indicator_processing/automated_download/cruise_air_visitors.R")   # Cruise visitors
source("indicator_processing/automated_download/population.R")            # Human population

# partially automated - pull from data in folder ----------------------

dir("indicator_processing/non_automated")

#  source("indicator_processing/non_automated/marine_debris.R")         # marine debris
source("indicator_processing/non_automated/OA.R")                    # ocean acidification
source("indicator_processing/non_automated/CRMP_compile.R")          # fishery-indepenedent fish density, slope of size spectrum, coral cover indicators
source("indicator_processing/non_automated/Sargassum_inundation.R")  # sargassum indicator
source("indicator_processing/non_automated/SAU_recreational_catch.R") # recreational catch based on Sea Around Us database
source("indicator_processing/non_automated/pollution.R")              # reported superfund sites etc. 
source("indicator_processing/non_automated/tier_designation.R")      # ABC control rule Tier designation
source("indicator_processing/non_automated/oceanNAICS.R")      # Ocean economy
source("indicator_processing/non_automated/RVC.R")             # RVC
source("indicator_processing/non_automated/regulations.R")     # number of new regulations   # might not be updates
source("indicator_processing/non_automated/enforcement.R")    # enforcement stats
source("indicator_processing/non_automated/outreach.R")       # outreach stats
source("indicator_processing/non_automated/social_vulnerability.R")   # Tarsila social indicators

# fishery indicators - includes confidential data ----------------------

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/"

dir("indicator_processing/fishery_dependent/")

source("indicator_processing/fishery_dependent/INDICATOR_total_landings.R")   #  calc total landings by group
source("indicator_processing/fishery_dependent/INDICATOR_gearchanges.R")      #  produces plots but no indicator objects
source("indicator_processing/fishery_dependent/INDICATOR_gini.R")             #  calculate gini index based on revenues
source("indicator_processing/fishery_dependent/INDICATOR_disturbance.R")      #  calculate fishery disturbances based on distribution of landings over year
source("indicator_processing/fishery_dependent/INDICATOR_fishery_indicators_PR.R")      # % revenue, pelagic:demersal ratio, and Lmax indicators for Puerto Rico
source("indicator_processing/fishery_dependent/INDICATOR_fishery_indicators_STT.R")     # % revenue, pelagic:demersal ratio, and Lmax indicators for STT/STJ
source("indicator_processing/fishery_dependent/INDICATOR_fishery_indicators_STX.R")     # % revenue, pelagic:demersal ratio, and Lmax indicators for STX 
source("indicator_processing/fishery_dependent/compile_indicators.R")         # compile Lmax and P:D indicators for all islands


###############################################################

# Notes for standardizing scripts:  
  
#  Top: 
  
# specification file and libraries -----------------------------
rm(list = ls())
dev.off()

library(maps)
library(plotTimeSeries)

load("../spec_file.RData")

# define years  --------------------------------
styear <- 1961
enyear <- terminal_year


#  Bottom: 
#  save all indicators as "ind" object

save(ind, file = "indicator_objects/INDICATOR_NAME.RData")

##




