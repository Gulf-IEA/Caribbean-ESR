# 
# script to update ALL indicators for report 
# M. Karnauskas Mar 3 2024

# load libraries ---------------------------------
rm(list = ls())

library(plotTimeSeries)

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()

# first process automated downloads --------------

# run all scripts in folder ---------------------

plot(1)

# fully automated - pull from internet ----------

setwd(directory)
dir("indicator_processing/automated_download/")

source("indicator_processing/automated_download/sst.R")                   # sea surface temperature
source("indicator_processing/automated_download/ACE_index_Carib.R")       # hurricane energy index
source("indicator_processing/automated_download/chl_caribbean.R")         # primary productivity
source("indicator_processing/automated_download/DHW.R")                   # degree heating weeks
source("indicator_processing/automated_download/earthquakes.R")           # earthquakes
source("indicator_processing/automated_download/kd490.R")                 # turbidity from Kd490   # THIS DOWNLOAD CAN BE FINICKY
source("indicator_processing/automated_download/Unemployment.R")          # unemployment rate
source("indicator_processing/automated_download/GDP.R")                   # Gross Domestic Product

# partially automated - pull from data in folder ----------------------

dir("indicator_processing/non_automated")

source("indicator_processing/non_automated/marine_debris.R")         # marine debris
source("indicator_processing/non_automated/OA.R")                    # ocean acidification
source("indicator_processing/non_automated/CRMP_compile.R")          # fishery-indepenedent fish density, slope of size spectrum, coral cover indicators
source("indicator_processing/non_automated/Sargassum_inundation.R")  # sargassum indicator
source("indicator_processing/non_automated/SAU_recreational_catch.R") # recreational catch based on Sea Around Us database
source("indicator_processing/non_automated/pollution.R")              # reported superfund sites etc. 



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

save(ind, file = "../../indicator_objects/INDICATOR_NAME.RData")

##
