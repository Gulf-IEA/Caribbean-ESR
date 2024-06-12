# 
# script to plot ALL indicators for report 
# C. Gervasi Jun 6 2024

# load libraries ---------------------------------
rm(list = ls())

library(plotTimeSeries)

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()

# source all .RData files in indicator_objects folder --------------

# run all scripts in folder ---------------------

setwd(directory)
dir("indicator_objects")

# fully automated - pull from internet ----------

#source("sst.RData")                   # sea surface temperature
source("ACEindex.RData")       # hurricane energy index
source("carib_Chl.RData")         # primary productivity
source("DegreeHeatingWeeks.RData")                   # degree heating weeks
source("earthquakes.RData")           # earthquakes
#source("kd490.R")                 # turbidity from Kd490   # THIS DOWNLOAD CAN BE FINICKY
source("unemployment.RData")          # unemployment rate
source("GDP.RData")                   # Gross Domestic Product

# partially automated - pull from data in folder ----------------------

source("marine_debris.RData")         # marine debris
source("OA.RData")                    # ocean acidification
source("indicator_processing/non_automated/CRMP_compile.R")          # fishery-indepenedent fish density, slope of size spectrum, coral cover indicators
source("indicator_processing/non_automated/Sargassum_inundation.R")  # sargassum indicator
source("indicator_processing/non_automated/SAU_recreational_catch.R") # recreational catch based on Sea Around Us database
source("indicator_processing/non_automated/pollution.R")              # reported superfund sites etc. 

# fishery indicators - includes confidential data ----------------------

confpath <- "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/"

dir("indicator_processing/fishery_dependent/")

source("indicator_processing/fishery_dependent/INDICATOR_total_landings.R")   #  calc total landings by group
source("indicator_processing/fishery_dependent/INDICATOR_gearchanges.R")      #  produces plots but no indicator objects
source("indicator_processing/fishery_dependent/INDICATOR_gini.R")             #  calculate gini index based on revenues
# not done below

# 
source("indicator_processing/fishery_dependent/INDICATOR_disturbance.R")                # 
source("indicator_processing/fishery_dependent/INDICATOR_fishery_indicators_PR.R")      # 
source("indicator_processing/fishery_dependent/INDICATOR_fishery_indicators_STT.R")     #  
source("indicator_processing/fishery_dependent/INDICATOR_fishery_indicators_STX.R")     #  





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
