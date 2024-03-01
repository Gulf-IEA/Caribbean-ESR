# 
# script to update ALL indicators for report 
# M. Karnauskas Mar 3 2024

# load libraries ---------------------------------
rm(list = ls())

library(plotTimeSeries)

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()

# first process automated downloads --------------

setwd(directory)
setwd("indicator_processing/automated_download/")
dir()

# run all scripts in folder ---------------------

plot(1)

source("ACE_index_Carib.R")       # hurricane energy index
source("chl_caribbean.R")         # primary productivity
source("DHW.R")                   # degree heating weeks
source("earthquakes.R")           # earthquakes
source("kd490.R")                 # turbidity from Kd490
source("sst.R")                   # sea surface temperature


###############################################################

# Notes for standardizing scripts:  
  
#  Top: 
  
# specification file and libraries -----------------------------
rm(list = ls())
dev.off()

library(maps)
library(plotTimeSeries)

load("spec_file.RData")

# define years  --------------------------------
styear <- 1961
enyear <- terminal_year

  
#  Bottom: 
#  save all indicators as "ind" object

save(ind, file = "../../indicator_objects/INDICATOR_NAME.RData")

##
