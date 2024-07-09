# 
# script to plot ALL indicators for report - DRAFT, STILL WORKING ON 
# C. Gervasi Jun 6 2024

rm(list = ls())

#  find root directory for project ---------------

directory <- rprojroot::find_rstudio_root_file()
setwd(directory)


library(plotTimeSeries)
library(spam)

### 1. Degree heating weeks

png(file="indicator_plots/DegreeHeatingWeeks.png", width=5, height=4, units="in", res=250)
load("indicator_objects/DegreeHeatingWeeks.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T, dateformat = "%Y%m", plotrownum = 2, plotcolnum = 1)
dev.off()

### 2. Ocean acidification via aragonite saturation state

load("indicator_objects/OA.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T)

### 3. Hurricane activity

load("indicator_objects/ACEindex.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T)

### 4. Turbidity

load("indicator_objects/turbidity.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 2, trendAnalysis = T, dateformat = "%m-%Y", sublabel = T)
                        
### 5. Sea surface temperature

load("indicator_objects/Carib_SST.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, trendAnalysis = T, sublabel = T, dateformat = "%m-%Y")
                                                
### 6. Marine debris
                                                
load("indicator_objects/marine_debris.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = F)
                                              
### 7. Identified point source pollution sites

load("indicator_objects/pollution.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = F, sublabel = T)
                                              
### 8. Primary productivity via ocean color
                                                
load("indicator_objects/carib_Chl.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T, dateformat = "%m-%Y")
                                              
### 9. Coastal development via land cover

# This is just a figure so skip
                                                
### 10. Number of major earthquakes
                                                
load("indicator_objects/earthquakes.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T)
                                              
### 11. Fishery/market disturbance indicator 

load("indicator_objects/disturbance.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, trendAnalysis = T, sublabel = T)
                                                
### 12. Sargassum inundation
                                                
load("indicator_objects/Sargassum.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T)
                                              
### 13. Tourism via hotel occupancy
                                                                        
load("indicator_objects/hotel_occupancy_rates_USVI_and_PR.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = T, trendAnalysis = T, dateformat = "%Y%b")

load("indicator_objects/hotel_occupancy.RData")                      
plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = T, trendAnalysis = T,dateformat = "%Y%b")
                                                                                          ### 14. Population change

# still working on this (census data plus maps?)

### 15. Fishery independent surveys of economically important species

load("indicator_objects/RVC_PR.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("indicator_objects/RVC_STSJ.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("indicator_objects/RVC_STX.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

### 16. Fish density 

load("indicator_objects/fish_density.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T)

### 17. Slope of the size spectrum

# missing

### 18. Pelagic:demersal ratio

load("indicator_objects/PD_ratio.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, trendAnalysis = T, sublabel = T)

### 19 Maximum length and size structure

load("indicator_objects/PR_Lmax_classes.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:5, trendAnalysis = T, sublabel = T)

load("indicator_objects/STT_Lmax_classes.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:5, trendAnalysis = T, sublabel = T)

### 20. Total landings

load("indicator_objects/total_landings.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, trendAnalysis = T, sublabel = T)
                        
### 21. Percent revenues by species group
                        
# Just figures - per_landings_<island>
                        
### 22. Number of trips
                        
# Just figures - gear_types_<island> 
                        
### 23. Ocean economy employment and wages
                        
load("indicator_objects/oceanNAICS.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:8, plotrownum = 2, trendAnalysis = T, sublabel = T)
                        
### 24. GDP

load("indicator_objects/GDP.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T)
                    
### 25. Unemployment

load("indicator_objects/unemployment.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T, dateformat = "%Y%b")
                        
### 26. Gini coefficient for distribution of landings and revenue

load("indicator_objects/gini.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 2, trendAnalysis = T, sublabel = T) 
                        
### 27. Commercial fishing community engagement and reliance
                        
# Think this should be the EJ, Economic, & gentrification pressure indicators
                        
### 28. Recreational landings
                        
load("indicator_objects/total_rec_catch.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T) 
                        
### 29. Commercial fishing engagement and participation

# Maps from Tarsila - <island>_comm_fishing_engagement and <island>_comm_fishing_reliance
                        
### 30. Changes in gear type
                        
# Just plots - gear_types_<island>;  NMDSgear_<island>

### 31. Number of seasonal closures implemented
                        
#Indicator 31

### 32. Number of seasonal closures implemented

load("indicator_objects/tier3.RData")
plotIndicatorTimeSeries(ind, plotrownum = 2, coltoplot = 1:2, sublabel = TRUE, dateformat = "%Y%b", trendAnalysis = T)
                        
### 33. Number of education and outreach events
                        
load("indicator_objects/outreach.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, trendAnalysis = T)
                        
### 34. Number of enforcement actions
                        
#Indicator 33
                        
### 35. Percent coral cover
                      
load("indicator_objects/coral_spprichness_cover.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:4, sublabel = T, trendAnalysis = T)
                        
### 36. Coral species diversity

load("indicator_objects/coral_spprichness_cover.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:4, sublabel = T, trendAnalysis = T)
                        
                          