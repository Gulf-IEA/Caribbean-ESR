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

load("indicator_objects/DegreeHeatingWeeks.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T, dateformat = "%Y%m", plotrownum = 2, plotcolnum = 1)

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
                                                
# Just a figure
                                                
### 10. Number of major earthquakes
                                                
load("indicator_objects/earthquakes.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T)
                                              
### 11. Fishery/market disturbance indicator 

load("indicator_objects/disturbance.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:3, trendAnalysis = T, sublabel = T)
                                                
### 12. Sargassum inundation
                                                
load("indicator_objects/Sargassum.RData")
plotIndicatorTimeSeries(ind, trendAnalysis = T)
                                              
### 13. Tourism via hotel occupancy
                                                                        
load("indicator_objects/hotel_occupancy_rates_USVI_and_PR.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = T, trendAnalysis = T, dateformat = "%Y%b")

load("indicator_objects/hotel_occupancy.RData")                      
plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = T, trendAnalysis = T,dateformat = "%Y%b")
                                                                                          ### 14. Population change

#14 - MAPS but also should be census data

### 15. Fishery independent surveys of economically important species

load("../indicator_objects/RVC_PR.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("../indicator_objects/RVC_STSJ.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("../indicator_objects/RVC_STX.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

#Indicator 16.1 - abundance of commercial fish based on TCRMP and PRCRMP 

#Indicator 16.2 - slope of the size spectrum based on TCRMP and PRCRMP

### 17. Commercial landings

load("../indicator_objects/landings.RData") 
plotIndicatorTimeSeries(inddata, coltoplot = 1:6, trendAnalysis = T, sublabel = T)

load("../indicator_objects/fish_density.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 2, trendAnalysis = T, sublabel = T)

### 18. Pelagic:demersal ratio

#Indicator 18

### 19 Maximum length and size structure

#Indicator 19 - Lmax indicator

### 20. Changes in target species / landing composition

load("../indicator_objects/total_landings.RData") 
plotIndicatorTimeSeries(inddata, coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, trendAnalysis = T, sublabel = T)
                        
### 21. Total, lobster and conch revenues
                        
# Just figures
                        
### 22. Total, lobster and conch trips
                        
# Indicator 22 - number of trips
                        
### 23. Ocean economy employment and wages
                        
load("../indicator_objects/OceanNAICS.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:8, plotrownum = 2, trendAnalysis = T, sublabel = T)
                        
### 24. GDP

load("../indicator_objects/GDP.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T)
                    
### 25. Unemployment

load("../indicator_objects/unemployment.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, trendAnalysis = T, sublabel = T, dateformat = "%Y%b")
                        
### 26. Gini coefficient for distribution of landings and revenue

load("../indicator_objects/gini.RData") 
plotIndicatorTimeSeries(inddata, coltoplot = 1:3, plotrownum = 2, trendAnalysis = T, sublabel = T) 
                        
### 27. Commercial fishing community engagement and reliance
                        
# Indicator 27 - working with Tarsila on
                        
### 28. Recreational fishing engagement and participation
                        
load("../indicator_objects/total_rec_catch.RData") 
plotIndicatorTimeSeries(inddata, coltoplot = 1:2, trendAnalysis = T, sublabel = T) 
                        
### 29. Commercial fishing engagement and participation
                        
# Indicator 29
                        
### 30. Changes in gear type
                        
# Just plots

### 31. Number of seasonal closures implemented
                        
#Indicator 31
                        
### 32. Number of education and outreach events
                        
#Indicator 32
                        
### 33. Number of enforcement actions
                        
#Indicator 33
                        
### 34 Percent coral cover
                      
load("../indicator_objects/NCRMP_coral_cover_richness.RData") 
plotIndicatorTimeSeries(inddata, coltoplot = 1:6, sublabel = T, trendAnalysis = F) 
                        
### 35. Coral species diversity

load("../indicator_objects/coral_spprichness_cover.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:4, sublabel = T, trendAnalysis = T)
                        
                          