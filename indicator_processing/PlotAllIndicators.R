# Caribbean ESR - all time series indicators plotted

#Script to plot all indicators from indicator_objects. Saves all plots as png files in the indicator_plots folder. Modified version of the script to make each plot more customizable. t.

rm(list = ls())

library(plotTimeSeries)

#last updated 10/30/24

### 1 - sea surface temperature

png(filename = "indicator_plots/Carib_SST_plot_final.png", width = 6, height = 8, units = "in", res = 300)
load("indicator_objects/Carib_SST.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = TRUE, widadj = 0.5, hgtadj = 0.3, anom = "stmon", yposadj = 1, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6)
dev.off()

### 2 - degree heating weeks

png(filename = "indicator_plots/DegreeHeatingWeeks_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/DegreeHeatingWeeks.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%Y%m", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 3 - ocean acidification

png(filename = "indicator_plots/OA_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/OA.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "stmon", yposadj = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
dev.off()

### 4 - hurricane activity

png(filename = "indicator_plots/ACEindex_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/ACEindex.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
dev.off()

### 5 - number of major earthquakes

png(filename = "indicator_plots/earthquakes_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/earthquakes.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
dev.off()

### 6 - identified point source pollution sites

png(filename = "indicator_plots/pollution_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/pollution.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 7 - turbidity

png(filename = "indicator_plots/turbidity_plot_final.png", width = 5, height = 6, units = "in", res = 300)
load("indicator_objects/turbidity.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
dev.off()

### 8 - water quality

#WORKING ON THIS

### 9 - coastal development

#SPATIAL MAP

### 10 - primary productivity via ocean color

png(filename = "indicator_plots/carib_Chl_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/carib_Chl.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "stmon", yposadj = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
dev.off()

### 11 - sargassum inundation

png(filename = "indicator_plots/Sargassum_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/Sargassum.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1.2, hgtadj = 0.9, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1, cex.main = 1)
dev.off()

### 12 - market disturbances

png(filename = "indicator_plots/disturbance_plot_final.png", width = 6, height = 8, units = "in", res = 300)
load("indicator_objects/disturbance.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 0.7, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6)
dev.off()

### 13 - tourism (air travel and cruise passengers)

png(filename = "indicator_plots/cruise_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/cruise_air_visitors.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:4, plotrownum = 2, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1.2, sameYscale = FALSE, cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.6)
dev.off()

### 14 - population

png(filename = "indicator_plots/population_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/population.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 15 - abundance of economically important fish

png(filename = "indicator_plots/RVC_PR_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/RVC_PR.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:6, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.3)
dev.off()

png(filename = "indicator_plots/RVC_STSJ_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/RVC_STSJ.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:6, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.3)
dev.off()

png(filename = "indicator_plots/RVC_STX_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/RVC_STX.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:6, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.3)
dev.off()

### 16 - abundance of commercial fish and slope of the size spectrum

png(filename = "indicator_plots/fish_density_plot_final.png", width = 7, height = 8, units = "in", res = 300)
load("indicator_objects/fish_density.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 0.7, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6)
dev.off()

### 17 - pelagic:demersal ratio

png(filename = "indicator_plots/PD_ratio_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/PD_ratio.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 18 - Lmax indicator

png(filename = "indicator_plots/avgLmax_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/mean_Lmax.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, sameYscale = FALSE, cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
dev.off()

png(filename = "indicator_plots/PR_Lmax_classes_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/PR_Lmax_classes.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:5, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

png(filename = "indicator_plots/STT_Lmax_classes_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/STT_Lmax_classes.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:5, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

png(filename = "indicator_plots/STX_Lmax_classes_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/STX_Lmax_classes.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:5, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 19 - total landings

png(filename = "indicator_plots/total_landings_plot_final.png", width = 9, height = 8, units = "in", res = 300)
load("indicator_objects/total_landings.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 0.9, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.3)
dev.off()

### 20 - percent revenues by species group

#NOT A TIME SERIES

### 21 - number of trips

#NOT A TIME SERIES

### 22 - ocean economy

png(filename = "indicator_plots/oceanNAICS_plot_final.png", width = 6, height = 7, units = "in", res = 300)
load("indicator_objects/oceanNAICS.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:8, plotrownum = 4, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 5, hgtadj = 1, anom = "none", yposadj = 1.2, cex.axis = 1, cex.lab = 1.1, cex.main = 1.3)
dev.off()

### 23 - GDP

png(filename = "indicator_plots/GDP_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/GDP.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 24 - unemployment

png(filename = "indicator_plots/unemployment_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/unemployment.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%Y%b", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 25 - Gini coefficient

png(filename = "indicator_plots/gini_plot_final.png", width = 7, height = 8, units = "in", res = 300)
load("indicator_objects/gini_revenue.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6, sameYscale = TRUE)
dev.off()

### 26 - environmental justice, economic, and gentrification indicators

#not time series plots

### 27 - recreational landings

png(filename = "indicator_plots/total_rec_catch_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/total_rec_catch.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

### 28 - commercial engagement and reliance

#maps not time series

### 29 - changes in gear type

#This includes time series but also NMDS plots and barplots

png(filename = "indicator_plots/prop_diving_trips_plot_final.png", width = 7, height = 8, units = "in", res = 300)
load("indicator_objects/prop_diving_trips.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6, sameYscale = FALSE)
dev.off()

png(filename = "indicator_plots/prop_trips_bycatch_plot_final.png", width = 7, height = 8, units = "in", res = 300)
load("indicator_objects/prop_trips_bycatch.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6, sameYscale = FALSE)
dev.off()


### 30 - number of new regulations

png(filename = "indicator_plots/FRsection_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/FRsection.RData") 
plotIndicatorTimeSeries(ind,coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1, type = "allLines")
dev.off()

### 31 - percent of species with informative catch limits

png(filename = "indicator_plots/tier3_plot_final.png", width = 8, height = 7, units = "in", res = 300)
load("indicator_objects/tier3.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1.2, sameYscale = F, cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.5)
dev.off()

### 32 - number of education / outreach events

png(filename = "indicator_plots/outreach_plot_final.png", width = 8, height = 7, units = "in", res = 300)
load("indicator_objects/outreach.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1.2, sameYscale = F, cex.axis = 1, cex.lab = 1.2, cex.main = 1.5)
dev.off()

### 33 - number of enforcement actions

png(filename = "indicator_plots/enforcement_plot_final.png", width = 10, height = 4, units = "in", res = 300)
load("indicator_objects/enforcement.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b-%y", sublabel = F, widadj = 7, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1, type = "allLines")
dev.off()

### 34 - percent coral cover and species diversity

png(filename = "indicator_plots/coral_spprichness_cover_plot_final.png", width = 7, height = 6, units = "in", res = 300)
load("indicator_objects/coral_spprichness_cover.RData") 
plotIndicatorTimeSeries(ind, coltoplot = 1:4, plotrownum = 2, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.axis = 1, cex.lab = 1.2, cex.main = 1)
dev.off()

