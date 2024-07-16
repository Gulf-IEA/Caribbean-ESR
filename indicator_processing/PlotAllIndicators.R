# Script to plot all indicators from indicator_objects. Saves all plots as png files in the indicator_plots folder.

# last updated 7/9/24 by Carissa Gervasi


rm(list = ls())
dev.off()

library(plotTimeSeries)

########################################################
# Define a function to plot and save the plot with optimal resolution
plot_and_save <- function(data_path, plot_params, save_path, width_inch = 5, height_inch = 6, dpi = 300) {
  tryCatch({
    
    # Load the data
    load(data_path)
    
    # Open a PNG device with specified dimensions and resolution
    png(filename = save_path, width = width_inch * dpi, height = height_inch * dpi, res = dpi)
    
    # Plot the data with the specified parameters
    plotIndicatorTimeSeries(ind, 
                            coltoplot = plot_params$coltoplot, 
                            plotrownum = plot_params$plotrownum, 
                            plotcolnum = plot_params$plotcolnum,
                            trendAnalysis = plot_params$trendAnalysis, 
                            dateformat = plot_params$dateformat, 
                            sublabel = plot_params$sublabel,
                            widadj = plot_params$widadj, 
                            hgtadj = plot_params$hgtadj, 
                            anom = plot_params$anom,
                            yposadj = plot_params$yposadj,
                            cex.lab = plot_params$cex.lab)
    
    # Close the PNG device
    dev.off()
  }, error = function(e) {
    message <- paste("Error with dataset:", data_path, "\n", e)
    writeLines(message, con = "error_log.txt")
    cat(message, "\n")
  })
}


# Define the datasets and their specific plot parameters
datasets <- list(
  # 1. Degree heating weeks
  list(data_path = "indicator_objects/DegreeHeatingWeeks.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%Y%m", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/DegreeHeatingWeeks_plot.png"),
  
  # 2. Ocean acidification via aragonite saturation state
  list(data_path = "indicator_objects/OA.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "stmon", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/OA_plot.png"),
  
  # 3. Hurricane activity
  list(data_path = "indicator_objects/ACEindex.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/ACEindex_plot.png"),
  
  # 4. Turbidity
  list(data_path = "indicator_objects/turbidity.RData", 
       plot_params = list(coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/turbidity_plot.png"),
  
  # 5. Sea surface temperature
  list(data_path = "indicator_objects/Carib_SST.RData", 
       plot_params = list(coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = TRUE, widadj = 0.5, hgtadj = 0.3, anom = "stmon", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/Carib_SST_plot.png"),
  
  # 6. Marine debris
  list(data_path = "indicator_objects/marine_debris.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = FALSE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/marine_debris_plot.png"),
  
  # 7. Identified point source pollution sites
  list(data_path = "indicator_objects/pollution.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/pollution_plot.png"),
  
  # 8. Primary productivity via ocean color
  list(data_path = "indicator_objects/carib_Chl.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "stmon", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/carib_Chl_plot.png"),
  
  # 10. Number of major earthquakes
  list(data_path = "indicator_objects/earthquakes.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/earthquakes_plot.png"),
  
  # 11. Fishery/market disturbance indicator 
  list(data_path = "indicator_objects/disturbance.RData", 
       plot_params = list(coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.lab = 1.5), 
       save_path = "indicator_plots/disturbance_plot.png"),
  
  # 12. Sargassum inundation
  list(data_path = "indicator_objects/Sargassum.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1.2, hgtadj = 0.9, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/Sargassum_plot.png"),
  
  # 13. Tourism via hotel occupancy
  list(data_path = "indicator_objects/hotel_occupancy.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%Y%b", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/hotel_occupancy_plot.png"),
  
  # 14. cruise passengers
  list(data_path = "indicator_objects/cruise.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/cruise_plot.png"),
  
  # 15. Population change
  list(data_path = "indicator_objects/population.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/population_plot.png"),
  
  # 16. Fishery independent surveys of economically important species
  list(data_path = "indicator_objects/RVC_PR.RData", 
       plot_params = list(coltoplot = 1:6, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/RVC_PR_plot.png"),
  list(data_path = "indicator_objects/RVC_STSJ.RData", 
       plot_params = list(coltoplot = 1:6, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/RVC_STSJ_plot.png"),
  list(data_path = "indicator_objects/RVC_STX.RData", 
       plot_params = list(coltoplot = 1:6, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/RVC_STX_plot.png"),
  
  # 17. Fish density / slope of size spectrum
  list(data_path = "indicator_objects/fish_density.RData", 
       plot_params = list(coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/fish_density_plot.png"),
  
  # 18. Pelagic:demersal ratio
  list(data_path = "indicator_objects/PD_ratio.RData", 
       plot_params = list(coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/PD_ratio_plot.png"),
  
  # 19. Maximum length and size structure
  list(data_path = "indicator_objects/PR_Lmax_classes.RData", 
       plot_params = list(coltoplot = 1:5, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/PR_Lmax_classes_plot.png"),
  list(data_path = "indicator_objects/STT_Lmax_classes.RData", 
       plot_params = list(coltoplot = 1:5, plotrownum = 3, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1.2, cex.lab = 1.5), 
       save_path = "indicator_plots/STT_Lmax_classes_plot.png"),
  
  # 20. Total landings
  list(data_path = "indicator_objects/total_landings.RData", 
       plot_params = list(coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1.3, hgtadj = 0.3, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/total_landings_plot.png"),
  
  # 23. Ocean economy employment and wages
  list(data_path = "indicator_objects/oceanNAICS.RData", 
       plot_params = list(coltoplot = 1:8, plotrownum = 4, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/oceanNAICS_plot.png"),
  
  # 24. GDP
  list(data_path = "indicator_objects/GDP.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/GDP_plot.png"),
  
  # 25. Unemployment rate
  list(data_path = "indicator_objects/unemployment.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%Y%b", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/unemployment_plot.png"),
  
  # 26. Economic inequality via gini index
  list(data_path = "indicator_objects/gini.RData", 
       plot_params = list(coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1.3, hgtadj = 0.5, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/gini_plot.png"),
  
  # 28. Total recreational catch
  list(data_path = "indicator_objects/total_rec_catch.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/total_rec_catch_plot.png"),
  
  # 29. tier 3
  list(data_path = "indicator_objects/tier3.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/tier3_plot.png"),
  
  # 30. outreach
  list(data_path = "indicator_objects/outreach.RData", 
       plot_params = list(coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/outreach_plot.png"),
  
  # 31. coral
  list(data_path = "indicator_objects/coral_spprichness_cover.RData", 
       plot_params = list(coltoplot = 1:4, plotrownum = 2, plotcolnum = 2, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1, cex.lab = 1.5), 
       save_path = "indicator_plots/coral_spprichness_cover_plot.png")
)


# Loop through each dataset and create plots
for (dataset in datasets) {
  tryCatch({
    plot_and_save(dataset$data_path, dataset$plot_params, dataset$save_path)
  }, error = function(e) {
    error_datasets <- c(error_datasets, dataset$data_path)
  })
}




########################################################
#### Plot single indicators in wide format

# Define a function to plot and save the plot with optimal resolution
plot_and_save <- function(data_path, plot_params, save_path, width_inch = 10, height_inch = 4, dpi = 300) {
  tryCatch({
    
    # Load the data
    load(data_path)
    
    # Open a PNG device with specified dimensions and resolution
    png(filename = save_path, width = width_inch * dpi, height = height_inch * dpi, res = dpi)
    
    # Plot the data with the specified parameters
    plotIndicatorTimeSeries(ind, 
                            coltoplot = plot_params$coltoplot, 
                            plotrownum = plot_params$plotrownum, 
                            plotcolnum = plot_params$plotcolnum,
                            trendAnalysis = plot_params$trendAnalysis, 
                            dateformat = plot_params$dateformat, 
                            sublabel = plot_params$sublabel,
                            widadj = plot_params$widadj, 
                            hgtadj = plot_params$hgtadj, 
                            anom = plot_params$anom,
                            yposadj = plot_params$yposadj,
                            cex.lab = plot_params$cex.lab)
    
    # Close the PNG device
    dev.off()
  }, error = function(e) {
    message <- paste("Error with dataset:", data_path, "\n", e)
    writeLines(message, con = "error_log.txt")
    cat(message, "\n")
  })
}


# Define the datasets and their specific plot parameters
datasets <- list(
  # 2. Ocean acidification via aragonite saturation state
  list(data_path = "indicator_objects/OA.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "stmon", yposadj = 1, cex.lab = 1), 
       save_path = "indicator_plots/OA_plot_wide.png"),
  
  # 3. Hurricane activity
  list(data_path = "indicator_objects/ACEindex.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1), 
       save_path = "indicator_plots/ACEindex_plot_wide.png"),
  
  # 6. Marine debris
  list(data_path = "indicator_objects/marine_debris.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = FALSE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1), 
       save_path = "indicator_plots/marine_debris_plot_wide.png"),
  
  # 8. Primary productivity via ocean color
  list(data_path = "indicator_objects/carib_Chl.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "stmon", yposadj = 1, cex.lab = 1), 
       save_path = "indicator_plots/carib_Chl_plot_wide.png"),
  
  # 10. Number of major earthquakes
  list(data_path = "indicator_objects/earthquakes.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1, hgtadj = 1, anom = "none", yposadj = 1, cex.lab = 1), 
       save_path = "indicator_plots/earthquakes_plot_wide.png"),
  
  # 12. Sargassum inundation
  list(data_path = "indicator_objects/Sargassum.RData", 
       plot_params = list(coltoplot = 1, plotrownum = 1, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = F, widadj = 1.2, hgtadj = 0.9, anom = "none", yposadj = 1, cex.lab = 1), 
       save_path = "indicator_plots/Sargassum_plot_wide.png")
)


# Loop through each dataset and create plots
for (dataset in datasets) {
  tryCatch({
    plot_and_save(dataset$data_path, dataset$plot_params, dataset$save_path)
  }, error = function(e) {
    error_datasets <- c(error_datasets, dataset$data_path)
  })
}





################################################################################
#### Just LMax

# Define a function to plot and save the plot with optimal resolution
plot_and_save <- function(data_path, plot_params, save_path, width_inch = 6, height_inch = 8, dpi = 300) {
  tryCatch({
    
    # Load the data
    load(data_path)
    
    # Open a PNG device with specified dimensions and resolution
    png(filename = save_path, width = width_inch * dpi, height = height_inch * dpi, res = dpi)
    
    # Plot the data with the specified parameters
    plotIndicatorTimeSeries(ind, 
                            coltoplot = plot_params$coltoplot, 
                            plotrownum = plot_params$plotrownum, 
                            plotcolnum = plot_params$plotcolnum,
                            trendAnalysis = plot_params$trendAnalysis, 
                            dateformat = plot_params$dateformat, 
                            sublabel = plot_params$sublabel,
                            widadj = plot_params$widadj, 
                            hgtadj = plot_params$hgtadj, 
                            anom = plot_params$anom,
                            yposadj = plot_params$yposadj)
    
    # Close the PNG device
    dev.off()
  }, error = function(e) {
    message <- paste("Error with dataset:", data_path, "\n", e)
    writeLines(message, con = "error_log.txt")
    cat(message, "\n")
  })
}


# Define the datasets and their specific plot parameters
datasets <- list(
  # 19. Maximum length and size structure
  list(data_path = "indicator_objects/PR_Lmax_classes.RData", 
       plot_params = list(coltoplot = 1:5, plotrownum = 5, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1), 
       save_path = "indicator_plots/PR_Lmax_classes_plot_wide.png"),
  list(data_path = "indicator_objects/STT_Lmax_classes.RData", 
       plot_params = list(coltoplot = 1:5, plotrownum = 5, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.3, anom = "none", yposadj = 1.2), 
       save_path = "indicator_plots/STT_Lmax_classes_plot_wide.png")
)


# Loop through each dataset and create plots
for (dataset in datasets) {
  tryCatch({
    plot_and_save(dataset$data_path, dataset$plot_params, dataset$save_path)
  }, error = function(e) {
    error_datasets <- c(error_datasets, dataset$data_path)
  })
}







#####################################################################################

# some extra plot configurations 

png(filename = "indicator_plots/avgLmax_plot_TAP.png", width = 5, height = 6, units = "in", res = 300)
load("indicator_objects/avgLmax.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1, sameYscale = TRUE, cex.axis = 1.3, cex.lab = 1.6, cex.main = 1.6)
dev.off()


png(filename = "indicator_plots/cruise_plot_TAP.png", width = 7, height = 5, units = "in", res = 300)
load("indicator_objects/cruise.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.6, anom = "none", yposadj = 1.2, sameYscale = TRUE, cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.6)
dev.off()

png(filename = "indicator_plots/Carib_SST_plot_TAP.png", width = 6, height = 8, units = "in", res = 300)
load("indicator_objects/Carib_SST.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%m-%Y", sublabel = TRUE, widadj = 0.5, hgtadj = 0.3, anom = "stmon", yposadj = 1, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6)
dev.off()


png(filename = "indicator_plots/disturbance_plot_TAP.png", width = 6, height = 8, units = "in", res = 300)
load("indicator_objects/disturbance.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 0.7, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6)
dev.off()



png(filename = "indicator_plots/fish_density_plot_TAP.png", width = 7, height = 8, units = "in", res = 300)
load("indicator_objects/fish_density.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 0.7, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6)
dev.off()



png(filename = "indicator_plots/gini_plot_TAP.png", width = 7, height = 8, units = "in", res = 300)
load("indicator_objects/gini.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:3, plotrownum = 3, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.5, anom = "none", yposadj = 1.2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 1.6, sameYscale = TRUE)
dev.off()


png(filename = "indicator_plots/tier3_plot_TAP.png", width = 8, height = 7, units = "in", res = 300)
load("indicator_objects/tier3.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.4, anom = "none", yposadj = 1.2, sameYscale = F, cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.5)
dev.off()


png(filename = "indicator_plots/total_landings_plot_TAP.png", width = 9, height = 8, units = "in", res = 300)
load("indicator_objects/total_landings.RData")
plotIndicatorTimeSeries(ind, coltoplot = 1:9, plotrownum = 3, plotcolnum = 3, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 0.9, hgtadj = 0.5, anom = "none", yposadj = 1, cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.3)
dev.off()
