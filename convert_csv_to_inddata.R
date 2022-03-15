

rm(list = ls())
# devtools::install_github("mandykarnauskas/plotTimeSeries")
library(plotTimeSeries)
library(stringr)


# get list of data files and convert to indicatordata format ------------

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data")
filelis <- dir()[grep(".csv", dir())]
filelis[1:10]

for (i in 1:length(filelis))  {

  setwd("../indicator_data")
  d <-  read.table(filelis[i], skip = 3, sep = ",", header = F)
  d1 <- read.table(filelis[i], skip = 2, sep = ",", header = T)
    
  if (class(d[,1]) == "factor" | class(d[,2]) == "factor") { 
    cat(filelis[i])
    cat(" -file not in default format\n")  }  else   {
  
    cat(filelis[i])
    cat(" ---------------------------------------- file ok\n")  
    nam <- unlist(strsplit(filelis[i], ".csv"))
    
    if (length(grep("ulim", names(d1))) == 0)  { 
      inddata <- conv2indicatordata(filelis[i], default = T)   }  else  { 
        ll <- grep("llim", names(d1))
        ul <- grep("ulim", names(d1))
      inddata <- conv2indicatordata(filelis[i], default = F, 
                                    labrows = 1:3, datecols = 1, 
                                    indcols = apply(cbind(ll, ul), 1, min) - 1, 
                                    ulimcols = ul, llimcols = ll)
    }
    setwd("../indicator_objects")
    save(inddata, file = paste0(unlist(strsplit(filelis[i], ".csv")), ".RData"))
  }
} 

# get list of indicatordata objects and generate plots -------------------

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects")

lis <- dir()[grep(".RData", dir())]
lis

for (i in 1:length(lis))  {
  setwd("../indicator_objects")
  load(lis[i])
  
  setwd("../indicator_plots")
    if (length(inddata$datelist) < 10)  { 
      plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotrownum = ncol(inddata$indicators), 
                              sublabel = T, outtype = "png", CItype = "pts", type = "ptsOnly") 
      } else {
      plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators),  plotrownum = ncol(inddata$indicators),
                              sublabel = T, dateformat = "%Y%B", outtype = "png")   }
}

# replot with individual settings -----------------------------------------

setwd("../indicator_plots")

load("../indicator_objects/sargassum_innundation_monthly_mean_hu.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1, sublabel = T, CItype = "band", widadj = 2, type = "allLines", 
                        dateformat = "%Y%B", outtype = "png") 

load("../indicator_objects/beach_litter_un_carib.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotcolnum = ncol(inddata$indicators), 
                        sublabel = T, outtype = "png", CItype = "pts", type = "ptsOnly", widadj = 1.5) 

load("../indicator_objects/NCRMP_coral_cover_richness.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotcolnum = 3, plotrownum = 2, 
                        sublabel = T, outtype = "png", CItype = "pts", type = "ptsOnly", widadj = 1.4) 

load("../indicator_objects/hotel_occupancy_rates_USVI_and_PR.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotrownum = 2, dateformat = "%Y%B",
                        sublabel = T, outtype = "png", widadj = 2.5, type = "allLines") #, anom = "stmon") 

load("../indicator_objects/gdp_PR_and_USVI.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotrownum = 2, 
                        sublabel = T, outtype = "png", widadj = 2) 

load("../indicator_objects/OceanNAICS.RData")
plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotcolnum = 4, plotrownum = 2, 
                        sublabel = T, outtype = "png", widadj = 1.25, hgtadj = 1.3) 

#load("../indicator_objects/PR_and_USVI_DHW-MonthlyMean.RData")
#plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), plotcolnum = ncol(inddata$indicators), 
#                        sublabel = T, outtype = "png", CItype = "pts", type = "ptsOnly", widadj = 1.5, yposadj = 0.9) 

load("../indicator_objects/Carib_SST.RData")
plotIndicatorTimeSeries(s, coltoplot = 1:3, plotrownum = 3, dateformat = "%m-%Y", sublabel = T, 
                        trendAnalysis = T, widadj = 0.5, anom = "mon", type = "allLines", outtype = "png", hgtadj = 0.8)








