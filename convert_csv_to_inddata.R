

rm(list = ls())
# devtools::install_github("mandykarnauskas/plotTimeSeries")
library(plotTimeSeries)


# get list of data files and convert to indicatordata format ------------

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_data")
filelis <- dir()[grep(".csv", dir())]
filelis[1:10]

for (i in 1:length(filelis))  {

  setwd("../indicator_data")
  d <- read.table(filelis[i], skip = 3, sep = ",", header = F)
  
  if (class(d[,1]) == "factor" | class(d[,2]) == "factor") { 
    cat(filelis[i])
    cat(" -file not in default format\n")  }  else   {
  
    cat(filelis[i])
    cat(" ---------------------------------------- file ok\n")  
    nam <- unlist(strsplit(filelis[i], ".csv"))
    
    inddata <- conv2indicatordata(filelis[i], default = T)
    
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
    if (length(inddata$datelist) < 5)  { 
      plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), sublabel = T, outtype = "png", type = "ptsOnly") 
      } else {
      plotIndicatorTimeSeries(inddata, coltoplot = 1:ncol(inddata$indicators), sublabel = T, outtype = "png")   }
}

