## Ocean economy indicators from NAICS. There is an API, but it is not working. Instead, the data were downloaded manually from here: https://coast.noaa.gov/quickreport/#/index.html


rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)


dat = read.csv("indicator_data/inputsToBeUpdatedAnnually/oceanEconomy_ENOW.csv")
head(dat)

yrs = as.integer(dat$year)

styear = min(yrs)
enyear = max(yrs)

PR = subset(dat, dat$geoName == "Puerto Rico" & dat$sector == "Ocean Economy")
USVI = subset(dat, dat$geoName == "U.S. Virgin Islands" & dat$sector == "Ocean Economy")

GDP.PR = as.numeric(PR$gdp)/1000000000
est.PR = as.numeric(PR$establishments)
emp.PR = as.numeric(PR$employment)/1000
wag.PR = as.numeric(PR$wages)/1000000000

GDP.USVI = as.numeric(USVI$gdp)/1000000000
est.USVI = as.numeric(USVI$establishments)
emp.USVI = as.numeric(USVI$employment)/1000
wag.USVI = as.numeric(USVI$wages)/1000000000



# save as indicator object ----------------------
datdata <- styear:enyear
inddata <- data.frame(cbind(GDP.PR,GDP.USVI,est.PR,est.USVI,emp.PR,emp.USVI,wag.PR,wag.USVI))
labs <- c("Gross domestic product" , "Billions of dollars", "Puerto Rico",
          "Gross domestic product" , "Billions of dollars", "USVI",
          "Ocean economy establishments" , "Number of places of work", "Puerto Rico",
          "Ocean economy establishments" , "Number of places of work", "USVI",
          "Ocean economy employees" , "People (thousands)" , "Puerto Rico",
          "Ocean economy employees" , "People (thousands)" , "USVI",
          "Ocean economy wages" , "Billions of dollars", "Puerto Rico",
          "Ocean economy wages" , "Billions of dollars", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
ind <- inddata
plotIndicatorTimeSeries(ind, plotrownum = 4, coltoplot = 1:8, sublabel = TRUE, trendAnalysis = T)

save(ind, file = "indicator_objects/oceanNAICS.RData")


print("ocean NAICS -- SUCCESSFULLY RUN")

########################################################################################

##### Below code is old, do not use ####################################################

########################################################################################


#### ************ As of 7/5/24, can't get the API for this working. For now, will just plot the data Seann uploaded to Github. At the end of this script is some code to work with the API but it isn't yet working.
# 
# rm(list = ls())
# dev.off()
# 
# 
# library(plotTimeSeries)
# 
# 
# dat = read.csv("indicator_data/OceanNAICS.csv")
# head(dat)
# 
# yrs = as.integer(dat$Indicator[-c(1,2)])
# 
# styear = min(yrs)
# enyear = max(yrs)
# 
# GDP.PR = as.numeric(dat$Gross.domestic.product[-c(1,2)])
# est.PR = as.numeric(dat$Ocean.economy.establishments[-c(1,2)])
# emp.PR = as.numeric(dat$Ocean.economy.employees[-c(1,2)])
# wag.PR = as.numeric(dat$Ocean.economy.wages[-c(1,2)])
# 
# GDP.USVI = as.numeric(dat$Gross.domestic.product.1[-c(1,2)])
# est.USVI = as.numeric(dat$Ocean.economy.establishments.1[-c(1,2)])
# emp.USVI = as.numeric(dat$Ocean.economy.employees.1[-c(1,2)])
# wag.USVI = as.numeric(dat$Ocean.economy.wages.1[-c(1,2)])
# 
# 
# 
# # save as indicator object ----------------------
# datdata <- styear:enyear
# inddata <- data.frame(cbind(GDP.PR,GDP.USVI,est.PR,est.USVI,emp.PR,emp.USVI,wag.PR,wag.USVI))
# labs <- c("Gross domestic product" , "Billions of dollars", "Puerto Rico",
#           "Gross domestic product" , "Billions of dollars", "USVI",
#           "Ocean economy establishments" , "Number of places of work", "Puerto Rico",
#           "Ocean economy establishments" , "Number of places of work", "USVI",
#           "Ocean economy employees" , "People (thousands)" , "Puerto Rico",
#           "Ocean economy employees" , "People (thousands)" , "USVI",
#           "Ocean economy wages" , "Billions of dollars", "Puerto Rico",
#           "Ocean economy wages" , "Billions of dollars", "USVI")
# indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
# inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
# class(inddata) <- "indicatordata"
# 
# # plot and save ----------------------------------
# ind <- inddata
# plotIndicatorTimeSeries(ind, plotrownum = 4, coltoplot = 1:8, sublabel = TRUE, trendAnalysis = T)
# 
# save(ind, file = "indicator_objects/oceanNAICS.RData")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ################## CODE BELOW DOES NOT WORK ##################################
# 
# 
# ## Data are from https://www.bls.gov/cew/downloadable-data-files.htm, accessed using the API.
# 
# ## Location NAICS codes are 72000 for Puerto Rico and 78000 for USVI
# 
# ## There are several industry codes related to the ocean. The list can be found here: https://coast.noaa.gov/data/digitalcoast/pdf/enow-faq.pdf
# 
# 
# # Last updated 6/27/2024 by Carissa Gervasi
# 
# # WORK IN PROGRESS
# 
# rm(list = ls())
# dev.off()
# 
# library(pdftools)
# library(dplyr)
# library(stringr)
# 
# 
# ## Scrape this list of codes from the pdf
# 
# # Specify the URL of the PDF
# url <- "https://coast.noaa.gov/data/digitalcoast/pdf/enow-faq.pdf"
# 
# # Specify the destination file path
# destfile <- "indicator_data/Ocean_NAICS_codes.pdf"
# 
# # Download the PDF
# download.file(url, destfile, mode = "wb")
# 
# # Check if the file has been downloaded
# file.exists(destfile)
# 
# # Load the PDF
# pdf_file <- "indicator_data/Ocean_NAICS_codes.pdf"
# 
# # Extract text from the PDF
# pdf_text <- pdf_text(pdf_file)
# 
# # Extract text from pages 7 and 8
# page7_text <- pdf_text[7]
# page8_text <- pdf_text[8]
# 
# # Combine text from both pages
# combined_text <- paste(page7_text, page8_text, sep = "\n")
# 
# # Find all 6-digit integers in the text
# naics_codes <- str_extract_all(combined_text, "\\b[0-9]{6}\\b") %>% unlist()
# 
# # Remove duplicates (if any)
# naics_codes <- unique(naics_codes)
# 
# # Create a data frame
# naics_df <- data.frame(
#   NAICS = naics_codes,
#   stringsAsFactors = FALSE
# )
# 
# # Display the data frame
# print(naics_df)
# 
# 
# ## Access the API
# 
# library(devtools)
# #install_github('mikeasilva/blsAPI')
# library(blsAPI)
# library(jsonlite)
# 
# # Define your API key (submitted a request for a key here: https://www.bls.gov/developers/home.htm )
# api_key <- "2d3bac2b5c42484ea04f085596a17760"
# 
# # Define the industry and area code
# industry <- "237990" # NAICS code for Other Heavy and Civil Engineering Construction
# area <- "72000" # Area code for Puerto Rico
# ownership_code <- "5" # Ownership code for Private
# type <- "1" #this is correct
# size <- "0"
# 
# # Function to construct the series ID
# construct_series_id <- function(industry, area, ownership, type, size) {
#   paste0("ENU", area, type, size, ownership, industry)
# }
# 
# # Prepare the series ID
# series_id <- construct_series_id(industry, area, ownership_code, type, size)
# 
# #series_id <- "ENU04013105111150"
# 
# # Print the series ID for verification
# print(paste("Requesting data for series ID:", series_id))
# 
# # Create the API request body
# request_body <- list(
#   'seriesid' = list(series_id),
#   'startyear' = "2023",
#   'endyear' = "2023",
#   'registrationKey' = api_key
# )
# 
# # Make the API call
# response <- blsAPI(request_body, api_version = 2)
# 
# # Parse the response
# data <- fromJSON(response)
# 
# # Check for errors in the response
# if (!is.null(data$Results$error)) {
#   print("Error in API response:")
#   print(data$Results$error)
# } else if (is.null(data$Results$series[[1]]$data)) {
#   print("No data available for the specified series ID and year.")
# } else {
#   # Extract the relevant data
#   qcew_data <- data$Results$series
#   # Print the data
#   print(qcew_data)
# }
# 
# 
# 
# 
# 
# 
# response <- blsAPI('ENU72000105237990')
# json     <- fromJSON(response)
# 
# data_list  <- json$Results$series[[1]]$data[-1]
# cpi        <- data.frame(matrix(unlist(data_list), ncol = 4, byrow = TRUE, 
#                                 dimnames = list(NULL, c("year", "period", 
#                                                         "periodName", "value"))), 
#                          stringsAsFactors = FALSE)
# cpi
# 
# 
# 
# response <- blsAPI('CIU1010000000000A')
# json     <- fromJSON(response)
# 
# data_list  <- json$Results$series[[1]]$data[-1]
# cpi        <- data.frame(matrix(unlist(data_list), ncol = 4, byrow = TRUE, 
#                                 dimnames = list(NULL, c("year", "period", 
#                                                         "periodName", "value"))), 
#                          stringsAsFactors = FALSE)
# cpi
# 
# 
# 
# 
# 
# 
# ## Pull the data via the API
# payload <- list(
#   'seriesid'  = c('ENU72000105237990'),
#   'startyear' = 2021,
#   'endyear'   = 2023)
# response <- blsAPI(payload, 2)
# json     <- fromJSON(response)
# 
# ## Process results
# apiDF <- function(data) {
#   df  <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE))
#   colnames(df) <- c("year", "period", "periodName", "value")
#   return(df)
# }
# 
# 
# unemployed.df  <- apiDF(json$Results$series[[1]]$data)
# labor.force.df <- apiDF(json$Results$series[[2]]$data)
# 
# unemployed.df  <- apiDF(json$Results$series[[1]])
