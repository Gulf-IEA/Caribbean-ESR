# Code to develop population indicator using UN API

# Last updated 6/24/2024 by Carissa Gervasi

## R tutorials for using the API can be found here: https://population.un.org/dataportal/about/dataapi

## NOTE: 2023 and 2024 are projections. Do we want to use these?

rm(list = ls())

plot.new()
dev.off()

library(jsonlite)
library(httr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(data.table)


# When code needs to be updated, just need to update the end year here:

styear <- 1990
enyear <- 2024


# Use the API to extract a list of indicators

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Creates the target URL, indicators, in this instance
target <- paste0(base_url, "/indicators/")

# Get the response, which includes data as well as information on pagination and number of records
response <- fromJSON(target)

# Get the first page of data
df1 <- response$data

# Loop until there are new pages with data
while (!is.null(response$nextPage)){
  
  #call the API for the next page
  response <- fromJSON(response$nextPage)
  
  #add the data of the new page to the data.frame with the data of the precious pages
  df1 <- rbind(df1, response$data)
  
}



# Code to pull all the locations

# Update relative path to retrieve records on locations
target <- paste0(base_url, "/locations/")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df2 <- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df2 <- rbind(df2, response$data)
  
}


###########################

# We want indicator number 49 (TPopulation) for areas 630 (Puerto Rico) and 850 (USVI)


# Your API key (needed to request an API key from population@un.org)
api_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1bmlxdWVfbmFtZSI6ImNhcmlzc2EuZ2VydmFzaUBub2FhLmdvdiIsIm5iZiI6MTcxOTQwOTA1MSwiZXhwIjoxNzUwOTQ1MDUwLCJpYXQiOjE3MTk0MDkwNTEsImlzcyI6ImRvdG5ldC11c2VyLWp3dHMiLCJhdWQiOiJkYXRhLXBvcnRhbC1hcGkifQ.BQunYH0GnNgP7Wun1hT3fcS8hIF5XvP2mWiKkkjf7gc"

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"




########### Puerto Rico

# Update the relative path to search for data on a specific indicator, location, and for specific years
target <- paste0(base_url, "/data/indicators/49/locations/630/start/",styear,"/end/",enyear)

# Print the target URL to ensure it's correct
print(target)

# Call the API and handle potential errors
response <- tryCatch({
  GET(target, add_headers(Authorization = paste("Bearer", api_key)))
}, error = function(e) {
  cat("Error: ", e$message, "\n")
  NULL
})

# Check if the response is NULL
if (is.null(response)) {
  stop("Failed to fetch data from the API.")
}

# Parse the response
response_content <- content(response, "text")
response_json <- fromJSON(response_content)

# Ensure the response contains data
if (!is.null(response_json$data)) {
  # Get the first page of data
  df_PR <- response_json$data
  
  # Get the other pages with data
  while (!is.null(response_json$nextPage)) {
    response <- tryCatch({
      GET(response_json$nextPage, add_headers(Authorization = paste("Bearer", api_key)))
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      NULL
    })
    
    # Check if the response is NULL
    if (is.null(response)) {
      stop("Failed to fetch data from the next page of the API.")
    }
    
    response_content <- content(response, "text")
    response_json <- fromJSON(response_content)
    
    df_PR <- rbind(df_PR, response_json$data)
  }
  
  # Print the dataframe to see the results
  print(df_PR)
} else {
  stop("No data found in the response.")
}





########### USVI

# Update the relative path to search for data on a specific indicator, location, and for specific years
target <- paste0(base_url, "/data/indicators/49/locations/850/start/",styear,"/end/",enyear)

# Print the target URL to ensure it's correct
print(target)

# Call the API and handle potential errors
response <- tryCatch({
  GET(target, add_headers(Authorization = paste("Bearer", api_key)))
}, error = function(e) {
  cat("Error: ", e$message, "\n")
  NULL
})

# Check if the response is NULL
if (is.null(response)) {
  stop("Failed to fetch data from the API.")
}

# Parse the response
response_content <- content(response, "text")
response_json <- fromJSON(response_content)

# Ensure the response contains data
if (!is.null(response_json$data)) {
  # Get the first page of data
  df_USVI <- response_json$data
  
  # Get the other pages with data
  while (!is.null(response_json$nextPage)) {
    response <- tryCatch({
      GET(response_json$nextPage, add_headers(Authorization = paste("Bearer", api_key)))
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      NULL
    })
    
    # Check if the response is NULL
    if (is.null(response)) {
      stop("Failed to fetch data from the next page of the API.")
    }
    
    response_content <- content(response, "text")
    response_json <- fromJSON(response_content)
    
    df_USVI <- rbind(df_USVI, response_json$data)
  }
  
  # Print the dataframe to see the results
  print(df_PR)
} else {
  stop("No data found in the response.")
}



### Subset to both sexes and to only the median estimate (since CIs are not available for the whole time series)

df_PR2 = subset(df_PR, df_PR$sex == "Both sexes" & df_PR$variant == "Median")
df_USVI2 = subset(df_USVI, df_USVI$sex == "Both sexes" & df_USVI$variant == "Median")



pop_PR = df_PR2$value/1000
pop_USVI = df_USVI2$value/1000


# save as indicator object ----------------------
datdata <- styear:enyear
inddata <- data.frame(cbind(pop_PR, pop_USVI))
labs <- c("Total population" , "Persons (thousands)", "Puerto Rico",
          "Total population" , "Persons (thousands)", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
ind <- inddata
plotIndicatorTimeSeries(ind, plotrownum = 2, coltoplot = 1:2, sublabel = TRUE, trendAnalysis = T)

save(ind, file = "indicator_objects/population.RData")

############  END  ################33

print("population -- SUCCESSFULLY RUN")
