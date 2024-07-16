# Script to pull all the indicators from the indicator_objects folder and merge the data into a matrix that can be used for stoplight plots.

# I am working on turning this into a function and including the indicators with monthly data.


rm(list = ls())

# Load necessary libraries
library(dplyr)
library(tidyr)

# Function to extract the data from the .RData files
extract_data <- function(file_path) {
  load(file_path)
  if (exists("ind")) {
    list(
      file_name = basename(file_path),
      datelist = ind$datelist,
      indicators = ind$indicators
    )
  } else {
    NULL
  }
}

# Read the descriptive names from the CSV file
descriptive_names <- read.csv("indicator_data/extracted_ind_object_names_REVISED.csv")

# Set the directory containing the .RData files
indicator_dir <- "indicator_objects"

# Get a list of all .RData files in the directory
rdata_files <- list.files(indicator_dir, pattern = "\\.RData$", full.names = TRUE)

# Extract data from all .RData files
all_data <- lapply(rdata_files, extract_data)
all_data <- Filter(Negate(is.null), all_data)

# Extract the datelist vectors and keep only those that are of class integer
datelists <- lapply(all_data, function(x) x$datelist)
datelists <- datelists[sapply(datelists, function(d) class(d) == "integer")]

# Determine the range of years
years <- unlist(datelists)
year_range <- seq(min(years, na.rm = TRUE), max(years, na.rm = TRUE))

# Create an empty matrix with "year" column
matrix_data <- data.frame(year = year_range)

# Function to add indicator columns to the matrix
add_indicator_columns <- function(matrix_data, data, descriptive_names) {
  datelist <- data$datelist
  if (class(datelist) != "integer") {
    return(matrix_data)
  }
  
  years <- datelist
  indicators <- data$indicators
  
  for (col_name in colnames(indicators)) {
    descriptive_name <- descriptive_names %>% 
      filter(file_name == data$file_name & ind_name == col_name) %>% 
      pull(desc_name)
    if (length(descriptive_name) == 0) next
    
    indicator_values <- indicators[[col_name]]
    indicator_df <- data.frame(year = years, value = indicator_values)
    indicator_df <- indicator_df %>% complete(year = year_range, fill = list(value = NA))
    
    matrix_data[[descriptive_name]] <- indicator_df$value
  }
  
  return(matrix_data)
}

# Add all indicator columns to the matrix
for (data in all_data) {
  matrix_data <- add_indicator_columns(matrix_data, data, descriptive_names)
}

# Print the resulting matrix
print(matrix_data)

# Save the matrix_data to an .rda file
save(matrix_data, file = "indicator_data/all_indicators_matrix.rda")


