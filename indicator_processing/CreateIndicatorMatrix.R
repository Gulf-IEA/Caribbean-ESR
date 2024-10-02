# Script to pull all the indicators from the indicator_objects folder and merge the data into a matrix that can be used for stoplight plots.

# For the monthly indicators we need to just average those into annual time steps.


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



# Before we go any further, we need to pull out the indicators with monthly data and average them into annual indicators

# the monthly indicators include Carib_Chl, Carib_SST, DegreeHeatingWeeks, OA, turbidity, and unemployment

# Convert monthly data to annual by averaging values per year
convert_to_annual <- function(data_list, date_format) {
  # Extract the date and indicator values
  name <- data_list$file_name
  dates <- data_list$datelist
  values <- data_list$indicators
  
  
  # Parse the date according to the specified format
  if (date_format == "%m-%Y") {
    years <- as.integer(format(as.Date(paste0("01-", dates), "%d-%m-%Y"), "%Y"))
  } else if (date_format == "%Y%m") {
    years <- as.integer(substr(dates, 1, 4))  # Extract the first 4 characters as year
  } else if (date_format == "%b%Y") {
    years <- as.integer(format(as.Date(paste0("01", dates), "%d%b%Y"), "%Y"))
  } else {
    stop("Unsupported date format")
  }
  
  # Create a dataframe for easier manipulation
  df <- data.frame(year = years, values)
  
  # Group by year and calculate the annual average
  annual_data <- aggregate(. ~ year, data = df, FUN = mean)
  
  # Update the datelist and indicators with the annual data
  data_list$datelist <- annual_data$year
  data_list$indicators <- as.data.frame(annual_data[, -1]) 
  
  return(data_list)
}

# Apply the function to each monthly dataset
carib_Chl <- convert_to_annual(all_data[[3]], "%m-%Y")
Carib_SST <- convert_to_annual(all_data[[4]], "%m-%Y")
DegreeHeatingWeeks <- convert_to_annual(all_data[[7]], "%Y%m")
OA <- convert_to_annual(all_data[[18]], "%b%Y")
turbidity <- convert_to_annual(all_data[[38]], "%m-%Y")
unemployment <- convert_to_annual(all_data[[39]], "%Y%m")


# Ok now we have all the monthly indicators as annual indicators. We need to replace these in the all_data list. 

all_data[[3]] = carib_Chl
all_data[[4]] = Carib_SST
all_data[[7]] = DegreeHeatingWeeks
all_data[[18]] = OA
all_data[[38]] = turbidity
all_data[[39]] = unemployment



########################################################

#Now back to making the indicator matrix

# Extract the datelist vectors and keep only those that are of class integer (this isn't really necessary now that we've taken the average of the monthly data, but can keep if we want to ignore the monthly data and skip the above code chunk)
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

# re-order the columns

# Get the current column names of the matrix (excluding the first one)
current_colnames <- colnames(matrix_data)[-1]

# Match the current column names with the "desc_name" in the descriptive_names data
reorder_index <- match(current_colnames, descriptive_names$desc_name)

# Check for any unmatched columns
if (any(is.na(reorder_index))) {
  print("Warning: Some columns in the matrix do not match the CSV 'desc_name'.")
}

# Remove NA values from reorder_index and sort according to the 'order' column
valid_columns <- !is.na(reorder_index)
sorted_columns <- current_colnames[valid_columns][order(descriptive_names$order[reorder_index[valid_columns]])]

# Reorganize the matrix, keeping the first column unchanged
matrix_data <- matrix_data[, c(1, match(sorted_columns, colnames(matrix_data)))]

# Save the matrix_data to an .rda file
save(matrix_data, file = "indicator_data/all_indicators_matrix.rda")

