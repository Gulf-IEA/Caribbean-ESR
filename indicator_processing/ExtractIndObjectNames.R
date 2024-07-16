# Code to extract the column names from the indicator dataframes within each indicator in the indicator_objects folder. The resulting csv file will be used as a lookup table so we can re-name the indicators.

rm(list = ls())

# Load necessary libraries
library(dplyr)

# Function to extract column names from the 'indicators' data frame within the 'ind' list
extract_column_names <- function(file_path) {
  load(file_path)
  file_name <- basename(file_path)
  
  # Check if the loaded object contains a list named 'ind' and a data frame named 'indicators' within 'ind'
  if (exists("ind") && is.list(ind) && "indicators" %in% names(ind)) {
    # Extract column names from the 'indicators' data frame within 'ind' list
    col_names <- colnames(ind$indicators)
    
    # Create a data frame with file name and column names
    data.frame(file_name = file_name, ind_name = col_names)
  } else {
    # Return an empty data frame if 'ind' list or 'indicators' data frame doesn't exist
    data.frame(file_name = character(), ind_name = character())
  }
}

# Set the directory containing the .RData files
indicator_dir <- "indicator_objects"

# Get a list of all .RData files in the directory
rdata_files <- list.files(indicator_dir, pattern = "\\.RData$", full.names = TRUE)

# Initialize an empty data frame to store the results
all_indicators <- data.frame(file_name = character(), ind_name = character())

# Loop through each .RData file and extract the column names
for (file in rdata_files) {
  indicators <- extract_column_names(file)
  all_indicators <- bind_rows(all_indicators, indicators)
}

# Print the resulting data frame
print(all_indicators)

# Save the data frame to a CSV file for manual edits
write.csv(all_indicators, "indicator_data/extracted_ind_object_names.csv", row.names = FALSE)

