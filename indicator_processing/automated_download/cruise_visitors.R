# Code to plot number of cruise ship visitors Puerto Rico & USVI 

# Last updated 6/19/2024 by Carissa Gervasi

rm(list = ls())
dev.off()


url1<-'https://www.bde.pr.gov/BDE/PREDDOCS/I_CRUISE.XLS'

library(readxl)
library(httr)
packageVersion("readxl")

GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_excel(tf, 2L)
str(df)


df2 = df[c(52,63),]

df2t = as.data.frame(t(df2))

df2t = df2t[-c(1,2,12),]


yrs_PR = as.integer(df2t$V1)
cruise_PR = as.numeric(df2t$V2)

# Create a data frame
PR_cruise <- data.frame(Year = yrs_PR, Cruise_passengers = cruise_PR)


############################

#USVI

# Load necessary libraries
library(pdftools)
library(dplyr)
library(tidyr)

# Specify the URL of the PDF
url2 <- "https://usviber.org/wp-content/uploads/2023/12/Tourism-indicator-2022-12-28-23-1.pdf"

# Specify the destination file path
destfile <- "indicator_data/Cruise-indicator-2022-12-28-23-1.pdf"

# Download the PDF
download.file(url2, destfile, mode = "wb")

# Check if the file has been downloaded
file.exists(destfile)

# Load the PDF
pdf_file <- "indicator_data/Cruise-indicator-2022-12-28-23-1.pdf"

# Extract text from the PDF
pdf_text <- pdf_text(pdf_file)

# Extract the relevant page (assuming the table is on the first page)
page_text <- pdf_text[1]

# Split the text into lines
lines <- strsplit(page_text, "\n")[[1]]

# Identify the lines containing the Visitor Arrivals table
# (assuming the table starts with "VISITOR ARRIVALS" and ends before "VISITOR EXPENDITURES")
start_line <- grep("VISITOR ARRIVALS", lines)
end_line <- grep("VISITOR EXPENDITURES", lines)
table_lines <- lines[(start_line + 1):(end_line - 1)]

# Combine lines into a single text block and split by spaces
table_text <- paste(table_lines, collapse = " ")
table_data <- strsplit(table_text, " +")[[1]]

# Create years column
years <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)

# Extract and clean the total visitors data by removing commas
passengers <- gsub(",", "", table_data[87:101])
passengers <- as.numeric(passengers)

# Create a data frame
USVI_cruise <- data.frame(Year = years, Cruise_passengers = passengers)

# Print the data frame
print(USVI_cruise)



##########

all_years <- data.frame(Year = min(PR_cruise$Year, USVI_cruise$Year) : max(PR_cruise$Year, USVI_cruise$Year))

# Merge the data frames with the complete data frame to fill in missing years with NA
combined_df <- merge(all_years, PR_cruise, by = "Year", all.x = TRUE)
combined_df <- merge(combined_df, USVI_cruise, by = "Year", all.x = TRUE)




# save as indicator object ----------------------
datdata <- all_years$Year
inddata <- data.frame(cbind(combined_df$Cruise_passengers.x, combined_df$Cruise_passengers.y))
labs <- c("Cruise passengers" , "thousands of people", "Puerto Rico",
          "Cruise passengers" , "thousands of people", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, plotcolnum = 1, trendAnalysis = TRUE, dateformat = "%b%Y", sublabel = TRUE, widadj = 1, hgtadj = 0.7, anom = "none", yposadj = 1)

save(ind, file = "indicator_objects/cruise.RData")

###############################  END  #############################