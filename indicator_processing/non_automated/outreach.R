# Code to develop outreach indicators - including MREP workshop industry participants over time and SeaGrant workshop participants. 

# Last updated 6/20/2024 by Carissa Gervasi

rm(list = ls())

plot.new()
dev.off()

library(pdftools)
library(stringr)
library(dplyr)

###############
## MREP participants (not able to automate, numbers emailed from Courtney Pickett)

years = c(2015:2024)

participants = c(21,0,52,0,33,0,0,32,22,33)

MREP = data.frame(year = years, part_MREP = participants)

MREP$cumu_MREP = cumsum(MREP$part_MREP)



###################
## Seagrant meeting/workshop participants (pdf annual reports emailed). Want to extract the number of attendees in SeaGrant meetings/workshops for each year.


# Function to extract attendees from a single PDF
extract_attendees <- function(pdf_path) {
  pdf_text <- pdf_text(pdf_path)
  last_page <- pdf_text[length(pdf_text)]
  attendees <- str_match(last_page, "Attendees in SG Meetings/Workshops\\s+(\\d+)")[2]
  return(as.numeric(attendees))
}

# Function to extract the year from the filename
extract_year <- function(filename) {
  year <- str_match(filename, "(\\d{4})")[1]
  return(as.numeric(year))
}

# Set the directory containing the PDF files
pdf_directory <- "indicator_data/inputsToBeUpdatedAnnually/outreach/"

# Get a list of all PDF files in the directory
pdf_files <- list.files(pdf_directory, pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty data frame to store results
Seagrant <- data.frame(year = integer(), attendees = integer(), stringsAsFactors = FALSE)

# Loop through each PDF file
for (pdf_file in pdf_files) {
  year <- extract_year(basename(pdf_file))
  attendees <- extract_attendees(pdf_file)
  Seagrant <- rbind(Seagrant, data.frame(year = year, attendees = attendees))
}

# Print the results
print(Seagrant)

Seagrant$cumu_Seagrant = cumsum(Seagrant$attendees)
Seagrant$cumu_Seagrant2 = Seagrant$cumu_Seagrant/1000




##### Combine MREP & SeaGrant participant data

df = merge(MREP, Seagrant, by = "year", all=T)


# save as indicator object ----------------------
datdata <- 2010:2024
inddata <- data.frame(cbind(df$cumu_MREP, df$cumu_Seagrant2))
labs <- c("MREP industry participants" , "cumulative graduates", "",
          "SeaGrant workshop/meeting participants" , "cumulative attendees (thousands)", "")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"


# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2)

save(ind, file = "indicator_objects/outreach.RData")

###############################  END  #############################

print("outreach -- SUCCESSFULLY RUN")
