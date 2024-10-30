# Water quality indicator from the EPA water quality data portal: https://www.epa.gov/waterdata/TADA

if(!"remotes"%in%installed.packages()){
  install.packages("remotes")
}

remotes::install_github("USEPA/EPATADA", ref = "develop", dependencies = TRUE, force = TRUE)

getwd() # find your working directory
TADA::TADA_GetTemplate() # download template to working directory

# uncomment below to review example dataset
# Data_Nutrients_UT <- TADA::Data_Nutrients_UT

EPATADA::TADA_GetTemplate()

dataset_0 = EPATADA::TADA_DataRetrieval(
     startDate = "2020-06-22",
     endDate = "null",
     countycode = "null",
     huc = "null",
     siteid = "null",
     siteType = "null",
     characteristicName = "null",
     characteristicType = "null",
     sampleMedia = "null",
     statecode = "PR",
     organization = "null",
     project = "null",
     applyautoclean = TRUE
   )
