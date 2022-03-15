
rm(list = ls())

library(rfishbase)

setwd("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData")

# concatenate landings data files -----------------------
lis <- dir()[grep("PR", dir())]
d1 <- read.csv(lis[1])
d2 <- read.csv(lis[2])
d3 <- read.csv(lis[3])
d4 <- read.csv(lis[4])

table(names(d1) == names(d2))
table(names(d1) == names(d3))
table(names(d1) == names(d4))

d <- rbind(d3, d4, d1, d2)
names(d)
head(d)

write.table(d, file = "PR_landings_83_20.csv", sep = ",", col.names = T, row.names = F)

# take a look at data fields ----------------------------
table(d$VESSEL, useNA = "always")
table(d$YEAR_LANDED, useNA = "always")
table(d$MONTH_LANDED, useNA = "always")
table(d$DAY_LANDED, useNA = "always")
table(d$FISHING_CENTER_ED, useNA = "always")
table(d$FISHING_CENTER_NAME, useNA = "always")
table(d$MUNICIPALITY, useNA = "always")
table(d$AREA_FISHED1, useNA = "always")
table(d$AREA_FISHED2, useNA = "always")
table(d$AREA_FISHED3, useNA = "always")
table(d$AREA_FISHED4, useNA = "always")
table(d$FIN_GEAR_CODE, useNA = "always")
table(d$FIN_GEAR_NAME, useNA = "always")
table(d$PR_ID_CODE_ED, useNA = "always")
table(d$NUMBER_OF_TRIPS_ED, useNA = "always")
table(d$GEAR_QTY_ED, useNA = "always")
table(d$GEAR_HOURS_ED, useNA = "always")
hist(d$MAXIMUM_DEPTH_ED)
hist(d$MINIMUM_DEPTH_ED)
table(d$SPECIES_ITIS, useNA = "always")
table(d$ITIS_COMMON_NAME, useNA = "always")
table(d$ITIS_SCIENTIFIC_NAME, useNA = "always")
hist(d$POUNDS_LANDED)
hist(d$VALUE_IN_DOLLARS)
hist(d$PRICE_PER_LB)
table(d$DISTANCE, useNA = "always")
table(d$DISTANCE_DESCRIBE, useNA = "always")
table(d$TRIP_TICKET_NUMBER_ED, useNA = "always")

# compile list of species names into dataframe ----------------------------
tab <- table(d$ITIS_COMMON_NAME, d$ITIS_SCIENTIFIC_NAME)
tab[which(tab > 0)] <- 1
table(colSums(tab))
table(rowSums(tab))

spp <- data.frame(unique(d$ITIS_SCIENTIFIC_NAME))
names(spp) <- "SCIname"
spp$COMname <- "NA"

for (i in 1: nrow(spp)) { 
  ta <- table(d$ITIS_COMMON_NAME[which(d$ITIS_SCIENTIFIC_NAME == spp$SCIname[i])])
  spp$COMname[i] <- names(ta) 
}

# get additional species field codes from fishbase -------------------------------
spp$common[i] <- NA
spp$genera[i] <- NA
spp$subfam[i] <- NA
spp$PD[i]     <- NA
spp$Lmax <- NA

for (i in 1:nrow(spp)) { 
  
  sp <- species(spp$SCIname[i])
  spp$common[i] <- sp$FBname
  spp$genera[i] <- sp$GenCode
  spp$subfam[i] <- sp$Subfamily
  spp$PD[i]     <- sp$DemersPelag
  spp$Lmax[i]   <- sp$Length
  
  if (is.na(sp$Subfamily)) { 
    if (length(strsplit(spp$SCIname[i], " ")[[1]]) == 1) { 
      spp$subfam[i] <- spp$SCIname[i]  }
      }
  }

# check outputs and write file --------------------------------------------
spp
head(spp)
table(spp$genera, useNA = "always")
table(spp$subfam, useNA = "always")
table(spp$PD, useNA = "always")
hist(spp$Lmax)

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")
#write.table(spp, file = "spp_ref.csv", sep = ",", row.names = F, col.names = T)

# fill in NAs manually
# 1. sort by genus and fill in with averages of genera level
# 2. sort by family or subfamily and fill in with averages of family level 
# 3. look up species that only have one representative per family
#    - for some species the identification is obvious (dolphin, snook)
#    - for others some assumptions have to be made (boxfish, burrfish); assumed representative is listed


# compile data for STT and STX -------------------------------

rm(list = ls())
library(rfishbase)

setwd("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData")

# concatenate landings data files -----------------------
lis <- dir()[grep("STT", dir())]
lis <- lis[grep("csv", lis)]
d1 <- read.csv(lis[1])
d2 <- read.csv(lis[2])

table(names(d1) == names(d2))

d <- rbind(d1, d2)
names(d)
head(d)
dim(d)

write.table(d, file = "STT_landings.csv", sep = ",", col.names = T, row.names = F)

table(d$TRIP_ID, useNA = "always")
table(d$TRIP_YEAR, useNA = "always")
table(d$TRIP_MONTH, useNA = "always")
table(d$TRIP_DAY, useNA = "always")
table(d$VESSEL_CD, useNA = "always")
table(d$FISHER_PERMIT, useNA = "always")
table(d$FISHER_FIRST_NAME, useNA = "always")
table(d$FISHER_LAST_NAME, useNA = "always")
table(d$CHARTER_TRIP, useNA = "always")
table(d$NUM_PARTNERS_OR_HELPERS, useNA = "always")
table(d$IS_CATCH_SPLIT, useNA = "always")
table(d$ISLAND, useNA = "always")
table(d$TOTAL_TRAPS_IN_WATER, useNA = "always")
table(d$FAD_CD, useNA = "always")
table(d$GEAR_TYPE_NM, useNA = "always")
table(d$GEAR1_NAME, useNA = "always")
table(d$SPECIES_CD, useNA = "always")
table(d$SPECIES_NM, useNA = "always")
table(d$TRIP_MAY_BE_DUPLICATE, useNA = "always")

sp <- unique(d$SPECIES_NM)

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")
ref <- read.csv("spp_ref_manualEdit.csv")
head(ref)
head(d)

table(sp %in% ref$COMname)
sp[which(sp %in% ref$COMname)]
sp[-which(sp %in% ref$COMname)]

ref$USVI <- NA
ref$USVI[which(ref$COMname %in% sp)] <- 1

ref$newnames <- NA
ref$newnames[1:length(sp[-which(sp %in% ref$COMname)])] <- sp[-which(sp %in% ref$COMname)]

#write.csv(ref, file = "spp_ref_STT.csv", row.names = F)

# manually edit to modify species names or add new species --------------------------------

# check that all names are included --------------------------------------------------
ref <- read.csv("spp_ref_STT_manualEdit.csv")
head(ref)
table(sp %in% ref$COMname)
sp[which(sp %in% ref$COMname)]
sp[-which(sp %in% ref$COMname)]

# end -------------------------


rm(list = ls())
library(rfishbase)

setwd("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData")

d <- read.csv("STX_072011_present_LANDINGS_trip_2021-03-11.csv")

table(d$TRIP_ID, useNA = "always")
table(d$TRIP_YEAR, useNA = "always")
table(d$TRIP_MONTH, useNA = "always")
table(d$TRIP_DAY, useNA = "always")
table(d$VESSEL_CD, useNA = "always")
table(d$FISHER_PERMIT, useNA = "always")
table(d$FISHER_FIRST_NAME, useNA = "always")
table(d$FISHER_LAST_NAME, useNA = "always")
table(d$CHARTER_TRIP, useNA = "always")
table(d$NUM_PARTNERS_OR_HELPERS, useNA = "always")
table(d$IS_CATCH_SPLIT, useNA = "always")
table(d$ISLAND, useNA = "always")
table(d$TOTAL_TRAPS_IN_WATER, useNA = "always")
table(d$FAD_CD, useNA = "always")
table(d$GEAR_TYPE_NM, useNA = "always")
table(d$GEAR1_NAME, useNA = "always")
table(d$SPECIES_CD, useNA = "always")
table(d$SPECIES_NM, useNA = "always")
table(d$TRIP_MAY_BE_DUPLICATE, useNA = "always")

sp <- unique(d$SPECIES_NM)

setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/")
ref <- read.csv("spp_ref_STT_manualEdit.csv")
head(ref)
head(d)

table(sp %in% ref$COMname)
sp[which(sp %in% ref$COMname)]
sp[-which(sp %in% ref$COMname)]

ref$USVI <- NA
ref$USVI[which(ref$COMname %in% sp)] <- 1

ref$newnames <- NA
ref$newnames[1:length(sp[-which(sp %in% ref$COMname)])] <- sp[-which(sp %in% ref$COMname)]

write.csv(ref, file = "spp_ref_STX.csv", row.names = F)