
rm(list = ls())

library(rfishbase)

setwd("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/")

# concatenate PR logbook data files  -----------------------
# ONLY NEED TO DO THIS ONCE! 

lis <- dir()[grep("PR_landings", dir())]
lis
#d1 <- read.csv(lis[1])
#d2 <- read.csv(lis[2])
#d3 <- read.csv(lis[3])
#d4 <- read.csv(lis[4])

table(names(d1) == names(d2))
table(names(d1) == names(d3))
table(names(d1) == names(d4))

d <- rbind(d3, d4, d1, d2)
names(d)
head(d)

#write.table(d, file = "PR_landings_83_20.csv", sep = ",", col.names = T, row.names = F)



# create file to match names in logbook and shellcatch ---------------
# DONE 06/02/2023 - don't redo until new shellcatch pull 

sc <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/shellcatch_pr_data_req_02152023_C.csv")   # original shellcatch data
apply(sc[1:4], 2, table)

dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20.csv")

head(sc)
head(dat)
names(sc) %in% names(dat)
d <- data.frame(as.matrix(table(dat$ITIS_COMMON_NAME)))  # data frame for matching
names(d) <- "N"
d$V2 <- rownames(d)
d$V3 <- NA
d$V4 <- NA
head(d)

for (i in 1:nrow(d)) {    # reverse names for matching
  b <- unlist(strsplit(d$V2[i], ","))
    if (length(b) == 1) { b1 <- paste(b[1])         }  
    if (length(b) == 2) { b1 <- paste(b[2], b[1])   }     
    if (length(b) == 3) { b1 <- paste(b[3], b[2], b[1]) } 
  d$V3[i] <- b1           }

sclis <- as.character(unique(sc$SPECIES_NM))
head(d)
dim(d)
length(unique(d$V2))
length(sclis)

d$V4 <- sclis[match(d$V3, sclis)]  # matching
sort(d$V4)
length(sclis[-which(sclis %in% d$V4)])
mis <- sclis[-which(sclis %in% d$V4)]

for (i in 1:length(mis)) {      # matching
  d$V4[grep(mis[i], d$V3)] <- mis[i]  } 

summary(is.na(d$V4))
length(sclis)
sclis[-which(sclis %in% d$V4)]
length(sclis[-which(sclis %in% d$V4)])

d$V5 <- c(sclis[-which(sclis %in% d$V4)], rep(NA, nrow(d) - length(sclis[-which(sclis %in% d$V4)])))
names(d) <- c("N", "logbook", "adj", "shellcatch", "unmatched")

# csv file for manual edits to match names ------------------
#write.table(d, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent/name_matches.csv", 
#            sep = ",", col.names = T, row.names = F)

# output and then manually match up remaining names - this takes time
# any species not in logbook, need to look up and insert into spp_ref file


# merge shellcatch with logbook -----------------------
# DONE 06/02/2023

rm(list = ls())
setwd("C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_processing/fishery_dependent")

sc <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/shellcatch_pr_data_req_02152023_C.csv")   # original shellcatch data
sc$POUNDS_LANDED2 <- as.numeric(as.vector(sc$POUNDS_LANDED))
sc$ADJUSTED_POUNDS2 <- as.numeric(as.vector(sc$ADJUSTED_POUNDS))
sc$CORRECTION_FACTOR <- as.numeric(as.vector(sc$CORRECTION_FACTOR))
sc$PRICE <- as.numeric(as.vector(sc$PRICE))
summary(sc$POUNDS_LANDED == sc$POUNDS_LANDED2)
summary(sc$ADJUSTED_POUNDS == sc$ADJUSTED_POUNDS2)

apply(sc[1:4], 2, table, useNA = "always")
table(sc$LANDING_AREA_COUNTY_OR_MUNICIPALITY)
table(sc$COAST)
table(sc$GEAR_NAME)
hist(sc$POUNDS_LANDED2)
hist(sc$ADJUSTED_POUNDS2)
table(sc$CORRECTION_FACTOR)
co <- sc$POUNDS_LANDED2 / sc$CORRECTION_FACTOR
table(round(co- sc$ADJUSTED_POUNDS2))
hist(sc$PRICE)
table(sc$AREA_CD1)

sc$SPECIES_NM <- as.character(sc$SPECIES_NM)

# replace duplicate names -----------------------
sc$SPECIES_NM[which(sc$SPECIES_NM == "CROAKERS")] <- "CROAKER"
sc$SPECIES_NM[which(sc$SPECIES_NM == "HERRING")] <- "THREAD HERRING"
sc$SPECIES_NM[which(sc$SPECIES_NM == "SMOOTHTAIL SPINY LOBSTER")] <- "SPINY LOBSTER"
sc$SPECIES_NM[which(sc$SPECIES_NM == "KING MACKAREL, KINGFISH")] <- "KINGFISH MACKEREL"
sc$SPECIES_NM[which(sc$SPECIES_NM == "WENCHMAN")] <- "CARDINAL"
sc$SPECIES_NM[which(sc$SPECIES_NM == "SPOTTED TRUNKFISH")] <- "TRUNKFISH"
sc$SPECIES_NM[which(sc$SPECIES_NM == "SMOOTH TRUNKFISH")] <- "TRUNKFISH"
sc$SPECIES_NM[which(sc$SPECIES_NM == "SEA BASSES")] <- "GROUPERS"

nam <- read.csv("name_matches_edited.csv")

dim(sc)
summary(sc$SPECIES_NM %in% nam$shellcatch)
sc[which((sc$SPECIES_NM %in% nam$shellcatch) == "FALSE"), ]
sc <- sc[-which(sc$SPECIES_NM  == "SELECT TO MANUALLY INPUT"), ]
summary(sc$SPECIES_NM %in% nam$shellcatch)

sc[which(sc$SPECIES_NM == "WARMOUTH BASS"),]
sc <- sc[-which(sc$SPECIES_NM == "WARMOUTH BASS"),]
dim(sc)

match(sc$SPECIES_NM, nam$shellcatch)
table(match(sc$SPECIES_NM, nam$shellcatch))
sc$logname <- as.character(nam$logbook[match(sc$SPECIES_NM, nam$shellcatch)])
cbind(sc$logname, sc$SPECIES_NM)

# merge with logbook -------------------------------
dat <- read.csv("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20.csv")

head(dat)
head(sc)

#                   VESSEL     YEAR_LANDED  MONTH_LANDED    DAY_LANDED FISHING_CENTER_ED   FISHING_CENTER_NAME       MUNICIPALITY  
sclog <- data.frame(sc$ISLAND, sc$TRIP_YEAR, sc$TRIP_MONTH, sc$TRIP_DAY, NA, sc$LANDING_AREA_COUNTY_OR_MUNICIPALITY, sc$LANDING_AREA_COUNTY_OR_MUNICIPALITY, 
#AREA_FISHED1 AREA_FISHED2 AREA_FISHED3 AREA_FISHED4 FIN_GEAR_CODE  FIN_GEAR_NAME PR_ID_CODE_ED        
 sc$AREA_CD1, sc$AREA_CD1, sc$AREA_CD1, sc$AREA_CD1, NA,            sc$GEAR_NAME, sc$LICENSE, 
#NUMBER_OF_TRIPS_ED   GEAR_QTY_ED   GEAR_HOURS_ED   MINIMUM_DEPTH_ED   MAXIMUM_DEPTH_ED  SPECIES_ITIS  ITIS_COMMON_NAME
NA, NA, NA, NA, NA, NA, sc$logname, 
#ITIS_SCIENTIFIC_NAME  POUNDS_LANDED      VALUE_IN_DOLLARS  CORRECTION_FACTOR     ADJUSTED_POUNDS PRICE_PER_LB  DISTANCE DISTANCE_DESCRIBE TRIP_TICKET_NUMBER_ED            
              NA,      sc$POUNDS_LANDED2, sc$PRICE,         sc$CORRECTION_FACTOR, sc$ADJUSTED_POUNDS2, sc$PRICE/sc$ADJUSTED_POUNDS2, NA, NA, sc$FISHING_TRIP_ID)

dim(dat)
dim(sclog)

cbind(names(dat), names(sclog))
names(sclog) <- names(dat)

dat2 <- rbind(dat, sclog)
head(dat2)
dim(sclog) + dim(dat)
dim(dat2)

#write.table(dat2, file = "C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/Jun2022/PR_landings_83_20_wSC.csv", 
#            sep = ",", col.names = T, row.names = F)

d <- dat2

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
table(d$CORRECTION_FACTOR, useNA = "always")
table(d$CORRECTION_FACTOR, d$YEAR_LANDED)
hist(d$ADJUSTED_POUNDS)
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

setwd("C:/Users/mandy.karnauskas/Desktop/CONFIDENTIAL/CaribbeanData/")

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
