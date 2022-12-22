rm(list = ls())

styear <- 1995

# load data -------------------------------------
dpr <- read.csv("../indicator_data/SAU_PR/SAU EEZ 630 v50-0.csv")
head(dpr)

# look at fields ------------------------------
apply(dpr[, 1:15], 2, table)
dpr <- dpr[which(dpr$year >= styear), ]

# calculate total rec catch for PR -------------
table(dpr$fishing_sector)
drec <- dpr[which(dpr$fishing_sector == "Recreational"), ]
dim(drec)

par(mar = c(5, 15, 1, 1))
barplot(sort(tapply(drec$tonnes, drec$common_name, sum, na.rm = T)), las = 2, horiz = T)

tapply(drec$uncertainty_score, drec$year, mean)
boxplot(drec$uncertainty_score ~ drec$year)

tot <- tapply(drec$tonnes, drec$year, sum, na.rm = T) * 2204.62

# load data -------------------------------------
dvi <- read.csv("../indicator_data/SAU_USVI/SAU EEZ 850 v50-0.csv")
head(dvi)

# look at fields ------------------------------
apply(dvi[, 1:15], 2, table)
dvi <- dvi[which(dvi$year >= styear), ]

# calculate total rec catch for USVI -----------
table(dvi$fishing_sector)
drec_vi <- dvi[which(dvi$fishing_sector == "Recreational"), ]
dim(drec_vi)

par(mar = c(5, 10, 1, 1))
barplot(sort(tapply(drec_vi$tonnes, drec_vi$common_name, sum, na.rm = T)), las = 2, horiz = T)

boxplot(drec_vi$uncertainty_score ~ drec_vi$year)
tot_vi <- tapply(drec_vi$tonnes, drec_vi$year, sum, na.rm = T) * 2204.62

summary(names(tot) == names(tot_vi))

# save as indicator object ----------------------
datdata <- as.integer(names(tot))
inddats <- data.frame(cbind(as.numeric(tot), as.numeric(tot_vi)))/ 10^6
labs <- c("Total recreational catch", "millions of pounds", "Puerto Rico", 
          "Total recreational catch", "millions of pounds", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))

inddata <- list(labels = indnames, indicators = inddats, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(inddata) <- "indicatordata"

plotIndicatorTimeSeries(inddata, coltoplot = 1:2, plotrownum = 2, sublabel = T, sameYscale = F)

# plot and save ----------------------------------
save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/total_rec_catch.RData")


