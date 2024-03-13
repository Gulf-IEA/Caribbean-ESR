
### Unemployment data for PR and USVI from Amy Freitag (manually downloaded from FRED https://fred.stlouisfed.org/)
### latest data run through 12/2023.

# USVI data: DON'T KNOW WHERE THIS CAME FROM, ASK AMY
# PR data: https://fred.stlouisfed.org/series/PRUR


rm(list = ls())

# load data -------------------------------------

d <- read.csv("indicator_data/unemployment.csv")
head(d)

# remove first row with units
d = d[-1,]

# Convert date column to Date object
d$indicator <- as.Date(d$indicator, format = "%m/%d/%Y")
# Format date column to %Y%b
d$indicator <- format(d$indicator, "%Y%b")

PR = (as.numeric(d$PR.unemployment, na.action = na.omit))
USVI = (as.numeric(d$USVI.unemployment, na.action = na.omit))

# save as indicator object ----------------------
datdata <- d$indicator
inddata <- data.frame(PR, USVI)
labs <- c("Unemployment" , "Percent", "Puerto Rico",
          "Unemployment" , "Percent", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
save(inddata, file = "indicator_objects/unemployment.RData")

plotIndicatorTimeSeries(inddata, coltoplot = 1:2, sublabel = TRUE, dateformat = "%Y%b")
