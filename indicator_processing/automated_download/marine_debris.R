
rm(list = ls())

#url <- "https://sdg-tracker.org/ca8b6a47-9731-4fd8-8369-1c69c25485d6"
#download.file(url = url, destfile = "C:/Users/mandy.karnauskas/Downloads/litter.csv")

dat <- read.csv("C:/Users/mandy.karnauskas/Downloads/beach-litter.csv", skip = 0, header = T)

d <- dat[which(dat$Entity == "Puerto Rico" | dat$Entity == "United States Virgin Islands"), ]

tab <- tapply(d$X14.1.1...Beach.litter.per.square.kilometer..Number....EN_MAR_BEALITSQ, list(d$Year, d$Entity), sum)

# format indicator object -----------------------------

datdata <- as.integer(rownames(tab))
inddata <- data.frame(tab/10^6)
labs <- c("Marine debris", "Millions of items per km^2", "Puerto Rico", 
          "Marine debris", "Millions of items per km^2", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

# save and plot -----------------------------------------
plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, sublabel = T)

inddata <- s
save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/marine_debris.RData")

