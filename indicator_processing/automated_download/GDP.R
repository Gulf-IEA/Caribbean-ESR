
### Whole GDP data for PR and USVI from Amy Freitag on 3/5/24 (manually downloaded from FRED https://fred.stlouisfed.org/)
### latest data run through 2021 for USVI and 2022 for PR.
# edited by M. Karnauskas 05-16-24 for automated download

# USVI data: https://fred.stlouisfed.org/series/MKTGDPVIA646NWDB
# PR data: https://fred.stlouisfed.org/series/NYGDPMKTPCDPRI

##########################################################

rm(list = ls())
dev.off()

library(maps)
library(plotTimeSeries)
library(rvest)

load("../spec_file.RData")

# data download --------------------------------------------------------

# Puerto Rico
page <- read_html("https://fred.stlouisfed.org/series/NYGDPMKTPCDPRI")
url <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")

url[grep("data/", url)]

url2 <- paste0("https://fred.stlouisfed.org", url[grep("data/", url)])
pr_info <- read.csv(url(url2), header = F)

pr <- pr_info[24:nrow(pr_info), ]

prtab <- data.frame(cbind(substr(pr$V1, 1, 4), substr(pr$V1, 11, 50)))
prtab$X1 <- as.numeric(as.vector(prtab$X1))
prtab$X2 <- as.numeric(as.vector(prtab$X2))


# USVI
page <- read_html("https://fred.stlouisfed.org/series/MKTGDPVIA646NWDB")
url <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")

url[grep("data/", url)]

url2 <- paste0("https://fred.stlouisfed.org", url[grep("data/", url)])
vi_info <- read.csv(url(url2), header = F)

vi <- vi_info[24:nrow(vi_info), ]

vitab <- data.frame(cbind(substr(vi$V1, 1, 4), substr(vi$V1, 11, 50)))
vitab$X1 <- as.numeric(as.vector(vitab$X1))
vitab$X2 <- as.numeric(as.vector(vitab$X2))

# merge data 

yrs <- min(vitab$X1, prtab$X1) : max(vitab$X1, prtab$X1)
mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 2))
rownames(mat) <- yrs 

mat[which(yrs %in% prtab$X1), 1] <- prtab[, 2]
mat[which(yrs %in% vitab$X1), 2] <- vitab[, 2]
mat <- mat / 10^9
mat

# save as indicator object ----------------------
datdata <- yrs
inddata <- mat
labs <- c("Gross Domestic Product" , "Current U.S. dollars (billions)", "Puerto Rico",
          "Gross Domestic Product" , "Current U.S. dollars (billions)", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------

ind <- inddata
plotIndicatorTimeSeries(ind, coltoplot = 1:2, plotrownum = 2, sublabel = TRUE)

save(ind, file = "indicator_objects/GDP.RData")

###############################  END  #############################