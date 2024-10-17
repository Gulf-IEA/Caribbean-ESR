
### Whole GDP data for PR and USVI from Amy Freitag on 3/5/24 (manually downloaded from FRED https://fred.stlouisfed.org/)
### latest data run through 2021 for USVI and 2022 for PR.
# edited by M. Karnauskas 05-16-24 for automated download

# USVI data: https://fred.stlouisfed.org/series/MKTGDPVIA646NWDB
# PR data: https://fred.stlouisfed.org/series/NYGDPMKTPCDPRI

##########################################################

rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)
library(rvest)

load("indicator_processing/spec_file.RData")

# data download --------------------------------------------------------

# Puerto Rico
page <- read_html("https://fred.stlouisfed.org/series/NYGDPMKTPCDPRI")
url <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")

url[grep("data/", url)]

#url2 <- paste0("https://fred.stlouisfed.org", url[grep("data/", url)])
#pr_info <- read.csv(url(url2), header = F)
#pr <- pr_info[24:nrow(pr_info), ]

url2 <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1140&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=NYGDPMKTPCDPRI&scale=left&cosd=1960-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-10-17&revision_date=2024-10-17&nd=1960-01-01"
pr <- read.csv(url2)
names(pr) <- c("date", "gdp")

#prtab <- data.frame(cbind(substr(pr$date, 1, 4), substr(pr$date, 11, 50)))
#prtab$X1 <- as.numeric(as.vector(prtab$X1))
#prtab$X2 <- as.numeric(as.vector(prtab$X2))

pr$year <- substr(pr$date, 1, 4)


# USVI
page <- read_html("https://fred.stlouisfed.org/series/MKTGDPVIA646NWDB")
url <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href")

url[grep("data/", url)]

url2 <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1140&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MKTGDPVIA646NWDB&scale=left&cosd=2002-01-01&coed=2021-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-10-17&revision_date=2024-10-17&nd=2002-01-01"
vi <- read.csv(url2)
names(vi) <- c("date", "gdp")
vi$year <- substr(vi$date, 1, 4)
vi


# merge data 

yrs <- min(vi$year, pr$year) : max(vi$year, pr$year)
mat <- data.frame(matrix(data = NA, nrow = length(yrs), ncol = 2))
rownames(mat) <- yrs 

mat[which(yrs %in% pr$year), 1] <- pr$gdp
mat[which(yrs %in% vi$year), 2] <- vi$gdp
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

print("GDP -- SUCCESSFULLY RUN")