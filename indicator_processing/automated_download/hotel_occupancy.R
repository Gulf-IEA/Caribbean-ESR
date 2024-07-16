
rm(list = ls())

# https://rstudio-pubs-static.s3.amazonaws.com/415060_553527fd13ed4f30aae0f1e4483aa970.html
# https://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r

library(pdftools)
library(xml2)
library(rvest)
library(tidyverse)
library(stringr)
library(purrr)

URL <- "https://usviber.org/archived-data/"

page <- read_html(URL)
  
raw_list <- page %>% # takes the page above for which we've read the html
html_nodes("a") %>%  # find all links in the page
html_attr("href") %>% # get the url for these links
str_subset("\\.pdf") %>% # find those that end in pdf only
  

pg <- read_html(URL)
lis <- html_attr(html_nodes(pg, "a"), "href")
lablis <- lis[9:32]

findat <- c()
miss <- c(1995, 1997, 1999, 2002, 2017)

for (i in 1:length(lablis))  { 
  pdf <- pdf_text(lablis[i]) %>% readr::read_lines()  

  pdf.labs <- pdf[c(grep("Period", pdf))]
  all_stat_lines <- pdf.labs %>% str_squish() %>% strsplit(split = " ")
  df1 <- plyr::ldply(all_stat_lines, header = T)
  
  st <- grep("January", pdf)[2]
  pdf.stats <- pdf[c(st:(st+11))]
  all_stat_lines <- pdf.stats %>% str_squish() %>% strsplit(split = " ")
  df <- plyr::ldply(all_stat_lines, header = T)
  names(df) <- df1

  df2 <- df[, c(2, 5)]
  names(df2) <- c("STT", "STX")
  df2$yr <- as.numeric(rep(df1[2], 12))
  df2$mon <- df[, 1]

  findat <- rbind(findat, df2)

  if (df1[3] %in% miss) { 
    df2 <- df[, c(3, 6)]
    names(df2) <- c("STT", "STX")
    df2$yr <- as.numeric(rep(df1[3], 12))
    df2$mon <- df[, 1]
    print(pdf[c(5:9)])
    print(df2)
    
    findat <- rbind(findat, df2)
  }
  print(pdf[c(5:9)])
  print(df2)
}

findat <- data.frame(findat, stringsAsFactors = FALSE)
findat$month <- substr(findat$mon, 1, 3)
findat$m <- match(findat$month, month.abb)

findat <- findat[order(findat$yr, findat$m), ]
table(findat$yr)
which(table(findat$yr) > 12)

which(findat$yr == 2006)[seq(1, 24, 2)]
findat[which(findat$yr == 2006)[seq(1, 24, 2)], ]
findat <- findat[-which(findat$yr == 2006)[seq(1, 24, 2)], ]
which(findat$yr == 2015)[seq(1, 24, 2)]
findat[which(findat$yr == 2015)[seq(1, 24, 2)], ]
findat <- findat[-which(findat$yr == 2015)[seq(1, 24, 2)], ]
which(table(findat$yr) > 12)

plot(names(table(findat$yr)))
findat$yrmo <- paste0(findat$yr, findat$month)

datdata <- as.vector(findat$yrmo)
inddata <- data.frame(cbind(findat$STT, findat$STX))
inddata[, 1] <- as.numeric(inddata[, 1])
inddata[, 2] <- as.numeric(inddata[, 2])
labs <- c(rep("Hotel occupancy rates", 2), 
          rep("percent occupancy", 2),
          "St. Thomas and St. John", "St. Croix")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = T))
s <- list(labels = indnames, indicators = inddata, datelist = datdata) #, ulim = ulidata, llim = llidata)
class(s) <- "indicatordata"

inddata <- s

save(inddata, file = "C:/Users/mandy.karnauskas/Desktop/Caribbean-ESR/indicator_objects/hotel_occupancy.RData")

plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, dateformat = "%Y%b", sublabel = T) # outtype = "png", sameYscale = F)
plotIndicatorTimeSeries(s, coltoplot = 1:2, plotrownum = 2, dateformat = "%Y%b", sublabel = T, anom = "mon") # outtype = "png", sameYscale = F)


