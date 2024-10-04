
rm(list = ls())

install.packages("pdftools")
library(pdftools)

load("indicator_data/hotel_occupancy_rates_USVI_and_PR.RData")

dates <- inddata$datelist
dat <- inddata$indicators
labs <- inddata$labels

d2021 <- c(55, 65.4, 77.1, 51.5, 83.9, 79.2, 47, 62, 58.6, 52.4, 63.8,  65)
d2022 <- c(49.2, 74.3, 72.3, 68.5, 66, 66.8, 65.9, 62.1, 44.6, 44, 55.9, 64)
d2023 <- c(64.9, 63.2, 71.0, 61.5, 60.8, 61.5, 61.0, 52.2, 41.1, 43.1, 54.7, 55.5)

mos <- rep(month.abb, 3)
yrs <- c(rep(2021, 12), rep(2022, 12), rep(2023, 12))
paste0(yrs, mos)




url <- "https://usviber.org/wp-content/uploads/2024/07/H24-MAY.pdf"
url <- "https://usviber.org/wp-content/uploads/2023/05/H22-DEC-revised-1.pdf"

pdf <- pdf_text(url) %>% readr::read_lines()  

pdf.labs <- pdf[c(grep("Period", pdf))]
all_stat_lines <- pdf.labs %>% str_squish() %>% strsplit(split = " ")
df1 <- plyr::ldply(all_stat_lines, header = T)

st <- grep("January", pdf)[2]
pdf.stats <- pdf[c(st:(st+11))]
all_stat_lines <- pdf.stats %>% str_squish() %>% strsplit(split = " ")
df <- plyr::ldply(all_stat_lines, header = T)
names(df) <- df1






[7] "                                ST. THOMAS/ST. JOHN                              ST. CROIX                      U.S.V.I., TOTAL"                  
[8] "           Period"                                                                                                                                
[9] "                                 2021          2022        % chg.         2021        2022      % chg.        2021       2022      % chg."        
[10] "YEAR TO DATE:"                                                                                                                                    
[11] "DECEMBER                          66.7          67.2           0.5        54.2         46.5        -7.7       63.2       60.9         -2.3"       
[12] ""                                                                                                                                                 
[13] "MONTH:"                                                                                                                                           
[14] "January                           58.8          52.8          -5.9        44.9         39.9        -5.0       55.0        49.2        -5.8"       
[15] "February                          69.9          79.1           9.3        52.9         62.0         9.1       65.4        74.3         8.9"       
[16] "March                             82.9          76.8          -6.1        65.4         60.5        -4.9       77.1        72.3        -4.8"       
[17] "April                             51.4          71.3          19.9        51.7         61.2         9.6       51.5        68.5       17.0"        
[18] "May                               85.0          72.5         -12.5        80.0         50.0       -30.1       83.9        66.0       -17.9"       
[19] "June                              81.7          73.6          -8.1        73.5         49.9       -23.6       79.2        66.8       -12.5"       
[20] "July                              41.4          74.7          33.4        61.7         46.9       -14.8       47.0        65.9       19.0"        
[21] "August                            64.8          70.6           5.8        54.3         44.2       -10.2       62.0        62.1         0.2"       
[22] "September                         63.9          48.3         -15.6        44.5         36.8        -7.7       58.6        44.6       -14.0"       
[23] "October                           59.8          47.8         -12.0        34.3         36.2         1.9       52.4        44.0        -8.3"       
[24] "November                          71.9          66.7          -5.2        44.3         33.5       -10.8       63.8        55.9        -7.9"       
[25] "December                          72.8          72.7          -0.1        46.4         46.0        -0.5       65.0        64.0         -1.0"       






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



# selecting the "a" HTML element storing the product URL 
a_element <- html_product %>% html_element("a") 
# selecting the "img" HTML element storing the product image 
img_element <- html_product %>% html_element("img") 
# selecting the "h2" HTML element storing the product name 
h2_element <- html_product %>% html_element("h2") 
# selecting the "span" HTML element storing the product price 
span_element <- html_product %>% html_element("span")
  
raw_list <- page %>% # takes the page above for which we've read the html
  html_nodes("a") %>%  # find all links in the page
  html_attr("href") 

%>% # get the url for these links
  str_subset("\\.pdf")

%>% # find those that end in pdf only
  str_c("https://rbi.org.in", .) %>% # prepend the website to the url
  map(read_html) %>% # take previously generated list of urls and read them
  map(html_node, "#raw-url") %>% # parse out the 'raw' url - the link for the download button
  map(html_attr, "href") %>% # return the set of raw urls for the download buttons
  str_c("https://www.rbi.org.in", .) %>% # prepend the website again to get a full url
  for (url in raw_list)
  { download.file(url, destfile = basename(url), mode = "wb") 
  } 
  

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


