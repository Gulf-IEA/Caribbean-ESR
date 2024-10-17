
### Unemployment data for PR and USVI from Amy Freitag on 3/5/24 (manually downloaded from FRED https://fred.stlouisfed.org/)
### latest data run through 12/2023.
# edited by M. Karnauskas 05-16-24 for automated download

# USVI data: https://fred.stlouisfed.org/series/VIRINSUREDUR
# PR data: https://fred.stlouisfed.org/series/PRUR


rm(list = ls())

plot.new()
dev.off()

library(maps)
library(plotTimeSeries)

load("indicator_processing/spec_file.RData")


# load data -------------------------------------

urlvi <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=958&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=VIRINSUREDUR&scale=left&cosd=1986-02-08&coed=2024-04-27&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Saturday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-05-16&revision_date=2024-05-16&nd=1986-02-08"
urlpr <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=958&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PRUR&scale=left&cosd=1976-01-01&coed=2024-03-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-05-16&revision_date=2024-05-16&nd=1976-01-01"

vi <- read.csv(url(urlvi))
pr <- read.csv(url(urlpr))

class(vi$VIRINSUREDUR)
class(pr$PRUR)

pr$PRUR <- as.numeric(as.vector(pr$PRUR))

vi$year <- substr(vi$DATE, 1, 4)
pr$year <- substr(pr$DATE, 1, 4)
vi$mon <- substr(vi$DATE, 6, 7)
pr$mon <- substr(pr$DATE, 6, 7)
vi$yrmon <- paste0(vi$year, vi$mon)
pr$yrmon <- paste0(pr$year, pr$mon)

vitab <- data.frame(tapply(vi$VIRINSUREDUR, vi$yrmon, mean, na.rm = T))
vitab <- data.frame(vitab)
vitab$date <- rownames(vitab)
names(vitab) <- c("vi", "date")
head(vitab)

length(unique(pr$yrmon)) == nrow(pr)

head(vitab)
head(pr)

d <- merge(pr, vitab, by.x = "yrmon", by.y = "date")
d$dat2 <- paste0(d$year, month.abb[as.numeric(d$mon)])
head(d)


# save as indicator object ----------------------
datdata <- d$dat2
inddata <- data.frame(cbind(d$PRUR, d$vi))
labs <- c("Unemployment rate" , "Percent", "Puerto Rico",
          "Unemployment rate" , "Percent", "USVI")
indnames <- data.frame(matrix(labs, nrow = 3, byrow = F))
inddata <- list(labels = indnames, indicators = inddata, datelist = datdata)
class(inddata) <- "indicatordata"

# plot and save ----------------------------------
ind <- inddata
plotIndicatorTimeSeries(ind, plotrownum = 2, coltoplot = 1:2, sublabel = TRUE, dateformat = "%Y%b", trendAnalysis = T)

save(ind, file = "indicator_objects/unemployment.RData")

#############################  END  ####################################

print("unemployment -- SUCCESSFULLY RUN")


