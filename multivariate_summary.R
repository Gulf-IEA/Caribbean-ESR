

rm(list = ls())
library(plotTimeSeries)

load("../Caribbean-ESR/indicator_data/all_indicators_matrix.rda")
matrix_data

class(matrix_data$year)
matrix_data$year <- as.numeric(matrix_data$year)

trafficLightPlot(matrix_data, noNAs = 0.99)
# 2000+ has some data, 2010+ is data-rich period

par(mfrow = c(7, 5))
for (i in 1:ncol(matrix_data))  { hist(matrix_data[, i], main = names(matrix_data)[i]) }

dev.off()

d <- matrix_data[which(matrix_data$year >= 2000 & matrix_data$year <= 2023), ]
trafficLightPlot(d, noNAs = 1)

head(d)
tail(d)

risks <- c("DHW_PR", "DHW_VI", "OA", "ACE", "TURB_PR", "TURB_STT", "TURB_STX", "SST_MEAN", "SST_MIN", "SST_MAX", 
           "POLL_PR", "POLL_VI", "CHL", "ETQ", "DISTB_PR", "DISTB_STT", "DISTB_STX", "SARG", "CRU_PR", "CRU_VI", "POP_PR", "POP_VI")
food_fi <- c("VETU_PR", "GUTT_PR", "ANAL_PR", "CHRY_PR", "AURO_PR", "VIRI_PR", "VETU_STT", "GUTT_STT", "ANAL_STT", "CHRY_STT", 
             "AURO_STT", "VIRI_STT", "VETU_STX", "GUTT_STX", "ANAL_STX", "CHRY_STX", "AURO_STX", "VIRI_STX", "DEN_PR", "DEN_VI", "SLPSIZ_VI")
food_fd <- c("PDR_PR", "PDR_STT", "PDR_STX", "MLMAX_PR", "MLMAX_STT", "MLMAX_STX", 
            "LOB_PR", "LOB_STT", "LOB_STX", "CONC_PR", "CONC_STT", "CONC_STX", "OTHR_PR", "OTHR_STT", "OTHR_STX")
soc <- c("O_GDP_PR", "O_GDP_VI", "EST_PR", "EST_VI", "EMP_PR", "EMP_VI", "WAG_PR", "WAG_VI", "GDP_PR", "GDP_VI", 
         "UNEM_PR", "UNEM_VI", "GINI_R_PR", "GINI_R_STT", "GINI_R_STX")
oth <- c("REC_PR", "REC_VI", "DIV_PR", "DIV_STT", "DIV_STX", "BYCAT_PR", "BYCAT_STT", "BYCAT_STX", "REGS", "MGMT", 
         "TR3_PR", "TR3_VI", "OUT_PR", "OUT_VI", "CRLRCH_PR", "CRLCVR_PR", "CRLRCH_VI", "CRLCVR_VI")

d_risk <- d[, c(1, which(names(d) %in% risks))]
d_foodfi <- d[, c(1, which(names(d) %in% food_fi))]
d_foodfd <- d[, c(1, which(names(d) %in% food_fd))]
d_soc <- d[, c(1, which(names(d) %in% soc))]
d_oth <- d[, c(1, which(names(d) %in% oth))]

dim(d_risk)
dim(d_foodfi)
dim(d_foodfd)
dim(d_soc)
dim(d_oth)

par(mar = c(0.5, 10, 0, 1), mfrow = c(5, 1))

trafficLightPlot(d_risk, noNAs = 1)
trafficLightPlot(d_foodfi, noNAs = 1)
trafficLightPlot(d_foodfd, noNAs = 1)
trafficLightPlot(d_soc, noNAs = 1)
trafficLightPlot(d_oth, noNAs = 1)

# alternative categorization

risks <- c("DHW_PR", "DHW_VI", "OA", "ACE", "TURB_PR", "TURB_STT", "TURB_STX", "SST_MEAN", "SST_MIN", "SST_MAX", 
           "POLL_PR", "POLL_VI", "CHL", "ETQ", "DISTB_PR", "DISTB_STT", "DISTB_STX", "SARG", "CRU_PR", "CRU_VI", "POP_PR", "POP_VI")
fi <- c("VETU_PR", "GUTT_PR", "ANAL_PR", "CHRY_PR", "AURO_PR", "VIRI_PR", "VETU_STT", "GUTT_STT", "ANAL_STT", "CHRY_STT", 
            "AURO_STT", "VIRI_STT", "VETU_STX", "GUTT_STX", "ANAL_STX", "CHRY_STX", "AURO_STX", "VIRI_STX", 
        "DEN_PR", "DEN_VI", "SLPSIZ_VI")
ccl <- c("PDR_PR", "PDR_STT", "PDR_STX", "MLMAX_PR", "MLMAX_STT", "MLMAX_STX", 
         "LOB_PR", "LOB_STT", "LOB_STX", "CONC_PR", "CONC_STT", "CONC_STX", "OTHR_PR", "OTHR_STT", "OTHR_STX", 
         "GINI_R_PR", "GINI_R_STT", "GINI_R_STX", 
         "DIV_PR", "DIV_STT", "DIV_STX", "BYCAT_PR", "BYCAT_STT", "BYCAT_STX")
oth <- c("EST_PR", "EST_VI", "EMP_PR", "EMP_VI", "WAG_PR", "WAG_VI", "GDP_PR", "GDP_VI", "UNEM_PR", "UNEM_VI",
         "REC_PR", "REC_VI", "REGS", "MGMT", "TR3_PR", "TR3_VI", "OUT_PR", "OUT_VI", "CRLRCH_PR", "CRLCVR_PR", "CRLRCH_VI", "CRLCVR_VI")

d_risk <- d[, c(1, which(names(d) %in% risks))]
d_fi <- d[, c(1, which(names(d) %in% fi))]
d_ccl <- d[, c(1, which(names(d) %in% ccl))]
d_oth <- d[, c(1, which(names(d) %in% oth))]

# traffic light plots ----------------------------------

png(filename = "traffic.png", units="in", width=3.8, height=11, pointsize=12, res=72*12)

par(mar = c(1.9, 8, 0.1, 1), mfrow = c(4, 1))

trafficLightPlot(d_risk, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "risks to meeting objectives", line = 6.5, cex = 0.9)
trafficLightPlot(d_fi, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "fishery-independent indicators", line = 6.5, cex = 0.9)
trafficLightPlot(d_ccl, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "fishery-dependent indicators", line = 6.5, cex = 0.9)
trafficLightPlot(d_oth, noNAs = 1, cexlabs = 0.8)
mtext(side = 2, "other management indicators", line = 6.5, cex = 0.9)

dev.off()

# ordination plots ----------------------------

thresh <- 0.1
minyr <- 2011

png(filename = "pcas.png", units="in", width=8, height=11, pointsize=12, res=72*12)

par(mar = c(4, 6, 1, 1), mfrow = c(3, 2))

pc <- plotOrdScores(d_risk, main = "", mintime = 2011, noNAs = thresh, tim.cex = 1.2) #, method = "nmds", nmdsk = 3)
mtext(side = 2, "risks to meeting objectives", line = 4.5, cex = 0.8)
barplot(pc$rotation[,1], las = 2, horiz = T, xlim = c(-0.45, 0.45), xlab = "PCA 1 loadings")
abline(v = c(-0.2, 0.2), col = 8, lty = 2)

#pc <- plotOrdScores(d_fi, main = "fishery-independent indicators", mintime = 2011, noNAs = thresh) #, method = "nmds", nmdsk = 3)
#barplot(pc$rotation[,1], las = 2, horiz = T)
#abline(v = c(-0.2, 0.2), col = 8, lty = 2)

pc <- plotOrdScores(d_ccl, main = "", mintime = 2011, noNAs = thresh, tim.cex = 1.2) #, method = "nmds", nmdsk = 3)
mtext(side = 2, "fishery-dependent indicators", line = 4.5, cex = 0.8)
barplot(pc$rotation[,1], las = 2, horiz = T, xlim = c(-0.45, 0.45), xlab = "PCA 1 loadings")
abline(v = c(-0.2, 0.2), col = 8, lty = 2)

pc <- plotOrdScores(d_oth, main = "", mintime = 2011, noNAs = thresh, tim.cex = 1.2) #, method = "nmds", nmdsk = 3)
mtext(side = 2, "other management indicators", line = 4.5, cex = 0.8)
barplot(pc$rotation[,1], las = 2, horiz = T, xlim = c(-0.45, 0.45), xlab = "PCA 1 loadings")
abline(v = c(-0.2, 0.2), col = 8, lty = 2)

dev.off()


