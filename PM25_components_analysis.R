#compare to model - try yang first? 

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/")
load("hivol_obs.Rdata")

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
load("YZ.RData")

library(openair)
library(plyr)

modsps1 <- rbind.fill(ROMS_SPS1, WRFCHEM_SPS1)

westmead <- subset(modsps1, site %in% "Westmead")

westmeadAM <- selectByDate(westmead, hour = 5:10)
westmeadAM <- timeAverage(westmeadAM, avg.time = "day", type = c("campaign", "data_source") )
westmeadAM$date <- as.POSIXct(paste(westmeadAM$date, "5:00"))
westmeadAM$TOD <- "AM"

westmeadPM <- selectByDate(westmead, hour = 11:18)
westmeadPM <- timeAverage(westmeadPM, avg.time = "day", type = c("campaign", "data_source"))
westmeadPM$date <- as.POSIXct(paste(westmeadPM$date, "11:00"))
westmeadPM$TOD <- "PM"

westmead_mod <- rbind(westmeadAM, westmeadPM)

#########

timePlot(westmead_mod, pollutant = "SO4", type = "data_source")
sps1_hivol <- subset(hivol_obs, campaign == "SPS1")

sps1_hivol$data_source <- "OBS"


#combine mods and obs 
sps1comp <- rbind.fill(westmead_mod, sps1_hivol)

scatterPlot(sps1comp, x = "date", y = "SO4", group = "data_source", plot.type = "l")
scatterPlot(sps1comp, x = "date", y = "NH4", group = "data_source", plot.type = "l")
scatterPlot(sps1comp, x = "date", y = "NO3", group = "data_source", plot.type = "l")

scatterPlot(sps1comp, x = "date", y = "EC", group = "data_source", plot.type = "l")
scatterPlot(sps1comp, x = "date", y = "OC", group = "data_source", plot.type = "l")

#everything too low - I guess YZ PM2.5 is too low? 

boxplot(sps1comp$SO4 ~ sps1comp$data_source, main = "SO4" )

#OK, so this seems to work - sps1 only - will have to modify to 
library(lattice)
bwplot(sps1comp$SO4 ~ sps1comp$data_source|sps1comp$TOD, main = "SPS1" )
bwplot(sps1comp$SO4 ~ sps1comp$data_source, main = "SPS1" )

#need to calc stats as well 

#will need to cut the models so they match with the obs 
#assign variables to select correct periods 
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("2013-01-01","2011-02-07", "2012-04-16")
date_end <- c("2013-02-15 23:00:00","2011-03-06 23:00:00","2012-05-13 23:00:00") 
