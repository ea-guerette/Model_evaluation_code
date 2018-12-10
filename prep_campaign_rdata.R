#this script is to read in the data I need at the campaign sites for the AQ paper 

#for each campaign, I will need the AQ data:
# Westmead is easy, it is part of oeh_obs - just select the site 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
load(paste0(dir_obs,"OEH_obs.RData"))

Westmead_AQ <- subset(oeh_obs, site %in% "Westmead")

#need HCHO and also maybe  C5H8, Toluene, Terpenes - read in the PTRMS files for each campaign 
library(openair)
library(plyr)
## PTRMS data 
setwd("C:/Documents and Settings/eag873/ownCloud/Sydney Particle Study 1")
ptrms_sps1 <- import(file = "SPS1_PTRMS.csv", header.at = 1, data.at = 4, date = "Timestamp", na.strings = "<mdl", 
                     date.format = "%d/%m/%Y %H:%M", tz = "Etc/GMT-11") #is this right? 

##select only the masses you are interested in 
names(ptrms_sps1)
ptrms_sps1 <- ptrms_sps1[,c(1,2,12,13,16,17,20,22,23,24)]
names(ptrms_sps1)[c(2,3,4,5,6,7,8,9,10)] <- c("HCHO", "C5H8","IsopRxnProd", "Benzene", "Terpenes",
                                                   "Toluene", "Xylenes","TMB", "Terpenes2") 
ptrms_sps1$site <- "Westmead"
#ptrms_sps1$data_source <- "OBS"
ptrms_sps1$campaign <- "SPS1"
#this data is so full of gaps, I can't make a diurnal

#SPS2
setwd("C:/Documents and Settings/eag873/ownCloud/Sydney Particle Study 2")
ptrms_sps2 <- import(file = "SPS2_PTRMS.csv", header.at = 1, data.at = 4, date = "Timestamp", na.strings = "<mdl", 
                     date.format = "%d/%m/%Y %H:%M", tz = "Etc/GMT-10")

##select only the masses you are interested in 
names(ptrms_sps2)
ptrms_sps2 <- ptrms_sps2[,c(1,3,19,20,22,24,29,35,42,48)]
names(ptrms_sps2)[c(2,3,4,5,6,7,8,9,10)] <- c("HCHO", "C5H8","IsopRxnProd", "Benzene", "Terpenes",
                                              "Toluene", "Xylenes","TMB", "Terpenes2") 

ptrms_sps2$site <- "Westmead"
#ptrms_sps1$data_source <- "OBS"
ptrms_sps2$campaign <- "SPS2"

#combine, then merge with AQ 
ptrms <- rbind(ptrms_sps1,ptrms_sps2)

Westmead <- merge(Westmead_AQ, ptrms, by = c("date", "site", "campaign"), all = T)

#also, acid alkaline gases, but this is for PM2.5 composition section 

#for MUMBA, I need to read this in - I need O3, NOx, FTIR?, met  - the hourly set used in Clare's paper might do

setwd("C:/Documents and Settings/eag873/My Documents/R_MUMBA/Data")
mumba.data <- import("MUMBA_hourly_data.csv",date.format = "%Y-%m-%d %H:%M:%S", tzone = "Etc/GMT-10")
#select only the variables you are interested in 
names(mumba.data)
mumba.data <- mumba.data[,c(1,3,4,5,6,7,12:16,18,26,28:34)]
#rename so it matches the models 
names(mumba.data)[c(3, 6:20)] <- c("RH", "W", "CO", "O3", "NO", "NO2",
                  "NOx", "HCHO", "IsopRxnProd", "Benzene", "Terpenes", "Terpenes2", "Toluene", "Xylenes", "TMB", "C5H8")

mumba.data$site <- "MUMBA"
mumba.data$campaign <- "MUMBA"
#mumba.data$data_source <- "OBS"

#combine with Westmead 

campaign_data <- rbind.fill(Westmead, mumba.data)

setwd(dir_obs)
save(campaign_data, file = "campaign_data.RData")

#I also need the MUMBA PM2.5, but this is to go with the Hi-Vol analysis 



#Some tests 
#remove the low O3 and NOx values (keep >0.1 ppb ) 
test <- campaign_data
test$O3[test$O3 < 0.1] <- NA
test$NOx[test$NOx < 0.1] <- NA
test <- within(test, ratio1 <- O3 / NOx)
timeVariation(test, pollutant = "ratio1", type = "campaign")
test <- within(test, ratio2 <- HCHO / NO2)
timeVariation(test, pollutant = "ratio2", type = "campaign")
timePlot(subset(test, campaign %in% "MUMBA"), pollutant = "ratio1")
timePlot(subset(test, campaign %in% "SPS1"), pollutant = "ratio1")
timePlot(subset(test, campaign %in% "SPS2"), pollutant = "ratio1")
#there are too few data points for HCHO during SPS1
scatterPlot(selectByDate(test, hour= c(8:18)), x = "ratio1", y = "ratio2", type = "campaign")
# I would like to select thresholds for these ratios that make sense 
#I'd say 15 and 1 covers most of the data in autumn, but these values maybe don't look so appropriate for summer 
#Maybe 1.5 and 20? I don't think it matters - the data isn't great, not sure weather to show it or not? 

