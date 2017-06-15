#AQ analysis script 
#copy over the first part of the code from met analysis, but change the species list and species name, etc.  

library(openair)
library(plyr)
library(lattice)
library(latticeExtra)
library(stringi)

#load in data 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
#load("BOM_data_updated.RData")
load("OEH_obs.RData")

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
#load("ANSTO_model_output.RData")
load("CMAQ_model_output.RData")
load("WRFCHEM_model_output.RData")
load("CSIRO_model_output.RData")
load("OEH_model_output.RData")

OBS <- oeh_obs
models <- rbind.fill(cmaq, wrf_chem, csiro, oeh_model)
site_list <- levels(as.factor(OBS$site))
species_list_aq <- c("O3", "NOx", "PM2.5","PM10","CO")
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("21/12/2012","07/02/2011", "16/04/2012") 
date_end <- c("15/02/2013","06/03/2011","13/05/2012")  
stat_list <- c("r", "RMSE", "MB")
model_list <- c("CMAQ", "CSIRO", "OEH", "WRF-Chem")

#select sites 
model_aq <- subset(models, site %in% site_list)

#merge obs and model output into wide format 
aq <- merge(OBS, model_aq, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#merge obs and model output into long format 
OBS$data_source <- "OBS"
aq_ln <- rbind.fill(OBS, model_aq)

#diurnal cycles 
for (i in 1:length(species_list_aq)) {
  d <- timeVariation(subset(aq_ln, site %in% "Wollongong"), pollutant = species_list_aq[i], group = "data_source", type = "campaign", ci = F, ylab = species_list_aq[i], key.columns = 2)
 # setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
#  png(filename = paste(species_list_aq[i],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  print(d, subset = "hour")
#  dev.off()
}
#look at timeseries
for (i in 1:length(species_list_aq)) {
  for (j in 1:length(date_start)){
    png(filename = paste(species_list_aq[i],campaign[j],"timeseries.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
    scatterPlot(selectByDate(aq_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list_aq[i], ylab = species_list_aq[i], group = "data_source", type = "campaign", plot.type = "l")#, main = campaign[j])
    dev.off()
  }
}








############these were quick checks - to be removed 
stats_ozone <- modStats(aq, obs = "O3.obs", mod = "O3.mod", type = c("data_source.mod","site", "campaign"))
stats_ozone_overall <- modStats(aq, obs = "O3.obs", mod = "O3.mod", type = c("data_source.mod", "campaign"))
stats_nox_overall <- modStats(aq, obs = "NOx.obs", mod = "NOx.mod", type = c("data_source.mod", "campaign"))

#diurnal cycles at Westmead
for (i in 1:length(species_list_aq)) {
  d <- timeVariation(subset(aq_ln, site %in% "Westmead"), pollutant = species_list_aq[i], group = "data_source", type = "campaign", ci = F, ylab = species_list_aq[i], key.columns = 2)
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
  png(filename = paste(species_list_aq[i],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  print(d, subset = "hour")
  dev.off()
}

setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis")
write.csv(stats_nox_overall, file = "stats_NOx_overall.csv", row.names = F)
write.csv(stats_ozone_overall, file = "stats_O3_overall.csv", row.names = F)
write.csv(stats_ozone, file ="stats_ozone_by_site.csv", row.names = F)
