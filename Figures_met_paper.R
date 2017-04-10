library(openair)
library(plyr)
library(lattice)
library(latticeExtra)

#load in data 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
load("BOM_data_updated.RData")
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
load("ANSTO_model_output.RData")
load("CMAQ_model_output.RData")

#assign variables
BOM <- bom_data_all_campaigns
models <- rbind.fill(wrf, cmaq)
site_list <- levels(as.factor(BOM$site))
species_list <- c("temp", "RH", "ws", "u10", "v10", "prcp")
species_names <- c("temperature", "RH (%)", "wind speed (m/s)", "u wind", "v wind", "precip")
date_start <- c("21/12/2012","01/02/2011", "01/04/2012") #check proper dates
date_end <- c("15/02/2013","07/03/2011","17/05/2012") #check proper dates 
stat_list <- c("r", "RMSE", "MB")

#select sites 
model_met <- subset(models, site %in% site_list)
#create site info 
site_lat <- as.numeric(levels(as.factor(model_met$site_lat))) 
site_lon <- as.numeric(levels(as.factor(model_met$site_lon)))
site <- site_list
site_info <- data.frame(site, site_lat, site_lon)

#merge obs and model output into wide format 
met <- merge(BOM, model_met, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#merge obs and model output into long format 
BOM$data_source <- "OBS"
met_ln <- rbind.fill(BOM, model_met)

#plot diurnal cycles and time series for all species in species_list 
for (i in 1:length(species_list)) {

  d <- timeVariation(met_ln, pollutant = species_list[i], group = "data_source", type = "campaign", ci = T, ylab = species_names[i])
  print(d, subset = "hour")
  
  for (j in 1:length(date_start)){
  scatterPlot(selectByDate(met_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "campaign", plot.type = "l")
  }
}
#the above shows all models, averaged across all sites, separately for each campaign 


#plot Taylor diagrams and compute stats for all species in species_list and plot google bubble plots
strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)

for (k in 1:length(species_list)){
  TaylorDiagram(met, obs = paste0(species_list[k],".obs"), mod = paste0(species_list[k],".mod"), group = "data_source", type = "campaign", main = paste0(species_names[k]))

  stats <- modStats(met, obs = paste0(species_list[k],".obs"), mod = paste0(species_list[k],".mod"), type = c("data_source","site", "campaign"))
  #merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
    for (m in 1:length(stat_list)) {
    a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                         maptype = "roadmap", col = "jet", cex = 2, main = paste(stat_list[m], "-",species_list[k]  ),
                         key.footer = "", xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
  useOuterStrips(a1$plot)
}
  #rename dataframe
  stats_name <- paste0("stats_",species_list[k])
  assign(stats_name,stats)
}



