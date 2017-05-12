#work in progress - I the moment this only includes wrf and cmaq - send that analysis to Alan (and Jeremy)

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
species_list <- c("temp", "RH", "ws","wd", "u10", "v10", "prcp", "pblh", "SWR")
species_list_2 <- c("temp", "RH", "ws","wd", "u10", "v10", "prcp") #, "pblh")
species_names <- c("temperature", "RH (%)", "wind speed (m/s)", "wind direction",  "u wind", "v wind", "precip", "pblh", "SWR")
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("21/12/2012","07/02/2011", "16/04/2012") 
date_end <- c("15/02/2013","06/03/2011","13/05/2012")  
stat_list <- c("r", "RMSE", "MB")
model_list <- c("CMAQ", "WRF_10", "WRF_11")

#select sites 
model_met <- subset(models, site %in% site_list)
#create site info 
site_info <- data.frame(site = model_met$site, site_lat = model_met$site_lat, site_lon = model_met$site_lon)
site_info <- unique(site_info)

#merge obs and model output into wide format 
met <- merge(BOM, model_met, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#merge obs and model output into long format 
BOM$data_source <- "OBS"
met_ln <- rbind.fill(BOM, model_met)

#plot diurnal cycles and time series for all species in species_list 
for (i in 1:length(species_list)) {

  d <- timeVariation(met_ln, pollutant = species_list[i], group = "data_source", type = "campaign", ci = T, ylab = species_names[i], key.columns = 2)
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list[i],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  print(d, subset = "hour")
  dev.off()
}
for (i in 1:length(species_list)) {
  for (j in 1:length(date_start)){
  png(filename = paste(species_list[i],campaign[j],"timeseries.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  scatterPlot(selectByDate(met_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "campaign", plot.type = "l")#, main = campaign[j])
  dev.off()
  }
}
#the above shows all models, averaged across all sites, separately for each campaign 
#need the same plots, for each site... 
for (k in 1:length(site_list)) {
for (i in 1:length(species_list)) {
  
  d <- timeVariation(subset(met_ln, site %in% site_list[k]), pollutant = species_list[i], group = "data_source", type = "campaign", ci = T, ylab = species_names[i], key.columns = 2, main = site_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list[i],site_list[k],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  print(d, subset = "hour")
  trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
  panel.text(0.15, 0.9, site_list[k], cex = 1, font = 1)
  trellis.unfocus()
  dev.off()
}
}
for (i in 1:length(species_list)) {
   
  for (j in 1:length(date_start)){
    png(filename = paste(species_list[i],campaign[j],"timeseries_by_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(met_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "site", plot.type = "l", main = campaign[j])
    dev.off()
  }
}
#plot median instead of mean in overall diurnal plots
for (i in 1:length(species_list)) {
  
  d <- timeVariation(met_ln, pollutant = species_list[i], group = "data_source", type = "campaign", ci = T, ylab = species_names[i], key.columns = 2, statistic = "median")
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list[i],"median_diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  print(d, subset = "hour")
  dev.off()
}
#looks ugly as is - may have to work on it 

#plot Taylor diagrams and compute stats for all species in species_list and plot google bubble plots
strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)

for (k in 1:length(species_list_2)){
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
   png(filename = paste(species_list_2[k],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
   TaylorDiagram(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = "data_source", type = "campaign", main = paste0(species_names[k]))
  dev.off()
  
  png(filename = paste(species_list_2[k],"Taylor_by_model.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  TaylorDiagram(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = "data_source", group = "campaign", main = paste0(species_names[k]))
  dev.off()
    
  stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source","site", "campaign"))
  #merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
    for (m in 1:length(stat_list)) {
  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                         maptype = "roadmap", col = "jet", cex = 1, main = paste(species_list[k] , "-", stat_list[m] ),
                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
  png(filename = paste(species_list[k], stat_list[m],"map.png", sep = '_'), width = 6 * 300, height = 6 * 300, res = 300)
  print(useOuterStrips(a1$plot))
  dev.off()
}
  #save stats and rename dataframe
  stats_name <- paste0("stats_",species_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}


#make the same Taylor plots, but for each site... 
for (j in 1:length(site_list)) {
for (k in 1:length(species_list_2)){
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list_2[k],site_list[j],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  TaylorDiagram(subset(met, site %in% site_list[j]), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = "data_source", type = "campaign", main = paste0(species_names[k], "-", site_list[j]))
  dev.off()
  
  png(filename = paste(species_list[k],site_list[j],"Taylor_by_model.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  TaylorDiagram(subset(met, site %in% site_list[j]), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = "data_source", group = "campaign", main = paste0(species_names[k], "-", site_list[j]))
  dev.off()
}
}

#make Taylor plots for each model, showing all sites for each campaign 
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
for (m in 1:length(model_list)) {
  for (k in 1:length(species_list_2)){
    png(filename = paste(species_list_2[k],model_list[m],"Taylor_by_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    TaylorDiagram(subset(met, data_source %in% model_list[m]), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"),  group = "site" ,type = "campaign", main = paste0(model_list[m], "-", species_list_2[k]))
  dev.off()
    }
}

#make more stats - overall (already have by site for each campaign)
#also make daily values by site - averages for everything except precipitation - see BOM code for that 
#then get stats for daily values, by site and overall 
#also make sum for entire campaigns, by site, for precipitation and run stats 


#next, figure out how to make quartiles, and plot for those... you saw that in a model evaluation paper 
#actually, they binned their data, and ran stats on those 

bin_temp <- cut(met$temp.obs, breaks = c(0, seq(5, 50, by = 5)), labels = 1:10)
hist(as.numeric(bin_temp))
a <- hist(met$temp.obs)

summary(met$temp.obs)
summary(bin_temp)

summary(met$ws.obs)
bin_ws <- cut(met$ws.obs, breaks = c(-2, seq(2, 20, by = 2)), labels = 1:10)
summary(bin_ws)

#so this should work to bin the data - will need different bins for different parameters 
#then, apply modStats, adding bin to type - maybe removing site - 
met$bin_ws <- cut(met$ws.obs, breaks = c(-2, seq(2, 20, by = 2)), labels = 1:10)
stats_test <- modStats(met, obs = "ws.obs", mod = "ws.mod", type = c("data_source", "campaign", "bin_ws"))

scatterPlot(stats_test, x = "bin_ws", y = "MB", type = "data_source", group = "campaign")
#it's a bit ugly but it works... need better labels...
#maybe add density of data to the plots - would require making my own xy plot - feasible but a pain 

met$bin_temp <- cut(met$temp.obs, breaks = c(0, seq(5, 50, by = 5)), labels = 1:10)
stats_test_temp <- modStats(met, obs = "temp.obs", mod = "temp.mod", type = c("data_source", "campaign", "bin_temp"))

scatterPlot(stats_test_temp, x = "bin_temp", y = "MB", type = "data_source", group = "campaign")
#or could I make the size of the dots proportional to the number of data points in the bin? 
#maybe add ref lines to indicate the range of MB we are aiming for (+-1?)

b<-boxplot(ws ~data_source, data = met_ln, outline = T, range = 0.72)
c <- b$stats
bxp(b, outline = F)
summary(cmaq$ws)
quantile(cmaq$ws, 0.21)
IQR(cmaq$ws, na.rm = T)

#doesn't work so well for ws, as most values are low (0-4): try with temp 
d<-boxplot(temp ~data_source, data = met_ln, outline = T, range = 0.72)
d<-boxplot(temp ~data_source, data = met_ln, outline = T)#, range = 0.72)
