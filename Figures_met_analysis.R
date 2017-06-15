#This is very similar to Figures_ANSTO_analysis, but in this one, I add all models for met evaluation
#myColours <- c("#1B9E77", "#386CB0", "#666666","#FFB843", "#F42E3C", "#520066") #that purple is hard to distinguish from black 
myColours <- c("#1B9E77", "#386CB0", "#000000","#FFB843", "#F42E3C", "#7570B3")
myColours <- c("#1B9E77", "#386CB0", "#000000","#FF7F00", "#F42E3C", "#7570B3") #trying a more vibrant orange (less yellow)

mylineTypes <- c("dashed","dotted","solid","dotdash","longdash","twodash")
mylineWidths <- c(2,2,3,2,2,2)

library(openair)
library(plyr)
library(lattice)
library(latticeExtra)
library(stringi)

#load in data 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
load("BOM_data_updated.RData")
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
load("ANSTO_model_output.RData")
load("CMAQ_model_output.RData")
load("WRFCHEM_model_output.RData")
load("CSIRO_model_output.RData")
load("OEH_model_output.RData")
load("site_info.RData")

#only include wrf-11 (issue with wrf-10)
levels(as.factor(wrf$data_source))
wrf <- subset(wrf, data_source == "WRF_11")

#assign variables
BOM <- bom_data_all_campaigns
models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model)
site_list <- levels(as.factor(BOM$site))
species_list <- c("temp", "RH", "ws","wd","u10", "v10", "prcp", "pblh", "SWR")
species_list_2 <- c("temp", "RH", "ws","wd", "u10", "v10", "prcp") #, "pblh")
species_names <- c("temperature", "RH (%)", "wind speed (m/s)", "wind direction",  "u wind", "v wind", "precip", "pblh", "SWR")
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("21/12/2012","07/02/2011", "16/04/2012") 
date_end <- c("15/02/2013","06/03/2011","13/05/2012")  
stat_list <- c("r", "RMSE", "MB")
#model_list <- c("CMAQ", "WRF_10", "WRF_11", "WRF-Chem", "CSIRO", "OEH")
model_list <- c("CMAQ", "WRF_11", "WRF-Chem", "CSIRO", "OEH")

#select sites 
model_met <- subset(models, site %in% site_list)

#create site info 
#site_info <- data.frame(site = model_met$site, site_lat = model_met$site_lat, site_lon = model_met$site_lon)
#site_info <- unique(site_info)

#merge obs and model output into wide format 
met <- merge(BOM, model_met, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#merge obs and model output into long format 
BOM$data_source <- "OBS"
met_ln <- rbind.fill(BOM, model_met)

#plot diurnal cycles and time series for all species in species_list 
for (i in 1:length(species_list)) {

  d <- timeVariation(met_ln, pollutant = species_list[1], group = "data_source", type = "campaign", ci = F, ylab = species_names[1], key.columns = 3, col = myColours, lty = mylineTypes, lwd = mylineWidths)
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
  
  d <- timeVariation(subset(met_ln, site %in% site_list[k]), pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 2, main = site_list[k])
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
  
  d <- timeVariation(met_ln, pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 2, statistic = "median", conf.int = 0.75)
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list[i],"median_diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  print(d, subset = "hour")
  dev.off()
}


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

#make more stats 

#overall stats, hourly values 
for (k in 1:length(species_list_2)){
stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign"))

#save stats and rename dataframe
stats_name <- paste0("stats_dom_avg_",species_list[k])
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
assign(stats_name,stats)
}
#overall stats, daily averages (or total, for precipitation)
met_daily <- timeAverage(met, avg.time = "1 day", type = c("data_source","site","campaign"))
met_daily_sum <- timeAverage(met, avg.time = "1 day", statistic = "sum", type = c("data_source", "site","campaign"))
met_daily$prcp.mod <- met_daily_sum$prcp.mod
met_daily$prcp.obs <- met_daily_sum$prcp.obs
for (k in 1:length(species_list_2)){
  stats <- modStats(met_daily, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign"))
  
  #save stats and rename dataframe
  stats_name <- paste0("daily_stats_dom_avg_",species_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}

for (k in 1:length(species_list_2)){
  stats <- modStats(met_daily, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source","site", "campaign"))
  
  #save stats and rename dataframe
  stats_name <- paste0("daily_stats_",species_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}
#It looks like daily is better for prcp than hourly - but what about campaign totals
sums <- ddply(met_ln, .(site, campaign, data_source), numcolwise(sum), na.rm = TRUE)
total_prcp <- subset(sums, select = c("site", "campaign", "data_source", "prcp"))
#try to plot this in a meaningful manner

b1 <- barchart(total_prcp$prcp ~ total_prcp$data_source|total_prcp$site * total_prcp$campaign,
         col = c("blue", "grey", "purple", "red"), ylab = "Total precipitaion (mm)", 
         strip.left = strip.custom(style=1, horizontal = F),
         par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
png(filename = "Total_precipitation.png", width = 12 * 300, height = 9 * 300, res = 300)
print(useOuterStrips(b1))
dev.off()
#need to fix colours 

#make google maps of mean bias, etc for total precipitation 
total_prcp_obs <- subset(total_prcp, data_source %in% "OBS")
total_prcp_models <- subset(total_prcp, data_source != "OBS")
total_prcp_wide <- merge(total_prcp_obs, total_prcp_models, by = c("site", "campaign"), suffixes = c(".obs", ".mod", all = T))

stats <- modStats(total_prcp_wide, mod = "prcp.mod", obs = "prcp.obs", type = c("data_source.mod","site", "campaign"))
stats <- merge(stats, site_info, by = "site")
stat_list_2 <- c("MB", "NMB")
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)
for (m in 1:length(stat_list_2)) {
  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list_2[m],
                       maptype = "roadmap", col = "jet", cex = 1, main = paste("Total precipitation", "-", stat_list_2[m]),
                       key.footer = stat_list_2[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source.mod"))
  png(filename = paste("Total_precipitation", stat_list_2[m],"map.png", sep = '_'), width = 6 * 300, height = 6 * 300, res = 300)
  print(useOuterStrips(a1$plot))
  dev.off()
}
#save stats as data.frame
stats_total_prcp <- stats

#as I prepared plots for ANSTO comparison, I thought of this: 

d1 <- densityplot(~wd|site * campaign, data = met_ln,
                  groups = data_source,
                  plot.points=FALSE,
                  auto.key = T, 
                  strip.left = strip.custom(style=1, horizontal = F),
                  par.settings = list(superpose.line = list(col = "increment", lty = 2)),
                  from=0,to=360)
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
png(filename = "wd_densities.png", width = 12 * 300, height = 9 * 300, res = 300)
print(useOuterStrips(d1))
dev.off()
#need to fix colours 

#this is to plot the binned MB versus observations 
#I still don't like the legend (I want three columns), and that the symbols are all open circles (hard to read in b/w)
#I also need to fix the x axis labelling 
species_col <- c(4,7)
obs_species <- c("temp.obs", "ws.obs")
mod_species <- c("temp.mod","ws.mod")
for (t in 1:length(species_col)) {
  x_1 <- floor((max(met[,species_col[t]], na.rm = T) - min(met[,species_col[t]], na.rm = T))/10)
  x_max <- ceiling(max(met[,species_col[t]], na.rm = T)/x_1) * x_1
  x_breaks <- c(0, seq(x_1, x_max, by = x_1))
 # a <- histogram(~ met[,species_col[t]]|campaign, data = met, #endpoints = c(0,ceiling(max(met[,species_col[t]], na.rm = T))),
 #             col = "grey90", xlab = names(met[species_col[t]]),
 #              breaks = x_breaks,
 #             scales = list(x = list(at = x_breaks)))
  c <- densityplot(~ met[,species_col[t]]|campaign, data = met, plot.points=FALSE, col = "grey", from = 0, to = x_max, xlab = names(met[species_col[t]]))
   
  met$bin <- cut(met[,species_col[t]], breaks = x_breaks, labels = (seq(0, (x_max-x_1), by = x_1)))
  stats_test <- modStats(met, obs = obs_species[t], mod = mod_species[t], type = c("data_source", "campaign", "bin"))
  stats_test$bin <- as.numeric(stats_test$bin)*x_1
  b <- xyplot(MB ~ bin|campaign, data = stats_test, groups = data_source, 
              xlab = names(met[species_col[t]]), auto.key = T, 
              panel =function(...){  
                panel.xyplot(...);
                panel.abline(h = 0, col = "blue", lty = 2)
              })
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(names(met[species_col[t]]),"mb_by_bin.png", sep = "_"), width = 9 * 300, height = 5 * 300, res = 300)
  print(doubleYScale(c, b, use.style = F, add.ylab2 = T))
  dev.off()
}
#making make a plot for every site? 

#investigate Q-Q plots (Alan's suggestion)
for (k in 1:length(species_list_2)) {
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list_2[k],"quantiles.png", sep = '_'), width = 6 * 300, height = 6 * 300, res = 300)
  conditionalQuantile(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("campaign", "data_source"), main = species_list_2[k])
  dev.off()
}
#these are not bad - include them in plots to send modellers 








#next, figure out how to make quartiles, and plot for those... you saw that in a model evaluation paper 
#actually, they binned their data, and ran stats on those 

bin_temp <- cut(met$temp.obs, breaks = c(0, seq(5, 50, by = 5)), labels = 1:10)
hist(as.numeric(bin_temp))
a <- hist(met$temp.obs, xlab = "observed temperatures", main = "")

a[["panel.args.common"]]$breaks

summary(met$temp.obs)
summary(bin_temp)

summary(met$ws.obs)
bin_ws <- cut(met$ws.obs, breaks = c(-2, seq(2, 20, by = 2)), labels = 1:10)
summary(bin_ws)

#so this should work to bin the data - will need different bins for different parameters 
#then, apply modStats, adding bin to type - maybe removing site - 
met$bin_ws <- cut(met$ws.obs, breaks = c(-2, seq(2, 20, by = 2)), labels = 1:10)
stats_test <- modStats(met, obs = "ws.obs", mod = "ws.mod", type = c("data_source", "campaign", "bin_ws"))
scatterPlot(stats_test, x = "bin_ws", y = "MB", group = "data_source", type = "campaign", pch = 16)
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
e<-boxplot(temp ~data_source, data = met_ln, outline = T, range = 0.72)
e<-boxplot(temp ~data_source, data = met_ln, outline = T)#, range = 0.72)

a <- histogram(~ws.obs|campaign, data = met, col = "grey", xlab = "observed wind speeds", xlim = c(0,NA))
a[["panel.args.common"]]$breaks

hist(subset(met, campaign %in% "MUMBA"), met$ws.obs)#, breaks = a[["panel.args.common"]]$breaks)
b <- hist(met$ws.obs, add = T, col = "grey90")
b$breaks


stats_test <- modStats(met, obs = "ws.obs", mod = "ws.mod", type = c("data_source", "campaign", "bin_ws"))

xyplot()