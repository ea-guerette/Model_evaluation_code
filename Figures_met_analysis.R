#This is a cleaned up version of the code, hopefully moving towards what we want in the paper 

library(openair)
library(plyr)
library(lattice)
library(latticeExtra)
library(stringi)

#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
#dir_stat_output <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis/"
#dir_stat_output <- "C:/Users/eag873/ownCloud/Figures_and_stats_met_paper/stats - 2018-05-14/"
dir_figures <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/"

#load in met observations from BOM  
load(paste0(dir_obs,"/BOM_data_updated3.RData"))

#load in original model data
load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_new_new_fixed.RData"))
load(paste0(dir_mod,"/OEH_model_output.RData"))
load(paste0(dir_mod, "/YZ.RData"))
#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

#assign variables
BOM <- bom_data_all_campaigns
site_list <- levels(as.factor(BOM$site)) #to select only BOM sites 
species_list <- c("temp", "W", "ws","wd","u10", "v10", "RH", "prcp", "pblh", "SWR", "pres") #variable we are interested in 
param_list <- c("date", "site", "campaign", "data_source", species_list)  #complete list of things to keep from model runs 
species_names <- c(expression("temperature (" * degree * "C)"),  "water mixing ratio (g/kg)", "wind speed (m/s)", expression("wind direction (" * degree *")"),  "u wind", "v wind","RH (%)", "precipitation (mm)", "pblh (m)", "SWR", "pressure (hPa)")
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

#model_list <- c("CMAQ", "WRF_10", "WRF_11", "WRF-Chem", "CSIRO", "OEH") #not sure why it is in this order???
#model_list <- c("CMAQ", "WRF_11", "WRF-Chem", "CSIRO", "OEH") #not used as far as I can tell 
species_list_2 <- c("temp", "W", "ws","wd", "u10", "v10") #reduced list of variables -to plot (order matters, need to match labels in species_names)
stat_list <- c("MB", "r","RMSE", "MGE", "IOA") #list of stats for plotting 
stat_list2 <- c("MB", "RMSE", "MGE")
#trim original model data 
#only include wrf-11 (issue with wrf-10)
#levels(as.factor(wrf$data_source))
#wrf <- subset(wrf, data_source == "W-A11")

#combine all original model data 
models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model, yz_mod)
#select model data for BOM sites only 
model_met <- subset(models, site %in% site_list)


#cut to length - some model runs were longer than the actual campaigns 
mumba_mod <- subset(model_met, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
sps1_mod <- subset(model_met, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(model_met, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
model_met <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 

#select variables that are of interest only 
#indices <- match(species_list, names(model_met)) #not for here - for later 
model_met <- model_met[,param_list]

##prepare dfs for analysis 
#merge obs and model output into wide format 
met <- merge(BOM, model_met, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)

#insert all stats and overall Taylor diagrams here 
library(reshape2)
library(plyr)

melted_BOM <- melt(BOM, id = c("date", "site", "campaign"), value.name = "obs")
melted_model_met <- melt(model_met, id = c("date", "site", "campaign", "data_source"), value.name = "mod")
melted <- merge(melted_BOM, melted_model_met, by = c("date", "site", "campaign", "variable"))

source(paste0(dir_code, "/mod_TaylorDiagram.R"))

setwd(dir_figures)
png(filename = "Taylor_all.png", width = 8 * 300, height = 12 * 300, res = 300)
mod_TaylorDiagram(subset(melted, variable != "prcp" & variable != "wd" ), obs = "obs", mod = "mod", normalise = T, 
              group = "variable", type = c("campaign", "data_source"), cex = 0.95, 
              annotate = "", rms.col = "grey40")
dev.off()

###

##analysis 
#I made a series of similar functions to output stats 
source(paste0(dir_code,"/makeStats_functions.R"))
#for plotting 
source(paste0(dir_code,"/lattice_plot_settings.R"))
#this defines myColours, myColours2 (no obs), mylineTypes, mylineWidths, my.settings 

for (k in 1:length(species_list_2)){   
  stats_name <- paste0("stats_",species_list[k])
  stats <- makeStats1(met, species_list_2[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(met, species_list_2[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(met, species_list_2[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_per_campaign_per_site.csv"), row.names =F)
  }   

met_daily <- timeAverage(met, avg.time = "1 day", type = c("site", "campaign", "data_source"))

for (k in 1:length(species_list_2)){   
  stats_name <- paste0("stats_",species_list[k])
  stats <- makeStats1(met_daily, species_list_2[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_", stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(met_daily, species_list_2[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_",stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(met_daily, species_list_2[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_" ,stats_name, "_per_campaign_per_site.csv"), row.names =F)
}  


#make summary plots for the stats  
require(latticeExtra)
setwd(paste0(dir_figures, "/visual_stats/"))
#species_col <- match(stat_list, names(stats2))

for (k in 1:length(species_list_2)){  
stats_hr <- makeStats2(met, species_list_2[k])
stats_hr$avg <- "hourly"
melted_stats_hr <- melt(stats_hr, id = c("data_source","campaign", "avg"), value.name = "value")
melted_stats_hr <- subset(melted_stats_hr, variable %in% stat_list)

stats_d <- makeStats2(met_daily, species_list_2[k])
stats_d$avg <- "daily"
melted_stats_d <- melt(stats_d, id = c("data_source","campaign", "avg"), value.name = "value")
melted_stats_d <- subset(melted_stats_d, variable %in% stat_list)

stats <- rbind(melted_stats_hr,melted_stats_d)
#melted_stats <- merge(melted_stats_hr, melted_stats_d, by = c("data_source", "campaign", "variable", "avg"))
e<- xyplot(value ~data_source|variable + campaign, data = stats, groups = avg,
           strip.left = T,
           scales = list(y = list(relation = "free"), x = list(alternating = 1, rot = c(40,0))),
           par.settings = my.settings,
           main = species_list_2[k],
           auto.key = T
           ) 
png(filename = paste0(species_list_2[k], "_stats_by_campaign.png"), width = 10*300, height = 8*300, res = 300) #this is the one we could try for Khalia's vertical profiles 
print(useOuterStrips(e),strip = mystrip, strip.left = mystrip) 
dev.off()

}

for (k in 1:length(species_list_2)){  
  for (i in 2: length(stat_list)) {
stats_hr <- makeStats3(met, species_list_2[k])
stats_hr$avg <- "hourly"
stats_d <- makeStats3(met_daily, species_list_2[k])
stats_d$avg <- "daily"
stats2 <- rbind(stats_hr,stats_d)
species_col <- match(stat_list[i], names(stats2))
d<- xyplot(stats2[,species_col] ~site|data_source + campaign, data = stats2, groups =  avg,  auto.key= T,
           par.settings = my.settings, ylab = stat_list[i],
           scales = list(x = list(alternating = 1, rot = c(40,0))),
           strip.left = T,
           type=c("p","g"),
           main = species_list_2[k])
png(filename = paste0(species_list_2[k],"_", stat_list[i], "_by_site.png"), width = 15*300, height = 10*300, res = 300)
print(useOuterStrips(d), strip = mystrip, strip.left = mystrip)
dev.off()
 }
}



#to check bias, etc. by time of day 

met$HOD <- as.factor(format(met$date, "%H"))

setwd(paste0(dir_figures,"/stats_by_HOD"))

for (k in 1:length(species_list_2)) {

stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("campaign", "data_source", "HOD"))


 for (i in 1: length(stat_list)){
 species_col <- match(stat_list[i], names(stats))
 c <- xyplot(stats[,species_col] ~HOD|campaign, data = stats, groups = data_source, type = "l", 
             scales = list(x = list(alternating = 1, at = c(0,3,6,9,12,15,18,21))), 
             ylab = stat_list[i], xlab = "HOD", main = species_list_2[k],
             par.settings = my.settings,
             auto.key = list(column = 1, space = "right", points = F, lines = T),
             layout = c(3,1))
 png(filename = paste0(species_list_2[k], "_", stat_list[i], "_by_HOD.png"), width = 8 *300, 4*300, res = 300)
 print(c)
       
dev.off()
  }
}



#merge obs and model output into long format 
BOM$data_source <- "OBS"
met_ln <- rbind.fill(BOM, model_met)

#make quantile plots
setwd(paste0(dir_figures,"/quantile_plots"))
model_list <- levels(as.factor(met$data_source))

for (i in 1:length(species_list_2)){ 
species_col <- match(species_list_2[i], names(met_ln))

for (k in 1:length(model_list)) {
  d <- qq(data_source ~met_ln[,species_col]|campaign, data = met_ln, subset = (data_source == "OBS"| data_source == model_list[k]), 
          layout = c(3,1), scales = list(x = list(alternating = 1)), strip = mystrip, main = species_list_2[i], col = myColours[k])  
  
png(filename = paste0(model_list[k], "_",species_list_2[i], "_quantile_plot.png"), width = 6 *300, height = 4*300, res = 300)
  print(d)
 dev.off()
}
} 


#make bubble plots 
setwd(paste0(dir_figures,"/bubble_plots"))
strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)

for (k in 1:length(species_list_2)) {
stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign", "site"))
#merge stats with site info... (lost when applying modStats)
stats <- merge(stats, site_info, by = "site")

for (m in 1:length(stat_list)) {
  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                       maptype = "roadmap", col = "jet", cex = 1, main = paste(species_list[k] , "-", stat_list[m] ),
                       key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
  png(filename = paste(species_list[k], stat_list[m],"map.png", sep = '_'), width = 6 * 300, height = 8 * 300, res = 300)
  print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
  dev.off()
}
}


#####################above has been tidied up, not below 



#plot diurnal cycles and time series for all species in species_list
setwd(dir_figures)
for (i in 1:length(species_list)) {
  d <- timeVariation(met_ln, pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 3,  col = myColours, lty = mylineTypes, lwd = mylineWidths)
  png(filename = paste0(dir_figures,species_list[i],"_diurnal.png"), width = 6 * 300, height = 4 * 300, res = 300)
  trellis.par.set(my.settings)
  print(d, subset = "hour")
  dev.off()
} 
trellis.par.set(original.settings)
 
#plot diurnal cycles and time series for all species in species_list (exclude Bellambi)
for (i in 1:length(species_list)) {
  d <- timeVariation(subset(met_ln, site != "Bellambi"), pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 3,  col = myColours, lty = mylineTypes, lwd = mylineWidths)
  png(filename = paste0(dir_figures,species_list[i],"_diurnal.png"), width = 6 * 300, height = 4 * 300, res = 300)
  trellis.par.set(my.settings)
  print(d, subset = "hour")
  dev.off()
} 
trellis.par.set(original.settings)

#plot diurnal difference for all species in species_list (exclude Bellambi) DOES NOT WORK with grouping variable 
#for (i in 1:length(species_list)) {
#  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
#  d <- timeVariation(subset(met, site != "Bellambi"), pollutant = c(paste0(species_list[i], ".obs"), paste0(species_list[i], ".mod")), group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 3,  col = myColours, lty = mylineTypes, lwd = mylineWidths)
#  png(filename = paste(species_list[i],"diurnal_differences.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
#  trellis.par.set(my.settings)
#  print(d, subset = "hour")
#  dev.off()
#} 
#trellis.par.set(original.settings)

#this is to plot water content 
#setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
#d <- 
#  timeVariation(subset(met_ln, site != "Bellambi"), pollutant = "W", group = "data_source", type = "campaign", ci = F, ylab = "water content", key.columns = 3,  col = myColours, lty = mylineTypes, lwd = mylineWidths)
#  timeVariation(subset(met_ln, site == "Bellambi"), pollutant = "W", group = "data_source", type = "campaign", ci = F, ylab = "water content", key.columns = 3,  col = myColours, lty = mylineTypes, lwd = mylineWidths)
  
  #png(filename = "water_diurnal.png", width = 6 * 300, height = 4 * 300, res = 300)
#trellis.par.set(my.settings)
#print(d, subset = "hour")
#dev.off()
#looks odd - not including it in preliminary evaluation - need to double check calculation of W from BOM data - did that, results still look off
  #need to add calculation to all models 

#these are for time series of hourly values - hard to read, not for paper 
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/timeseries")
for (i in 1:length(species_list)) {
  for (j in 1:length(date_start)){
  png(filename = paste(species_list[i],campaign[j],"timeseries.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  scatterPlot(selectByDate(subset(met_ln, site != "Bellambi"), start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], 
              ylab = species_names[i], group = "data_source", type = "campaign", plot.type = "l",
              col = myColours, lwd = mylineWidths,lty = mylineTypes,
              key.position = "top", key.columns =3, key.title = "")#, main = campaign[j])
  dev.off()
  }
}

#the above shows all models, averaged across all sites, separately for each campaign 
#need the same plots, for each site... 
for (k in 1:length(site_list)) {
for (i in 1:length(species_list)) {
    d <- timeVariation(subset(met_ln, site %in% site_list[k]), pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, 
                       ylab = species_names[i], key.columns = 3, main = site_list[k], col = myColours, lty = mylineTypes, lwd = mylineWidths)
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/site plots/")
  png(filename = paste(species_list[i],site_list[k],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  trellis.par.set(my.settings)
  print(d, subset = "hour")
  trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
  panel.text(0.15, 0.825, site_list[k], cex = 1, font = 1)
  trellis.unfocus()
  dev.off()
}
}
trellis.par.set(original.settings)

#timeseries by site 
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/site plots/timeseries")
for (i in 1:length(species_list)) {
  for (j in 1:length(date_start)){
    png(filename = paste(species_list[i],campaign[j],"timeseries_by_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(met_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
    dev.off()
  }
}

#plot median instead of mean in overall diurnal plots
for (i in 1:length(species_list)) {
  d <- timeVariation(met_ln, pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 3, statistic = "median", conf.int = F,
                     col = myColours, lty = mylineTypes, lwd = mylineWidths)
 # d <- timeVariation(subset(met_ln, site != "Bellambi"), pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, ylab = species_names[i], key.columns = 3, statistic = "median", conf.int = F,
#                     col = myColours, lty = mylineTypes, lwd = mylineWidths)
 # setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/")
  png(filename = paste(species_list[i],"median_diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  trellis.par.set(my.settings)
  print(d, subset = "hour")
  dev.off()
}
trellis.par.set(original.settings)

#same, but for each site 
for (k in 1:length(site_list)) {
  for (i in 1:length(species_list)) {
    d <- timeVariation(subset(met_ln, site %in% site_list[k]), pollutant = species_list[i], group = "data_source", type = "campaign", ci = F, statistic = "median",
                       ylab = species_names[i], key.columns = 3, main = site_list[k], col = myColours, lty = mylineTypes, lwd = mylineWidths)
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/site plots/median cycles/")
    png(filename = paste(species_list[i],site_list[k],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
    trellis.par.set(my.settings)
    print(d, subset = "hour")
    trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
    panel.text(0.15, 0.825, site_list[k], cex = 1, font = 1)
    trellis.unfocus()
    dev.off()
  }
}
trellis.par.set(original.settings)


strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)


         
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
#   png(filename = paste(species_list_2[k],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
#   TaylorDiagram(subset(met, site != "Bellambi"), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = "data_source", type = "campaign", 
#                 main = species_names[k], col = myColours_2)
#  dev.off()
 # t <- TaylorDiagram(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = c("data_source", "site"), type = "campaign", 
                    # main = species_names[k], col = myColours_2, cex = 0.7, normalise = T, key.tile = "Model")
  t <- TaylorDiagram(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = c("data_source"), type = "campaign", 
                   main = species_names[k], col = myColours_2, cex = 0.7, normalise = F, key.title = "Model")

  test <- t$data
  test <- within(test, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*R))
  png(filename = paste(species_list_2[k],"Taylor_Diagram.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  
  trellis.par.set(my.settings)
  print(t)
  dev.off()

#colours are not the same here as for the other plots...   
#  png(filename = paste(species_list_2[k],"Taylor_by_model.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
#  TaylorDiagram(subset(met, site != "Bellambi"), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = "data_source", group = "campaign", main = species_names[k])
#  dev.off()
    
  stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign"))
  #merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
    for (m in 1:length(stat_list)) {
  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                         maptype = "roadmap", col = "jet", cex = 1, main = paste(species_list[k] , "-", stat_list[m] ),
                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
  png(filename = paste(species_list[k], stat_list[m],"map.png", sep = '_'), width = 8 * 300, height = 8 * 300, res = 300)
  print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
  dev.off()
    }
  
  #add means.obs and sd.obs to stats 
  #means <- ddply(met, .(site, data_source), numcolwise(mean), na.rm = TRUE)
  stats$means.obs <- mean(met)
  
  #save stats and rename dataframe
  stats_name <- paste0("stats_",species_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)


#same Taylor diagrams, but coloured by site 
for (k in 1:length(species_list_2)){
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
  png(filename = paste(species_list_2[k],"Taylor_Diagram_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  TaylorDiagram(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = c("site", "data_source"), type = "campaign", 
                main = species_names[k], col = myColours_2)
  dev.off()
  png(filename = paste(species_list_2[k],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 4.5 * 300, res = 300)
  TaylorDiagram(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = "data_source", type = "campaign", 
                main = species_names[k], col = myColours_2)
  dev.off()
}




#make the same Taylor plots, but for each site... 
for (j in 1:length(site_list)) {
for (k in 1:length(species_list_2)){
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/site plots")
  png(filename = paste(species_list_2[k],site_list[j],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  TaylorDiagram(subset(met, site %in% site_list[j]), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), group = "data_source", type = "campaign", main = paste0(species_names[k], "-", site_list[j]), col = myColours_2)
  dev.off()
  
  png(filename = paste(species_list[k],site_list[j],"Taylor_by_model.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  TaylorDiagram(subset(met, site %in% site_list[j]), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = "data_source", group = "campaign", main = paste0(species_names[k], "-", site_list[j]))
  dev.off()
}
}

#make Taylor plots for each model, showing all sites for each campaign 
#setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/site plots")
#for (m in 1:length(model_list)) {
#  for (k in 1:length(species_list_2)){
#    png(filename = paste(species_list_2[k],model_list[m],"Taylor_by_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
#    TaylorDiagram(subset(met, data_source %in% model_list[m]), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"),  group = "site" ,type = "campaign", main = paste0(model_list[m], "-", species_list_2[k]))
#  dev.off()
#    }
#}
#falls over because OEH does not have RH - run again with m in 5: and k in 3: 


#make more stats 
#overall stats, hourly values 
for (k in 1:length(species_list_2)){
#stats <- modStats(subset(met, site != "Bellambi"), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign"))
stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign"))
stats_paper <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = "data_source")
stats_paper$mean.obs <- 

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
  #stats <- modStats(subset(met_daily, site != "Bellambi"), obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source", "campaign"))
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

#plot daily timeseries 
met_daily_ln <- timeAverage(met_ln, avg.time = "1 day", type = c("data_source","site","campaign"))
met_daily_sum_ln <- timeAverage(met_ln, avg.time = "1 day", statistic = "sum", type = c("data_source", "site","campaign"))
met_daily_ln$prcp <- met_daily_sum_ln$prcp

for (i in 1:length(species_list)) {
  for (j in 1:length(date_start)){
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/daily/")
    png(filename = paste(species_list[i],campaign[j],"daily_timeseries.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(met_daily_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
    dev.off()
  }
}


#plot timeseries of daily max temperature (and daily min?) and save stats 
met_max <- timeAverage(met, avg.time = "1 day", statistic = "max", type = c("data_source","site","campaign"))
for (k in 1:length(species_list_2)){
  stats <- modStats(met_max, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source","site", "campaign"))
  #save stats and rename dataframe
  stats_name <- paste0("daily_max_stats_",species_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}
met_max_ln <- timeAverage(met_ln, avg.time = "1 day", statistic = "max", type = c("data_source","site","campaign"))

for (i in 1:length(species_list)) {
  for (j in 1:length(date_start)){
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/daily max/")
    png(filename = paste(species_list[i],campaign[j],"timeseries_max.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(met_max_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
    dev.off()
  }
}

met_min <- timeAverage(met, avg.time = "1 day", statistic = "min", type = c("data_source","site","campaign"))
for (k in 1:length(species_list_2)){
  stats <- modStats(met_min, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("data_source","site", "campaign"))
  #save stats and rename dataframe
  stats_name <- paste0("daily_min_stats_",species_list[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}
met_min_ln <- timeAverage(met_ln, avg.time = "1 day", statistic = "min", type = c("data_source","site","campaign"))

for (i in 1:length(species_list)) {
  for (j in 1:length(date_start)){
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/daily min/")
    png(filename = paste(species_list[i],campaign[j],"timeseries_min.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(met_min_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list[i], ylab = species_names[i], group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
    dev.off()
  }
}

######
#This is done somewhere else, no? 
#It looks like daily is better for prcp than hourly - but what about campaign totals
sums <- ddply(met_ln, .(site, campaign, data_source), numcolwise(sum), na.rm = TRUE)
total_prcp <- subset(sums, select = c("site", "campaign", "data_source", "prcp"))

#try to plot this in a meaningful manner

#b1 <- update(b1, between = list(x = 0, y = 1))
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
png(filename = "Total_precipitation.png", width = 12 * 300, height = 7 * 300, res = 300)#, type = "windows")
trellis.par.set(my.settings) 
b1 <- barchart(total_prcp$prcp~total_prcp$site|total_prcp$campaign, group = total_prcp$data_source,
               #col=myColours,
               #superpose.polygon=list(col= myColours),
               ylab = "Total precipitaion (mm)", ylim = c(0,500),
               #strip.left = strip.custom(style=1, horizontal = F),
               auto.key = list(column = 3, space = "top"), 
               par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
#print(useOuterStrips(b1, strip = mystrip, strip.left = mystrip)) #useOuterStrips ignores specified strip parameters... 
plot(b1, strip = mystrip)
dev.off()

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
  png(filename = paste("Total_precipitation", stat_list_2[m],"map.png", sep = '_'), width = 8 * 300, height = 8 * 300, res = 300)
  print(useOuterStrips(a1$plot,strip = mystrip, strip.left = mystrip))
  dev.off()
}
#save stats as data.frame
stats_name <- "stats_total_prcp"
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
assign(stats_name,stats)

#also need the overall (domain averaged stats...)
sums_dom <- ddply(met_ln, .(campaign, data_source), numcolwise(sum), na.rm = TRUE)
total_prcp_dom <- subset(sums_dom, select = c("campaign", "data_source", "prcp"))

#make google maps of mean bias, etc for total precipitation 
total_prcp_dom_obs <- subset(total_prcp_dom, data_source %in% "OBS")
total_prcp_dom_models <- subset(total_prcp_dom, data_source != "OBS")
total_prcp_dom_wide <- merge(total_prcp_dom_obs, total_prcp_dom_models, by = "campaign", suffixes = c(".obs", ".mod", all = T))

stats <- modStats(total_prcp_dom_wide, mod = "prcp.mod", obs = "prcp.obs", type = c("data_source.mod", "campaign"))
stats_name <- "stats_dom_avg_total_prcp"
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
assign(stats_name,stats)
####################

#as I prepared plots for ANSTO comparison, I thought of this: 
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
png(filename = "wd_densities.png", width = 15 * 300, height = 12 * 300, res = 300)
to_exclude <- rownames(subset(met_ln, campaign %in% "SPS2" & data_source == "CSIRO")) #this is because the NAs make it fall over 
d1 <- densityplot(~wd|site*campaign, 
                  data = met_ln[-c(to_exclude[1]:to_exclude[length(to_exclude)]),],
                  groups = data_source, #why is this broken? is this because I have missing data?
                  plot.points=FALSE,
                  auto.key = list(column = 3, space = "top"), 
                  par.settings = list(superpose.line = list(col = myColours, lty = mylineTypes, lwd = mylineWidths)),#, strip.background = list(col = "white)")),
                  strip.left = strip.custom(style=1, horizontal = F, bg = "white"),
                  from=0, to=360) #na.rm =T) # does not work 
print(useOuterStrips(d1, strip = mystrip, strip.left = mystrip))
dev.off()

#repeat this, but excluding low wind speeds as wd is more uncertain at low wind speeds 
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
png(filename = "wd_densities_low_ws_excluded.png", width = 15 * 300, height = 12 * 300, res = 300)
to_exclude <- rownames(subset(met_ln, campaign %in% "SPS2" & data_source == "CSIRO")) #this is because the NAs make it fall over 
d1 <- densityplot(~wd|site*campaign, 
                  data = subset(met_ln[-c(to_exclude[1]:to_exclude[length(to_exclude)]),], ws>2),
                  groups = data_source, #why is this broken? is this because I have missing data?
                  plot.points=FALSE,
                  auto.key = list(column = 3, space = "top"), 
                  par.settings = list(superpose.line = list(col = myColours, lty = mylineTypes, lwd = mylineWidths)),#, strip.background = list(col = "white)")),
                  strip.left = strip.custom(style=1, horizontal = F, bg = "white"),
                  from=0, to=360) #na.rm =T) # does not work 
print(useOuterStrips(d1, strip = mystrip, strip.left = mystrip))
dev.off()
#this is slightly wrong - ideally, you want to exclude only OBSERVED wind speeds <2m/s 
#(at the moment, the subsetting removes modeled values as well) 

#this is to plot the binned MB versus observations 
#I still don't like the legend (I want three columns), and that the symbols are all open circles (hard to read in b/w)
#I also need to fix the x axis labelling 
species_col <- c(4,7,8)
obs_species <- c("temp.obs", "ws.obs", "wd.obs")
mod_species <- c("temp.mod","ws.mod","wd.mod")
x_labels <- c("observed temperature", "observed wind speed", "observed wind direction")
add_line <- c(1,1.5,30)
#exclude Bellambi 
met_sub <- subset(met, site!="Bellambi")
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
for (t in 1:length(species_col)) {
  png(filename = paste(names(met_sub[species_col[t]]),"mb_by_bin.png", sep = "_"), width = 9 * 300, height = 5 * 300, res = 300)
  x_1 <- floor((max(met_sub[,species_col[t]], na.rm = T) - min(met_sub[,species_col[t]], na.rm = T))/10)
  x_max <- ceiling(max(met_sub[,species_col[t]], na.rm = T)/x_1) * x_1
  x_breaks <- c(0, seq(x_1, x_max, by = x_1))
 # a <- histogram(~ met[,species_col[t]]|campaign, data = met, #endpoints = c(0,ceiling(max(met[,species_col[t]], na.rm = T))),
 #             col = "grey90", xlab = names(met[species_col[t]]),
 #              breaks = x_breaks,
 #             scales = list(x = list(at = x_breaks)))
  c <- densityplot(~ met_sub[,species_col[t]]|campaign, data = met_sub, plot.points=FALSE, col = "grey", from = 0, to = x_max, xlab = x_labels[t])
   
  met_sub$bin <- cut(met_sub[,species_col[t]], breaks = x_breaks, labels = (seq(0, (x_max-x_1), by = x_1)))
  stats_test <- modStats(met_sub, obs = obs_species[t], mod = mod_species[t], type = c("data_source", "campaign", "bin"))
  stats_test$bin <- as.numeric(stats_test$bin)*x_1
  b <- xyplot(MB ~ bin|campaign, data = stats_test, groups = data_source, 
              xlab = x_labels[t],  
              auto.key = list(column = 3, space = "top"), 
              par.settings = my.settings,
              scales = list(alternating = 1),
              panel =function(...){  
                panel.xyplot(...);
                panel.abline(h = c(0,add_line[t],-(add_line[t])), col = c("blue","red", "red"), lty = c(2,3,3))
              })
   print(doubleYScale(b, c, use.style = F, add.ylab2 = T,auto.key = list(column = 3, space = "top")))
   #update(b2, strip = mystrip)#, auto.key = list(column = 3, space = "top"))
   #update(b2, strip = mystrip)
   dev.off()
}
#make a plot for every site? not sure, not easy! 

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



