#AQ analysis script 
#copy over the first part of the code from met analysis, but change the species list and species name, etc.  

#myColours <- c("#1B9E77", "#386CB0", "#000000","#FF7F00", "#F42E3C", "#7570B3") #trying a more vibrant orange (less yellow)
myColours_2_aq <-  c("#1B9E77", "#386CB0", "#FF7F00", "#F42E3C")#, "#7570B3") #for when there is no obs... 
myColours_aq <- c("#1B9E77", "#386CB0", "#000000","#FF7F00", "#F42E3C")#, "#7570B3") #exclude WRF colour 
mylineTypes <- c("dashed","dotted","solid","dotdash","longdash")#,"twodash")
mylineWidths <- c(2,2,3,2,2)#,2)


library(openair)
library(plyr)
library(lattice)
library(latticeExtra)
library(stringi)

#load in data 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
#load("BOM_data_updated.RData")
#load("OEH_obs_updated.RData")
load("OEH_obs.RData")

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
#load("ANSTO_model_output.RData")
load("CMAQ_model_output.RData")
load("WRFCHEM_model_output_new.RData")
#load("WRFCHEM_model_output.RData")
load("CSIRO_model_output.RData")
load("OEH_model_output.RData")
load("site_info.RData")

OBS <- oeh_obs
#OBS <- oeh_obs_updated #this updated file sucks - look at Newcastle... (can't believe I shared that!)
models_SPS1 <- rbind.fill(cmaq_SPS1, wrf_chem_SPS1, csiro_SPS1, oeh_model_SPS1)
models_SPS2 <- rbind.fill(cmaq_SPS2, wrf_chem_SPS2, oeh_model_SPS2)
models_MUMBA <- rbind.fill(cmaq_MUMBA, wrf_chem_MUMBA, csiro_MUMBA, oeh_model_mumba)
models <- rbind.fill(cmaq, wrf_chem, csiro, oeh_model)
site_list <- levels(as.factor(OBS$site))
site_list <- site_list[-17] 
species_list_aq <- c("O3", "NOx", "PM2.5","PM10","CO", "SO2")
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("21/12/2012","07/02/2011", "16/04/2012") 
date_end <- c("15/02/2013","06/03/2011","13/05/2012")  
stat_list <- c("r", "RMSE", "MB")
model_list <- c("CMAQ", "CSIRO", "OEH", "WRF-Chem") 
#other_species_list <- c("C5H8, HCHO")
#select sites 
model_aq <- subset(models, site %in% site_list) # does not really work 

#merge obs and model output into wide format 
aq <- merge(OBS, model_aq, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#merge obs and model output into long format 
OBS$data_source <- "OBS"
aq_ln <- rbind.fill(OBS, model_aq)

#models_test <- merge(OBS, models, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#models_test <- subset(models_test, data_source %in% c("WRF-Chem", "OBS"))
#models_test <- subset(models_test, site %in% c("Chullora", "Liverpool", "Richmond", "Wollongong"))
#timePlot(selectByDate(models_test, year = "2013"), pollutant = c("PM2.5.obs", "PM2.5.mod"), type = "site")
#c<- timeVariation(selectByDate(models_test, year = "2013"),pollutant = c("PM2.5.obs", "PM2.5.mod"), type = "site" )
#print(c, subset = "hour")
#models_oeh <- merge(OBS, models, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#models_oeh <- subset(models_oeh, data_source %in% c("CSIRO", "OBS"))
#models_oeh <- subset(models_oeh, site %in% c("Chullora", "Liverpool", "Richmond", "Wollongong"))
#timePlot(selectByDate(models_oeh, year = "2013"), pollutant = c("PM2.5.obs", "PM2.5.mod"), type = "site")
#d <- timeVariation(selectByDate(models_oeh, year = "2013"),pollutant = c("PM2.5.obs", "PM2.5.mod"), type = "site" )
#print(d, subset = "hour")
#mean(models_test$PM2.5.mod, na.rm= T)
#mean(models_test$PM2.5.obs, na.rm= T)

#settings for lattice plotting
original.settings <- trellis.par.get()
my.settings <- trellis.par.get()
names(my.settings)
my.settings$superpose.line$col = myColours_aq  
my.settings$superpose.line$lty = mylineTypes
my.settings$superpose.line$lwd = mylineWidths
my.settings$superpose.polygon$col = myColours_aq
my.settings$superpose.symbol$col = myColours_2_aq
my.settings$superpose.symbol$pch = c(16:21)
my.settings$strip.background$col <- "white"
#trellis.par.set(my.settings) #need to include this inside the graphing device if I want white strips and the right colours 
#no good because it changes the default settings - but necessary for timeVariation... 

mystrip <- strip.custom(bg ="white")

#plot diurnal cycles and time series for all species in species_list 
for (i in 1:length(species_list_aq)) {
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
  d <- timeVariation(aq_ln, pollutant = species_list_aq[i], group = "data_source", type = "campaign", ci = F, ylab = species_list_aq[i], key.columns = 3,  col = myColours_aq, lty = mylineTypes, lwd = mylineWidths)
  png(filename = paste(species_list_aq[i],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  trellis.par.set(my.settings)
  print(d, subset = "hour")
  dev.off()
} 
trellis.par.set(original.settings)
#there is an issue - I need to plot only the model results for the sites at which we have observations! 
#O3 and NOx are measured everywhere - should be OK, but all the others are unfair comparisons

#plot median instead of mean in overall diurnal plots - Does not seem to improve things for AQ
for (i in 1:length(species_list_aq)) {
  
  d <- timeVariation(aq_ln, pollutant = species_list_aq[i], group = "data_source", type = "campaign", ci = F, ylab = species_list_aq[i], key.columns = 3, statistic = "median", conf.int = F,
                     col = myColours_aq, lty = mylineTypes, lwd = mylineWidths)
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
  png(filename = paste(species_list_aq[i],"median_diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
  trellis.par.set(my.settings)
  print(d, subset = "hour")
  dev.off()
}
trellis.par.set(original.settings)

#these are for time series of hourly values - hard to read, not for paper - but check if anything looks really weird 
for (i in 1:length(species_list_aq)) {
  for (j in 1:length(date_start)){
    png(filename = paste(species_list_aq[i],campaign[j],"timeseries.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
    scatterPlot(selectByDate(aq_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list_aq[i], 
                ylab = species_list_aq[i], group = "data_source", type = "campaign", plot.type = "l",
                col = myColours_aq, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")#, main = campaign[j])
    dev.off()
  }
}
#green is repeated as a sixth colour... why? because line type and line width had 6 entries

#the above shows all models, averaged across all sites, separately for each campaign 
#need the same plots, for each site... MEDIAN
for (k in 1:length(site_list)) {
  for (i in 1:length(species_list_aq)) {
    d <- timeVariation(subset(aq_ln, site %in% site_list[k]), pollutant = species_list_aq[i], group = "data_source", type = "campaign", ci = F, statistic = "median",
                       ylab = species_list_aq[i], key.columns = 3, main = site_list[k], col = myColours_aq, lty = mylineTypes, lwd = mylineWidths)
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/site plots/median_cycles")
    png(filename = paste(species_list_aq[i],site_list[k],"diurnal.png", sep = '_'), width = 6 * 300, height = 4 * 300, res = 300)
    trellis.par.set(my.settings)
    print(d, subset = "hour")
    trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
    panel.text(0.15, 0.825, site_list[k], cex = 1, font = 1)
    trellis.unfocus()
    dev.off()
  }
}
trellis.par.set(original.settings)


for (i in 1:length(species_list_aq)) {
  for (j in 1:length(date_start)){
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/site plots/")
    png(filename = paste(species_list_aq[i],campaign[j],"timeseries_by_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(aq_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list_aq[i], ylab = species_list_aq[i], group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours_aq, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
    dev.off()
  }
}


###############
#plot Taylor diagrams and compute stats for all species in species_list and plot google bubble plots
strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)

for (k in 1:length(species_list_aq)){
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
  #png(filename = paste(species_list_aq[k],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  #TaylorDiagram(aq, obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), group = "data_source", type = "campaign", 
  #              main = species_list_aq[k], col = myColours_2_aq)
  #dev.off()
  #colours are wrong - not sure why... Four models, list of 4 colours... but it adds a brown in there? This worked for _met... :/
  # it is because there are obs unmatched by model? NAs... (644 = one site for one campaign) 
  #png(filename = paste(species_list_aq[k],"Taylor_by_model.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
  #TaylorDiagram(aq, obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = "data_source", group = "campaign", main = species_list_aq[k])
  #dev.off()
  
  stats <- modStats(aq, obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = c("data_source","site", "campaign"))
  #merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
  for (m in 1:length(stat_list)) {
    a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                         maptype = "roadmap", col = "jet", cex = 1, main = paste(species_list_aq[k] , "-", stat_list[m] ),
                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
    png(filename = paste(species_list_aq[k], stat_list[m],"map.png", sep = '_'), width = 8 * 300, height = 8 * 300, res = 300)
    print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
    dev.off()
  }
  #save stats and rename dataframe
  stats_name <- paste0("stats_",species_list_aq[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}


#make the same Taylor plots, but for each site... 
#for (j in 1:length(site_list)) {
#  for (k in 1:length(species_list_aq)){
#    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/site plots")
#    png(filename = paste(species_list_aq[k],site_list[j],"Taylor_by_campaign.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
#    TaylorDiagram(subset(aq, site %in% site_list[j]), obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), group = "data_source", type = "campaign", main = paste0(species_list_aq[k], "-", site_list[j]), col = myColours_2_aq)
#    dev.off()
    
#    png(filename = paste(species_list_aq[k],site_list[j],"Taylor_by_model.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
#    TaylorDiagram(subset(aq, site %in% site_list[j]), obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = "data_source", group = "campaign", main = paste0(species_list_aq[k], "-", site_list[j]))
#    dev.off()
#  }
#}
#does not work for species not measured-code falls over :/ 


#this is to plot the binned MB versus observations 
#I still don't like the legend (I want three columns), and that the symbols are all open circles (hard to read in b/w)
#I also need to fix the x axis labelling 
species_col <- 10#c(4,7,8)
obs_species <-  "O3.obs"#c("temp.obs", "ws.obs", "wd.obs")
mod_species <-  "O3.mod" #c("temp.mod","ws.mod","wd.mod")
x_labels <-  "observed ozone" # c("observed temperature", "observed wind speed", "observed wind direction")
add_line <- 2 # c(2,1.5,30)

setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
for (t in 1:length(species_col)) {
  png(filename = paste(names(aq[species_col[t]]),"mb_by_bin.png", sep = "_"), width = 9 * 300, height = 5 * 300, res = 300)
  x_1 <- floor((max(aq[,species_col[t]], na.rm = T) - min(aq[,species_col[t]], na.rm = T))/10)
  x_max <- ceiling(max(aq[,species_col[t]], na.rm = T)/x_1) * x_1
  x_breaks <- c(0, seq(x_1, x_max, by = x_1))
  # a <- histogram(~ met[,species_col[t]]|campaign, data = met, #endpoints = c(0,ceiling(max(met[,species_col[t]], na.rm = T))),
  #             col = "grey90", xlab = names(met[species_col[t]]),
  #              breaks = x_breaks,
  #             scales = list(x = list(at = x_breaks)))
  c <- densityplot(~ aq[,species_col[t]]|campaign, data = aq, plot.points=FALSE, col = "grey", from = 0, to = x_max, xlab = x_labels[t])
  
  aq$bin <- cut(aq[,species_col[t]], breaks = x_breaks, labels = (seq(0, (x_max-x_1), by = x_1)))
  stats_test <- modStats(aq, obs = obs_species[t], mod = mod_species[t], type = c("data_source", "campaign", "bin"))
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







#overall stats, daily averages 
aq_daily <- timeAverage(aq, avg.time = "1 day", type = c("data_source","site","campaign"))

for (k in 1:length(species_list_aq)){
  stats <- modStats(aq_daily,obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = c("data_source", "campaign"))
  #save stats and rename dataframe
  stats_name <- paste0("daily_stats_dom_avg_",species_list_aq[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}

for (k in 1:length(species_list_aq)){
  stats <- modStats(aq_daily, obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = c("data_source","site", "campaign"))
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/daily")
  #make map of daily stats 
  stats <- merge(stats, site_info, by = "site")
    for (m in 1:length(stat_list)) {
    a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                         maptype = "roadmap", col = "jet", cex = 1, main = paste(species_list_aq[k] , "-", stat_list[m], "- daily" ),
                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
    png(filename = paste(species_list_aq[k], stat_list[m],"daily_map.png", sep = '_'), width = 8 * 300, height = 8 * 300, res = 300)
    print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
    dev.off()
  }
  #save stats and rename dataframe
  stats_name <- paste0("daily_stats_",species_list_aq[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}






#??
#plot daily timeseries 
aq_daily_ln <- timeAverage(aq_ln, avg.time = "1 day", type = c("data_source","site","campaign"))

for (i in 1:length(species_list_aq)) {
  for (j in 1:length(date_start)){
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/daily/")
  #  png(filename = paste(species_list_aq[i],campaign[j],"daily_timeseries.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(aq_daily_ln, start = date_start[j], end = date_end[j]), x = "date", y = species_list_aq[i], ylab = species_list_aq[i], group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours_aq, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
  #  dev.off()
  }
}


#overall stats, hourly values 
for (k in 1:length(species_list_aq)){
  stats <- modStats(aq, obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = c("data_source", "campaign"))
  #save stats and rename dataframe
  stats_name <- paste0("stats_dom_avg_",species_list_aq[k])
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis")
  write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
  assign(stats_name,stats)
}

#max ozone plots and stats 

#quantile plots 
#investigate Q-Q plots (Alan's suggestion)
for (k in 1:length(species_list_aq)) {
  setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis")
  png(filename = paste(species_list_aq[k],"quantiles.png", sep = '_'), width = 6 * 300, height = 6 * 300, res = 300)
  conditionalQuantile(aq, obs = paste0(species_list_aq[k],".obs"), mod = paste0(species_list_aq[k],".mod"), type = c("campaign", "data_source"), main = species_list_aq[k])
  dev.off()
}


#daily PM2.5 values +plots 
site_PM2.5 <- c("Wollongong", "Chullora", "Earlwood", "Liverpool", "Richmond")

for (i in 1:length(site_PM2.5)) {
  for (j in 1:length(date_start)){
    setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/daily/")
      png(filename = paste("PM2.5",site_PM2.5[i],campaign[j],"daily_timeseries.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(aq_daily_ln, start = date_start[j], end = date_end[j]), x = "date", y = "PM2.5", ylab = "PM2.5", group = "data_source",  plot.type = "l",
                main = campaign[j], col = myColours_aq, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
      dev.off()
  }
}




#############

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






#just for fun 
  for (j in 1:length(date_start)){
   # setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/site plots/")
   #png(filename = paste(species_list_aq[i],campaign[j],"timeseries_by_site.png", sep = '_'), width = 9 * 300, height = 6 * 300, res = 300)
    scatterPlot(selectByDate(aq_ln, start = date_start[j], end = date_end[j]), x = "date", y = "C5H8", ylab = "isoprene", group = "data_source", type = "site", plot.type = "l",
                main = campaign[j], col = myColours_aq, lwd = mylineWidths,lty = mylineTypes,
                key.position = "top", key.columns =3, key.title = "")
   # dev.off()
  }
scatterPlot(oeh_model_SPS1, x = "NO2", y = "NOx", linear = T)
scatterPlot(data, x = "NO2", y = "NOx")




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
