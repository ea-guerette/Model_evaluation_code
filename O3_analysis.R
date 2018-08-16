#figures and analysis for ozone (AQ paper) 
Sys.setenv(TZ='GMT')
#LOAD PACKAGES
library(openair)
library(plyr)
library(reshape2)

#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
dir_stat_output <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis/"
dir_figures_paper <- "C:/Users/eag873/Documents/GitHub/AQ_paper/Figures_AQ_paper/"
dir_figures <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/"

#load in OEH observations  
load(paste0(dir_obs,"OEH_obs.RData"))

#load in original model data
#load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_new_new_fixed.RData"))
load(paste0(dir_mod,"/OEH_model_output2.RData"))
load(paste0(dir_mod, "/YZ.RData"))
#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

#try fixing tzones... 
#library(lubridate)
#yz_mod$date <- with_tz(yz_mod$date, tzone = "UTC")
#oeh_model$date <- with_tz(oeh_model$date, tzone = "UTC")

#assign variables
OBS <- oeh_obs
site_list_aq <- levels(as.factor(OBS$site)) #to select only OEH sites 
site_list_aq <- site_list_aq[-c(9,17:18)] #remove Macarthur (SPS1 only), Warrawong and Westmead (SPS1 and 2 only)

OBS <- subset(OBS, site %in% site_list_aq)

species_list_aq <- c("O3","NO", "NO2","NOx", "PM2.5","PM10","CO", "SO2", "NH4", "NO3", "SO4", "EC", "ws", "temp")
species_list_o3 <-  c("O3","NOx")
param_list <- c("date", "site", "campaign", "data_source", species_list_aq)  #complete list of things to keep from model runs 

#campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "NMB", "MB", "FAC2") #list of stats for plotting - not sure about these just yet


#combine all original model data 
models <- rbind.fill(cmaq, wrf_chem, csiro, oeh_model, yz_mod)
#select model data for BOM sites only 
model_aq <- subset(models, site %in% site_list_aq)

#cut to length - some model runs were longer than the actual campaigns 
mumba_mod <- subset(model_aq, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00")
sps1_mod <- subset(model_aq, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(model_aq, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
model_aq <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 

#select variables that are of interest only 
#indices <- match(species_list, names(model_met)) #not for here - for later 
model_aq <- model_aq[,param_list]

##prepare dfs for analysis 
#merge obs and model output into wide format 
aq <- merge(OBS, model_aq, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#melt OBS before adding data_source to OBS dataframe:
melted_OBS <- melt(OBS, id = c("date", "site", "campaign"), value.name = "obs")

#merge obs and model output into long format 
OBS$data_source <- "OBS"
aq_ln <- rbind.fill(OBS, model_aq)

#make daily averages 
aq_daily <- data.frame(timeAverage(aq, avg.time = "1 day", type = c("site", "data_source", "campaign")))
aq_ln_daily <- data.frame(timeAverage(aq_ln, avg.time = "1 day", type = c("site", "data_source", "campaign")))
daily_aq_ln <- data.frame(timeAverage(aq_ln, avg.time = "1 day", type = c("data_source", "campaign"))) #for overall daily timeseries
daily_aq <- data.frame(timeAverage(aq, avg.time = "1 day", type = c("data_source", "campaign")))

#for plotting 
source(paste0(dir_code,"/lattice_plot_settings_aq.R"))
source(paste0(dir_code, "/mod_TaylorDiagram.R"))
myKey_aq <- list(column = 4, space = "top", text = list(c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours_aq))
resolution = 600



#data checks - plot data for each site - but what kind of plot? 
setwd(paste0(dir_figures, "site plots"))
for (i in 1:length(species_list_o3)) {
  species_col <- match(species_list_o3[i], names(aq_ln_daily))
  for (k in 1:length(site_list_aq)) {
 p <-xyplot(aq_ln_daily[,species_col] ~date|campaign, data = aq_ln_daily, subset = (site %in% site_list_aq[k]), 
       groups = ordered(aq_ln_daily$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
       par.settings = my.settings, layout = c(3,1),
       scales = list(x = list(relation = "free")), ylab = species_list_o3[i],
       between = list(x = 0.75), key = myKey_aq, type = "l", main = site_list_aq[k])
    png(filename = paste0("daily_",species_list_o3[i], "_", site_list_aq[k], ".png"), width = 7 *300, height =5*300, res = 300)
    print(p)
    dev.off()
 }
}

#panel plot - O3, NOx only 

#diurnal cycles, taylorDiagrams, binned quantiles on one panel, with only one legend 
species <- c("O3", "NOx")
y.lab1 <- c("Ozone (ppb)", "NOx (ppb)")
y.lab3 <- c("Mean bias (ppb)","Mean bias (ppb)")
fig_name <- c("ozone_panel", "NOx_panel")
add_line <- c(2,2) #not sure if 2 is OK for NOx

setwd(dir_figures)

for (i in 1:2){
  a <- timeVariation(aq_ln, pollutant = species[i], group = "data_source", type = "campaign")
  temp_hour <- a$data$hour
  
  t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
                ylab =y.lab1[i], type = "l", col = myColours_aq, par.settings = my.settings,
                auto.key = list(column = 4, space = "top", points = F, lines = T), 
                scales = list(x = list(alternating = 1)), layout = c(3,1)
                , aspect = 1
  )
  #taylor diagram 
  
  t2 <- mod_TaylorDiagram(aq, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours_2_aq, key = F,
                          annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  
  ###binned quantiles 
  
  species_col <- match(paste0(species[i], ".obs"), names(aq))
  bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
  bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 
  
  x_test <- quantile(aq[,species_col], probs = bins, na.rm = T)
  unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
  x_test <- x_test[unik] ## the values 
  bin_labels <- bin_labels[unik]
  
  aq$bin <- cut(aq[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
  stats_bin <- modStats(aq, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))
  
  #plot:  
  
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
               xlab = "" ,#species_names[1], 
               type = "l", 
               ylab = y.lab3[i],
               auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
               par.settings = my.settings,
               scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),
               layout = c(3,1),
               aspect = 1,
               panel =function(...){  
                 panel.xyplot(...);
                 panel.abline(h = c(0,add_line[i],-(add_line[i])), col = c("black","grey60", "grey60"), lty = c(2,3,3))
               })
  print(t3, auto.key = list(column = 4, space = "bottom", lines = T, points = F))
  
  

  png(filename = paste0(fig_name[i], "_v3.png"), width = 7 * 300, height = 9 * 300, res = 300)
  
  print(t1, position = c(0,2/3-1/24.5,1,1), more = TRUE)
  print(t2, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE)
  print(t3, position = c(0,0,1,1/3+1/65))
  
  dev.off() 
  
}

#make daily panel


species_col1 <- match(species, names(daily_aq_ln))

for (i in 1:length(species)) {
  
  t1 <- xyplot(daily_aq_ln[, species_col1[i]] ~date|campaign, groups = ordered(daily_aq_ln$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = daily_aq_ln, 
               scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey_aq, ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )
  t2 <- mod_TaylorDiagram(daily_aq, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours_2_aq, key = F,
                          annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  
  
  ###binned quantiles 
  
  species_col <- match(paste0(species[i], ".obs"), names(aq))
  bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
  bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 
  
  x_test <- quantile(daily_aq[,species_col], probs = bins, na.rm = T)
  unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
  x_test <- x_test[unik] ## the values 
  bin_labels <- bin_labels[unik]
  
  daily_aq$bin <- cut(daily_aq[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
  stats_bin <- modStats(daily_aq, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))
  
  #plot:  
  
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
               xlab = "" ,#species_names[1], 
               type = "l", 
               ylab = y.lab3[i],
               auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
               #key = myKey,
               par.settings = my.settings,
               scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),
               layout = c(3,1),
               aspect = 1,
               panel =function(...){  
                 panel.xyplot(...);
                 panel.abline(h = c(0,add_line[i],-(add_line[i])), col = c("black","grey60", "grey60"), lty = c(2,3,3))
               })
  
  
  png(filename = paste0("daily_",fig_name[i], "_v1.png"),  width = 7 * resolution, height = 9 * resolution, res = resolution)
  
  print(t1, position = c(0,2/3,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
  print(t2, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
  print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)
  
  
  dev.off() 
} 


#calc stats 
source(paste0(dir_code,"/makeStats_functions.R"))

for (k in 1:length(species_list_o3)){   
  stats_name <- paste0("stats_",species_list_o3[k])
  stats <- makeStats1(aq, species_list_o3[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(aq, species_list_o3[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(aq, species_list_o3[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_per_campaign_per_site.csv"), row.names =F)
}   

for (k in 1:length(species_list_o3)){   
  stats_name <- paste0("stats_",species_list_o3[k])
  stats <- makeStats1(aq_daily, species_list_o3[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_", stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(aq_daily, species_list_o3[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_",stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(aq_daily, species_list_o3[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_" ,stats_name, "_per_campaign_per_site.csv"), row.names =F)
}  

#examine bias by time of day 

aq$HOD <- as.factor(format(aq$date, "%H"))

for (k in 1:length(species_list_o3)) {
  
  stats <- modStats(aq, obs = paste0(species_list_o3[k],".obs"), mod = paste0(species_list_o3[k],".mod"), type = c("campaign", "data_source", "HOD"))
  write.csv(stats, file = paste0(dir_stat_output, "HOD_stats_", species_list_o3[k], ".csv"), row.names = F)
  
  stat_list <- c("MB", "NMB")
  
  for (i in 1: length(stat_list)){
    species_col <- match(stat_list[i], names(stats))
    c <- xyplot(stats[,species_col] ~HOD|campaign, data = stats, groups = data_source, type = "l", 
                scales = list(x = list(alternating = 1, at = c(0,3,6,9,12,15,18,21))), 
                ylab = stat_list[i], xlab = "HOD", main = species_list_o3[k],
                par.settings = my.settings,
                auto.key = list(column = 1, space = "right", points = F, lines = T),
                layout = c(3,1),
                panel =function(...){  
                  panel.xyplot(...);
                  panel.abline(h = 0, col = "black", lty = 2)
                })
    png(filename = paste0(species_list_o3[k], "_", stat_list[i], "_by_HOD.png"), width = 8 *300, 4*300, res = 300)
    print(c)
    
    dev.off()
  }
}

stat_o3 <- read.csv(file.choose(), header = TRUE)
stat_NOx <- read.csv(file.choose(), header = TRUE)

stat_HOD <- merge(stat_o3, stat_NOx, by = c("campaign", "data_source", "HOD"), suffixes = c(".o3", ".nox"))

xyplot(MB.o3 ~MB.nox|campaign, groups = data_source, data = stat_HOD)


###############


#make overall Taylor - not for paper
#melted_OBS <- melt(OBS, id = c("date", "site", "campaign"), value.name = "obs")
#melted_model_aq <- melt(model_aq, id = c("date", "site", "campaign", "data_source"), value.name = "mod")
#melted <- merge(melted_OBS, melted_model_aq, by = c("date", "site", "campaign", "variable"))

