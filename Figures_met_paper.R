#this is to make the following figures for the met paper - surface temp, W, ws, u10 and v10 
#prcp and pblh will be made separately I think - or I might copy the code here... 
#the code for stats output is in Figures_met_analysis 

library(openair)
library(plyr)
#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
#save figures on cloudstor 
dir_figures <- "C:/Users/eag873/ownCloud/Figures_and_stats_met_paper/"

#load in met observations from BOM  
load(paste0(dir_obs,"/BOM_data_updated3.RData"))

#load in original model data
load(paste0(dir_mod,"/ANSTO_model_output.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_new_new.RData"))
load(paste0(dir_mod,"/OEH_model_output.RData"))
load(paste0(dir_mod, "/YZ.RData"))
#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

#assign variables
BOM <- bom_data_all_campaigns
site_list <- levels(as.factor(BOM$site)) #to select only BOM sites 

species_list <- c("temp", "W", "ws","u10", "v10","wd","RH", "prcp", "pblh", "SWR", "pres") #met variables we are interested in 
species_list_2 <- c("temp", "W", "ws", "u10", "v10") #reduced list of variables -to plot (order matters, need to match labels in species_names)
species_names <- c(expression("Temperature (" * degree * "C)"),  "water mixing ratio (g/kg)", "wind speed (m/s)", "u wind", "v wind", expression("wind direction (" * degree *")"),"RH (%)", "precipitation (mm)", "pblh (m)", "SWR", "pressure (hPa)")

param_list <- c("date", "site", "campaign", "data_source", species_list)  #complete list of things to keep from model runs 
#campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "RMSE", "MB") #list of stats for plotting 

#trim original model data 
#only include wrf-11 (issue with wrf-10)
levels(as.factor(wrf$data_source))
wrf <- subset(wrf, data_source == "W-A11")

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

#merge obs and model output into long format 
BOM$data_source <- "OBS"
met_ln <- rbind.fill(BOM, model_met)

#for plotting 
source(paste0(dir_code,"/lattice_plot_settings.R"))

#################
#TEMPERATURE 
#diurnal cycles, taylorDiagrams, binned quantiles on one panel, with only one legend 

a <- timeVariation(met_ln, pollutant = "temp", group = "data_source", type = "campaign")
temp_hour <- a$data$hour

t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
       ylab =species_names[1], type = "l", col = myColours, par.settings = my.settings,
       auto.key = list(column = 4, space = "top", points = F, lines = T), 
       scales = list(x = list(alternating = 1)), layout = c(3,1)
       , aspect = 1
              )

source(paste0(dir_code, "/mod_TaylorDiagram.R"))
t2 <- mod_TaylorDiagram(met, obs = "temp.obs",mod = "temp.mod", group = "data_source", type = "campaign", col = myColours, key = F,
                   annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
#note: not easy to hack the colour of  "observed
#also, this won't plot nicely with the others - make my own plot, based on theirs 





species <- "temp"
species_col <- match(paste0(species, ".obs"), names(met))
bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") #fix those, need 10 

x_test <- quantile(met[,species_col], probs = bins, na.rm = T)
unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
x_test <- x_test[unik] ## the values 
bin_labels <- bin_labels[unik]
  
met$bin <- cut(met[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
stats_bin <- modStats(met, obs = paste0(species, ".obs"), mod = paste0(species, ".mod"), type = c("data_source", "campaign" ,"bin"))
  
  
   
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
              xlab = "" ,#species_names[1], 
              type = "l", 
              ylab = expression("Mean bias ("* degree * "C)"),
              auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
              par.settings = my.settings,
              scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),
              layout = c(3,1),
              aspect = 1,
              panel =function(...){  
                panel.xyplot(...);
                panel.abline(h = c(0,1,-1), col = c("black","grey60", "grey60"), lty = c(2,3,3))
              })
  print(t3, auto.key = list(column = 4, space = "bottom", lines = T, points = F))
 
  
 setwd(dir_figures)
 png(filename = "surface_temperature_panel_v1.png", width = 7 * 300, height = 9 * 300, res = 300)

  print(t1, position = c(0,2/3-1/24.5,1,1), more = TRUE)
  print(t2, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE)
  print(t3, position = c(0,0,1,1/3+1/65))
  
  dev.off() 




