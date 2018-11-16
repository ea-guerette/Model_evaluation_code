#figures for the AQ paper - not quite 
# not all variables are available everywhere 

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
load(paste0(dir_mod,"/models.RData"))
#load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
#load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
#load(paste0(dir_mod,"/CSIRO_model_output_new_new_new_fixed.RData"))
#load(paste0(dir_mod,"/OEH_model_output2.RData"))
#load(paste0(dir_mod, "/YZ.RData"))
#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

aq_models <- subset(models, data_source != "W-A11")


#assign variables
OBS <- oeh_obs
site_list <- levels(as.factor(OBS$site)) #to select only OEH sites 
site_list <- site_list[-c(9,17:18)] #remove Macarthur (SPS1 only), Warrawong and Westmead (SPS1 and 2 only)

species_list_aq <- species_list_aq <- c("O3","NO", "NO2","NOx", "PM2.5","PM10","CO", "SO2", "NH4", "NO3", "SO4", "EC", "ws", "temp","HCHO")

param_list <- c("date", "site", "campaign", "data_source", species_list_aq)  #complete list of things to keep from model runs 
#campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "RMSE", "MB", "FAC2") #list of stats for plotting 


#combine all original model data 
#models <- rbind.fill(cmaq, wrf_chem, csiro, oeh_model, yz_mod)
#select model data for BOM sites only 
model_aq <- subset(aq_models, site %in% site_list)

#cut to length - some model runs were longer than the actual campaigns 
mumba_mod <- subset(model_aq, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
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

#make overall Taylor - not for paper
melted_OBS <- melt(OBS, id = c("date", "site", "campaign"), value.name = "obs")
melted_model_aq <- melt(model_aq, id = c("date", "site", "campaign", "data_source"), value.name = "mod")
melted <- merge(melted_OBS, melted_model_aq, by = c("date", "site", "campaign", "variable"))

#for plotting 
source(paste0(dir_code,"/lattice_plot_settings_aq.R"))
source(paste0(dir_code, "/mod_TaylorDiagram.R"))
myKey <- list(column = 4, space = "top", text = list(c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours))


setwd(dir_figures)
png(filename = "Taylor_all_new.png", width = 8 * 300, height = 12 * 300, res = 300)
TaylorDiagram(melted, obs = "obs", mod = "mod", normalise = T, 
              group = "variable", type = c("campaign", "data_source"), cex = 0.95, 
              annotate = "", rms.col = "grey40")
dev.off()


#calculate stats -paired so doesn't matter if extra sites (?)
source(paste0(dir_code,"/makeStats_functions_MSE.R"))

for (k in c(1,3:8,13:14)){   #NO, NH4, NO3, SO4, EC do not work -run 1,3,13
  stats_name <- paste0("stats_",species_list_aq[k])
  stats <- makeStats1(aq, species_list_aq[k])
  #barchart(r ~ data_source, data = stats, main = species_list_aq[k])
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(aq, species_list_aq[k])
  #barchart(r~ data_source|campaign, data = stats,
  #         par.settings = my.settings,
  #         par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(aq, species_list_aq[k])
  #barchart(r~ site|campaign, data = stats, groups = data_source,
  #         par.settings = my.settings,
  #         par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2), 
  #         main   = species_list_aq[k],
  #         col=myColours_2_aq,
  #         superpose.polygon=list(col= myColours_2_aq))
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_per_campaign_per_site.csv"), row.names =F)
}   

aq_daily <- timeAverage(aq, avg.time = "1 day", type = c("site", "campaign", "data_source"))

for (k in c(1,3:8,13:14)){   
  stats_name <- paste0("stats_",species_list_aq[k])
  stats <- makeStats1(aq_daily, species_list_aq[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_", stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(aq_daily, species_list_aq[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_",stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(aq_daily, species_list_aq[k])
  write.csv(stats, file = paste0(dir_stat_output, "daily_" ,stats_name, "_per_campaign_per_site.csv"), row.names =F)
}  


#use aq_daily to plot timeseries? YES - will need those for Khalia too 


#merge obs and model output into long format 
OBS$data_source <- "OBS"
aq_ln <- rbind.fill(OBS, model_aq)


#make ozone panel - as in met paper - use all sites in site_list 
#for PM2.5, will need a shorter list of sites... 

#diurnal cycles, taylorDiagrams, binned quantiles on one panel, with only one legend 
species <- c("O3", "NOx","NO2", "ws", "temp")
y.lab <- c("Mean bias (ppb)","Mean bias (ppb)","Mean bias (ppb)", expression("Mean bias (m s"^-1*")"), expression("Mean bias ("* degree * "C)"))
fig_name <- c("ozone_panel", "NOx_panel", "NO2_panel", "wind_speed_panel", "temperature_panel")
add_line <- c(2,2,2,1.5,1) #not sure if 2 is OK for NOx


for (i in 1:5){
  a <- timeVariation(aq_ln, pollutant = species[i], group = "data_source", type = "campaign")
  temp_hour <- a$data$hour
  
  t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
                ylab =species[i], type = "l", col = myColours_aq, par.settings = my.settings,
                auto.key = list(column = 4, space = "top", points = F, lines = T), 
                scales = list(x = list(alternating = 1)), layout = c(3,1)
                , aspect = 1
  )
  #taylor diagram 
  
  t2 <- mod_TaylorDiagram(aq, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours_aq, key = F,
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
               ylab = y.lab[i],
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
  
  
  setwd(dir_figures_paper)
  png(filename = paste0(fig_name[i], "_v3.png"), width = 7 * 300, height = 9 * 300, res = 300)
  
  print(t1, position = c(0,2/3-1/24.5,1,1), more = TRUE)
  print(t2, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE)
  print(t3, position = c(0,0,1,1/3+1/65))
  
  dev.off() 
  
}


sites_PM2.5 <- c("Chullora", "Earlwood", "Liverpool", "Richmond", "Wollongong")
sites_CO <- c("Chullora", "Liverpool", "Prospect", "Rozelle", "Wollongong")
sites_SO2 <- c("Albion_Park_Sth", "Bargo", "Bringelly", "Chullora", "Lindfield", "Prospect", "Randwick", "Richmond", "Wollongong")



