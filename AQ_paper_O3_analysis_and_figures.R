# This script is to produce the stats / figures needed for the ozone part of the AQ modelling intercomparison paper
Sys.setenv(TZ='GMT')
#LOAD PACKAGES
library(openair)
library(latticeExtra)
library(plyr)
library(reshape2)

#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
dir_stat_output <- "C:/Users/eag873/Documents/CAUL/Model intercomparison/paper drafts/AQ/stats"
#dir_figures_paper <- "C:/Users/eag873/Documents/GitHub/AQ_paper/Figures_AQ_paper/"
dir_figures <- "C:/Users/eag873/Documents/CAUL/Model intercomparison/paper drafts/AQ/Figures"

#get data in 
load(paste0(dir_obs,"OEH_obs.RData")) 

#load in model data
load(paste0(dir_mod,"/models.RData"))
#load in site information
load(paste0(dir_mod,"/site_info.RData"))

#select AQ model results 
aq_models <- subset(models, data_source != "W-A11")

OBS <- oeh_obs
#select sites 
site_list_aq <- levels(as.factor(OBS$site)) #to select only OEH sites 
site_list_aq <- site_list_aq[-c(9,17:18)] #remove Macarthur (SPS1 only), Warrawong and Westmead (SPS1 and 2 only)

OBS <- subset(OBS, site %in% site_list_aq)

#remove negative values in the obs - replacing them by zeros 
ids <- which(OBS$O3 < 0) #only 157 values, does not affect the mean 
OBS$O3[ids] <- 0
id <- which(OBS$NOx < 0) 
OBS$NOx[id] <- 0 #>1300 values, increases mean by 0.05 ppb 
id <- which(OBS$NO2 < 0) #>1400  values 
OBS$NO2[id] <- 0  #increases mean by 0.053 ppb 
id <- which(OBS$NO < 0) #>6350  values 
OBS$NO[id] <- 0  
#NO, NO2, CO, PM2.5 all have negative values, but this analysis only needs NOx, O3 (maybe NO2?)

campaigns <- c("MUMBA", "SPS1", "SPS2")

model_aq <- subset(aq_models, site %in% site_list_aq)

species_list_aq <- c("O3","NOx", "NO2","NO")#, "PM2.5","PM10","CO", "SO2", "NH4", "NO3", "SO4", "EC", "ws", "temp", "HCHO", "C5H8")
#species_list_o3 <-  c("O3","NOx", "ws","temp")
param_list <- c("date", "site", "campaign", "data_source", species_list_aq)  #complete list of things to keep from model runs 

stat_list <- c("MB", "NMB", "NMGE", "r") #list of stats for plotting - not sure about these just yet


model_aq <- model_aq[,param_list]

#make the datasets needed (hourly, 4-hourly rolling mean, daily max )
#merge obs and model output into wide format 
aq <- merge(OBS, model_aq, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#melt OBS before adding data_source to OBS dataframe:
melted_OBS <- melt(OBS, id = c("date", "site", "campaign"), value.name = "obs")

OBS4 <-  rollingMean(OBS, pollutant = "O3", width = 4, align = "right", data.thresh = 75, new.name = "O3.roll4")
#ids <- which(OBS4$O3.roll4 > 80) #4 hour standard exceeded ONCE only  
#sort(OBS4$date[ids]) #all during MUMBA # do the dates match? YES
#OBS4$site[ids] # same sites 
#so there is something here to compare to

model_aq4 <- rollingMean(model_aq, pollutant = "O3", width = 4, align = "right", data.thresh = 75, new.name = "O3.roll4")

aq4 <- merge(OBS4, model_aq4, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)

#merge obs and model output into long format 
OBS$data_source <- "OBS"
aq_ln <- rbind.fill(OBS, model_aq)
OBS4$data_source <- "OBS"
aq_ln4 <- rbind.fill(OBS4, model_aq4)

#make daily averages 
aq_daily <- data.frame(timeAverage(aq, avg.time = "1 day", type = c("site", "data_source", "campaign")))
aq_ln_daily <- data.frame(timeAverage(aq_ln, avg.time = "1 day", type = c("site", "data_source", "campaign")))
daily_aq_ln <- data.frame(timeAverage(aq_ln, avg.time = "1 day", type = c("data_source", "campaign"))) #for overall daily timeseries
daily_aq <- data.frame(timeAverage(aq, avg.time = "1 day", type = c("data_source", "campaign")))

#make daily max 
aq_daily_max <- data.frame(timeAverage(aq, avg.time = "1 day", statistics = "max", type = c("site", "data_source", "campaign")))
aq_ln_daily_max <- data.frame(timeAverage(aq_ln, avg.time = "1 day",statistics = "max", type = c("site", "data_source", "campaign")))
max_daily_aq_ln <- data.frame(timeAverage(aq_ln, avg.time = "1 day", statistics = "max", type = c("data_source", "campaign"))) #for overall daily timeseries
max_daily_aq <- data.frame(timeAverage(aq, avg.time = "1 day", statistics = "max", type = c("data_source", "campaign")))


#for plotting 
source(paste0(dir_code,"/lattice_plot_settings_aq.R"))
#source(paste0(dir_code, "/mod_TaylorDiagram.R"))
source(paste0(dir_code, "/GoogleMaps_support.R"))
myKey_aq <- list(column = 4, space = "top", text = list(c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours_aq))
resolution = 600

#for stats 

source(paste0(dir_code,"/makeStats_functions_MSE.R"))

setwd("C:/Users/eag873/Documents/R_Model_Intercomparison")
#load("mymap2.RData")
load("mymap3.RData") #colour satellite map 
#source(paste0(dir_code, "/GoogleMaps_support_met.R"))

setwd(dir_figures)

#calc hourly stats 
species <- species_list_aq[c(1,2)] #only O3 and NOx for now
#make figure like for Khalia's paper, showing variation in stats 
#also plot maps of MB, NMB, NMGE, r 
for (k in 1:length(species)){   
  stats_name <- paste0("stats_",species[k])
  stats <- makeStats2(aq, species[k])
  write.csv(stats, file = paste0(dir_stat_output,"/", stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(aq, species[k])
  write.csv(stats, file = paste0(dir_stat_output, "/",stats_name, "_per_campaign_per_site.csv"), row.names =F)
  
  #insert bw plot here - MB, NMGE, r; 1 row/column? per campaign 
  sub_stats <- subset(stats, select = c("site", "data_source", "campaign", "NMB","NMGE","r"))
  sub_stats <- melt(sub_stats, id = c("site", "campaign", "data_source"))

  b <- bwplot(value ~data_source|variable*campaign, data = sub_stats, scales = list(y = list(relation = "free",rot = c(0,90) ), x = list(rot = c(45,0))),between = list(y = 0.5),# layout = c(3,1),
              ylab = "",as.table = T, #strip = strip.custom(factor.levels= c("MB", "R", "CRMSE")),#strip = strip.custom(factor.levels = rep(species_names[k],3))# main = species_names[k], aspect =1,
              par.settings = list(box.umbrella=list(col= "black"), 
                                  box.dot=list(col= "black"), 
                                  plot.symbol   = list(col = "black"),
                                  box.rectangle = list( col = myColours2)))
  
  png(filename = paste0(species[k], "_stat_bw_plots.png"), width = 8 * resolution, height = 6 *resolution, res = resolution )
  useOuterStrips(b)
  dev.off()  
  
  stats <- merge(stats, site_info, by = "site")
  for (m in 1:2){
    g1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m], # maptype = "roadmap", map.cols = "greyscale",
                         col = colBubble, cex = 1.5, main = "",  key = BubbleKey(stats, stat_list[m], 0), 
                         key.footer = stat_list[m], xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
    png(filename = paste(species[k], stat_list[m],"horizontal_map.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
    print(useOuterStrips(g1$plot, strip = mystrip, strip.left = mystrip))
    dev.off()
  }
  for (m in 3:4){
    g1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m], # maptype = "roadmap", map.cols = "greyscale",
                         col = "Reds", cex = 1.5, main = "",  #key = BubbleKey(stats, stat_list[m], 0), 
                         key.footer = stat_list[m], xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
    png(filename = paste(species[k], stat_list[m],"horizontal_map.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
    print(useOuterStrips(g1$plot, strip = mystrip, strip.left = mystrip))
    dev.off()
  }
}   

#calc hourly stats for daytime only (try 8:18, 10:18, 10:16?)


#calc 4-hour rolling mean stats 
