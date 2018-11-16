#figures and analysis for ozone (AQ paper) 
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
dir_stat_output <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis/"
dir_figures_paper <- "C:/Users/eag873/Documents/GitHub/AQ_paper/Figures_AQ_paper/"
dir_figures <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/"

#load in OEH observations  
load(paste0(dir_obs,"OEH_obs.RData")) #why not "updated" ? check this 

#load in original model data
load(paste0(dir_mod,"/models.RData"))
#load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
#load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
#load(paste0(dir_mod,"/CSIRO_model_output_new_new_fixed.RData"))
#load(paste0(dir_mod,"/OEH_model_output2.RData"))
#load(paste0(dir_mod, "/YZ.RData"))
#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

aq_models <- subset(models, data_source != "W-A11")

#assign variables
OBS <- oeh_obs
site_list_aq <- levels(as.factor(OBS$site)) #to select only OEH sites 
site_list_aq <- site_list_aq[-c(9,17:18)] #remove Macarthur (SPS1 only), Warrawong and Westmead (SPS1 and 2 only)
#should I keep Westmead? + add MUMBA? 

OBS <- subset(OBS, site %in% site_list_aq)


species_list_aq <- c("O3","NO", "NO2","NOx", "PM2.5","PM10","CO", "SO2", "NH4", "NO3", "SO4", "EC", "ws", "temp", "HCHO", "C5H8")
species_list_o3 <-  c("O3","NOx", "ws","temp")
param_list <- c("date", "site", "campaign", "data_source", species_list_aq)  #complete list of things to keep from model runs 

#campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "NMB", "MB", "FAC2") #list of stats for plotting - not sure about these just yet


#combine all original model data 
#models <- rbind.fill(cmaq, wrf_chem, csiro, oeh_model, yz_mod)
#select model data for BOM sites only 
model_aq <- subset(aq_models, site %in% site_list_aq)

#cut to length - some model runs were longer than the actual campaigns 
#mumba_mod <- subset(model_aq, campaign %in% "MUMBA")
#mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00")
#sps1_mod <- subset(model_aq, campaign %in% "SPS1")
#sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
#sps2_mod <- subset(model_aq, campaign %in% "SPS2")
#sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
#model_aq <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 

#select variables that are of interest only 
#indices <- match(species_list, names(model_met)) #not for here - for later 
model_aq <- model_aq[,param_list]

#check a few things in the OBS: 
a <-timeVariation(OBS, pollutant = "ratio", group = "campaign", type = "site")
plot(a, subset = "hour")

xyplot(ratio ~ RH|site, data = OBS, groups = campaign,
       panel = function(x,y, ...) {
         panel.xyplot(x,y,...)
         panel.abline(h =15, col = "red")
               })
ids <- which(OBS$O3 > 100) #no exceedances of the 1 hour standard
ids <- which(OBS$O3 > 80) #18! (out of 37536 obs)
OBS$date[ids] #all during MUMBA 
sort(OBS$site[ids]) #all in SW 
OBS$temp[ids] #all reasonably hot (>= 28.7)
ids <- which(OBS$O3 > 60) #165 hours above 60 ppb
OBS$date[ids] #mostly during MUMBA, 15 or so in SPS1 
summary(as.factor(OBS$site[ids])) #not all in SW - 33 in Bargo, 32 in Oakdale, a few in Chullora, Earlwood, Rozelle 
min(OBS$temp[ids]) #min temp 22.7 (quite cool)
#so there is maybe something here to compare to

##prepare dfs for analysis 
#merge obs and model output into wide format 
aq <- merge(OBS, model_aq, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#melt OBS before adding data_source to OBS dataframe:
melted_OBS <- melt(OBS, id = c("date", "site", "campaign"), value.name = "obs")

OBS4 <-  rollingMean(OBS, pollutant = "O3", width = 4, align = "right", data.thresh = 75, new.name = "O3.roll4")
ids <- which(OBS4$O3 > 80) #4 hour standard exceeded 18 times 
sort(OBS4$date[ids]) #all during MUMBA # do the dates match? YES
OBS4$site[ids] # same sites 
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


#for plotting 
source(paste0(dir_code,"/lattice_plot_settings_aq.R"))
source(paste0(dir_code, "/mod_TaylorDiagram.R"))
source(paste0(dir_code, "/GoogleMaps_support.R"))
myKey_aq <- list(column = 4, space = "top", text = list(c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours_aq))
resolution = 600



#determine VOC or NOx limited ?
c <-xyplot(HCHO ~ NO2|campaign*site, data = model_aq, par.settings = my.settings, as.table = T, groups =  ordered(model_aq$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
        panel = function(x,y, ...) {
         panel.xyplot(x,y,...)
          panel.abline(b= 1, a =0)
        # panel.lmlineq(x,y,..., r.squared = TRUE)
       })
    
useOuterStrips(c) 
#this is looking busy, but it is interesting - Yang uses afternoon values only 

model_aq_day <- selectByDate(model_aq, hour = c(0:8))
means_model_aq_day <- ddply(model_aq_day, .(data_source, campaign, site), numcolwise(mean), na.rm = TRUE)
means_model_aq_day$ratio1 <- means_model_aq_day$HCHO / means_model_aq_day$NO2
means_model_aq_day <- merge(means_model_aq_day, site_info, by = "site")

a2 <- GoogleMapsPlot(means_model_aq_day, latitude = "site_lat", longitude = "site_lon", pollutant = "ratio1",  maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1, main = "",  key = BubbleKey(means_model_aq_day, "ratio1", 1),
                     key.footer = "HCHO/NO2", xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
#png(filename = paste(species_list[k], stat_list[m],"map.png", sep = '_'), width = 6 * 300, height = 8 * 300, res = 300)
print(useOuterStrips(a2$plot, strip = mystrip, strip.left = mystrip))
#dev.off()


c <-xyplot(HCHO ~ NO2|campaign*site, data = model_aq_day, par.settings = my.settings, as.table = T, groups =  ordered(model_aq_day$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
           panel = function(x,y, ...) {
             panel.xyplot(x,y,...)
             panel.abline(b= 1, a =0)
             # panel.lmlineq(x,y,..., r.squared = TRUE)
           })

useOuterStrips(c) 

#I don't have NOy 

d <- xyplot(O3 ~ NOx|campaign*site, data = model_aq_day, par.settings = my.settings, as.table = T, groups =  ordered(model_aq_day$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
       panel = function(x,y, ...) {
         panel.xyplot(x,y,...)
         panel.abline(b= 10, a =0)
         #panel.lmlineq(x,y,..., r.squared = TRUE)
       })

useOuterStrips(d) 

d <- xyplot(O3/NOx ~date|campaign*site, data = OBS_day, subset = (site %in% site_list_aq[9:16]),
       scales = list(x = list(relation = "free")), par.settings = my.settings, col = "black",
       panel = function(x,y, ...) {
         panel.xyplot(x,y,...)
         panel.abline(h =15, col = "red")
         #panel.lmlineq(x,y,..., r.squared = TRUE)
       })
useOuterStrips(d) 

#looking for exceedances - is the threshold 100ppb? yes - so there are none 
d <- xyplot(O3 ~date|campaign*site, data = OBS_day, subset = (site %in% site_list_aq[1:8]),
            scales = list(x = list(relation = "free")), par.settings = my.settings, col = "black",
            panel = function(x,y, ...) {
              panel.xyplot(x,y,...)
              panel.abline(h =100, col = "red")
              #panel.lmlineq(x,y,..., r.squared = TRUE)
            })
useOuterStrips(d) 

OBS_day <- selectByDate(OBS, hour = c(10:16))
e <- xyplot(O3 ~ NOx|campaign*site, data = OBS_day, par.settings = my.settings, as.table = T,            
            panel = function(x,y, ...) {
              panel.xyplot(x,y,...)
              panel.abline(b= 10, a =0)
              #panel.lmlineq(x,y,..., r.squared = TRUE)
            })

useOuterStrips(e) 

#OBS_day$ratio <- OBS_day$O3 / OBS_day$NOx

means_OBS_day <-  ddply(OBS_day, .(data_source, campaign, site), numcolwise(mean), na.rm = TRUE)
means_OBS_day$ratio <- means_OBS_day$O3 / means_OBS_day$NOx
means_OBS_day <- merge(means_OBS_day, site_info, by = "site")  


a1 <- GoogleMapsPlot(means_OBS_day, latitude = "site_lat", longitude = "site_lon", pollutant = "ratio",  maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 2, main = "", key = BubbleKey(means_OBS_day, "ratio", 15),
                     key.footer = "O3/NOx*", xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
#png(filename = paste(species_list[k], stat_list[m],"map.png", sep = '_'), width = 6 * 300, height = 8 * 300, res = 300)
print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
#dev.off()


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

#panel plot - O3, NOx only - 

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
  
  
  png(filename = paste0(fig_name[1], "_v1.png"),  width = 7 * resolution, height = 9 * resolution, res = resolution)
  
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

#this is to make "overall" (domain-averaged) plots in which each data point represent an hour of the day) 
for (k in 1:length(species_list_o3)) {
  
  stats <- modStats(aq, obs = paste0(species_list_o3[k],".obs"), mod = paste0(species_list_o3[k],".mod"), type = c("campaign", "data_source", "HOD"))
  write.csv(stats, file = paste0(dir_stat_output, "HOD_stats_", species_list_o3[k], ".csv"), row.names = F)
  
  stat_list <- c("MB", "NMB")
  setwd(dir_figures)
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
stat_ws <- read.csv(file.choose(), header = TRUE)
stat_temp <- read.csv(file.choose(), header = TRUE)

stat_HOD <- merge(stat_o3, stat_NOx, by = c("campaign", "data_source", "HOD"), suffixes = c(".o3", ".nox"))

library(latticeExtra)


a <- xyplot(MB.o3 ~MB.nox|campaign*data_source,  data = stat_HOD, as.table = T,
       panel = function(x,y, ...) {
         panel.xyplot(x,y,...)
         panel.lmlineq(x,y,..., r.squared = TRUE)
       })

useOuterStrips(a, strip = mystrip, strip.left = mystrip)

#this is interesting - to be continued 

stat_HOD_2 <- merge(stat_o3, stat_ws, by = c("campaign", "data_source", "HOD"), suffixes = c(".o3", ".ws"))

b <- xyplot(MB.o3 ~MB.ws|campaign*data_source,  data = stat_HOD_2, as.table = T,
            panel = function(x,y, ...) {
              panel.xyplot(x,y,...)
              panel.lmlineq(x,y,..., r.squared = TRUE)
            })

useOuterStrips(b, strip = mystrip, strip.left = mystrip)


stat_HOD_3 <- merge(stat_o3, stat_temp, by = c("campaign", "data_source", "HOD"), suffixes = c(".o3", ".temp"))

b <- xyplot(MB.o3 ~MB.temp|campaign*data_source,  data = stat_HOD_3, as.table = T,
            panel = function(x,y, ...) {
              panel.xyplot(x,y,...)
              panel.lmlineq(x,y,..., r.squared = TRUE)
            })

useOuterStrips(b, strip = mystrip, strip.left = mystrip)




#now, make one plot for each HOD, with a data point for each site - there is a lot of variation between sites
#could also make an average one (all hours) for each site

#this is to make plots in which each data point represents a site) 
for (k in 1:length(species_list_o3)) {
  
  stats <- modStats(aq, obs = paste0(species_list_o3[k],".obs"), mod = paste0(species_list_o3[k],".mod"), type = c("campaign", "data_source", "HOD", "site"))
  write.csv(stats, file = paste0(dir_stat_output, "HOD_stats_", species_list_o3[k], "by_site.csv"), row.names = F)
  
  }


stat_o3_by_site <- read.csv(file.choose(), header = TRUE)
stat_NOx_by_site <- read.csv(file.choose(), header = TRUE)
stat_ws_by_site <- read.csv(file.choose(), header = TRUE)
stat_temp_by_site <- read.csv(file.choose(), header = TRUE)

stat_HOD_by_site <- merge(stat_o3_by_site, stat_NOx_by_site, by = c("campaign", "data_source", "HOD", "site"), suffixes = c(".o3", ".nox"))

a <- xyplot(MB.o3 ~MB.nox|campaign*data_source,  data = stat_HOD_by_site, as.table = T, 
            subset = (site %in% site_list_aq[16]), main = site_list_aq[16],
            panel = function(x,y, ...) {
              panel.xyplot(x,y,...)
              panel.lmlineq(x,y,..., r.squared = TRUE)
            })

useOuterStrips(a, strip = mystrip, strip.left = mystrip)


#subset = (HOD %in% 23),
#groups = site,


stats_o3_by_site <- read.csv(file.choose(), header = TRUE)
stats_NOx_by_site <- read.csv(file.choose(), header = TRUE)

stats_by_site <- merge(stats_o3_by_site, stats_NOx_by_site, by = c("campaign", "data_source", "site"), suffixes = c(".o3", ".nox"))

a <- xyplot(MB.o3 ~MB.nox|campaign*data_source,  data = stats_by_site, as.table = T, 
            groups = site,
            panel = function(x,y, ...) {
              panel.xyplot(x,y,...)
              panel.abline(h = 0, v = 0, lty = 3)
            })

useOuterStrips(a, strip = mystrip, strip.left = mystrip)



############
#try plotting timeseries of HCHO/NO2 and O3/NOx 

c <-xyplot(HCHO/NO2 ~date|campaign*data_source, data = model_aq, par.settings = my.settings, as.table = T, #groups =  ordered(model_aq$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
           scales = list(x = list(relation = "free")),
           panel = function(x,y, ...) {
             panel.xyplot(x,y,...)
             panel.abline(h =1)
             # panel.lmlineq(x,y,..., r.squared = TRUE)
           })
useOuterStrips(c, strip = mystrip, strip.left = mystrip)

#4-hour rolling averages - calc stats and make plots 
species_list <- "O3.roll4"
 
  stats_name <- paste0("stats_",species_list)
  stats <- makeStats1(aq4, species_list)
  write.csv(stats, file = paste0(dir_stat_output, "daily_", stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(aq4, species_list)
  write.csv(stats, file = paste0(dir_stat_output, "daily_",stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  stats <- makeStats3(aq4, species_list)
  write.csv(stats, file = paste0(dir_stat_output, "daily_" ,stats_name, "_per_campaign_per_site.csv"), row.names =F)

#the stats are good - maybe better than hourly?  r is for most models except NC1 and NC2 (SPS1 and SPS2) 
  
  species_col1 <- match(species_list, names(aq_ln4))
  

    
    t1 <- xyplot(aq_ln4[,species_col1] ~date|campaign, groups = ordered(aq_ln4$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = aq_ln4, 
                 scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey_aq, ylab = y.lab1[1], layout =c(3,1), aspect =1, between = list(x = 1) )
    t2 <- mod_TaylorDiagram(aq4, obs = paste0(species_list, ".obs"),mod = paste0(species_list, ".mod"), group = "data_source", type = "campaign", col = myColours_2_aq, key = F,
                            annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
    
    
    ###binned quantiles 
    
    species_col <- match(paste0(species_list, ".obs"), names(aq4))
    bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
    bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 
    
    x_test <- quantile(aq4[,species_col], probs = bins, na.rm = T)
    unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
    x_test <- x_test[unik] ## the values 
    bin_labels <- bin_labels[unik]
    
    aq4$bin <- cut(aq4[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
    stats_bin <- modStats(aq4, obs = paste0(species_list, ".obs"), mod = paste0(species_list, ".mod"), type = c("data_source", "campaign" ,"bin"))
    
    #plot:  
    
    t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
                 xlab = "" ,#species_names[1], 
                 type = "l", 
                 ylab = y.lab3[1],
                 auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
                 #key = myKey,
                 par.settings = my.settings,
                 scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),
                 layout = c(3,1),
                 aspect = 1,
                 panel =function(...){  
                   panel.xyplot(...);
                   panel.abline(h = c(0,add_line[1],-(add_line[1])), col = c("black","grey60", "grey60"), lty = c(2,3,3))
                   #panel.abline(h = 0, col = "black", lty = 2)                   
                   })
    
   
  
  
#plot NMB instead - very different!    
   
          t3 <- xyplot(NMB ~ bin|campaign, data = stats_bin, groups = data_source, 
             xlab = "" ,#species_names[1], 
             type = "l", 
             ylab = y.lab3[1],
             auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
             #key = myKey,
             par.settings = my.settings,
             scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),
             layout = c(3,1),
             aspect = 1,
             panel =function(...){  
               panel.xyplot(...);
               #panel.abline(h = c(0,add_line[1],-(add_line[1])), col = c("black","grey60", "grey60"), lty = c(2,3,3))
               panel.abline(h = 0, col = "black", lty = 2)                   
             })

###############

  
  
  #panel plot - O3, NOx only - by site
  
  #diurnal cycles, taylorDiagrams, binned quantiles on one panel, with only one legend 
  species <- c("O3", "NOx")
  y.lab1 <- c("Ozone (ppb)", "NOx (ppb)")
  y.lab3 <- c("Mean bias (ppb)","Mean bias (ppb)")
  fig_name <- c("ozone_panel", "NOx_panel")
  add_line <- c(2,2) #not sure if 2 is OK for NOx
  
  setwd(paste0(dir_figures, "site plots"))
  for (k in 1:length(site_list_aq)) {
    for (i in 1:2){
      a <- timeVariation(subset(aq_ln, site %in% site_list_aq[k]), pollutant = species[i], group = "data_source", type = "campaign")
      temp_hour <- a$data$hour
      
      t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
                    ylab =y.lab1[i], type = "l", col = myColours_aq, par.settings = my.settings,
                    auto.key = list(column = 4, space = "top", points = F, lines = T), 
                    scales = list(x = list(alternating = 1)), layout = c(3,1)
                    , aspect = 1
      )
      #taylor diagram 
      
      t2 <- mod_TaylorDiagram(subset(aq, site %in% site_list_aq[k]), obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours_2_aq, key = F,
                              annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
      
      ###binned quantiles 
      sub <- subset(aq, site %in% site_list_aq[k])
      species_col <- match(paste0(species[i], ".obs"), names(sub))
      bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
      bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 
      
      x_test <- quantile(sub[,species_col], probs = bins, na.rm = T)
      unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
      x_test <- x_test[unik] ## the values 
      bin_labels <- bin_labels[unik]
      
      sub$bin <- cut(sub[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
      stats_bin <- modStats(sub, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))
      
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
      
      
      
      png(filename = paste0(site_list_aq[k],fig_name[i], "_v3.png"), width = 7 * 300, height = 9 * 300, res = 300)
      
      print(t1, position = c(0,2/3-1/24.5,1,1), more = TRUE)
      print(t2, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE)
      print(t3, position = c(0,0,1,1/3+1/65))
      
      dev.off() 
      
    }
  }  

#make overall Taylor - not for paper
#melted_OBS <- melt(OBS, id = c("date", "site", "campaign"), value.name = "obs")
#melted_model_aq <- melt(model_aq, id = c("date", "site", "campaign", "data_source"), value.name = "mod")
#melted <- merge(melted_OBS, melted_model_aq, by = c("date", "site", "campaign", "variable"))


##check PM10 for SPS2 
  timeVariation(subset(aq_ln, campaign %in% "SPS2"), pollutant = "PM10", group = "data_source")
stats_pm10<-  modStats(aq, mod = "PM10.mod", obs = "PM10.obs", type = c("campaign", "data_source" ))
  