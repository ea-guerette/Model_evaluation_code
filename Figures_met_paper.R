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
dir_figures <- "C:/Users/eag873/ownCloud_uow/Figures_and_stats_met_paper/FINAL/"

#load in met observations from BOM 
load(paste0(dir_obs,"/BOM_data_final.RData"))

#load in original model data
load(paste0(dir_mod,"/models.RData"))
#load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
#load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
#load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
#load(paste0(dir_mod,"/CSIRO_model_output_new_new_new_fixed.RData"))
#load(paste0(dir_mod,"/CSIRO_model_output_newTemp.RData"))
#load(paste0(dir_mod,"/OEH_model_output2.RData"))
#load(paste0(dir_mod,"/OEH_model_output_newMET.RData"))
#load(paste0(dir_mod, "/YZ.RData"))

#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

#assign variables
BOM <- bom_data_all_campaigns
BOM <- subset(BOM, site != "Williamtown_RAAF")
site_list <- levels(as.factor(BOM$site)) #to select only BOM sites 
site_list <- site_list[-7] #removing Williamtown - outside of domain? figures v4 and up


species_list <- c("temp", "W", "ws","u10", "v10","wd","RH", "prcp", "pblh", "SWR", "pres") #met variables we are interested in 
species_list_2 <- c("temp", "W", "ws", "u10", "v10") #reduced list of variables -to plot (order matters, need to match labels in species_names)
species_names <- c(expression("Temperature (" * degree * "C)"),   expression("water mixing ratio (g/kg)"), expression("wind speed (m s"^-1 *")"), expression("u wind (m s"^-1 *")"), expression("v wind (m s"^-1 *")"), expression("wind direction (" * degree *")"),"RH (%)", "precipitation (mm)", "pblh (m)", "SWR", "pressure (hPa)")

param_list <- c("date", "site", "campaign", "data_source", species_list)  #complete list of things to keep from model runs 

campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "NMB", "MB") #list of stats for plotting #removed RMSE, added NMB - shows ws pattern better
stat_list2 <- c("MB", "cRMS", "r")

#select model data for BOM sites only 
model_met <- subset(models, site %in% site_list)

#cut to length - some model runs were longer than the actual campaigns 
#mumba_mod <- subset(model_met, campaign %in% "MUMBA")
#mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
#sps1_mod <- subset(model_met, campaign %in% "SPS1")
#sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
#sps2_mod <- subset(model_met, campaign %in% "SPS2")
#sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
#model_met <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 

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
source(paste0(dir_code,"/GoogleMaps_support_met.R"))
myKey <- list(column = 4, space = "top", cex = 0.8, text = list(c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours, lwd = mylineWidths))
              #title = "", cex.title = 0.5)
resolution = 600
#################
#TEMPERATURE, W, WS, u10, v10   
#diurnal cycles, taylorDiagrams, binned quantiles on one panel, with only one legend 
species <- c("temp", "W","ws", "u10", "v10")
y.lab <- c(expression("Mean bias ("* degree * "C)"), expression("Mean bias (g/kg)"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))
fig_name <- c("surface_temperature_panel", "surface_water_content_panel", "surface_wind_speed_panel", "surface_u10_panel", "surface_v10_panel")
add_line <- c(1,1,1.5, 1.5,1.5)
#source(paste0(dir_code, "/mod_TaylorDiagram.R"))

for (i in 1:5){
a <- timeVariation(met_ln, pollutant = species[i], group = "data_source", type = "campaign", statistic = "mean")
temp_hour <- a$data$hour

t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
       ylab =species_names[i], type = "l", col = myColours, par.settings = my.settings,
       #auto.key = list(column = 4, space = "top", points = F, lines = T), 
       key = myKey,
       scales = list(x = list(alternating = 1)), layout = c(3,1)
       , aspect = 1
       )
              
#taylor diagram 

t2 <- TaylorDiagram(met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours2, key = F,
                   annotate = "", rms.col = "gray60", normalise = F, layout = c(3,1), cex = 1.6)
#mod_ doesn't work, but TaylorDiagram does - I don't know why!!! 
###binned quantiles 

species_col <- match(paste0(species[i], ".obs"), names(met))
bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 

x_test <- quantile(met[,species_col], probs = bins, na.rm = T)
unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
x_test <- x_test[unik] ## the values 
bin_labels <- bin_labels[unik]
  
met$bin <- cut(met[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
stats_bin <- modStats(met, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))
  
#plot:  
   
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = ordered(stats_bin$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
              xlab = "Quantiles of observed values" ,#species_names[1], 
              type = "l", 
              ylab = y.lab[i],
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
  print(t3 )#auto.key = list(column = 4, space = "bottom", lines = T, points = F))
 
  
setwd(dir_figures)
png(filename = paste0(fig_name[i], "_final_edited.png"), width = 7 * resolution, height = 9 * resolution, res = resolution)

  print(t1, position = c(0,2/3-1/28,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
  trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
  panel.text(0.15, 0.075, "(a)", cex = 1, font = 1)
  trellis.unfocus()
  print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
  trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
  panel.text(0.15, 0.075, "(b)", cex = 1, font = 1)
  trellis.unfocus()
  print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)
  trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
  panel.text(0.15, 0.075, "(c)", cex = 1, font = 1)
  trellis.unfocus()
  
dev.off() 

}
#not sure I like the one panel format so much... but then I would need a legend on each? 

##############
#U10 AND V10  - reduced panels, side by side 

#species <- c("u10", "v10")
#y.lab <- c(expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))
#fig_name <- "surface_u10_and_v10_panel"
#names_species <- c(expression("u wind (m s"^-1 *")"), expression("v wind (m s"^-1 *")"))
#fig_index <- list(c("t1", "t2"), c("t3", "t4"))



#source(paste0(dir_code, "/mod_TaylorDiagram.R"))

#for (i in 1:2){
#  a <- timeVariation(met_ln, pollutant = species[i], group = "data_source", type = "campaign")
#  temp_hour <- a$data$hour
#  
#  t <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
#                ylab =names_species[i], type = "l", col = myColours, par.settings = my.settings,
                #auto.key = list(column = 4, space = "top", points = F, lines = T), 
#               key = myKey, 
#               scales = list(x = list(alternating = 1)), ylim = c(-5, 3), layout = c(3,1)                , aspect = 1,
        #       panel =function(...){  
        #         panel.xyplot(...);
        #         panel.abline(h = 0, col = "black", lty = 2)
        #       }
#  )
#  assign(fig_index[[i]][1], t)
  
   #taylor diagram 
  
#  x <- TaylorDiagram(met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours2, key = F,
#                          annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  

#  assign(fig_index[[i]][2], x)
  
#}

#setwd(dir_figures)
#png(filename = paste0(fig_name, "_v10_newMET.png"), width = 14 * 600, height = 6 * 600, res = 600)

#print(t1, position = c(0,0.5-1/16,0.5,1), more = TRUE)
#print(t2, trellis.par.set(my.settings), position = c(0,0,0.5,0.5), more = TRUE)
#print(t3, position = c(0.5,0.5-1/16,1,1), more = TRUE)
#print(t4, trellis.par.set(my.settings), position = c(0.5,0,1,0.5))
#dev.off()



#make total prcp plot 

setwd(dir_obs)
list.files()
load("MSWEPv1_2.RData")

names(mswep)
attributes(mswep$date)
head(mswep)

grid_prcp <- subset(mswep, site %in% site_list)


prcp_ln <- rbind.fill(BOM, grid_prcp)
#make daily averages
prcp_ln_daily <- timeAverage(prcp_ln, avg.time = "1 day", statistic = "sum", type = c("data_source", "site","campaign"))         

#make daily averages of model output 
model_met_daily <- timeAverage(model_met, avg.time = "1 day", statistic = "sum", type = c("data_source", "site","campaign")) 


#combine both 
prcp_daily <- rbind.fill(prcp_ln_daily, model_met_daily)

sums <- ddply(prcp_daily, .(site, campaign, data_source), numcolwise(sum), na.rm = TRUE)
total_prcp <- subset(sums, select = c("site", "campaign", "data_source", "prcp"))

myColours_prcp <- c("grey80","#000000", myColours2)
mystrip <- strip.custom(bg ="white")
library(lattice)
library(latticeExtra)
my.settings_prcp <- my.settings
my.settings_prcp$superpose.polygon$col <- myColours_prcp


#trying a daily sum precipitation plot 
xyplot(prcp ~date|campaign, groups = data_source, data = subset(prcp_daily, site %in% "Bankstown_Airport"), col = myColours_prcp,
       scales = list(x = list(relation = "free")),  type = "l", layout =c(3,1), aspect =1, between = list(x = 1), par.settings = my.settings_prcp )
#ugly, would need fixing and a legend 




setwd(dir_figures)
png(filename = "Total_prcp_w_MSWEP_by_site_final.png", width = 12 * resolution, height = 7 * resolution, res = resolution)#, type = "windows")
#trellis.par.set(my.settings) 
mystrip <- strip.custom(bg ="white")
b1 <- barchart(total_prcp$prcp~total_prcp$site|total_prcp$campaign, group = total_prcp$data_source,
               col=myColours_prcp,
               superpose.polygon=list(col= myColours_prcp),
               ylab = "Total precipitation (mm)", ylim = c(0, 800),
               par.settings = my.settings_prcp,
               #strip.left = strip.custom(style=1, horizontal = F),
               auto.key = list(column = 4, space = "top"), 
               par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
#print(useOuterStrips(b1, strip = mystrip, strip.left = mystrip)) #useOuterStrips ignores specified strip parameters... 
plot(b1, strip = mystrip)
dev.off() 
trellis.par.set(my.settings)

#make the same plot, but not showing individual sites (models should be one bar only for each campaign)
sums2 <- ddply(prcp_daily, .(campaign, data_source), numcolwise(sum), na.rm = TRUE)
total_prcp2 <- subset(sums2, select = c("campaign", "data_source", "prcp"))

png(filename = "Total_prcp_w_MSWEP_final.png", width = 12 * resolution, height = 7 * resolution, res = resolution)#, type = "windows")
trellis.par.set(my.settings_prcp) 
mystrip <- strip.custom(bg ="white")
b2 <- barchart(total_prcp2$prcp~total_prcp2$data_source|total_prcp2$campaign,# group = total_prcp$data_source,
               col=myColours_prcp,
               superpose.polygon=list(col= myColours_prcp),
               ylab = "Total precipitation (mm)", ylim = c(0, max(total_prcp2$prcp, na.rm = T)+100),
               par.settings = my.settings_prcp,  scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
               #strip.left = strip.custom(style=1, horizontal = F),
               #auto.key = list(column = 3, space = "top", text = levels(as.factor(total_prcp2$data_source))), #this works, but not really needed, the models
#print(useOuterStrips(b1, strip = mystrip, strip.left = mystrip)) #useOuterStrips ignores specified strip parameters... 
plot(b2, strip = mystrip)
dev.off() 

trellis.par.set(my.settings)


#make daily means plots: 

daily_met_ln <- data.frame(timeAverage(met_ln, avg.time = "1 day", type = c("data_source", "campaign")))
daily_met <- data.frame(timeAverage(met, avg.time = "1 day", type = c("data_source", "campaign")))

summary(models$wd, na.rm= T) #why negative wd??
#plot daily timeseries 
setwd(paste0(dir_figures))

species <- c("temp", "W","ws", "u10", "v10")
y.lab1 <- c(expression("Temperature ("* degree * "C)"), expression("water (g/kg)"), expression("wind speed (m s"^-1*")"), expression("u10 (m s"^-1*")"), expression("v10 (m s"^-1*")"))
y.lab3 <- c(expression("Mean bias ("* degree * "C)"), expression("Mean bias (g/kg)"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))

fig_name <- c("surface_temperature_panel", "surface_water_content_panel", "surface_wind_speed_panel", "surface_u10_panel", "surface_v10_panel")
species_col1 <- match(species, names(daily_met_ln))

for (i in 1:length(species)) {

t1 <- xyplot(daily_met_ln[, species_col1[i]] ~date|campaign, groups = ordered(daily_met_ln$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = daily_met_ln, 
       scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey, ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )
t2 <- TaylorDiagram(daily_met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours2, key = F,
                       annotate = "", rms.col = "gray60", normalise = F, layout = c(3,1), cex = 1.6)


###binned quantiles 

species_col <- match(paste0(species[i], ".obs"), names(met))
bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 

x_test <- quantile(daily_met[,species_col], probs = bins, na.rm = T)
unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
x_test <- x_test[unik] ## the values 
bin_labels <- bin_labels[unik]

daily_met$bin <- cut(daily_met[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
stats_bin <- modStats(daily_met, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))

#plot:  

t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = ordered(stats_bin$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
             xlab = "Quantiles of observed values" ,#species_names[1], 
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


png(filename = paste0("daily_",fig_name[i], "_final_edited.png"),  width = 7 * resolution, height = 9 * resolution, res = resolution)
#added "letters" but not tested 
print(t1, position = c(0,2/3,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
panel.text(0.125, 0.075, "(a)", cex = 1, font = 1)
trellis.unfocus()
print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
panel.text(0.125, 0.075, "(b)", cex = 1, font = 1)
trellis.unfocus()
print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)
trellis.focus("toplevel") ## has coordinate system [0,1] x [0,1]
panel.text(0.125, 0.075, "(c)", cex = 1, font = 1)
trellis.unfocus()

dev.off() 
} 


#bubble plot of ws, MB ##hmmm tried this again on Sept 5 2018 and the call failed, could not load the map, also could not applied the supplied lat lon 
#I think it is because we are missing an API key? Will have to learn to make these myself now? how do I get the map though?
#will have to rewrite this using RgoogleMaps - bubbleMap? and maybe use OSM instead of google 
#Actually, none of the mapping packages work today - the urls won't load
#I border authenticated but it did not help
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison")
#load("mymap2.RData")
load("mymap3.RData") #colour satellite map 
#load("mymap4.RData")
#load("mymap6.RData") #too white - small biases don't show up well 
source(paste0(dir_code, "/GoogleMaps_support_met.R"))
setwd(paste0(dir_figures))#, "bubble_plots/"))

#only using the MB one... so 
stat_list <- "MB"

#hourly values:
for (k in 1:length(species_list_2)){
stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("site","data_source", "campaign"))
#merge stats with site info... (lost when applying modStats)
stats <- merge(stats, site_info, by = "site")

for (m in 1:length(stat_list)) {
#  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
#                       maptype = "roadmap", col = "jet", cex = 1.5, main = y.lab1[k], 
#                       key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
 # a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],  maptype = "roadmap", map.cols = "greyscale",
#                       col = colBubble, cex = 1.5, main = "",  key = BubbleKey(stats, stat_list[m], 0),
#                       key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"),
#                       map = mymap2)
  #almost - but missing bottom site? 
  
#  png(filename = paste(species_list[k], stat_list[m],"map_v4.png", sep = '_'), width = 10 * resolution, height = 12 *resolution, res = resolution)
#  print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
#  dev.off()
#}

#}

#to try for Khalia
a2 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],  #maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = y.lab1[k],  key = BubbleKey(stats, stat_list[m], 0), 
                     key.footer = stat_list[m], xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
png(filename = paste(species_list[k], stat_list[m],"new_colour_horizontal_map_final.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
print(useOuterStrips(a2$plot, strip = mystrip, strip.left = mystrip))
dev.off()
}
}


#daily values: NOT NEEDED FOR MB - will be the same!
met_daily <- data.frame(timeAverage(met, avg.time = "1 day", type = c("site","data_source", "campaign")))

for (k in 1:length(species_list_2)){
  stats <- modStats(met_daily, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("site","data_source", "campaign"))
 # merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
  for (m in 1:length(stat_list)) {
#    a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
#                         maptype = "roadmap", col = "jet", cex = 1, main = y.lab1[k],
#                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
#    png(filename = paste("daily_",species_list[k], stat_list[m],"map.png", sep = '_'), width = 10 * resolution, height = 12 * resolution, res = resolution)
#    print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
#    dev.off()
    a3 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m], # maptype = "roadmap", map.cols = "greyscale",
                         col = colBubble, cex = 1.5, main = y.lab1[k],  key = BubbleKey(stats, stat_list[m], 0), 
                         key.footer = stat_list[m], xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
    png(filename = paste("daily",species_list[k], stat_list[m],"new_colour_horizontal_map_final.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
    print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))
    dev.off()    
    
  }
  
}  



#prettier version of ws q-q plot 
setwd(dir_figures)
#try something different - one panel per parameter 
#setwd(paste0(dir_figures,"/quantile_plots/panels"))
resolution = 600
met_ln$data_type <- "OBS"
ids <- which(met_ln$data_source != "OBS") #there was a typo here ids <- ids <- which()
met_ln$data_type[ids] <- "MODEL"
met_ln$data_type <- ordered(met_ln$data_type, levels = c("OBS", "MODEL"))

model_list <- levels(as.factor(met$data_source))
#rearrange data so that each model has a set of obs
Data <- met_ln
Data2 <- NULL
for(m in 1:length(model_list)){
  Data2 <- rbind(Data2,
                 cbind(rbind(Data[Data$data_source == model_list[m],],Data[Data$data_source == 'OBS',]),
                       model_list[m]))
  
}
names(Data2)[18] <- "model"
#cols <- match(species_list_2, names(Data2))
#Data3 <- Data2[,c(1,2,12,13,14,17,18)]

#make the plots

for (i in c(1,3)) {  
  species_col <- match(species[i], names(Data2))
  d <- qq(data_type~Data2[,species_col]|model*campaign, data=Data2,
          as.table = T, col = rep(myColours2,3), cex = 0.8,
          aspect = 1, main = "", scales = list(alternating =1), par.settings = my.settings,# between = list(y =0.25, x = 0.25),
          panel=function(x, col=col,...){
            panel.qq(x,col=col[packet.number()],...) #gets color for each panel
          }
          )
  
  
  
  png(filename = paste0(species_list_2[i], "_quantile_plot_final.png"), width = 10 *resolution, height = 8*resolution, res = resolution)
  useOuterStrips(d)
  dev.off()
}


#make bw plots of stats (by site)
source(paste0(dir_code,"/makeStats_functions.R"))
library(reshape2)
#calculate stats by site, but not by campaign? or both? 
#fig_index <- c("b1", "b2", "b3", "b4","b5") 
stat_stack <- list() 
   #one plot for each species in species_list_2, MB, cRMS and r (stat_list_2)  
  for (k in 1:length(species_list_2)){   
    stats <- makeStats3(met, species_list_2[k])
    stats$species <- species_list_2[k]
    stat_stack[[k]] <- stats
  }
 stats <- do.call(rbind, stat_stack)
   # write.csv(stats, file = paste0(dir_stat_output, stats_name, "_per_campaign_per_site.csv"), row.names =F)
    sub_stats <- subset(stats, select = c("site", "data_source", "campaign", "species", "MB","r", "cRMS"))
    names(sub_stats)[c(6,7)] <- c("R", "CRMSE")
    sub_stats <- melt(sub_stats, id = c("site", "campaign", "data_source", "species"))
    sub_stats$species <- ordered(sub_stats$species, levels = c("temp", "W", "ws", "u10", "v10"))
    b <- bwplot(value ~data_source|variable*species, data = sub_stats, scales = list(y = list(relation = "free",rot = c(0,90) ), x = list(rot = c(45,0))),between = list(y = 0.5),# layout = c(3,1),
          ylab = "",as.table = T, #strip = strip.custom(factor.levels= c("MB", "R", "CRMSE")),#strip = strip.custom(factor.levels = rep(species_names[k],3))# main = species_names[k], aspect =1,
          par.settings = list(box.umbrella=list(col= "black"), 
                               box.dot=list(col= "black"), 
                               plot.symbol   = list(col = "black"),
                               box.rectangle = list( col = myColours2)))
    
 
    
          
png(filename = "stat_bw_plots_final.png", width = 8 * resolution, height = 6 *resolution, res = resolution )
useOuterStrips(b)
dev.off()  

#map of u10 at 3 pm - or something
library(dplyr)
tpm <- selectByDate(met_ln, hour = 15)
means_tpm <- ddply(tpm, .(data_source, campaign, site), numcolwise(mean), na.rm = TRUE) 
means_tpm <- merge(means_tpm, site_info, by = "site")
means_tpm$data_source <- ordered(means_tpm$data_source, levels = c("OBS", "C-CTM", "O-CTM", "A-W11", "W-NC1", "W-NC2", "W-UM1", "W-UM2"))
a3 <- GoogleMapsPlot(means_tpm, latitude = "site_lat", longitude = "site_lon", pollutant = "u10",  #maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = "",  key = BubbleKey(means_tpm, "u10", 0), 
                     key.footer = "u10 at 3 pm", xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
png(filename = paste("u10_3pm","horizontal_map_v1.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))
dev.off()


a4 <- GoogleMapsPlot(means_tpm, latitude = "site_lat", longitude = "site_lon", pollutant = "v10",  #maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = "",  key = BubbleKey(means_tpm, "v10", 0), 
                     key.footer = "v10 at 3 pm", xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
png(filename = paste("v10_3pm","horizontal_map_v1.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
print(useOuterStrips(a4$plot, strip = mystrip, strip.left = mystrip))
dev.off()

a5 <- GoogleMapsPlot(means_tpm, latitude = "site_lat", longitude = "site_lon", pollutant = "ws",  #maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = "",  key = BubbleKey(means_tpm, "ws", 4), 
                     key.footer = "ws at 3 pm", xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
#png(filename = paste("ws_3pm","horizontal_map_v1.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
print(useOuterStrips(a4$plot, strip = mystrip, strip.left = mystrip))

#not bad but wd might be better? recalc so averagins is OK 
means_tpm <- within(means_tpm, wd <- atan2(-u10, -v10) * 180 / pi)
## correct for negative wind directions
ids = which(means_tpm$wd < 0) # ids where wd < 0
means_tpm$wd[ids] = means_tpm$wd[ids] + 360

a5 <- GoogleMapsPlot(means_tpm, latitude = "site_lat", longitude = "site_lon", pollutant = "wd",  #maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = "",  key = BubbleKey(means_tpm, "wd", 180), 
                     key.footer = "wd at 3 pm", xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
#not so easy to pick differences?


#progression might be more interesting?
#maps of wd/u10/v10 at various times of day  
library(dplyr)
tms <- c(6,9,12,15,18,21)
for (i in 1:length(tms)){
tpm <- selectByDate(met_ln, hour = tms[i])
means_tpm <- ddply(tpm, .(data_source, campaign, site), numcolwise(mean), na.rm = TRUE) 
means_tpm <- merge(means_tpm, site_info, by = "site")
means_tpm$data_source <- ordered(means_tpm$data_source, levels = c("OBS", "C-CTM", "O-CTM", "A-W11", "W-NC1", "W-NC2", "W-UM1", "W-UM2"))
a3 <- GoogleMapsPlot(means_tpm, latitude = "site_lat", longitude = "site_lon", pollutant = "wd",  #maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = "",  key = BubbleKey(means_tpm, "wd", 180), 
                     key.footer = "wd", xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3)
png(filename = paste(tms[i],"wd","horizontal_map.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))
dev.off()
}

#I think it would be better to see the progression on one panel - so all hours, all models, but one campaign at a time 
for (i in 1:length(campaign)) {
  sub <- subset(met_ln, campaign == campaign[i])
  stms <- selectByDate(sub, hour = tms)
  stms$hour <- format(as.POSIXct(stms$date,format="%%Y-%m-%d H:%M:%S"),"%H")
  means_stms <- ddply(stms, .(data_source, campaign, site, hour), numcolwise(mean), na.rm = TRUE) 
  #this is wrong for wd!
  means_stms <- within(means_stms, wd <- atan2(-u10, -v10) * 180 / pi)
  ## correct for negative wind directions
  ids = which(means_stms$wd < 0) # ids where wd < 0
  means_stms$wd[ids] = means_stms$wd[ids] + 360

  means_stms$data_source <- ordered(means_stms$data_source, levels = c("OBS", "C-CTM", "O-CTM", "A-W11", "W-NC1", "W-NC2", "W-UM1", "W-UM2"))
  means_stms<- merge(means_stms, site_info, by = "site")
  a3 <- GoogleMapsPlot(means_stms, latitude = "site_lat", longitude = "site_lon", pollutant = "u10", # maptype = "roadmap", map.cols = "greyscale",
                       col = colBubble, cex = 1.5, main = campaign[i],  key = BubbleKey(means_stms, "u10", 0), as.table = T,
                       key.footer = "u10", xlab = "Longitude", ylab = "Latitude", type = c("hour","data_source"), map = mymap3)
  png(filename = paste("wind progression", campaign[i],"u10","horizontal_map.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
  print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))
  dev.off()
  }
#these still look identical - I don't understand what I'm doing wrong 

#maybe use timeVariation... but this only does one variable at a time - wd?, u10? 
hours <- c(6,9,12,15,18,21)
sp <- "u10" #"wd" still don't think wd makes sense 
#camp <- "MUMBA"
for (i in 1:length(campaign)) {
  camp <- campaign[i]
  sub <- subset(met_ln, campaign == camp) #somehow subset does not work with indices anymore...
  a <- timeVariation(sub, pollutant = sp, group = "data_source",type = "site") 
  all_hours <- a$data$hour
  sel_hours <- subset(all_hours, hour %in% hours)
  sel_hours <- merge(sel_hours, site_info, by = "site")
  sel_hours$variable <-  ordered(sel_hours$variable, levels = c("OBS", "C-CTM", "O-CTM", "A-W11", "W-NC1", "W-NC2", "W-UM1", "W-UM2"))
  a3 <- GoogleMapsPlot(sel_hours, latitude = "site_lat", longitude = "site_lon", pollutant = "Mean", # maptype = "roadmap", map.cols = "greyscale",
                       col = colBubble, cex = 1.5, main = camp,  key = BubbleKey(sel_hours, "Mean", 0), as.table = T,
                       key.footer = sp, xlab = "Longitude", ylab = "Latitude", type = c("hour","variable"), map = mymap3)
  png(filename = paste("wind progression", camp,sp,"horizontal_map_final.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
  print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))
  dev.off()
}
 

#maybe select campaign first, then make averages? 
#for (i in 1:length(campaign)){
#sub <- subset(means_stms, campaign == "SPS1")#why is this broken - it only selects MUMBA
#  a3 <- GoogleMapsPlot(sub, latitude = "site_lat", longitude = "site_lon", pollutant = "u10", # maptype = "roadmap", map.cols = "greyscale",
#                     col = colBubble, cex = 1.5, main = campaign[i],  key = BubbleKey(means_stms, "u10", 0), as.table = T,
#                     key.footer = "u10", xlab = "Longitude", ylab = "Latitude", type = c("hour","data_source"), map = mymap3)
#png(filename = paste("wind progression", campaign[i],"u10","horizontal_map.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
#print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))
#dev.off()
#}

daily_max_met <- data.frame(timeAverage(met, avg.time = "1 day",  statistic = "max", type = c("data_source", "site","campaign"))  )
daily_max_met_ln <- data.frame(timeAverage(met_ln, avg.time = "1 day",  statistic = "max", type = c("data_source", "site","campaign")) )        
#this has all sites - not ideal for plotting

#plot daily max timeseries 
setwd(paste0(dir_figures))

species <- c("temp", "W", "ws", "u10", "v10")
y.lab1 <- c(expression("max temperature ("* degree * "C)"), expression(" max water (g kg"^-1*")"), expression(" max wind speed (m s"^-1*")"), expression("max u10 (m s"^-1*")"), expression("max v10 (m s"^-1*")"))
y.lab3 <- c(expression("Mean bias ("* degree * "C)"), expression("Mean bias (g kg"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))

fig_name <- c("surface_temperature_panel", "surface_water_content_panel", "surface_wind_speed_panel", "surface_u10_panel", "surface_v10_panel")
species_col1 <- match(species, names(daily_max_met_ln))

for (i in 1:length(species)) {
  
  t1 <- xyplot(daily_max_met_ln[, species_col1[i]] ~date|campaign, groups = ordered(daily_max_met_ln$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = daily_max_met_ln, 
               scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey, ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )
  t2 <- TaylorDiagram(daily_max_met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours2, key = F,
                      annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  
  
  ###binned quantiles 
  
  species_col <- match(paste0(species[i], ".obs"), names(met))
  bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
  bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 
  
  x_test <- quantile(daily_max_met[,species_col], probs = bins, na.rm = T)
  unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
  x_test <- x_test[unik] ## the values 
  bin_labels <- bin_labels[unik]
  
  daily_max_met$bin <- cut(daily_max_met[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
  stats_bin <- modStats(daily_max_met, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))
  
  #plot:  
  
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = ordered(stats_bin$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
               xlab = "Quantiles of observed values" ,#species_names[1], 
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
  
  
  png(filename = paste0("daily_max_",fig_name[i], "v0_newMET.png"),  width = 7 * resolution, height = 9 * resolution, res = resolution)
  
  print(t1, position = c(0,2/3,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
  print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
  print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)
  
  dev.off() 
} 

for (k in 1:length(species_list_2)){
  stats <- modStats(daily_max_met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("site","data_source", "campaign"))
  # merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
  for (m in 1:length(stat_list)) {
    #    a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
    #                         maptype = "roadmap", col = "jet", cex = 1, main = y.lab1[k],
    #                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
    #    png(filename = paste("daily_",species_list[k], stat_list[m],"map.png", sep = '_'), width = 10 * resolution, height = 12 * resolution, res = resolution)
    #    print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
    #    dev.off()
    a2 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],  map.cols = "Greens",#plot.transparent = T,
                         col = colBubble, cex = 1.5, main = y.lab1[k],  key = BubbleKey(stats, stat_list[m], 0), 
                         key.footer = stat_list[m], xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap3) #, alpha = 0.5) this makes the DOTS transparent 
    png(filename = paste("daily_max",species_list[k], stat_list[m],"horizontal_map2.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
    print(useOuterStrips(a2$plot, strip = mystrip, strip.left = mystrip))
    dev.off()    
    
  }
  
} 

#try plotting timeseries of mean bias 

met$temp.bias <- met$temp.mod - met$temp.obs

t1 <- xyplot(temp.bias ~date|campaign, groups = ordered(met$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = met, 
             scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey)# #ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )

daily_max_met$temp.bias <- daily_max_met$temp.mod - daily_max_met$temp.obs
t1 <- xyplot(temp.bias ~date|campaign+site, groups = ordered(daily_max_met$data_source, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = daily_max_met, 
             scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey)# #ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )
print(useOuterStrips(t1, strip = mystrip, strip.left = mystrip))

###################
#would plotting the mean bias be clearer? 
#NO, the result is hard to interpret 
tpm_w <- selectByDate(met, hour = 15)
stats <- modStats(tpm_w, mod = "u10.mod", obs = "u10.obs", type = c("data_source", "campaign", "site")) 
stats <- merge(stats, site_info, by ="site")

a3 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = "MB",  maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = "",  key = BubbleKey(stats, "MB", 0), 
                     key.footer = "MB in u10 at 3 pm", xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap2)
print(useOuterStrips(a3$plot, strip = mystrip, strip.left = mystrip))

