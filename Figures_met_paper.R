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
dir_figures <- "C:/Users/eag873/ownCloud/Figures_and_stats_met_paper/newMET"

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
species_names <- c(expression("Temperature (" * degree * "C)"),   expression("water mixing ratio (g kg"^-1 *")"), expression("wind speed (m s"^-1 *")"), expression("u wind (m s"^-1 *")"), expression("v wind (m s"^-1 *")"), expression("wind direction (" * degree *")"),"RH (%)", "precipitation (mm)", "pblh (m)", "SWR", "pressure (hPa)")

param_list <- c("date", "site", "campaign", "data_source", species_list)  #complete list of things to keep from model runs 

#campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "NMB", "MB") #list of stats for plotting #removed RMSE, added NMB - shows ws pattern better


#combine all original model data 
#models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model, yz_mod)
#models <- rbind.fill(wrf, cmaq, wrf_chem, csiro_newTemp, oeh_model_new_met, yz_mod)
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
y.lab <- c(expression("Mean bias ("* degree * "C)"), expression("Mean bias (g kg"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))
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
                   annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
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
   
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
              xlab = "Binned observed values" ,#species_names[1], 
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
png(filename = paste0(fig_name[i], "_v10_newMET.png"), width = 7 * resolution, height = 9 * resolution, res = resolution)

  print(t1, position = c(0,2/3-1/28,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
  print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
  print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)
  
dev.off() 

}
#not sure I like the one panel format so much... but then I would need a legend on each? 

##############
#U10 AND V10  - reduced panels, side by side 

species <- c("u10", "v10")
y.lab <- c(expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))
fig_name <- "surface_u10_and_v10_panel"
names_species <- c(expression("u wind (m s"^-1 *")"), expression("v wind (m s"^-1 *")"))
fig_index <- list(c("t1", "t2"), c("t3", "t4"))



#source(paste0(dir_code, "/mod_TaylorDiagram.R"))

for (i in 1:2){
  a <- timeVariation(met_ln, pollutant = species[i], group = "data_source", type = "campaign")
  temp_hour <- a$data$hour
  
  t <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
                ylab =names_species[i], type = "l", col = myColours, par.settings = my.settings,
                #auto.key = list(column = 4, space = "top", points = F, lines = T), 
               key = myKey, 
                scales = list(x = list(alternating = 1)), ylim = c(-5, 3), layout = c(3,1)
                , aspect = 1,
               panel =function(...){  
                 panel.xyplot(...);
                 panel.abline(h = 0, col = "black", lty = 2)
               }
  )
  assign(fig_index[[i]][1], t)
  
   #taylor diagram 
  
  x <- TaylorDiagram(met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours2, key = F,
                          annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  

  assign(fig_index[[i]][2], x)
  
}

setwd(dir_figures)
png(filename = paste0(fig_name, "_v10_newMET.png"), width = 14 * 600, height = 6 * 600, res = 600)

print(t1, position = c(0,0.5-1/16,0.5,1), more = TRUE)
print(t2, trellis.par.set(my.settings), position = c(0,0,0.5,0.5), more = TRUE)
print(t3, position = c(0.5,0.5-1/16,1,1), more = TRUE)
print(t4, trellis.par.set(my.settings), position = c(0.5,0,1,0.5))
dev.off()



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
png(filename = "Total_prcp_w_MSWEP_by_site_v10_newMET.png", width = 12 * resolution, height = 7 * resolution, res = resolution)#, type = "windows")
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

png(filename = "Total_prcp_w_MSWEP_v10_newMET.png", width = 12 * resolution, height = 7 * resolution, res = resolution)#, type = "windows")
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





daily_met_ln <- data.frame(timeAverage(met_ln, avg.time = "1 day", type = c("data_source", "campaign")))
daily_met <- data.frame(timeAverage(met, avg.time = "1 day", type = c("data_source", "campaign")))


#plot daily timeseries 
setwd(paste0(dir_figures, "/daily/"))

species <- c("temp", "W","ws", "u10", "v10")
y.lab1 <- c(expression("Temperature ("* degree * "C)"), expression("water (g kg"^-1*")"), expression("wind speed (m s"^-1*")"), expression("u10 (m s"^-1*")"), expression("v10 (m s"^-1*")"))
y.lab3 <- c(expression("Mean bias ("* degree * "C)"), expression("Mean bias (g kg"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))

fig_name <- c("surface_temperature_panel", "surface_water_content_panel", "surface_wind_speed_panel", "surface_u10_panel", "surface_v10_panel")
species_col1 <- match(species, names(daily_met_ln))

for (i in 1:length(species)) {

t1 <- xyplot(daily_met_ln[, species_col1[i]] ~date|campaign, groups = data_source, data = daily_met_ln, 
       scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey, ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )
t2 <- TaylorDiagram(daily_met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours2, key = F,
                       annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)


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

t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
             xlab = "Binned observed values" ,#species_names[1], 
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


png(filename = paste0("daily_",fig_name[i], "_newMET.png"),  width = 7 * resolution, height = 9 * resolution, res = resolution)

print(t1, position = c(0,2/3,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)


dev.off() 
} 


#bubble plot of ws, MB ##hmmm tried this again on Sept 5 2018 and the call failed, could not load the map, also could not applied the supplied lat lon 
#I think it is because we are missing an API key? Will have to learn to make these myself now? how do I get the map though?
#will have to rewrite this using RgoogleMaps - bubbleMap? and maybe use OSM instead of google 
#Actually, none of the mapping packages work today - the urls won't load
#I border authenticated but it did not help
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison")
load("mymap.RData")

source(paste0(dir_code, "/GoogleMaps_support_met.R"))
setwd(paste0(dir_figures, "bubble_plots/"))
#hourly values:
for (k in 1:length(species_list_2)){
stats <- modStats(met, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("site","data_source", "campaign"))
#merge stats with site info... (lost when applying modStats)
stats <- merge(stats, site_info, by = "site")

for (m in 1:length(stat_list)) {
#  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
#                       maptype = "roadmap", col = "jet", cex = 1.5, main = y.lab1[k], 
#                       key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],  maptype = "roadmap", map.cols = "greyscale",
                       col = colBubble, cex = 1.5, main = y.lab1[k],  key = BubbleKey(stats, stat_list[m], 0),
                       key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"),
                       map = mymap)
  
  
  png(filename = paste(species_list[k], stat_list[m],"map_v4.png", sep = '_'), width = 10 * resolution, height = 12 *resolution, res = resolution)
  print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
  dev.off()
}

}

#to try for Khalia
a2 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],  maptype = "roadmap", map.cols = "greyscale",
                     col = colBubble, cex = 1.5, main = y.lab1[k],  key = BubbleKey(stats, stat_list[m], 0),
                     key.footer = stat_list[m], xlab = "Longitude", ylab = "Latitude", type = c("data_source", "campaign"), map = mymap)
png(filename = paste(species_list[k], stat_list[m],"map_horizontal_v1.png", sep = '_'), width = 10 * resolution, height = 8 *resolution, res = resolution)
print(useOuterStrips(a2$plot, strip = mystrip, strip.left = mystrip))
dev.off()

#daily values: 
met_daily <- data.frame(timeAverage(met, avg.time = "1 day", type = c("site","data_source", "campaign")))

for (k in 1:length(species_list_2)){
  stats <- modStats(met_daily, obs = paste0(species_list_2[k],".obs"), mod = paste0(species_list_2[k],".mod"), type = c("site","data_source", "campaign"))
  #merge stats with site info... (lost when applying modStats)
  stats <- merge(stats, site_info, by = "site")
  
  for (m in 1:length(stat_list)) {
    a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list[m],
                         maptype = "roadmap", col = "jet", cex = 1, main = y.lab1[k],
                         key.footer = stat_list[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"))
    png(filename = paste("daily_",species_list[k], stat_list[m],"map.png", sep = '_'), width = 10 * resolution, height = 12 * resolution, res = resolution)
    print(useOuterStrips(a1$plot, strip = mystrip, strip.left = mystrip))
    dev.off()
  }
  
} #forgot to make the dots bigger... 



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

  
  species_col <- match("ws", names(Data2))
  d <- qq(data_type~Data2[,species_col]|model*campaign, data=Data2,
          as.table = T, col = rep(myColours2,3), cex = 0.8,
          aspect = 1, main = "", scales = list(alternating =1), par.settings = my.settings,# between = list(y =0.25, x = 0.25),
          panel=function(x, col=col,...){
            panel.qq(x,col=col[packet.number()],...) #gets color for each panel
          }
  )
  
  
  
  png(filename = paste0(species_list_2[3], "_quantile_plot_newMET.png"), width = 10 *resolution, height = 14*resolution, res = resolution)
  useOuterStrips(d)
  dev.off()






