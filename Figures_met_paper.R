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
load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_new_new_fixed.RData"))
load(paste0(dir_mod,"/OEH_model_output2.RData"))
load(paste0(dir_mod, "/YZ.RData"))
#load in coordinates of all sites 
load(paste0(dir_mod,"/site_info.RData"))

#assign variables
BOM <- bom_data_all_campaigns
site_list <- levels(as.factor(BOM$site)) #to select only BOM sites 
site_list <- site_list[-7] #removing Williamtown - outside of domain? figures v4

species_list <- c("temp", "W", "ws","u10", "v10","wd","RH", "prcp", "pblh", "SWR", "pres") #met variables we are interested in 
species_list_2 <- c("temp", "W", "ws", "u10", "v10") #reduced list of variables -to plot (order matters, need to match labels in species_names)
species_names <- c(expression("Temperature (" * degree * "C)"),   expression("water mixing ration (g kg"^-1 *")"), expression("wind speed (m s"^-1 *")"), expression("u wind (m s"^-1 *")"), expression("v wind (m s"^-1 *")"), expression("wind direction (" * degree *")"),"RH (%)", "precipitation (mm)", "pblh (m)", "SWR", "pressure (hPa)")

param_list <- c("date", "site", "campaign", "data_source", species_list)  #complete list of things to keep from model runs 
#campaign <- c("MUMBA","SPS1", "SPS2")
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

stat_list <- c("r", "RMSE", "MB") #list of stats for plotting 


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
#TEMPERATURE, W, WS, u10, v10   
#diurnal cycles, taylorDiagrams, binned quantiles on one panel, with only one legend 
species <- c("temp", "W","ws", "u10", "v10")
y.lab <- c(expression("Mean bias ("* degree * "C)"), expression("Mean bias (g kg"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"), expression("Mean bias (m s"^-1*")"))
fig_name <- c("surface_temperature_panel", "surface_water_content_panel", "surface_wind_speed_panel", "surface_u10_panel", "surface_v10_panel")
add_line <- c(1,1,1.5, 1.5,1.5)
source(paste0(dir_code, "/mod_TaylorDiagram.R"))

for (i in 1:5){
a <- timeVariation(met_ln, pollutant = species[i], group = "data_source", type = "campaign")
temp_hour <- a$data$hour

t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
       ylab =species_names[i], type = "l", col = myColours, par.settings = my.settings,
       auto.key = list(column = 4, space = "top", points = F, lines = T), 
       scales = list(x = list(alternating = 1)), layout = c(3,1)
       , aspect = 1
       )
              
#taylor diagram 

t2 <- mod_TaylorDiagram(met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours, key = F,
                   annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)

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
 
  
setwd(dir_figures)
png(filename = paste0(fig_name[i], "_v4.png"), width = 7 * 300, height = 9 * 300, res = 300)

  print(t1, position = c(0,2/3-1/24.5,1,1), more = TRUE)
  print(t2, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE)
  print(t3, position = c(0,0,1,1/3+1/65))
  
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



source(paste0(dir_code, "/mod_TaylorDiagram.R"))

for (i in 1:2){
  a <- timeVariation(met_ln, pollutant = species[i], group = "data_source", type = "campaign")
  temp_hour <- a$data$hour
  
  t <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
                ylab =names_species[i], type = "l", col = myColours, par.settings = my.settings,
                auto.key = list(column = 4, space = "top", points = F, lines = T), 
                scales = list(x = list(alternating = 1)), ylim = c(-5, 3), layout = c(3,1)
                , aspect = 1,
               panel =function(...){  
                 panel.xyplot(...);
                 panel.abline(h = 0, col = "black", lty = 2)
               }
  )
  assign(fig_index[[i]][1], t)
  
   #taylor diagram 
  
  x <- mod_TaylorDiagram(met, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours, key = F,
                          annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  

  assign(fig_index[[i]][2], x)
  
}

setwd(dir_figures)
png(filename = paste0(fig_name, "_v5.png"), width = 14 * 600, height = 6 * 600, res = 600)

print(t1, position = c(0,0.5-1/16,0.5,1), more = TRUE)
print(t2, trellis.par.set(my.settings), position = c(0,0,0.5,0.5), more = TRUE)
print(t3, position = c(0.5,0.5-1/16,1,1), more = TRUE)
print(t4, trellis.par.set(my.settings), position = c(0.5,0,1,0.5))
dev.off()



#make total prcp plot 

setwd(dir_obs)
list.files()
load("MSWEPv1_2.RData")

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

setwd(dir_figures)
png(filename = "Total_prcp_w_MSWEP_by_site_v4.png", width = 12 * 300, height = 7 * 300, res = 300)#, type = "windows")
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

png(filename = "Total_prcp_w_MSWEP_v4.png", width = 12 * 300, height = 7 * 300, res = 300)#, type = "windows")
trellis.par.set(my.settings_prcp) 
mystrip <- strip.custom(bg ="white")
b2 <- barchart(total_prcp2$prcp~total_prcp2$data_source|total_prcp2$campaign,# group = total_prcp$data_source,
               col=myColours_prcp,
               superpose.polygon=list(col= myColours_prcp),
               ylab = "Total precipitation (mm)", ylim = c(0, max(total_prcp2$prcp, na.rm = T)+100),
               par.settings = my.settings_prcp,
               #strip.left = strip.custom(style=1, horizontal = F),
               #auto.key = list(column = 3, space = "top", text = levels(as.factor(total_prcp2$data_source))), #this works, but not really needed, the models
               par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
#print(useOuterStrips(b1, strip = mystrip, strip.left = mystrip)) #useOuterStrips ignores specified strip parameters... 
plot(b2, strip = mystrip)
dev.off() 

trellis.par.set(my.settings)
