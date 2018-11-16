#PM2.5 components - model evaluation 

library(openair)
library(plyr)

#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
dir_stat_output <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/aq_analysis/"
#dir_stat_output <- "C:/Users/eag873/ownCloud/Figures_and_stats_met_paper/stats - 2018-05-14/"
dir_figures <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/aq_analysis/"


#load in hivol data 
load(paste0(dir_obs, "/hivol_obs.Rdata"))

#load model data 
load(paste0(dir_mod,"/models.RData"))

#exclude W-A11 (no PM information)

aq_models <- subset(models, data_source != "W-A11")


#THE SELECTED MODEL TIMES ARE WRONG _ THE MODELS ARE IN UTC!!!!! ########

#need to cut models to match length of observations, and average from 5-10 and 11-19, and choose appropriate site for the campaign 
##SPS1 #need to make this uniform with the other analyses ... 

modsps1 <- subset(aq_models, campaign %in% "SPS1")
#modsps1 <- subset(modsps1, site %in% "Westmead") #select site early on or not? if not, then can use other sites to see how variable things are 
sps1am <-  selectByDate(modsps1, hour = 18:23)
sps1am <-  timeAverage(sps1am, avg.time = "day", type = c("campaign", "data_source", "site")) 
sps1am$date <- as.POSIXct(paste(sps1am$date, "18:00"))
sps1am$TOD <- "AM"

sps1pm <-  selectByDate(modsps1, hour = 0:8)
sps1pm <-  timeAverage(sps1pm, avg.time = "day", type = c("campaign", "data_source", "site")) 
sps1pm$date <- as.POSIXct(paste(sps1pm$date, "0:00"))
sps1pm$TOD <- "PM"

sps1 <- rbind(sps1am, sps1pm)
sps1_westmead <- subset(sps1, site %in% "Westmead")

##SPS2

modsps2 <- subset(aq_models, campaign %in% "SPS2")

sps2am <-  selectByDate(modsps2, hour = 19:0) #not sure this will work  
sps2am <-  timeAverage(sps2am, avg.time = "day", type = c("campaign", "data_source", "site")) 
sps2am$date <- as.POSIXct(paste(sps2am$date, "19:00"))
sps2am$TOD <- "AM"

sps2pm <-  selectByDate(modsps2, hour = 1:9)
sps2pm <-  timeAverage(sps2pm, avg.time = "day", type = c("campaign", "data_source", "site")) 
sps2pm$date <- as.POSIXct(paste(sps2pm$date, "1:00"))
sps2pm$TOD <- "PM"

sps2 <- rbind(sps2am, sps2pm)
sps2_westmead <- subset(sps2, site %in% "Westmead")


#MUMBA

modmumba <- subset(aq_models, campaign %in% "MUMBA") 
modmumba <- subset(modmumba, date >= "2013-01-21 14:00 UTC" & date <= "2013-02-15 13:00 UTC") #shorter dates to match hivol obs

mumbaam <-  selectByDate(modmumba, hour = 18:23)
mumbaam <-  timeAverage(mumbaam, avg.time = "day", type = c("campaign", "data_source", "site")) 
mumbaam$date <- as.POSIXct(paste(mumbaam$date, "18:00"))
mumbaam$TOD <- "AM"

mumbapm <-  selectByDate(modmumba, hour = 0:8)
mumbapm <-  timeAverage(mumbapm, avg.time = "day", type = c("campaign", "data_source", "site")) 
mumbapm$date <- as.POSIXct(paste(mumbapm$date, "0:00"))
mumbapm$TOD <- "PM"

mumba <- rbind(mumbaam, mumbapm)
mumba_MUMBA <- subset(mumba, site %in% "MUMBA")


pm_models <- rbind.fill(sps1,sps2,mumba)
setwd(dir_mod)
save(pm_models, file = "models_AM_PM.RData")

pm_mod <- rbind.fill(sps1_westmead, sps2_westmead, mumba_MUMBA)

#chullora <- subset(pm_models, site %in% c("Chullora", "Westmead", "Liverpool"))
#sub <- chullora[,c(param_list, "TOD")]
#means_sub <- ddply(sub, .(data_source, site, campaign, TOD), numcolwise(mean), na.rm = TRUE)
#barchart(data_source ~  EC + NO3 + SO4 + NH4|campaign + site, data= na.omit(means_sub), auto.key = TRUE, stack = T)


#
species_pm <- c("NH4", "NIT", "SO4", "EC")
species_list_aq <- c("O3","NO", "NO2","NOx", "PM2.5","PM10","CO", "SO2", "ws", "temp", "NH3")
param_list <- c("date", "site", "campaign", "data_source", species_list_aq, species_pm) 


#merge wide 
pm <- merge(hivol_obs, pm_mod, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#this is wrong, the times don't match - look at what I did for PBLH

#taylor diagrams and stats 
#this does not work - I removed "data_source from id lisit in melted_obs, did not fix it 
melted_obs <- melt(hivol_obs, id = c("sample","date",  "campaign", "TOD"), value.name = "obs")
melted_pm_mod <- melt(pm_mod[, c(param_list, "TOD")], id = c("date", "site", "campaign", "data_source", "TOD"), value.name = "mod")
melted <- merge(melted_obs, melted_pm_mod, by = c("date", "campaign", "variable", "TOD"))


#for plotting 
source(paste0(dir_code,"/lattice_plot_settings_aq.R"))
source(paste0(dir_code, "/mod_TaylorDiagram.R"))

setwd(dir_figures)
png(filename = "Taylor_pm_components.png", width = 8 * 300, height = 12 * 300, res = 300)
mod_TaylorDiagram(melted, obs = "obs", mod = "mod", normalise = T, 
              group = "variable", type = c("campaign", "data_source"), cex = 0.95, 
              annotate = "", rms.col = "grey40")
dev.off()


#calculate stats -paired so doesn't matter if extra sites 
source(paste0(dir_code,"/makeStats_functions.R"))

for (k in 1:length(species_pm)){   #does not work
  stats_name <- paste0("stats_",species_pm[k])
  #stats <- makeStats1(pm, species_pm[k])
  #barchart(r ~ data_source, data = stats, main = species_pm[k])
  #write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg.csv"), row.names =F)
  stats <- makeStats2(pm, species_pm[k])
  #barchart(r~ data_source|campaign, data = stats,
  #         par.settings = my.settings,
  #         par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
  write.csv(stats, file = paste0(dir_stat_output, stats_name, "_dom_avg_per_campaign.csv"), row.names =F)
  #stats <- makeStats3(pm, species_pm[k])
  #barchart(r~ site|campaign, data = stats, groups = data_source,
  #         par.settings = my.settings,
  #         par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2), 
  #         main   = species_pm[k],
  #         col=myColours_2_aq,
  #         superpose.polygon=list(col= myColours_2_aq))
  #write.csv(stats, file = paste0(dir_stat_output, stats_name, "_per_campaign_per_site.csv"), row.names =F)
}   



#merge in long format 
hivol_obs$data_source <- "OBS"
pm_ln <- rbind.fill(hivol_obs, pm_mod)



a <-   bwplot(EC ~ data_source| campaign + TOD, data = pm_ln,
       par.settings = list(box.umbrella=list(col= "black"), 
       box.dot=list(col= "black"), 
       plot.symbol   = list(col = "black"),
       box.rectangle = list( col = myColours_2_aq),
       dot.symbol = list(col = "black"))) # this is better - change the arrows to black, and the box to the model colour 
useOuterStrips(a)
#colours and order are wrong - not sure which are the obs 

bwplot(pm_ln$SO4 ~ pm_ln$data_source|pm_ln$TOD )
bwplot(pm_ln$SO4 ~ pm_ln$data_source)

b <- bwplot(pm_ln$NO3 ~ pm_ln$data_source|pm_ln$TOD + pm_ln$campaign )
useOuterStrips(b)
 bwplot(pm_ln$NO3 ~ pm_ln$data_source|pm_ln$TOD )
bwplot(pm_ln$NO3 ~ pm_ln$data_source)
bwplot(pm_ln$NO3 ~  pm_ln$data_source |pm_ln$campaign )

bwplot(pm_ln$NH4 ~ pm_ln$data_source|pm_ln$campaign + pm_ln$TOD ) #this is the one that is too high in W-UM2
bwplot(pm_ln$NH4 ~ pm_ln$data_source|pm_ln$TOD )
bwplot(pm_ln$NH4 ~ pm_ln$data_source)

bwplot(pm_ln$EC ~ pm_ln$data_source|pm_ln$TOD + pm_ln$campaign )
bwplot(pm_ln$EC ~ pm_ln$data_source|pm_ln$TOD )
bwplot(pm_ln$EC ~ pm_ln$data_source)

bwplot(pm_ln$PM2.5 ~ pm_ln$data_source|pm_ln$campaign +pm_ln$TOD)

barchart(data_source ~ EC + NO3 + SO4 + NH4|campaign, data= na.omit(pm_ln[,c(2:5,7:11)]), auto.key = TRUE, stack = T)
#this does not look quite right - need to sum/average first 

means_PM2.5 <- ddply(pm_ln, .(data_source, site, campaign, TOD), numcolwise(mean), na.rm = TRUE)
barchart(data_source ~  NO3 + SO4 + NH4|campaign + TOD, data= na.omit(means_PM2.5[,c(1:7,9)]), auto.key = TRUE, stack = T)



#DSN <- NH4 + NIT / SO4 



library(ggplot2)
#this would be using only one model, and the entire data set (only one site?)
melted_wrf_chem <- subset(melted_pm_mod, data_source %in% "W-UM2" & site %in% "MUMBA" & variable %in% c("EC","NH4", "NO3", "SO4"))

ggplot(melted_wrf_chem, aes(x = date, y = mod, fill = variable)) + geom_area(position = 'stack')

melted_UM1 <- subset(melted_pm_mod, data_source %in% "W-UM1" & site %in% "MUMBA" & variable %in% c("EC","NH4", "NO3", "SO4"))
ggplot(melted_UM1, aes(x = date, y = mod, fill = variable)) + geom_area(position = 'stack')

melted_NC1 <- subset(melted_pm_mod, data_source %in% "W-NC1" & site %in% "MUMBA" & variable %in% c("EC","NH4", "NO3", "SO4"))
ggplot(melted_NC1, aes(x = date, y = mod, fill = variable)) + geom_area(position = 'stack')

melted_pm_mumba <- subset(melted_obs, site %in% "MUMBA" & variable %in% c("EC","NH4", "NO3", "SO4", "OC"))
ggplot() + geom_area(data = melted_pm_mumba, mapping = aes(x = date, y = obs, fill = variable), position = 'stack') 

ggplot()+ geom_line(aes(x =date, y = PM2.5.obs),subset(aq, campaign %in% "MUMBA"))  + geom_area(data = melted_pm_mumba, mapping = aes(x = date, y = obs, fill = variable), position = 'stack') 
#works but ugly at the moment 

#ggplot() + geom_line(aes(x =date, y = PM2.5.obs),subset(aq, campaign %in% "MUMBA"))  + geom_line(aes(x = date, y = NH4),wrf_chem_MUMBA ) + geom_line(aes(x = date, y = NH4, colour = "blue"), cmaq_MUMBA) #not quite right









set.seed(11)
df <- data.frame(a = rlnorm(30), b = 1:10, c = rep(LETTERS[1:3], each = 10)) #looks like melted data
library(ggplot2)
ggplot(df, aes(x = b, y = a, fill = c)) + geom_area(position = 'stack')






#try with Yang's first 
modsps1 <- rbind.fill(ROMS_SPS1, WRFCHEM_SPS1)

westmead <- subset(modsps1, site %in% "Westmead")

westmeadAM <- selectByDate(westmead, hour = 5:10)
westmeadAM <- timeAverage(westmeadAM, avg.time = "day", type = c("campaign", "data_source") )
westmeadAM$date <- as.POSIXct(paste(westmeadAM$date, "5:00"))
westmeadAM$TOD <- "AM"

westmeadPM <- selectByDate(westmead, hour = 11:18)
westmeadPM <- timeAverage(westmeadPM, avg.time = "day", type = c("campaign", "data_source"))
westmeadPM$date <- as.POSIXct(paste(westmeadPM$date, "11:00"))
westmeadPM$TOD <- "PM"

westmead_mod <- rbind(westmeadAM, westmeadPM)

#########

timePlot(westmead_mod, pollutant = "SO4", type = "data_source")
sps1_hivol <- subset(hivol_obs, campaign == "SPS1")

sps1_hivol$data_source <- "OBS"


#combine mods and obs 
sps1comp <- rbind.fill(westmead_mod, sps1_hivol)

scatterPlot(sps1comp, x = "date", y = "SO4", group = "data_source", plot.type = "l")
scatterPlot(sps1comp, x = "date", y = "NH4", group = "data_source", plot.type = "l")
scatterPlot(sps1comp, x = "date", y = "NO3", group = "data_source", plot.type = "l")

scatterPlot(sps1comp, x = "date", y = "EC", group = "data_source", plot.type = "l")
scatterPlot(sps1comp, x = "date", y = "OC", group = "data_source", plot.type = "l")

#everything too low - I guess YZ PM2.5 is too low? 

boxplot(sps1comp$SO4 ~ sps1comp$data_source, main = "SO4" )

#OK, so this seems to work - sps1 only - will have to modify to 
library(lattice)
bwplot(sps1comp$SO4 ~ sps1comp$data_source|sps1comp$TOD, main = "SPS1" )
bwplot(sps1comp$SO4 ~ sps1comp$data_source, main = "SPS1" )

#need to calc stats as well 

