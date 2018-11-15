#to make an example plot - load in model and OBS
library(latticeExtra)
library(openair)
library(plyr)

dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
dir_stat_output <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis/"
dir_figures <- "C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis/"

#load obs 
load(paste0(dir_obs,"/BOM_pblh.RData"))

#load models 
load(paste0(dir_mod,"cmaq_pblh.RData"))
load(paste0(dir_mod,"ansto_pblh.RData"))
load(paste0(dir_mod,"csiro_pblh.RData"))
load(paste0(dir_mod,"yz_pblh.RData"))
load(paste0(dir_mod,"oeh_pblh.RData"))
load(paste0(dir_mod,"wrf_chem_pblh.RData"))
#merge models 

mod_pblh <- rbind(cmaq_pblh, csiro_pblh, ansto_pblh, yz_pblh, oeh_pblh, wrf_chem_pblh)
timeVariation(mod_pblh, pollutant = "pblh", type = "campaign", group = 'data_source', ci = F, local.tz = "Etc/GMT-10")
scatterPlot(subset(mod_pblh, campaign %in% "MUMBA"), x = "date", y = "pblh", group = "data_source", plot.type = "l" )
scatterPlot(selectByDate(mod_pblh, start = "17/01/2013", end = "19/01/2013"), x = "date", y = "pblh", group = "data_source", plot.type = "l", cols = myColours2 )

##############
#weird spikes at 4 and 22? in OEH model 


source(paste0(dir_code, "lattice_plot_settings.R"))
#make a generic key for all the plots 
myKey <- list(column = 4, space = "bottom", text = list(c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours, lwd = mylineWidths), 
              title = " ", cex.title = 0.5)

#plot the diurnal variation 
a <- timeVariation(mod_pblh, pollutant = "pblh", type = "campaign", group = 'data_source', ci = F, local.tz = "Etc/GMT-10")
temp_hour <- a$data$hour
t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
              ylab ="pbl height (m)", type = "l", col = myColours, par.settings = my.settings,
              auto.key = list(column = 4, space = "top", points = F, lines = T), 
              scales = list(x = list(alternating = 1)), layout = c(3,1)
              , aspect = 1, ylim = c(0,1300))


b <- timeVariation(BOM_pblh, pollutant = "pblh", type = "campaign")
bom_summary <- b$data$hour
bom_summary <- na.omit(bom_summary)
bom_summary <- bom_summary[-3,]

t2 <- xyplot(Mean ~hour|campaign, data = bom_summary, col = "black", pch = 16, strip = mystrip)

t1+ as.layer(t2) #this is a latticeExtra feature - nice 


#plot the same, but using the median instead 
a <- timeVariation(mod_pblh, pollutant = "pblh", type = "campaign", group = 'data_source', ci = F, local.tz = "Etc/GMT-10", statistic = "median")
temp_hour <- a$data$hour
temp_hour <-subset(temp_hour, ci %in% 0.95)
t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
              ylab ="pbl height (m)", type = "l", col = myColours, par.settings = my.settings,
              auto.key = list(column = 4, space = "top", points = F, lines = T), 
              scales = list(x = list(alternating = 1)), layout = c(3,1)
              , aspect = 1, ylim = c(0,1300))


b <- timeVariation(BOM_pblh, pollutant = "pblh", type = "campaign", statistic = "median")
bom_summary <- b$data$hour
bom_summary <- subset(bom_summary, ci %in% 0.95)
bom_summary <- bom_summary[c(1,3,5,6,8,10),]

t2 <- xyplot(Mean ~hour|campaign, data = bom_summary, col = "black", pch = 16, strip = mystrip)

t1+ as.layer(t2) #this is a latticeExtra feature - nice 


#clean up obs, add AM, PM tags - do the same for the models 
obs_am <- selectByDate(subset(BOM_pblh, campaign != "SPS2"), hour = 5) 
obs_am_SPS2 <- selectByDate(subset(BOM_pblh, campaign == "SPS2"), hour = 6) 
obs_am <- rbind(obs_am, obs_am_SPS2)       
obs_am$TOD <- "AM"

obs_pm <- selectByDate(subset(BOM_pblh, campaign != "SPS2"), hour = 14) 
obs_pm_SPS2 <- selectByDate(subset(BOM_pblh, campaign == "SPS2"), hour = 15) 
obs_pm <- rbind(obs_pm, obs_pm_SPS2)       
obs_pm$TOD <- "PM"

pblh_obs <- rbind(obs_am, obs_pm)


#select the same times for the models 
mod_am <- selectByDate(subset(mod_pblh, campaign != "SPS2"), hour = 19) #utc 
mod_am_SPS2 <- selectByDate(subset(mod_pblh, campaign == "SPS2"), hour = 20) #utc
mod_am <- rbind(mod_am, mod_am_SPS2)       
mod_am$TOD <- "AM"

mod_pm <- selectByDate(subset(mod_pblh, campaign != "SPS2"), hour = 4) 
mod_pm_SPS2 <- selectByDate(subset(mod_pblh, campaign == "SPS2"), hour = 5) 
mod_pm <- rbind(mod_pm, mod_pm_SPS2)       
mod_pm$TOD <- "PM"

pblh_mod <- rbind(mod_am, mod_pm)




#combine obs and models - wide 
#look at stats - paired 
pblh <- merge(pblh_obs, pblh_mod, by = c("date", "campaign", "TOD"), suffixes = c(".obs", ".mod"))

#make Taylor diagram 

source(paste0(dir_code, "/mod_TaylorDiagram.R"))

setwd(dir_figures)
png(filename = "Taylor_pblh_all_models_v2.png", width = 12 * 600, height = 8 * 600, res = 600)
mod_TaylorDiagram(pblh,  obs = "pblh.obs", mod = "pblh.mod", normalise = T, 
                  group = "data_source", type = c("campaign", "TOD"), cex = 1.25, 
                  annotate = "", rms.col = "grey40", cols = myColours2, key.pos = "bottom", key.columns = 4, key.title = "  ")

dev.off()





pblh_stats <- modStats(pblh, obs = "pblh.obs", mod = "pblh.mod", type = c("campaign", "data_source"))
pblh_stats_TOD <- modStats(pblh, obs = "pblh.obs", mod = "pblh.mod", type = c("campaign", "TOD", "data_source"))

#make complete stats - sd and means are especially interesting in this case 

makeStats_pblh <- function(df, species) {
  means <- ddply(df, .(data_source, campaign, TOD), numcolwise(mean), na.rm = TRUE)
  sds <- ddply(df, .(data_source, campaign, TOD), numcolwise(sd), na.rm = TRUE)
  #run basic stats 
  stats <- modStats(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), type = c("data_source", "campaign", "TOD"))
  #add mean obs and modelled values, and rename columns 
  stats <- cbind(stats, means[,grep(pattern = paste0("^",species), names(means))])
  names(stats)[grep(pattern = ".obs", names(stats))] <- "mean.obs"
  names(stats)[grep(pattern = ".mod", names(stats))] <- "mean.mod"
  #add sd of obs and modelled values and rename columns 
  stats <- cbind(stats, sds[,grep(pattern = paste0("^",species), names(sds))])
  names(stats) <- gsub(paste0(species,".obs"), "sd.obs", names(stats))
  names(stats) <- gsub(paste0(species,".mod"), "sd.mod", names(stats))
  #calc cRMS
  stats <-within(stats, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*r))

  
  return(stats)
}

stats_pblh_TOD <- makeStats_pblh(pblh, "pblh")


makeStats_pblh2 <- function(df, species) {
  means <- ddply(df, .(data_source, campaign), numcolwise(mean), na.rm = TRUE)
  sds <- ddply(df, .(data_source, campaign), numcolwise(sd), na.rm = TRUE)
  #run basic stats 
  stats <- modStats(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), type = c("data_source", "campaign"))
  #add mean obs and modelled values, and rename columns 
  stats <- cbind(stats, means[,grep(pattern = paste0("^",species), names(means))])
  names(stats)[grep(pattern = ".obs", names(stats))] <- "mean.obs"
  names(stats)[grep(pattern = ".mod", names(stats))] <- "mean.mod"
  #add sd of obs and modelled values and rename columns 
  stats <- cbind(stats, sds[,grep(pattern = paste0("^",species), names(sds))])
  names(stats) <- gsub(paste0(species,".obs"), "sd.obs", names(stats))
  names(stats) <- gsub(paste0(species,".mod"), "sd.mod", names(stats))
  #calc cRMS
  stats <-within(stats, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*r))
  
  
  return(stats)
}

stats_pblh <- makeStats_pblh2(pblh, "pblh")

setwd(dir_stat_output)
write.csv(stats_pblh_TOD, file = "stats_pblh_TOD_all_models.csv", row.names = F)
write.csv(stats_pblh, file = "stats_pblh_all_models.csv", row.names = F)
#remake the same, but using the original model PBL heights 

load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_new_new_fixed.RData"))
load(paste0(dir_mod,"/OEH_model_output2.RData"))
load(paste0(dir_mod, "/YZ.RData"))


models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model, yz_mod)

mumba_mod <- subset(models, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
sps1_mod <- subset(models, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(models, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
models <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 

orig_mod <- subset(models, site %in% "Sydney_Airport")
#westmead <- subset(sps2_mod, site %in% "Westmead")
#d <-timeVariation(westmead, pollutant = "pblh", group = 'data_source', ci = F, local.tz = "Etc/GMT-10")
#temp_hour <- d$data$hour
#xyplot(Mean ~hour, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
       ylab ="pbl height (m)", type = "l", col = myColours, par.settings = my.settings,
       auto.key = list(column = 4, space = "top", points = F, lines = T), 
       scales = list(x = list(alternating = 1)), layout = c(1,1)
       , aspect = 1, ylim = c(0,2000))

c <- timeVariation(orig_mod, pollutant = "pblh", type = "campaign", group = 'data_source', ci = F, local.tz = "Etc/GMT-10")
temp_hour <- c$data$hour
t3 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
              ylab ="pbl height (m)", type = "l", col = myColours, par.settings = my.settings,
              auto.key = list(column = 4, space = "top", points = F, lines = T), 
              scales = list(x = list(alternating = 1)), layout = c(3,1)
              , aspect = 1, ylim = c(0,2000))


b <- timeVariation(BOM_pblh, pollutant = "pblh", type = "campaign")
bom_summary <- b$data$hour
bom_summary <- na.omit(bom_summary)
bom_summary <- bom_summary[-3,]

t2 <- xyplot(Mean ~hour|campaign, data = bom_summary, col = "black", pch = 16, strip = mystrip)

t3 + as.layer(t2) #this is a latticeExtra feature - nice 
#does not look better - which is kind of a relief 

#select the same times for the models 
orig_mod_am <- selectByDate(subset(orig_mod, campaign != "SPS2"), hour = 19) #utc 
orig_mod_am_SPS2 <- selectByDate(subset(orig_mod, campaign == "SPS2"), hour = 20) #utc
orig_mod_am <- rbind(orig_mod_am, orig_mod_am_SPS2)       
orig_mod_am$TOD <- "AM"

orig_mod_pm <- selectByDate(subset(orig_mod, campaign != "SPS2"), hour = 4) 
orig_mod_pm_SPS2 <- selectByDate(subset(orig_mod, campaign == "SPS2"), hour = 5) 
orig_mod_pm <- rbind(orig_mod_pm, orig_mod_pm_SPS2)       
orig_mod_pm$TOD <- "PM"

orig_pblh_mod <- rbind(orig_mod_am, orig_mod_pm)

orig_pblh <- merge(pblh_obs, orig_pblh_mod, by = c("date", "campaign", "TOD"), suffixes = c(".obs", ".mod"))

#make Taylor diagram 

setwd(dir_figures)
#png(filename = "Taylor_pblh.png", width = 8 * 300, height = 12 * 300, res = 300)
mod_TaylorDiagram(orig_pblh,  obs = "pblh.obs", mod = "pblh.mod", normalise = T, 
                  group = "data_source.mod", type = c("campaign", "TOD"), cex = 1.25, 
                  annotate = "", rms.col = "grey40")
#dev.off()

#long - test with orig PBL heights 
pblh_obs$data_source <- "OBS"
orig_pblh_ln <- rbind.fill(orig_pblh_mod, pblh_obs)
orig_pblh_ln <- orig_pblh_ln[order(as.Date(orig_pblh_ln$date, format="%Y/%m/%d %H:%M:%S")),]

a <- xyplot(pblh ~ date|campaign + TOD, data = orig_pblh_mod, groups = data_source, type = "l",
            #xlim = list(c("01/01/2013","15/02/2013"), c("07/02/2011","06/03/2011"), c("16/04/2012","13/05/2012")),
            #ylim = list(c(0,4500)),
            scales = list(x = list(relation = "free")),
            par.settings = my.settings, auto.key = T, as.table = T)
useOuterStrips(a)


b <- xyplot(pblh ~ date|campaign + TOD, data = pblh_obs, groups = data_source, type = "p",
            #xlim = list(c("01/01/2013","15/02/2013"), c("07/02/2011","06/03/2011"), c("16/04/2012","13/05/2012")),
            #ylim = list(c(0,4500)),
            scales = list(x = list(relation = "free")),
            par.settings = my.settings, auto.key = F, as.table = T, col = "black")

useOuterStrips(a) + as.layer(b)



#long 

pblh_obs$data_source <- "OBS"
pblh_ln <- rbind.fill(pblh_mod, pblh_obs)
pblh_ln$data_source <- factor(pblh_ln$data_source, levels = c("OBS","C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2"))

b1 <- bwplot(pblh_ln$pblh ~ pblh_ln$data_source| pblh_ln$campaign + pblh_ln$TOD , as.table = T, ylab = "pbl height (m)",
             par.settings= my.settings) 
useOuterStrips(b1)




#investigate timseries 
#date_start <- c("01/01/2013","07/02/2011", "16/04/2012") #check those
#date_end <- c("15/02/2013","06/03/2011","13/05/2012")  #check those

#scatterPlot(pblh, start = date_start[j], end = date_end[j], x = "date", y = "pblh", 
 #           ylab = species_names[i], group = "data_source", type = "campaign", plot.type = "l",
 #          col = myColours, lwd = mylineWidths,lty = mylineTypes,
 #           key.position = "top", key.columns =3, key.title = "")

#still have issues with the times, etc  
pblh_ln <- pblh_ln[order(as.Date(pblh_ln$date, format="%Y/%m/%d %H:%M:%S")),]

a <- xyplot(pblh ~ date|campaign + TOD, data = pblh_mod, groups = data_source, type = "l",
       xlim = list(c(as.POSIXct("2013-01-01"),as.POSIXct("2013-02-15")), c(as.POSIXct("2011-02-07"),as.POSIXct("2011-03-06")), c(as.POSIXct("2012-04-16"),as.POSIXct("2012-05-13"))),
       #ylim = list(c(0,4500)),
       scales = list(x = list(relation = "free"), alternating =1),
       par.settings = my.settings, 
       #auto.key = list(column = 4, space = "top", points = F, lines = T), 
       key = myKey, as.table = T,
       between = list(x =0.75))
useOuterStrips(a)


b <- xyplot(pblh ~ date|campaign + TOD, data = pblh_obs, groups = data_source, type = "p",
            xlim = list(c(as.POSIXct("2013-01-01"),as.POSIXct("2013-02-15")), c(as.POSIXct("2011-02-07"),as.POSIXct("2011-03-06")), c(as.POSIXct("2012-04-16"),as.POSIXct("2012-05-13"))),
            #ylim = list(c(0,4500)),
            scales = list(x = list(relation = "free")),
            par.settings = my.settings, auto.key = F, as.table = T, col = "black", between = list(x =0.75))

setwd(dir_figures)
png(filename = "pblh_timeseries_v2.png", width = 12 * 600, height = 8 * 600, res = 600 )
useOuterStrips(a) + as.layer(b)
dev.off()

#look at hot days only - all data 
mod_pblh <- mod_pblh[order(as.POSIXct(mod_pblh$date, format="%Y-%m-%d %H:%M:%S")),]
day1 <- xyplot(pblh ~ date|campaign, data = subset(mod_pblh, date >= "2013-01-08 00:00 UTC" & date <= "2013-01-09 10:00 UTC"), groups = data_source, type = "l",
            #xlim = list(c("01/01/2013","15/02/2013"), c("07/02/2011","06/03/2011"), c("16/04/2012","13/05/2012")),
            #ylim = list(c(0,4500)),
            scales = list(x = list(relation = "free")),
            par.settings = my.settings, 
            #auto.key = list(column = 4, space = "top", points = F, lines = T), 
            key = myKey,
            as.table = T)

b <- xyplot(pblh ~ date|campaign, data = subset(pblh_obs, date >= "2013-01-08 00:00 UTC" & date <= "2013-01-09 10:00 UTC"), groups = data_source, type = "p",
            #xlim = list(c("01/01/2013","15/02/2013"), c("07/02/2011","06/03/2011"), c("16/04/2012","13/05/2012")),
            #ylim = list(c(0,4500)),
            scales = list(x = list(relation = "free")),
            par.settings = my.settings, auto.key = F, as.table = T, col = "black")

day1 + as.layer(b)




day2 <- xyplot(pblh ~ date|campaign, data = subset(mod_pblh, date >= "2013-01-18 00:00 UTC" & date <= "2013-01-19 10:00 UTC"), groups = data_source, type = "l",
               #xlim = list(c("01/01/2013","15/02/2013"), c("07/02/2011","06/03/2011"), c("16/04/2012","13/05/2012")),
               #ylim = list(c(0,4500)),
               scales = list(x = list(relation = "free")),
               par.settings = my.settings, 
               auto.key = F, # list(column = 4, space = "top", points = F, lines = T), 
               as.table = T, key = myKey)
               
b2 <- xyplot(pblh ~ date|campaign, data = subset(pblh_obs, date >= "2013-01-18 00:00 UTC" & date <= "2013-01-19 10:00 UTC"), groups = data_source, type = "p",
            #xlim = list(c("01/01/2013","15/02/2013"), c("07/02/2011","06/03/2011"), c("16/04/2012","13/05/2012")),
            #ylim = list(c(0,4500)),
            scales = list(x = list(relation = "free")),
            par.settings = my.settings, auto.key = F, as.table = T, col = "black")

day2 + as.layer(b2) 


#plot diurnal cycles only on the days with complete obs 
#date_list <- c("2011-02-07", "2011-02-08", "2011-02-09","2011-02-10","2011-02-12", "2011-02-13", "2011-02-14") 
#this is long as, and error prone... can I select the days where there are two obs... 
#pblh_obs$date_only <-  as.character(as.Date(pblh_obs$date, format = "%Y-%m-%d", tz = "Etc/GMT-10"))
s <- subset(summary(as.factor(as.Date(pblh_obs$date, format = "%Y-%m-%d", tz = "Etc/GMT-10"))),  summary(as.factor(as.Date(pblh_obs$date, format = "%Y-%m-%d", tz = "Etc/GMT-10")))== 2)
date_list <- names(s)


sub_pblh_obs <- subset(pblh_obs, as.character(as.Date(pblh_obs$date, format = "%Y-%m-%d", tz = "Etc/GMT-10")) %in% date_list) 

sub_mod_pblh <- subset(mod_pblh, as.character(as.Date(pblh_mod$date, format = "%Y-%m-%d", tz = "Etc/GMT-10")) %in% date_list) 




d <- timeVariation(sub_mod_pblh, pollutant = "pblh", type = "campaign", group = 'data_source', ci = F, local.tz = "Etc/GMT-10")
temp_hour <- d$data$hour
t3 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
              ylab ="pbl height (m)", type = "l", col = myColours, par.settings = my.settings,
              auto.key = list(column = 4, space = "top", points = F, lines = T), 
              scales = list(x = list(alternating = 1)), layout = c(3,1)
              , aspect = 1, ylim = c(0,2000))


e <- timeVariation(sub_pblh_obs, pollutant = "pblh", type = "campaign")
sub_bom_summary <- e$data$hour


t2 <- xyplot(Mean ~hour|campaign, data = sub_bom_summary, col = "black", pch = 16, strip = mystrip)

t3 + as.layer(t2) #this is a latticeExtra feature - nice 




#to make box plots of values at 6 and 15 local time, on the days with complete obs only 
sub_pblh_mod <- subset(pblh_mod, as.character(as.Date(pblh_mod$date, format = "%Y-%m-%d", tz = "Etc/GMT-10")) %in% date_list) 

sub_pblh_ln <- rbind.fill(sub_pblh_mod, sub_pblh_obs)
sub_pblh_ln$data_source <- factor(sub_pblh_ln$data_source, levels = c("OBS","C-CTM", "O-CTM", "W-A11", "W-NC1", "W-NC2", "W-UM1", "W-UM2"))


b1 <- bwplot(pblh ~ data_source|campaign + TOD , data = sub_pblh_ln, as.table = T, ylab = "pbl height (m)",
             par.settings= my.settings) 
useOuterStrips(b1)












#old code - used to make inital figures 




#Try with met-derived 
bwplot(models$pblh ~ models$data_source|models$campaign ,
       par.settings = list(box.umbrella=list(col= "black"), 
                           box.dot=list(col= "black"), 
                           plot.symbol   = list(col = "black"),
                           box.rectangle = list( col = myColours2),
                           dot.symbol = list(col = "black"))) # this is better - change the arrows to black, and the box to the model colour 
scatterPlot(subset(models, campaign %in% "MUMBA"), x = "date", y = "pblh", group = "data_source", plot.type = "l" )

#use timeVariation on each data set to get mean values for each hour of the day 
a <- timeVariation(BOM_pblh, pollutant = "pblh", type = "campaign")
bom_summary <- a$data$hour
bom_summary <- na.omit(bom_summary)
bom_summary <- bom_summary[-3,]

b<- timeVariation(cmaq_pblh, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10" ) #local tz does work
cmaq_summary <- b$data$hour

for_stats <- merge(bom_summary,cmaq_summary, by = c("variable", "hour", "campaign"), suffixes = c(".obs", ".mod"))
names(for_stats)[2] <-"HOD"
for_stats$HOD <- as.factor(for_stats$HOD)
stats_pblh <- modStats(for_stats, mod = "Mean.mod", obs = "Mean.obs", type = c("campaign", "HOD"))

#OK, now plot all three 
bom_summary$data_source <- "OBS"
cmaq_summary$data_source <- "CMAQ"

c <- timeVariation(Syd_cmaq, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10")
Syd_cmaq_summary <- c$data$hour
Syd_cmaq_summary$data_source <- "CMAQ TKE"

#use mean hourly values to make xy.plots 
hourly_pblh <- rbind(bom_summary, cmaq_summary, Syd_cmaq_summary)


library(lattice)

original.settings <- trellis.par.get()
my.settings <- trellis.par.get()
names(my.settings)
my.settings$superpose.line$col = c("#1B9E77","dark gray", "black" )  
my.settings$superpose.line$lty = c(1,3,1)
my.settings$superpose.line$lwd = c(1,1,1)
my.settings$superpose.polygon$col = c( "#1B9E77","dark gray", "black") 
my.settings$superpose.symbol$col = c( "#1B9E77","dark gray", "black") 
my.settings$superpose.symbol$pch = c(20:20:16)
my.settings$strip.background$col <- "white"
#my.settings$dot.symbol$pch <- c(20:21)
#my.settings$plot.line <- c("l", "p")

xyplot(Mean ~hour|campaign, data = hourly_pblh, groups = data_source, 
       ylab ="pblh (m)", 
       auto.key = list(column = 2, space = "top"), 
       par.settings = my.settings
)



####################
##############
