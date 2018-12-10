#PM2.5 analysis 

#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"

#load in OEH observations  
load(paste0(dir_obs,"OEH_obs.RData")) 

#load in prepared model data
load(paste0(dir_mod,"/models.RData"))
#load site lat lon
load(paste0(dir_mod,"/site_info.RData"))

aq_models <- subset(models, data_source != "W-A11")

#assign variables #should I recycle OBS or use something else? 
OBS <- oeh_obs
site_list_pm <- c("Chullora","Earwood", "Liverpool", "Richmond", "Wollongong")
site_list_pm_SPS2 <- c("Chullora","Earwood", "Wollongong") #not strictly necessary at this stage - it will be for error apportionment

PM_OBS <- subset(OBS, site %in% site_list_pm)

species_list_pm <-  c("PM2.5") #what else?
param_list_pm <- c("date", "site", "campaign", "data_source", species_list_pm)  #complete list of things to keep from model runs 

model_pm <- model_aq[,param_list_pm] #I think this var name is free 

pm <- merge(PM_OBS, model_pm, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#pm is already taken... OK, change pm to pmc in PM25_components 


PM_OBS$data_source <- "OBS"
pm_ln <- rbind.fill(OBS, model_pm)


#make daily averages 
#one _per_site the other one _per_domain (for plots)

pm_daily <- data.frame(timeAverage(pm, avg.time = "1 day", type = c("site", "data_source", "campaign"))) #for stats
#daily_pm <- data.frame(timeAverage(aq, avg.time = "1 day", type = c("data_source", "campaign")))
#pm_ln_daily <- data.frame(timeAverage(aq_ln, avg.time = "1 day", type = c("site", "data_source", "campaign")))

daily_pm_ln <- data.frame(timeAverage(aq_ln, avg.time = "1 day", type = c("data_source", "campaign"))) #for overall daily timeseries

#make hourly panel, daily panel 
#calc stats







