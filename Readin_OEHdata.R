#this script selects the right range of OEH obs for the intercomparison 
#modified following the fixing of date/time issues in OEH obs
#also, I think I was loading an old one - the site names are now OK straight off "oeh_data_2011_2013_long.RData" 

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
load("oeh_data_2011_2013_long.RData") #made using OEH_data_formatting

#load packages and functions
library(openair)
library(stringi)
library(plyr)


#Modify site names 
#oeh_data_2011_2013_long$site <- tolower(oeh_data_2011_2013_long$site)
#oeh_data_2011_2013_long$site <- stri_replace_all_fixed(oeh_data_2011_2013_long$site, ".", " ")
#oeh_data_2011_2013_long$site <- stri_trans_totitle(oeh_data_2011_2013_long$site)
#oeh_data_2011_2013_long$site <- stri_replace_all_fixed(oeh_data_2011_2013_long$site, " ", "_")

#select appropriate sites - use the list from site_info... 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
load("site_info.RData")
site_list <- levels(site_info$site)
oeh_data_2011_2013_long_sub <- subset(oeh_data_2011_2013_long, site %in% site_list)

#assign variables to select correct periods 
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("2013-01-01","2011-02-07", "2012-04-16")
date_end <- c("2013-02-15 23:00:00","2011-03-06 23:00:00","2012-05-13 23:00:00") 

for (i in 1:length(campaign)) {
  data <- subset(oeh_data_2011_2013_long_sub, date >= as.POSIXct(date_start[i], tz = "Etc/GMT-10") & date <= as.POSIXct(date_end[i],tz = "Etc/GMT-10"))
  data$campaign <- campaign[i]
  #data$data_source <- 'OBS'
  #names(data)[c(7,8,10,12,13,15)] <- c("NOx", "O3", "PM2.5", "temp", "wd", "ws")
  data <- transform(data, CO = CO*1000) #not done in OEH_data_formatting
  
#need to add Westmead manually for SPS1 and SPS2 - units already in ppb (inc. CO)
  if (i >= 2 & i <= 3 ) {
  data <- subset(data, site != "Westmead")
  
  #Prepare new Westmead data 
  setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign Data/", campaign[i]))
  
  westmead <- import(file = paste0(campaign[i],"_Air_Quality_Station_Data.csv"), header.at = 3, data.at = 4, date = "Date", time = "Time", 
                         date.format = "%d/%m/%Y", time.format = "%H:%M", tz = "Etc/GMT-10", correct.time = -3600)
  
  #Fix the variable names 
  sub.data <- westmead
  sub_names <- names(sub.data)
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "CO 1h average [ppb]", "CO")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "HUMID 1h average [%]", "RH")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "NEPH 1h average [bsp]", "NEPH")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "NO 1h average [ppb]", "NO")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "NO2 1h average [ppb]", "NO2")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "NOX 1h average [ppb]", "NOx")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "OZONE 1h average [ppb]", "O3")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "PM10 1h average [µg/m³]", "PM10")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "SOLAR 1h average [W/m2]", "SWR")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "SO2 1h average [ppb]", "SO2")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "TEMP 1h average [°C]", "temp")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "WDR 1h average [°]", "wd")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "SD1 1h average [°]", "SD1")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = "WSP 1h average [m/s]", "ws")
  
  sub_names <- stri_extract_last_words(sub_names)  
  
  names(westmead) <- sub_names
  
  ##add site and data_source info 
  westmead$site <- "Westmead"
  #westmead$data_source <- "OBS"
  westmead$campaign <- campaign[i]
  westmead <- subset(westmead, select = -Time)

  ## merge with the rest 
  data <- rbind.fill(data, westmead)
}  
  #remove 2am from dataframe - calibration 
  data <- selectByDate(data, hour = c(0,2:23)) 
  #save data under a another name 
  dataframe_name <- paste0("oeh_obs_",campaign[i]) 
  assign(dataframe_name,data)
}

oeh_obs <- rbind.fill(oeh_obs_MUMBA, oeh_obs_SPS1, oeh_obs_SPS2)

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/")
save(oeh_obs, oeh_obs_MUMBA, oeh_obs_SPS1, oeh_obs_SPS2, file = "OEH_obs.RData")



#for Yang Zhang - make replacement individual csv files 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/OEH data")
write.csv(oeh_obs_MUMBA_up, file = "OEH_data_MUMBA.csv", row.names = F)
write.csv(oeh_obs_SPS1, file = "OEH_data_SPS1.csv", row.names = F)
oeh_obs_SPS2_up <- subset(oeh_obs_SPS2_up, select = -SWR)
names(oeh_obs_SPS2_up)
write.csv(oeh_obs_SPS2_up, file = "OEH_data_SPS2.csv", row.names = F)

#for Steve 
write.csv(bom_data_mumba, row.names = F, file = "BOM_data_MUMBA.csv")
