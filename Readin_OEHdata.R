#this script selects the right range of OEH obs for the intercomparison 

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/OEH data")
load("oeh_data_2011_2013_long.RData")

#load packages and functions
library(openair)
library(stringi)
library(plyr)
#function to capitalise words 
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#Modify site names 
oeh_data_2011_2013_long$site <- tolower(oeh_data_2011_2013_long$site)
oeh_data_2011_2013_long$site <- stri_replace_all_fixed(oeh_data_2011_2013_long$site, ".", " ")
oeh_data_2011_2013_long$site <- capwords(oeh_data_2011_2013_long$site)
oeh_data_2011_2013_long$site <- stri_replace_all_fixed(oeh_data_2011_2013_long$site, " ", "_")

#select appropriate sites
site_list <- levels(site_info$site)
oeh_data_2011_2013_long_sub <- subset(oeh_data_2011_2013_long, site %in% site_list)

#assign variables to select correct periods 
campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("2012-12-21 01:00:00","2011-02-07 01:00:00", "2012-04-16 01:00:00")
date_end <- c("2013-02-16 00:00:00","2011-03-07 00:00:00","2012-05-14 00:00:00") 

for (i in 1:length(campaign)) {
  data <- subset(oeh_data_2011_2013_long_sub, date >= date_start[i] & date <=date_end[i] )
  data$campaign <- campaign[i]
  data$data_source <- 'OBS'
  names(data)[c(7,8,10,12,13,15)] <- c("NOx", "O3", "PM2.5", "temp", "wd", "ws")
 
#need to add Westmead manually for SPS1 and SPS2 
  if (i >= 2 & i <= 3 ) {
  data <- subset(data, site != "Westmead")
  
  #Prepare new Westmead data 
  setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign Data/", campaign[i]))
  
  westmead <- import(file = paste0(campaign[i],"_Air_Quality_Station_Data.csv"), header.at = 3, data.at = 4, date = "Date", time = "Time", 
                         date.format = "%d/%m/%Y", time.format = "%H:%M", tz = "Etc/GMT-10")
  
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
  westmead$data_source <- "OBS"
  westmead$campaign <- campaign[i]
  westmead <- subset(westmead, select = -Time)

  ## merge with the rest 
  data <- rbind.fill(data, westmead)
}  
  dataframe_name <- paste0("oeh_",campaign[i]) 
  assign(dataframe_name,data)
}

oeh_data <- rbind.fill(oeh_MUMBA, oeh_SPS1, oeh_SPS2)

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/")
save(oeh_data, oeh_MUMBA, oeh_SPS1, oeh_SPS2, file = "OEH_obs.RData")
