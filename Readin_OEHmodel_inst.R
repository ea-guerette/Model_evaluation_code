#this script is to read in instantaneous met model output from OEH - Khalia gave me new files on March 5th 2019 

#the output contains surface temp (temp) AND temp at 2m (tscr_ave)
# surface RH (rh) and 2m RH 
# surface  and 2m q 
#winds and 10 m winds 
# rename the 2m and 10m variable so they match the ones in the other models 

#load packages
library(stringi)
library(plyr)
#assign variables

campaign <- c("SPS1", "SPS2", "MUMBA")
#start_date <- c("2011-02-06 14:00 UTC", "2012-04-15 14:00 UTC") 
#end_date <- c("2011-03-06 13:00 UTC","2012-05-13 13:00 UTC") 

#read MET files in 
for (i in 1:length(campaign)) {
#go to folder: 
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_InstTemp/InstTemp/Output_CCAM_daily3km_2011_", campaign[i], "_ReRun"))

##this reads in the list of files 
data <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE))
##this combines all the files into one (stacked on top of each other)
data <- do.call(rbind, data)


#create site vector using the names of the files in the folder 
site_name <- list.files(pattern = ".csv")
site <- sort(rep(site_name,  nrow(data)/length(site_name)))

data <- cbind(data,site)
data$data_source <- "O-CTM"
data$campaign <- campaign[i]

#calculate wd from uas and vas 
data =  within(data, wd <- atan2(-uas, -vas) * 180 / pi)
## correct for negative wind directions
ids = which(data$wd < 0) # ids where wd < 0
data$wd[ids] = data$wd[ids] + 360

#calculate ws from uas and vas 
data = within(data, ws <- sqrt(uas^2 + vas^2))

#save surface temperature under a different name
#data$surf_temp <- data$temp - 273.15
#data$surf_rh <- data$rh

#data$temp <- as.numeric(as.character(data$tscr_ave)) - 273.15
#data$RH <- as.numeric(as.character(data$rhscrn))
#data$W <- as.numeric(as.character(data$qgscrn)) *1000

#data$prcp <- as.numeric(as.character(data$rnd)) /24 #conversion to mm/hr (Khalia says original units are mm/day) 
##I do not know if this is right 
#if (i ==3) {
#data$rnd <- data$rnd /24
#}

###############


#make site names pretty

data$site <- stri_replace_all_fixed(data$site, "-CCAM-level-0.csv", "")
data$site <- stri_replace_all_fixed(data$site, "_AWS ", "") #to fix Bellambi
data$site <- stri_replace_all_fixed(data$site, "_AWS", "")
data$site <- stri_replace_all_fixed(data$site, "_AMO", "")
data$site <- stri_replace_all_fixed(data$site, "_Albion_Park", "")
levels(as.factor(data$site))
data$site <- gsub("Parramatta_north", "Westmead", data$site) 

#make Date into date
date <- stri_replace_all_fixed(data$Date, "+00:00", "")
date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
date <- date - 3600 # I suspect data is 1:24
data$Date <- date

#select variables 
#data <- data[,c(1,5:7,14,15,19:23,26,27,28)]
#rename variables 
#names(data)[c(1,3,5,6)] <- c("date", "pres","u10", "v10")

dataframe_name <- paste0("oeh_model_inst_met_",campaign[i]) 
assign(dataframe_name,data)

}

oeh_model_inst_met <- rbind.fill(oeh_model_inst_met_SPS1, oeh_model_inst_met_SPS2, oeh_model_inst_met_MUMBA)
###############
#setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
#save(oeh_model_final_met, oeh_model_final_met_SPS1,oeh_model_final_met_SPS2, oeh_model_final_met_MUMBA, file = "OEH_model_output_newMET.RData")


#read in the averaged data again - not changing variable names or units 

campaign <- c("SPS1", "SPS2", "MUMBA")
#start_date <- c("2011-02-06 14:00 UTC", "2012-04-15 14:00 UTC") 
#end_date <- c("2011-03-06 13:00 UTC","2012-05-13 13:00 UTC") 

#read MET files in 
for (i in 1:length(campaign)) {
  #go to folder: 
  setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_final_surface_MET/OEH_final_surface_", campaign[i], "_MET"))
  
  ##this reads in the list of files 
  data <- lapply(list.files(pattern = ".csv"),
                 function(.file) read.csv(.file, header = TRUE))
  ##this combines all the files into one (stacked on top of each other)
  data <- do.call(rbind, data)
  
  
  #create site vector using the names of the files in the folder 
  site_name <- list.files(pattern = ".csv")
  site <- sort(rep(site_name,  nrow(data)/length(site_name)))
  
  data <- cbind(data,site)
  data$data_source <- "O-CTM"
  data$campaign <- campaign[i]
  
  #calculate wd from uas and vas 
  data =  within(data, wd <- atan2(-uas, -vas) * 180 / pi)
  ## correct for negative wind directions
  ids = which(data$wd < 0) # ids where wd < 0
  data$wd[ids] = data$wd[ids] + 360
  
  #calculate ws from uas and vas 
  data = within(data, ws <- sqrt(uas^2 + vas^2))
  
  #save surface temperature under a different name
#  data$surf_temp <- data$temp - 273.15
#  data$surf_rh <- data$rh
  
  data$tscr_ave <- as.numeric(as.character(data$tscr_ave)) 
  data$rhscrn <- as.numeric(as.character(data$rhscrn))
#  data$W <- as.numeric(as.character(data$qgscrn)) *1000
  
 data$rnd <- as.numeric(as.character(data$rnd)) #  /24 #conversion to mm/hr (Khalia says original units are mm/day) 
  
  #if (i ==3) {
  #data$rnd <- data$rnd /24
  #}
  
  ###############
  
  
  #make site names pretty
  
  data$site <- stri_replace_all_fixed(data$site, "-CCAM-level-0.csv", "")
  data$site <- stri_replace_all_fixed(data$site, "_AWS ", "") #to fix Bellambi
  data$site <- stri_replace_all_fixed(data$site, "_AWS", "")
  data$site <- stri_replace_all_fixed(data$site, "_AMO", "")
  data$site <- stri_replace_all_fixed(data$site, "_Albion_Park", "")
  levels(as.factor(data$site))
  data$site <- gsub("Parramatta_north", "Westmead", data$site) 
  
  #make Date into date
  date <- stri_replace_all_fixed(data$Date, "+00:00", "")
  date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  date <- date - 3600 # I suspect data is 1:24
  data$Date <- date
  
  #select variables 
 # data <- data[,c(1,5:7,14,15,19:23,26,27,28)]
  #rename variables 
#  names(data)[c(1,3,5,6)] <- c("date", "pres","u10", "v10")
  
  dataframe_name <- paste0("oeh_model_avg_met_",campaign[i]) 
  assign(dataframe_name,data)
  
}

oeh_model_avg_met <- rbind.fill(oeh_model_avg_met_SPS1, oeh_model_avg_met_SPS2, oeh_model_avg_met_MUMBA)
###############
#setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
#save(oeh_model_final_met, oeh_model_final_met_SPS1,oeh_model_final_met_SPS2, oeh_model_final_met_MUMBA, file = "OEH_model_output_newMET.RData")
#select only relevant sites 
site_list <- levels(as.factor(oeh_model_inst_met$site))
oeh_model_avg_met <- subset(oeh_model_avg_met, site %in% site_list)

#combine the two datasets - for comparison 
oeh_model_met <- merge(oeh_model_inst_met, oeh_model_avg_met, by = c("Date", "site", "campaign", "data_source"), suffixes = c(".inst", ".avg"), all = TRUE)
names(oeh_model_met)[1] <- "date"
#long format 
oeh_model_inst_met$data_type <- "instantaneous"
oeh_model_avg_met$data_type <- "averaged"

oeh_model_met_ln <- rbind.fill(oeh_model_inst_met, oeh_model_avg_met)
names(oeh_model_met_ln)[1] <- "date"

# plot things 

library(openair)
xyplot(tscrn + tscr_ave ~date|campaign, data = oeh_model_met, 
       scales = list(x = list(relation = "free")),
       col = c("black", "red"), pch = c(20,1))
p1 <-timeVariation(oeh_model_met, pollutant = c("tscrn", "tscr_ave"), type = "campaign", local.tz = "Etc/GMT-10")
print(p1, subset = "hour")
timeVariation(oeh_model_met, pollutant = c("temp.inst", "temp.avg"), type = "campaign")

#there is a difference for screen temp, but not for temp 
#double check what "temp" is - it looks better, especially if we fix the time issue
timeVariation(oeh_model_met, pollutant = c("tscr_ave", "temp.avg"), type = "campaign")
timeVariation(oeh_model_met, pollutant = c("tscrn", "temp.inst"), type = "campaign")
