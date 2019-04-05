#this script is to read in model output from OEH - Khalia gave me new files on Nov 10 2018 - and again in April 2019
#there are separate files for met and for AQ - need to read both in and combine - AQ files have met variables, discard those 
#Nov 27th - *final* data from Khalia 
#Apr 2019 - revised final CCAM data - need to change some variables name - ws and wd, u10, v10 aren't there?

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
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_revised/Final/Output_CCAM_daily3km_2011_", campaign[i], "_Final"))

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
data$temp10 <- data$temp - 273.15
data$rh10 <- data$rh

data$temp <- data$tscr_ave - 273.15
data$RH <- data$rhscrn
data$W <- data$qgscrn *1000

data$prcp <- as.numeric(as.character(data$rnd)) /24 #conversion to mm/hr (Khalia says original units are mm/day) 

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
data <- data[,c(1,5:7,13,14,20:24,27:29)] #had to change this 
#rename variables 
names(data)[c(1,3,5,6)] <- c("date", "pres","u10", "v10") #

dataframe_name <- paste0("oeh_model_revised_final_met_",campaign[i]) 
assign(dataframe_name,data)

}

oeh_model_final_met <- rbind.fill(oeh_model_revised_final_met_SPS1, oeh_model_revised_final_met_SPS2, oeh_model_revised_final_met_MUMBA)
###############
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(oeh_model_final_met, file = "OEH_model_output_newMET_revised.RData")


#Still need to deal with the AQ files - and merge both  


for (i in 1:length(campaign)) {
  #go to folder: 
  setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_final_surface_AQ/OEH_final_surface_", campaign[i], "_AQ"))
  
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
  
 
  
  #create NOx column
  data$NOx <- data$NO + data$NO2
  
  #make site names pretty
  
  data$site <- stri_replace_all_fixed(data$site, ".csv", "")
  data$site <- stri_replace_all_fixed(data$site, "_AWS ", "")
  data$site <- stri_replace_all_fixed(data$site, "_AWS", "")
  data$site <- stri_replace_all_fixed(data$site, "_AMO", "")
  data$site <- stri_replace_all_fixed(data$site, "_Albion_Park", "")
  levels(as.factor(data$site))
  data$site <- gsub("Parramatta_north", "Westmead", data$site) 
  
  #make Date into date
  
  date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  
  data$Date <- date
  
  names(data)[c(1,7,9:11,17,20)] <- c("date", "HCHO", "C5H8", "Toluene", "Xylenes", "SO4", "EC") 

  dataframe_name <- paste0("oeh_model_final_aq_",campaign[i]) 
  assign(dataframe_name,data)
  
}

oeh_model_final_aq <- rbind.fill(oeh_model_final_aq_SPS1, oeh_model_final_aq_SPS2, oeh_model_final_aq_MUMBA)
###############

#merge both 

oeh_model <- merge(oeh_model_final_met, oeh_model_final_aq, by = c("date", "site", "campaign", "data_source"))

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(oeh_model, file = "OEH_model_output_final.RData")



######
timeVariation(oeh_model, pollutant = c("T", "O3"), type = "campaign", normalise = T)
a <-timeVariation(oeh_model, pollutant = c("ws", "WindSpeed"), type = "campaign")
plot(a, subset = "hour")
b <- timeVariation(oeh_model, pollutant = c("pblh", "PBL"), type = "campaign")
plot(b, subset = "hour")
timeVariation(oeh_model, pollutant = c("u10", "U"), type = "campaign")
#why is there an hour offset between the two? 
names(oeh_model_final_met)
