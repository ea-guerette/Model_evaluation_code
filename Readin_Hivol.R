#need code to read in hivol data - I must have had something at some point, but I cannot find it 
#need to include a site in this!! otherwise OBS in _ln won't have a site - fixed

library(openair)

main_dir <- "C:/Users/eag873/ownCloud"
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
campaign <- c("MUMBA", "SPS1", "SPS2")
site <- c("MUMBA", "Westmead", "Westmead")
folders <- c( "/MUMBA data for model intercomp/", "/Sydney Particle Study 1/", "/Sydney Particle Study 2/")
files <- c("MUMBA HiVol Chem results.csv" , "SPS1_HiVol_Chem.csv", "SPS2_HiVol_Chem.csv")

for (i in 1:length(campaign)){
  setwd(paste0(main_dir, folders[i]))
  #
  if (i == 1) { 
  chem <- import(file = files[i], date = "Start", date.format = "%d/%m/%y %H:%M", tzone = "Etc/GMT-11", 
                 na.strings = c("", "<mdl"), header.at = 1, data.at = 8 )
  
  chem <- chem[,c(1,2,15,16,17,18,19,20,22,23)]
  chem[,c(3:10)] <-  chem[,c(3:10)] /1000
  names(chem) <- c("sample", "date", "Na", "NH4", "K", "Mg", "Ca", "Cl", "NIT", "SO4")
  
  ec <- import(file = "MUMBA_HiVol_22Jan2013_15Feb2013.csv", date = "Time_ON", date.format = "%Y-%m-%dT%H:%M:%S+1000", tzone = "Etc/GMT-10")
  ec <- ec[, c(1,3,4)]
  names(ec) <- c("date", "OC", "EC")
  
  data <- cbind(na.omit(ec), chem)
  
  data <- data[,-5]
  
  data$campaign <- campaign[i]
  data$site <- site[i]
  data$TOD <- "AM"
  ids <-grep("PM", data$sample)
  data$TOD[ids] <- "PM"
  }
  
  if (i == 2) {
    
    data <- import(file = files[i], date = "DATE / TIME", date.format = "%Y-%m-%dT%H", tzone = "Etc/GMT-11", 
                   na.strings = c("", "<mdl"), header.at = 1, data.at = 8)
    data <- data[,c(1,2,4:9,12,13,24,25)]
    names(data) <- c("sample", "date", "Na","NH4","K","Mg","Ca","Cl", "NIT", "SO4", "OC", "EC")
    
    data$TOD <- "AM"
    ids <-grep("pm", data$sample)
    data$TOD[ids] <- "PM"
    
    data$campaign <- campaign[i]
    data$site <- site[i]
  }
  
  if (i == 3) {
    data <- import(file = files[i], date = "DATE / TIME", date.format = "%Y-%m-%dT%H", tzone = "Etc/GMT-10", 
                   na.strings = c("", "<mdl"), header.at = 1, data.at = 8)
    
    data <- data[,c(1,2,3,5:10,13,14,25,26)]
    names(data) <- c("sample", "TOD", "date", "Na","NH4","K","Mg","Ca","Cl", "NIT", "SO4", "OC", "EC")
    data$campaign <- campaign[i]
    data$site <- site[i]
  }
  
  
  data_name <- paste0("hivol_", campaign[i])
  assign(data_name, data)
}

hivol_obs <- rbind(hivol_SPS1, hivol_SPS2, hivol_MUMBA)

setwd(dir_obs)
save(hivol_obs, file = "hivol_obs.RData")


#read in acid and alkaline gases 
folders <- c( "/Sydney Particle Study 1/", "/Sydney Particle Study 2/")
files <- c("SPS1_Acid_Alkaline_Gases.csv", "SPS2_Acid_Alkaline_Gases.csv")
campaign <- c( "SPS1", "SPS2")
for (i in 1:length(campaign)){
  setwd(paste0(main_dir, folders[i]))
  #
  if (i == 1) { 
    aalk <- import(file = files[i], date = "DATE / TIME", date.format = "%Y-%m-%dT%H", tzone = "Etc/GMT-11", 
                   na.strings = c("", "<mdl"), header.at = 1, data.at = 8 )
    aalk$TOD <- "AM"
    ids <-grep("pm", aalk$Sample)
    aalk$TOD[ids] <- "PM"
    
    aalk <- aalk[,-3]
  
    aalk$campaign[i] 
    
  }
  if (i == 2) { 
    aalk <- import(file = files[i], date = "DATE / TIME", date.format = "%Y-%m-%dT%H", tzone = "Etc/GMT-10", 
                   na.strings = c("", "<mdl"), header.at = 1, data.at = 8 )
    aalk <- aalk[,-4]
    names(aalk)[2] <- "TOD"
  }
  
  data_name <- paste0("aalk_", campaign[i])
  assign(data_name, aalk)
} 

aalk_obs <- rbind(aalk_SPS1, aalk_SPS2)

setwd(dir_obs)
save(aalk_obs, file = "acid_and_alkaline_gases.RData")

load("hivol_obs.RData")


#merge hivol and aalk 
names(aalk_obs)[1] <- "sample"
filter_data <- merge(hivol_obs, aalk_obs, by = c("sample", "date", "TOD"), all = T)

#setwd(dir_obs)
#save(filter_data, file = "filter_data.RData")

#read in MUMBA PM2.5 and select filter times 
setwd(paste0(main_dir, "/MUMBA data for PANGAEA/final files inc metadata - as published on PANGAEA"))

mumba_pm25 <- import(file = "MUMBA_PM2.5_eSampler_24Jan2013_15Feb2013.csv", date.format = "%Y-%m-%dT%H:%M", tzone = "Etc/GMT-10")
names(mumba_pm25)[2] <- "PM2.5"

mumbaam <-  selectByDate(mumba_pm25, hour = 4:8)
mumbaam <-  timeAverage(mumbaam, avg.time = "day", data.thresh = (4/5*5/24*100))
mumbaam$date <- as.POSIXct(paste(mumbaam$date, "4:00"), tz = "Etc/GMT-10")
mumbaam$TOD <- "AM"

mumbapm <-  selectByDate(mumba_pm25, hour = 10:17)
mumbapm <-  timeAverage(mumbapm, avg.time = "day", data.thresh = (6/8*5/24*100) ) 
mumbapm$date <- as.POSIXct(paste(mumbapm$date, "10:00"), tz = "Etc/GMT-10")
mumbapm$TOD <- "PM"

mumba.pm25 <- rbind(mumbaam, mumbapm)
mumba.pm25$campaign <- "MUMBA" 
#merge with filter_data 

filterdata <-merge(filter_data, mumba.pm25, by = c("date", "TOD", "campaign"), all = T)
setwd(dir_obs)
save(filterdata, file = "filter_data.RData")

