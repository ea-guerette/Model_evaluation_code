#need code to read in hivol data - I must have had something at some point, but I cannot find it 

library(openair)

main_dir <- "C:/Users/eag873/ownCloud"
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
campaign <- c("MUMBA", "SPS1", "SPS2")
#site <- c("MUMBA", "Westmead", "Westmead")
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
  }
  
  if (i == 3) {
    data <- import(file = files[i], date = "DATE / TIME", date.format = "%Y-%m-%dT%H", tzone = "Etc/GMT-10", 
                   na.strings = c("", "<mdl"), header.at = 1, data.at = 8)
    
    data <- data[,c(1,2,3,5:10,13,14,25,26)]
    names(data) <- c("sample", "TOD", "date", "Na","NH4","K","Mg","Ca","Cl", "NIT", "SO4", "OC", "EC")
    data$campaign <- campaign[i]
  }
  
  
  data_name <- paste0("hivol_", campaign[i])
  assign(data_name, data)
}

hivol_obs <- rbind(hivol_SPS1, hivol_SPS2, hivol_MUMBA)

setwd(dir_obs)
save(hivol_obs, file = "hivol_obs.RData")
