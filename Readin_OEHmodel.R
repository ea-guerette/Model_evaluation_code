#this script is to read in model output from OEH 

#load packages
library(stringi)

#assign variables
campaign <- c("SPS1", "SPS2")

for (i in 1:length(campaign)) {
#go to folder: 
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_CCAM_CTM_", campaign[i],"_updated"))

#create site vector using the names of the files in the folder 
site_name <- list.files(pattern = ".csv")
site_name <- stri_replace_all_fixed(site_name, paste0(" ",campaign[i],".csv"), "" ) 
site_name <- stri_replace_all_charclass(site_name, "\\p{WHITE_SPACE}", "_")

#modify the remaining inconsistencies by hand 
if (i == 1) {
site_name[c(1,22,25,26)] = c("Albion_Park", "St_Marys", "Westmead", "Williamtown_RAAF")
site <- site_name
site <- rep(site, 744)
site <- sort(site)
site[1:744] <- "Wollongong_Airport"
} 

if (i == 2) {
site_name[c(1,22)] = c("Albion_Park", "St_Marys")
site <- site_name
site <- rep(site, 696)
site <- sort(site)
site[1:696] <- "Wollongong_Airport"
} 

##this reads in the list of files 
data <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE))
##this combines all the files into one (stacked on top of each other)
data <- do.call(rbind, data)

sub.data <- data[ , grep(pattern = "ctm", names(data))]
names(sub.data) <- stri_replace_all_fixed(names(sub.data), ".ctm.", "" )

#create a date vector - run time is 20110205.00 UTC - 20110307.23 UTC (31days, in total of 744 hours)
Sys.setenv(TZ = "UTC")
if (i == 1) {
date = seq(as.POSIXct("2011-02-05"), by = "hours", length = 744, tzone = "UTC")
date = rep(date, 27)
} 

if (i == 2) {
date = seq(as.POSIXct("2012-04-16"), by = "hours", length = 696, tzone = "UTC")
date = rep(date, 27)
}

##create dataframe 
data <- data.frame(date, sub.data, site)

#fix the variable names - give them the same names as for the other models 
names(data)
names(data)[c(9,13,14,15,17,18)] <- c("PM2.5","ws","wd","temp", "pblh", "prcp") 

mwd <- 270- data$wd
ifelse( mwd < 0, mwd +360, mwd) 
data$v10 <- data$ws * sin(pi*mwd/180)
data$u10 <- data$ws * cos(pi*mwd/180)

#create NOx column
data$NOx <- data$NO + data$NO2

#add data_source and campaign tag: 
data$data_source <- "OEH"
data$campaign <- campaign[i]

#save dataframe
dataframe_name <- paste0("oeh_model_",campaign[i]) 
assign(dataframe_name,data)

}

#MUMBA output is messy, so I have to code it separately 
#the output for the actual MUMBA site is different from that of the other sites 
start_date <- c("2013-01-01","2013-02-01")
folders <- c("jan13_mumba_CTM.csv", "feb13_mumba_CTM.csv") 

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_CCAM_CTM_MUMBA/")
for (j in 1:length(folders)) {
#read file in
data <- read.csv(file = folders[j], header = T)
#create date vector 
date = seq(as.POSIXct(start_date[j]), by = "hours", length = nrow(data), tzone = "UTC")
#create other vector
site <- "MUMBA"
campaign <- "MUMBA"

#select only model output 
data <- data[ , grep(pattern = "cell_", names(data))]
names(data) <- stri_replace_all_fixed(names(data), "cell_", "" )
names(data) <- toupper(names(data))
names(data)[c(12,18,24,25,26,36,37,38,47,48,49,50,51,42,43,44,45,46)] <- c("HCHO", "Methanol", "C5H8", "IsopRxnProd", "Terpenes", "Toluene", "Xylenes", "Benzene", "ws", "wd", "temp", "H2O", "pblh", "Na", "Mg","Ca", "K", "Cl")

data <- cbind(date,data,site,campaign)
data$data_source <- "OEH"
#create NOx column
data$NOx <- data$NO + data$NO2
#calculate u10 and v10
mwd <- 270- data$wd
ifelse( mwd < 0, mwd +360, mwd) 
data$v10 <- data$ws * sin(pi*mwd/180)
data$u10 <- data$ws * cos(pi*mwd/180)

dataframe_name <- paste0("oeh_model_mumba_site_",j) 
assign(dataframe_name,data)
}
oeh_model_MUMBA_site <- rbind(oeh_model_mumba_site_1, oeh_model_mumba_site_2)

#now, read in the other files... again, two folders 
start_date <- c("2013-01-01","2013-02-01")
date_length <- c(744,672)
folders <- c("OEHsites_CTMjan13","OEHsites_CTMfeb13")

for (i in 1:length(folders)) {
  setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/", folders[i], "/"))

  data <- lapply(list.files(pattern = ".csv"),
                 function(.file) read.csv(.file, header = TRUE))
  ##this combines all the files into one (stacked on top of each other)
  data <- do.call(rbind, data)
  #make date vector 
  date = seq(as.POSIXct(start_date[i]), by = "hours", length = date_length[i], tzone = "UTC")
  #make other vectors 
  campaign <- "MUMBA"
  site <- c(rep("Wollongong_Airport",date_length[i]), rep("Albion_Park_Sth", date_length[i]),
            rep("Badgerys_Creek", date_length[i]), rep("Bankstown_Airport", date_length[i]),
            rep("Bargo", date_length[i]), rep("Bringelly", date_length[i]),
            rep("Camden_Airport", date_length[i]), rep("Chullora", date_length[i]),
            rep("Earlwood", date_length[i]),rep("Kembla_Grange", date_length[i]),
            rep("Lindfield", date_length[i]),rep("Liverpool", date_length[i]),
            rep("Macarthur", date_length[i]),rep("Newcastle", date_length[i]),
            rep("Oakdale", date_length[i]), rep("Prospect", date_length[i]),
            rep("Randwick", date_length[i]),rep("Richmond", date_length[i]),
            rep("Richmond_RAAF", date_length[i]),rep("Rozelle", date_length[i]),
            rep("St_Marys", date_length[i]),rep("Sydney_Airport", date_length[i]),
            rep("Vineyard", date_length[i]),rep("Westmead", date_length[i]),
            rep("Williamtown_RAAF", date_length[i]),rep("Wollongong", date_length[i]))
  #clean up data 
  data <- data[ , grep(pattern = "ctm", names(data))]
  names(data) <- stri_replace_all_fixed(names(data), ".ctm.", "" )
  names(data)[c(8,12,13,14,15,16,17)] <- c("PM2.5", "ws","wd","temp","H2O","pblh", "prcp")
 
  #calculate u10 and v10
  mwd <- 270- data$wd
  ifelse( mwd < 0, mwd +360, mwd) 
  data$v10 <- data$ws * sin(pi*mwd/180)
  data$u10 <- data$ws * cos(pi*mwd/180)
  
   #combine
  data <- cbind(date, data, site, campaign)
  data$NOx <- data$NO + data$NO2
  data$data_source <- "OEH"  
  dataframe_name <- paste0("oeh_model_",folders[i]) 
  assign(dataframe_name,data)
}
#combine jan and feb
oeh_model_other_sites <- rbind(oeh_model_OEHsites_CTMjan13, oeh_model_OEHsites_CTMfeb13)


#make a MUMBA dataframe 
oeh_model_mumba <- rbind.fill(oeh_model_MUMBA_site, oeh_model_other_sites)

#combine campaigns 
oeh_model <- rbind.fill(oeh_model_SPS1, oeh_model_SPS2, oeh_model_mumba)

#save 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(oeh_model, oeh_model_SPS1,oeh_model_SPS2, oeh_model_mumba, file = "OEH_model_output.RData")
