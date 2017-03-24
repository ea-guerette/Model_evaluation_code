#this script is to read in model output from OEH 
library(stringi)

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

#add data_source and campaign tag: 
data$data_source <- "OEH"
data$campaign <- campaign[i]

#save dataframe
if (i == 1){
  oeh_sps1 <- data
}

if (i == 2){
  oeh_sps2 <- data
}

}

#combine campaigns 
oeh_mod <- rbind(oeh_sps1, oeh_sps2)

#save 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(oeh_mod, oeh_sps1,oeh_sps2, file = "OEH_model_output.RData")
