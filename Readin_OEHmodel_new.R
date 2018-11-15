#this script is to read in model output from OEH - Khalia gave me new files on Nov 10 2018
#there are separate files for met and for AQ - need to read both in and combine - AQ files have met variables, discard those 
#Actually, I think she only extracted the CCAM met at the BOM sites - OK, but not great 
#this actually makes it pretty easy - read in and combine the met sites, save that to use for Khalia's plot
#then save the AQ site and save that and use it in my analysis (except that, all sites are in AQ, with the wrong met - so maybe cut out the met? 
#But it was going to be useful for diagnostic tests...) - I requested CCAM met at all sites from Khalia 

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
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/OEH_new_surface_MET/OEH_surface_MET_", campaign[i]))

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

data$temp <- data$temp - 273.15 #conversion to degrees C 
data$mixr <- data$mixr *1000 #conversion to g/kg
data$rnd <- as.numeric(as.character(data$rnd)) #/24 #conversion to mm/hr (Khalia says original units are mm/day) but I think only for MUMBA?

if (i ==3) {
data$rnd <- data$rnd /24
}

#make site names pretty

data$site <- stri_replace_all_fixed(data$site, "-CCAM-level-0.csv", "")
data$site <- stri_replace_all_fixed(data$site, "_AWS", "")
data$site <- stri_replace_all_fixed(data$site, "_AMO", "")
data$site <- stri_replace_all_fixed(data$site, "_Albion_Park", "")
levels(as.factor(data$site))
#data$site <- gsub("Parramatta_north", "Westmead", data$site) 

#make Date into date
date <- stri_replace_all_fixed(data$Date, "+00:00", "")
date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

data$Date <- date

#rename variables 
names(data)[1:10] <- c("date", "u10", "v10", "W", "temp", "pres", "pblh", "prcp", "ws", "wd")

dataframe_name <- paste0("oeh_model_new_met_",campaign[i]) 
assign(dataframe_name,data)

}

oeh_model_new_met <- rbind.fill(oeh_model_new_met_SPS1, oeh_model_new_met_SPS2, oeh_model_new_met_MUMBA)
###############
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(oeh_model_new_met, oeh_model_new_met_SPS1,oeh_model_new_met_SPS2, oeh_model_new_met_MUMBA, file = "OEH_model_output_newMET.RData")



#Still need to deal with the AQ files - do it later 





#Check, old vs. new met 
############################
load("OEH_model_output_newMET.RData")
oeh_model_new_met$data_source <- "O-CTM_newMET"
#check new against old 
load("OEH_model_output2.RData")
oeh_model <- subset(oeh_model, site %in% site_list)

oeh <- rbind.fill(oeh_model, oeh_model_new_met)

mumba_mod <- subset(oeh, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
sps1_mod <- subset(oeh, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(oeh, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
oeh_met <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 


timeVariation(oeh_met, pollutant = "ws", type = "campaign", group = "data_source")
timeVariation(oeh_met, pollutant = "W", type = "campaign", group = "data_source")
timeVariation(oeh_met, pollutant = "u10", type = "campaign", group = "data_source")
timeVariation(oeh_met, pollutant = "temp", type = "campaign", group = "data_source")
#windspeed has changed too? 

xyplot(prcp ~date |campaign, data = mumba_mod, groups = data_source)
xyplot(prcp ~date |campaign, data = sps1_mod, groups = data_source, auto.key = T)

sums2 <- ddply(oeh_met, .(campaign, data_source), numcolwise(sum), na.rm = TRUE)


########



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
data$data_source <- "O-CTM"
data$campaign <- campaign[i]

#cut to length 
#data <- subset(data, date >= start_date[i] & date <= end_date[i])

#save dataframe
dataframe_name <- paste0("oeh_model_",campaign[i]) 
assign(dataframe_name,data)
timePlot(data, pollutant = "O3", type = "site")




#MUMBA output - met only 

setwd("C:/Users/eag873/ownCloud/OEH_surface_MUMBA/")

data <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE))
data <- do.call(rbind, data)

#create site vector using the names of the files in the folder 
site_name <- list.files(pattern = ".csv")
site <- sort(rep(site_name,  nrow(data)/length(site_name)))
campaign <- "MUMBA"


data <- cbind(data,site,campaign)
data$data_source <- "O-CTM"

data$temp <- data$temp - 273.15 #conversion to degrees C 
data$mixr <- data$mixr *1000 #conversion to g/kg
data$rnd <- data$rnd /24 #conversion to mm/hr
#make site names pretty

data$site <- stri_replace_all_fixed(data$site, "-CCAM-level-0.csv", "")
data$site <- stri_replace_all_fixed(data$site, "_AWS", "")
data$site <- stri_replace_all_fixed(data$site, "_AMO", "")
data$site <- stri_replace_all_fixed(data$site, "_Albion_Park", "")
levels(as.factor(data$site))
data$site <- gsub("Parramatta_north", "Westmead", data$site) 

#make Date into date
date <- stri_replace_all_fixed(data$Date, "+00:00", "")
date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

data$Date <- date

#rename variables 
names(data)[1:10] <- c("date", "u10", "v10", "W", "temp", "pres", "pblh", "prcp", "ws", "wd")

#select dates 
data <- subset(data, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")


#make a MUMBA met dataframe 
oeh_model_met_mumba <- data

#AQ only 
setwd("C:/Users/eag873/ownCloud/OEH_surface_MUMBA/OEH_AQ_MUMBA")

data <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE, as.is = TRUE ))
data <- do.call(rbind, data)

#create site vector using the names of the files in the folder 
site_name <- list.files(pattern = ".csv")
site <- sort(rep(site_name,  nrow(data)/length(site_name)))
#campaign <- "MUMBA"


data <- cbind(data,site)#,campaign)
data$site <- stri_replace_all_fixed(data$site, ".csv", "")
data$site <- stri_replace_all_fixed(data$site, "_AWS", "")
data$site <- stri_replace_all_fixed(data$site, "_AMO", "")
data$site <- stri_replace_all_fixed(data$site, "_Albion_Park", "")
levels(as.factor(data$site))
data$site <- gsub("Parramatta_north", "Westmead", data$site) 

date <- as.POSIXct(data$Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
data$Date <- date
data <- data[,c(1:26,33)]

names(data)[c(1,7,9)] <- c("date", "HCHO", "C5H8", "Toluene", "Benzene", "Xylenes")

data$NOx <- data$NO  + data$NO2

data <- subset(data, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")

oeh_model_AQ_mumba <- data

#combine met and AQ

oeh_model_mumba <- merge(oeh_model_met_mumba, oeh_model_AQ_mumba, by = c("date", "site"))

#combine campaigns 
oeh_model <- rbind.fill(oeh_model_SPS1, oeh_model_SPS2, oeh_model_mumba)

#save 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(oeh_model, oeh_model_SPS1,oeh_model_SPS2, oeh_model_mumba, file = "OEH_model_output2.RData")


