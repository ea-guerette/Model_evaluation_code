#this script is to get CSIRO model output in - might need to separate met, aq and give different dates 
# June 2019 - using original met output from CTM (not new met (2m)) - need to separate met, aq to give different timestamps :/ 
library(lubridate)
library(stringi)
library(openair)
library(plyr)

#assign variables 
campaign <- c("SPS1", "SPS2",  "MUMBA") #had to modify SPS2 Richmond file so it matched the others
folders <- c("SPS1_2011", "SPS2_2012", "MUMBA_2013")
#start_date <- c("2011-02-06 14:00 UTC", "2012-04-15 14:00 UTC", "2012-12-31 14:00 UTC") 
#end_date <- c("2011-03-06 13:00 UTC", "2012-05-13 13:00 UTC" ,"2013-02-15 13:00 UTC") 

for (i in 1:length(campaign)) {
#for (i in 1:length(campaign)) {
##go to folder containing .csv files  
#setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/CSIRO_", folders[i], "/RE__Westmead_data"))
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/CSIRO_", folders[i]))#, "/newTemp")) #renamed folder for consistency #May - reverting to 10m temp
##this lists the .csv files present in the folder 
list.files(pattern = ".csv")
##this reads in the list of files 
data <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE))
##this combines all the files into one (stacked on top of each other)
data <- do.call(rbind, data)

##create a vector of site names, using the file names 
site_name <- list.files(pattern = ".csv", full.names = FALSE)
##remove .csv from file names 
site_name <- stri_replace_all_fixed(site_name, ".csv", "" )

##populate the vector
site <- rep(site_name, nrow(data)/length(site_name))
##sort names in alphabetical order
site <- sort(site)

#create a date vector from the year, month, day, hour columns 
date <- paste(data$Yr, data$Mth, data$Dy, sep = "-")
date <- paste(date, data$Hr)
date <- paste(date, ":00", sep = "")
date <- strptime(date, "%Y-%m-%d %H:%M", tz = "UTC") 
#date <- with_tz(date, tzone = "UTC") #trying this to fix tz issue 
head(date)

#read in q from new files (18 Apr 2019)
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/CSIRO_", folders[i], "/newTemp/W")) #renamed folder for consistency 
list.files(pattern = ".csv")
##this reads in the list of files 
dataW <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE))
##this combines all the files into one (stacked on top of each other)
dataW <- do.call(rbind, dataW)
#add the date and site vectors/columns to the dataframe
#data <- cbind(date, data,dataW, site)
if(i==1){
  aq_data <- cbind(date, data[, -c(82:88)], site)
  met_data <- cbind(date+3600, data[,c(82:88)], dataW, site)
  names(met_data)[1] <- "date"
}
if(i==2){
  aq_data <- cbind(date, data[, -c(59:67)], site)
  met_data <- cbind(date-3600, data[,c(59:65)], dataW, site)
  names(met_data)[1] <- "date"
}
if(i==3){
  aq_data <- cbind(date, data[, -c(59:65)], site)
  met_data <- cbind(date+3600, data[,c(59:65)], dataW[,-2], site)
  names(met_data)[1] <- "date"
}

data <- merge(aq_data, met_data, by = c("date", "site"),all =T)

##fix the columns/variable names 
##remove the cell_ prefix 
names(data) <- stri_replace_all_fixed(names(data), "cell_", "" )
#take out useless columns 
data <- data[-c(3,4,5,6,7)]
#fix the met data names - give them the same names as for the other models #using pbenz and ptol in this 
if (i ==1){

names(data)[c(79,80,81,82,83,84,85,86,14,19,20,21,22,15,23,27)] <- c("ws", "wd", "temp", "RH", "pblh", "pres", "prcp", "qgscrn",
                                                               "HCHO","Methanol", "C5H8", "IsopRxnProd","Terpenes","CH3CHO",
                                                               "Toluene", "Xylenes")
}
if (i != 1) {
  names(data)[c(56,57,58,59,60,61,62,63,9,10,11,12,13,14,16,17,18)] <- c("ws", "wd", "temp", "RH", "pblh", "pres","prcp",  "qgscrn",
                                                                        "HCHO","Methanol", "C5H8", "IsopRxnProd","Terpenes","CH3CHO",
                                                                        "Toluene", "Xylenes", "Benzene" )
}
#if (i == 3) {
#names(data)[c(55,56,57,58,59,60,61,62,8,9,10,11,12,13,15,16,17)] <- c("ws", "wd", "temp", "RH", "pblh", "pres","prcp",  "qgscrn",
 #                                                                 "HCHO","Methanol", "C5H8", "IsopRxnProd","Terpenes","CH3CHO",
  #                                                                "Toluene", "Xylenes", "Benzene" )
#}

#data$temp <- data$temp -  273.15 #10m temp already in degree C
#data$RH[data$RH >100] <- NA

#calculate water mixing ratio
#es <- 6.112*exp((17.67*data$temp)/(data$temp+243.5))
#e <- es * (data$RH/100.0)
#q <- (0.622*e)/(data$pres - (0.378*e)) #specific humidity in kg/kg
#q <- (0.622*e)/(data$pres - (e)) #specific humidity in kg/kg #apparently, the 0.378 was incorrect
# I want w: grams of vapor per kg of dry air
#data$W_calc <- q*1000  

data$W <- data$qgscrn*1000 #WOW that is VERY different 



##create NOX, PM2.5, PM10 columns  
names(data)
if (i !=3){
data$prcp <- data$prcp *24 
}

  
data$NOx <- data$NO + data$NO2
data$PM2.5 <- (data$NH4 + data$NIT + data$ASO4 + data$SS25
                     + data$EC25 + data$OT25 + data$APA1 + data$APA2
                     + data$APA3 + data$APA4 + data$APA5 + data$APA6
                     + data$APA7 + data$APA8 + data$APA9 + data$AOA1
                     + data$AOA2 + data$AOA3 + data$AOA4 + data$AOA5
                     + data$AOA6 + data$AOA7 + data$AOA8 + data$BOA1
                     + data$BOA2 + data$BOA3 + data$BOA4 + data$BOA5
                     + data$BOA6 + data$DU02)
data$PM10 <- data$PM2.5 + data$AS10 + data$SS10 + data$OT10 + data$EC10 + data$OC10 + data$DU05 #+ data$DU10

if (i ==1 ){
  
  names(data)[c(32, 39)] <- c("EC", "SO4")
}

if (i != 1){ 
names(data)[c(19, 26)] <- c("EC", "SO4")
} 

#calculate u10 and v10
mwd <- 270- data$wd
ifelse( mwd < 0, mwd +360, mwd) 
data$v10 <- data$ws * sin(pi*mwd/180)
data$u10 <- data$ws * cos(pi*mwd/180)

data$site <- gsub("Albion_Park_South", "Albion_Park_Sth", data$site)
data$site <- gsub("MUMBA_Uni", "UOW", data$site)

#also add a column specifying the data_source 
data$data_source <- "C-CTM"
data$campaign <- campaign[i]
#data <- subset(data, date >= start_date[i] & date <= end_date[i])

dataframe_name <- paste0("csiro",campaign[i]) 
assign(dataframe_name,data)
#timePlot(data, pollutant = "temp", type = "site")
}

#combine 
csiro <- rbind.fill(csiroSPS1, csiroSPS2, csiroMUMBA)

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(csiro, csiroSPS1, csiroSPS2, csiroMUMBA, file = "CSIRO_model_output_final_W_10m_temp_timeshifted.RData")



######################


#check versus previous output 
load("csiro_model_output_new_new_fixed.RData")

#csiro_newTemp$data_source <- "C-CTM_newTemp"
csiro_final$data_source <- "C-CTM_newTemp"

#csiro <- rbind.fill(csiro_newTemp, csiro)
csiro <- rbind.fill(csiro_final, csiro)
timeVariation(csiro, pollutant = "ws", type = "campaign", group = "data_source")

timeVariation(csiro, pollutant = "W", type = "campaign", group = "data_source", local.tz = "Etc/GMT-10")
timeVariation(csiro, pollutant = "u10", type = "campaign", group = "data_source")
timeVariation(csiro, pollutant = c("O3", "temp"), type = "campaign")
timeVariation(csiro, pollutant = "temp", type = "campaign", group = "data_source")
#OK, temp has changed but not the others - not sure temp is better though. 
#################################

#load("CSIRO_model_output_newTemp.RData")

#timePlot(csiro_MUMBA, pollutant = c("EC","SO4","PM2.5"), type = "site")
load("CSIRO_model_output_SPS2_newTemp.RData")
load("CSIRO_model_output_new_new.RData")

csiro_newTemp_SPS1$data_source <- "C-CTM_newTemp"

SPS1_mod <- rbind.fill(csiro_newTemp_SPS1,csiro_SPS1)


a <- timeVariation(SPS1_mod, pollutant = "temp", group = "data_source", local.tz = "Etc/GMt-10", ci = FALSE)
print(a, subset = "hour")


SPS2_mod <- subset(SPS2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
SPS2_mod$date <- SPS2_mod$date -3600 #to put time stamp at the end of the hour

dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
#load in OEH observations  
load(paste0(dir_obs,"BOM_data_updated3.RData"))

BOM <- bom_data_sps1

BOM$data_source <- "OBS"

SPS1_aq <- rbind.fill(SPS1_mod, BOM)

a2 <- timeVariation(SPS1_aq, pollutant = "temp", group = "data_source", local.tz = "Etc/GMt-10", ci = FALSE, cols = c("red", "blue", "black"))
print(a2, subset = "hour")

a2 <- timeVariation(subset(SPS2_aq, site %in% "Westmead"), pollutant = "O3", group = "data_source", local.tz = "Etc/GMt-10", ci = FALSE, cols = c("red", "blue", "black"))
print(a2, subset = "hour")
#no change in RH 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
#load in met observations from BOM  
load(paste0(dir_obs,"/BOM_data_updated3.RData"))
#assign variables
BOM <- bom_data_sps2
BOM <- subset(BOM, site != "Williamtown_RAAF")
site_list <- levels(as.factor(BOM$site)) #to select only BOM sites 
site_list <- site_list[-7] #removing Williamtown - outside of domain? figures v4 and up

SPS2_mod_met <- subset(SPS2_mod, site %in% site_list)

BOM$data_source <- "OBS"
SPS2 <- rbind.fill(SPS2_mod_met, BOM)

b <- timeVariation(SPS2, pollutant = "temp", group = "data_source", local.tz = "Etc/GMt-10", ci = FALSE, cols = c("red", "blue", "black"))
print(b, subset = "hour")


