#this script is to get CSIRO model output in 
library(lubridate)
library(stringi)
library(openair)
library(plyr)

#assign variables 
campaign <- c("SPS1", "SPS2",  "MUMBA")
folders <- c("SPS1_2011", "SPS2_2012", "MUMBA_2013")
#start_date <- c("2011-02-06 14:00 UTC", "2012-04-15 14:00 UTC", "2012-12-31 14:00 UTC") 
#end_date <- c("2011-03-06 13:00 UTC", "2012-05-13 13:00 UTC" ,"2013-02-15 13:00 UTC") 


for (i in 1:length(campaign)) {
##go to folder containing .csv files  
setwd(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/CSIRO_", folders[i]))
##this lists the .csv files present in the folder 
list.files(pattern = ".csv")
##this reads in the list of files 
data <- lapply(list.files(pattern = ".csv"),
               function(.file) read.csv(.file, header = TRUE))
##this combines all the files into one (stacked on top of each other)
data <- do.call(rbind, data)

##create a vector of site names, using the file names 
site_name <- list.files(pattern = ".csv", full.names = FALSE)
##removed .csv from file names 
site_name <- stri_replace_all_fixed(site_name, ".csv", "" )
#Replace MUMBA_Uni by UOW 
#if (i == 1) {
 site_name[c(1,15)] <- c("Albion_Park_Sth", "UOW")
#}
#if (i == 2) {
#  site_name[c(1,8)] <- c("Albion_Park_Sth", "UOW")
#}
##populate the vector
site <- rep(site_name, nrow(data)/length(site_name))
##sort names in alphabetical order
site <- sort(site)

#create a date vector from the year, month, day, hour columns 
date <- paste(data$Yr, data$Mth, data$Dy, sep = "-")
date <- paste(date, data$Hr)
date <- paste(date, ":00", sep = "")
date <- strptime(date, "%Y-%m-%d %H:%M", tz = "UTC") #supposed to be in Etc/GMT-10
#date <- with_tz(date, tzone = "UTC") #trying this to fix tz issue 
head(date)

#add the date and site vectors/columns to the dataframe
data <- cbind(date, data, site)

##fix the columns/variable names 
##remove the cell_ prefix 
names(data) <- stri_replace_all_fixed(names(data), "cell_", "" )
#take out useless columns 
data <- data[-c(2,3,4,5,6)]
#fix the met data names - give them the same names as for the other models #using pbenz and ptol in this 
if (i ==1){
names(data)[c(55,56,57,58,59,60,8,9,10,11,12,13,15,16,17)] <- c("ws", "wd", "temp", "RH", "pblh", "prcp", 
                                                               "HCHO","Methanol", "C5H8", "IsopRxnProd","Terpenes","CH3CHO",
                                                               "Toluene", "Xylenes", "Benzene" )
}
if (i != 1) {
names(data)[c(55,56,57,58,59,60,61,8,9,10,11,12,13,15,16,17)] <- c("ws", "wd", "temp", "RH", "pblh", "pres","prcp", 
                                                                  "HCHO","Methanol", "C5H8", "IsopRxnProd","Terpenes","CH3CHO",
                                                                  "Toluene", "Xylenes", "Benzene" )
  
  
}

##create NOX, PM2.5, PM10 columns  
names(data)
if (i ==1){
data$prcp <- data$prcp /24 #this is a test - kathryn thinks the data is /day not /hour
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

#calculate u10 and v10
mwd <- 270- data$wd
ifelse( mwd < 0, mwd +360, mwd) 
data$v10 <- data$ws * sin(pi*mwd/180)
data$u10 <- data$ws * cos(pi*mwd/180)

#also add a column specifying the data_source 
data$data_source <- "CSIRO"
data$campaign <- campaign[i]
#data <- subset(data, date >= start_date[i] & date <= end_date[i])

dataframe_name <- paste0("csiro_",campaign[i]) 
assign(dataframe_name,data)
timePlot(data, pollutant = "temp", type = "site")
}

#Not needed 
#make some dummy SPS2 data so I can avoid issues when plotting things
#csiro_SPS2 <-data.frame(date = rep(seq(as.POSIXct("2012-04-16"), by = "hours", length = 672, tzone = "UTC"),11),
 #                       data_source = rep("CSIRO", 672*11), campaign = rep("SPS2", 672*11), 
 #                      site = c(sort(rep(levels(csiro_MUMBA$site),672)),rep("Williamtown_RAAF",672)), ws = rep(NA, 672*11))


csiro <- rbind.fill(csiro_SPS1, csiro_SPS2, csiro_MUMBA)
#csiro <- rbind.fill(csiro, csiro_SPS2)
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(csiro, csiro_MUMBA, csiro_SPS1, csiro_SPS2, file = "CSIRO_model_output_new.RData")
load("CSIRO_model_output_new.RData")
