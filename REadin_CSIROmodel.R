#this script is to get CSIRO model output in 

#assign variables 
campaign <- c("SPS1", "MUMBA")
folders <- c("SPS1_2011", "MUMBA_2013")

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
if (i == 1) {
 site_name[c(1,15)] <- c("Albion_Park_Sth", "UOW")
}
if (i == 2) {
  site_name[c(1,8)] <- c("Albion_Park_Sth", "UOW")
}
##populate the vector
site <- rep(site_name, nrow(data)/length(site_name))
##sort names in alphabetical order
site <- sort(site)

#create a date vector from the year, month, day, hour columns 
date <- paste(data$Yr, data$Mth, data$Dy, sep = "-")
date <- paste(date, data$Hr)
date <- paste(date, ":00", sep = "")
date <- strptime(date, "%Y-%m-%d %H:%M")#, tz = "Etc/GMT-10")
head(date)

#add the date and site vectors/columns to the sps2_tapm dataframe
data <- cbind(date, data, site)

##fix the columns/variable names 
##remove the cell_ prefix 
names(data) <- stri_replace_all_fixed(names(data), "cell_", "" )
#take out useless columns 
data <- data[-c(2,3,4,5,6,7)]
#fix the met data names - give them the same names as for the other models #using pbenz and ptol in this 
names(data)[c(54,55,56,57,58,59,7,8,9,10,11,12,14,15,16)] <- c("ws", "wd", "temp", "RH", "pblh", "prcp", 
                                                               "HCHO","Methanol", "C5H8", "IsopRxnProd","Terpenes","CH3CHO",
                                                               "Toluene", "Xylenes", "Benzene" )

##create NOX, PM2.5, PM10 columns  
names(data)
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

dataframe_name <- paste0("csiro_",campaign[i]) 
assign(dataframe_name,data)
}
csiro <- rbind(csiro_MUMBA, csiro_SPS1)
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(csiro, csiro_MUMBA, csiro_SPS1, file = "CSIRO_model_output.RData")