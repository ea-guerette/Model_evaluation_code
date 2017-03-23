##This script was given to me by Khalia Monk
##This script extracts one minute data from BOM and saves is as hourly averages in csv format
#modified to fix precipitation - needed to sum, not average over the hour 
#added pres and a calculation of W (water mixing ratio)
#modified for tzone

# SET WORKING DIRECTORY #
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/Monk06062016OneMinute")
# ACTIVATE OPENAIR #
library(openair)
library(reshape2)
library(stringr)
library(plyr)


stns <- c("067108",#"Badgerys Creek Aws
         "066137",#"Bankstown Airport Aws
         "068192", #"Camden Airport Aws
         "067105",#"Richmond Raaf
         "066037", # Sydney Airport
         "061078") #Williamtown
stnnames <- c("Badgerys_Creek",
              "Bankstown_Airport",
              "Camden_Airport",
              "Richmond_RAAF",
              "Sydney_Airport",
              "Williamtown_RAAF")  
stn_no <- c("stn20","stn21","stn22","stn23","stn24","stn25")

period <- c("MUMBA", "SPS2", "SPS1")
period_start <-c( "2012-12-20 13:00", "2012-04-15 13:00", "2011-02-03 00:00")
period_end <- c("2013-02-16 12:00", "2012-05-13 23:00","2011-03-10 23:00")


for (i in 1:length(stns)){

bomdata <- read.csv(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/Monk06062016OneMinute/HD01D_Data_",stns[i],"_999999999259801.txt"), header=TRUE) 

bomdate <- ISOdatetime(bomdata$Year.Month.Day.Hour.Minutes.in.YYYY.1,
                   bomdata$MM.1, bomdata$DD.1, hour = bomdata$HH24.1, 
                   min = bomdata$MI.format.in.Local.standard.time, sec = 0, tz="Etc/GMT-10")

bom_data <- data.frame( date = bomdate,
                        prcp = bomdata$Precipitation.since.last..AWS..observation.in.mm, 
                        prcp_int =bomdata$Period.over.which.precipitation.since.last..AWS..observation.is.measured.in.minutes,
                        temp = bomdata$Air.Temperature.in.degrees.Celsius,
                        td = bomdata$Dew.point.temperature.in.degrees.Celsius,
                        RH = bomdata$Relative.humidity.in.percentage.., 
                        ws = bomdata$Wind..1.minute..speed.in.km.h*0.277778, #conversion to m/s 
                        wd = bomdata$Wind..1.minute..direction.in.degrees.true, 
                        pres = bomdata$Station.level.pressure.in.hPa) #to calculate W

mwd <- 270-bom_data$wd
ifelse( mwd < 0, mwd +360, mwd) 


bom_data$u10 <- bom_data$ws * cos(pi*mwd/180)
bom_data$v10 <- bom_data$ws * sin(pi*mwd/180)

PWS <- 6.116 * 10^((7.591386*bom_data$temp)/(bom_data$temp + 240.7263))
PW <- bom_data$RH /100 * PWS
bom_data$W <- 622 * PW / (bom_data$pres - PW)

bom_data_hrlyavg <-timeAverage(bom_data[,-(2:3)], avg.time = "hour", data.thresh = 0, statistic = "mean", interval = "min", fill =FALSE) #exclude precipitation
prcp <- as.data.frame(timeAverage(bom_data[,(1:3)], avg.time = "hour", data.thresh = 0, statistic = "sum", interval = "min", fill =FALSE))
prcp$prcp[prcp$prcp_int !=60] <- NA

if (i == 1){
prcp$prcp[9406] <- NA
prcp$prcp[29548] <- NA
}

bom_data_hrlyavg$prcp <- as.numeric(prcp[,2])
bom_data_hrlyavg$site <- stnnames[i] #I added this so the file would include a site column - this makes merging files later on easier

# write for each modelling period
for (p in 1:3){

    bom_data_subset <-subset(bom_data_hrlyavg, date >= period_start[p] & date <=period_end[p] )
    write.csv(bom_data_subset,paste("~/R_Model_Intercomparison/Campaign data/BOM data/",stnnames[i],"_",period[p],"_",stn_no[i],".csv", sep=""), row.names=FALSE)
    
}
  
}


##Add Albion Park 

stnnames <- "Wollongong_Airport"

bomdata <- read.csv(paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/Monk06062016OneMinute/HD01D_Data_068241_999999999271975.txt"), header=TRUE) 

bomdate <- ISOdatetime(bomdata$Year.Month.Day.Hour.Minutes.in.YYYY.1,
                       bomdata$MM.1, bomdata$DD.1, hour = bomdata$HH24.1, 
                       min = bomdata$MI.format.in.Local.standard.time, sec = 0, tz= "Etc/GMT-10")

bom_data <- data.frame( date = bomdate,
                        prcp = bomdata$Precipitation.since.last..AWS..observation.in.mm, 
                        prcp_int =bomdata$Period.over.which.precipitation.since.last..AWS..observation.is.measured.in.minutes,
                        temp = bomdata$Air.Temperature.in.degrees.Celsius,
                        td = bomdata$Dew.point.temperature.in.degrees.Celsius,
                        RH = bomdata$Relative.humidity.in.percentage.., 
                        ws = bomdata$Wind..1.minute..speed.in.km.h*0.277778,
                        wd = bomdata$Wind..1.minute..direction.in.degrees.true, 
                        pres = bomdata$Station.level.pressure.in.hPa) #to calculate W)

mwd <- 270-bom_data$wd
ifelse( mwd < 0, mwd +360, mwd) 


bom_data$u10 <- bom_data$ws * cos(pi*mwd/180)
bom_data$v10 <- bom_data$ws * sin(pi*mwd/180)

PWS <- 6.116 * 10^((7.591386*bom_data$temp)/(bom_data$temp + 240.7263))
PW <- bom_data$RH /100 * PWS
bom_data$W <- 622 * PW / (bom_data$pres - PW)

bom_data_hrlyavg <-timeAverage(bom_data[,-(2:3)], avg.time = "hour", data.thresh = 0, statistic = "mean", interval = "min", fill =FALSE) #exclude precipitation
prcp <- as.data.frame(timeAverage(bom_data[,(1:3)], avg.time = "hour", data.thresh = 0, statistic = "sum", interval = "min", fill =FALSE))
prcp$prcp[prcp$prcp_int !=60] <- NA

bom_data_hrlyavg$prcp <- as.numeric(prcp[,2])
bom_data_hrlyavg$site <- "Wollongong_Airport"

# write for each modelling period
for (p in 1:3){
  
  bom_data_subset <-subset(bom_data_hrlyavg, date >= period_start[p] & date <=period_end[p] )
  write.csv(bom_data_subset,paste("~/R_Model_Intercomparison/Campaign data/BOM data/",stnnames[1],"_",period[p],".csv", sep=""), row.names=FALSE)
  
}




##This is now my own code, I want to read in and combine the stations into one file (per campaign)

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/BOM data/")
##this lists the .csv files present in the folder 
list.files(pattern = "_SPS1") #first campaign
##this reads in the list of files 
bom_data <- lapply(list.files(pattern = "_SPS1"),
                     function(.file) read.csv(.file, header = TRUE,  na.strings = "NA"))
##this combines all the files into one (stacked on top of each other)

#combine all sites into one data frame
bom_data_sps1 <- do.call(rbind, bom_data) #first campaign
#fix date format 
bom_data_sps1$date <- as.POSIXct(bom_data_sps1$date, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-10")
#test
timePlot(bom_data_sps1, pollutant = "ws", type = "site")
timePlot(bom_data_sps1, pollutant = "prcp", type = "site", plot.type = "h")


list.files(pattern = "_SPS2") #second campaign
bom_data <- lapply(list.files(pattern = "_SPS2"),
                   function(.file) read.csv(.file, header = TRUE, na.strings = "NA"))
##this combines all the files into one (stacked on top of each other)
bom_data_sps2 <- do.call(rbind, bom_data) #second campaign
#fix date format 
bom_data_sps2$date <- as.POSIXct(bom_data_sps2$date, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-10")

timePlot(bom_data_sps2, pollutant = "ws", type = "site")
timePlot(bom_data_sps2, pollutant = "prcp", type = "site", plot.type = "h")
#bom_data_sps2$prcp <- bom_data_sps2$prcp *10 

list.files(pattern = "_MUMBA") #third campaign
bom_data <- lapply(list.files(pattern = "_MUMBA"),
                   function(.file) read.csv(.file, header = TRUE, na.strings = "NA"))
bom_data_mumba <- do.call(rbind, bom_data) #third campaign 
#fix date format 
bom_data_mumba$date <- as.POSIXct(bom_data_mumba$date, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-10")
#bom_data_mumba$prcp <- bom_data_mumba$prcp *10 

timePlot(bom_data_mumba, pollutant = "prcp", type = "site", plot.type = "h")

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison")
save(bom_data_mumba, bom_data_sps1, bom_data_sps2, file = "BOM_data_updated.RData")

load("BOM_data_updated.RData")

#for Yang Zhang, write .csv files 
write.csv(bom_data_sps1, file = "bom_data_sps1.csv", row.names = F, na = "")
write.csv(bom_data_sps2, file = "bom_data_sps2.csv", row.names = F, na = "")
write.csv(bom_data_mumba, file = "bom_data_mumba.csv", row.names = F, na = "")
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison")
load("sps1_model_data.Rdata")

means_bom <- ddply(bom_data_sps1, .(site), numcolwise(mean), na.rm = TRUE)
means_bom <- merge(means_bom, site_info, by = "site")
GoogleMapsPlot(means_bom, latitude = "site_lat", longitude = "site_lon", pollutant = "ws", 
               maptype = "roadmap", col = "jet", cex = 3, main = "SPS1 - Mean wind speed")
GoogleMapsPlot(means_bom, latitude = "site_lat", longitude = "site_lon", pollutant = "prcp", 
               maptype = "roadmap", col = "jet", cex = 3, main = "SPS1 - Mean precipitation")
