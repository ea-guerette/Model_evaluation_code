## this script transforms wide OEH files to a long format 

library(openair)
library(stringi)
library(reshape2)
library(plyr)
library(lubridate)

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## choose a OEH file to read in:
data <- read.csv(file = file.choose(), skip = 2, na.strings = "") #skip 2 first rows, since column headers are on row 3 

#data_names <- names(data)
#data_names <- data_names[3:length(data_names)]

#sites <- stri_replace_all_fixed(data_names, ".", " " )
#sites <- stri_extract_first_words(sites)
#sites <- as.factor(sites)
#site_list <- levels(sites) #this is a character vector containing the names of all the sites present in the OEH file
#Note, if present, Albion Park South has been shortened to Albion, Maison Dieu to Maison and Wagga Wagga to Wagga, etc   
####THIS IS AN ISSUE AS THERE ARE TWO ALBION PARK SITES AND TWO CAMPBELLTOWN SITES!!!

#some playing around 
data_names <- names(data)
data_names <- data_names[3:length(data_names)]

site <- stri_replace_all_fixed(data_names, pattern = ".CO.1h.average..ppm.", "")
site <- stri_replace_all_fixed(site, pattern = ".HUMID.1h.average....", "")
site <- stri_replace_all_fixed(site, pattern = ".NEPH.1h.average..bsp.", "")
site <- stri_replace_all_fixed(site, pattern = ".NO.1h.average..pphm.", "")
site <- stri_replace_all_fixed(site, pattern = ".NO2.1h.average..pphm.", "")
site <- stri_replace_all_fixed(site, pattern = ".NOX.1h.average..pphm.", "")
site <- stri_replace_all_fixed(site, pattern = ".OZONE.1h.average..pphm.", "")
site <- stri_replace_all_fixed(site, pattern = ".PM10.1h.average..µg.m³.", "")
site <- stri_replace_all_fixed(site, pattern = ".PM2.5.1h.average..µg.m³.", "")
site <- stri_replace_all_fixed(site, pattern = ".SO2.1h.average..pphm.", "")
site <- stri_replace_all_fixed(site, pattern = ".TEMP.1h.average...C.", "")
site <- stri_replace_all_fixed(site, pattern = ".WDR.1h.average....", "")
site <- stri_replace_all_fixed(site, pattern = ".SD1.1h.average....", "")
site <- stri_replace_all_fixed(site, pattern = ".WSP.1h.average..m.s.", "")

#site <- stri_replace_all_fixed(site, pattern = ".", "_")

site <- as.factor(site)

site_list <- levels(site)
##the above is clunky as hell, but it works... on 2011 file anyway; and on 2012-2013 files 
##create pretty site names to assign during the loop 
site_names <- levels(as.factor(site))
site_names <- stri_replace_all_fixed(site_names, ".", " ")
site_names <- tolower(site_names)
site_names <- capwords(site_names)
site_names <- stri_replace_all_fixed(site_names, " ", "_")

#extract date and time and save them as vectors 
Date <- data$Date
Time <- data$Time

# create a sub data frame for each site and store them in a list 
data_list <- list()
for(i in 1:length(site_list)) {
  sub.data <- data[ , grep(pattern = site_list[i], fixed = TRUE, names(data))]  
  sub_names <- names(sub.data)
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".CO.1h.average..ppm.", " CO")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".HUMID.1h.average....", " RH")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".NEPH.1h.average..bsp.", " NEPH")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".NO.1h.average..pphm.", " NO")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".NO2.1h.average..pphm.", " NO2")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".NOX.1h.average..pphm.", " NOX")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".OZONE.1h.average..pphm.", " OZONE")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".PM10.1h.average..µg.m³.", " PM10")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".PM2.5.1h.average..µg.m³.", " PM25")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".SO2.1h.average..pphm.", " SO2")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".TEMP.1h.average...C.", " TEMP")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".WDR.1h.average....", " WDR")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".SD1.1h.average....", " SD1")
  sub_names <- stri_replace_all_fixed(sub_names, pattern = ".WSP.1h.average..m.s.", " WSP")

  sub_names <- stri_extract_last_words(sub_names)  

  names(sub.data) <- sub_names
  sub.data <- data.frame(Date,Time,sub.data)
  sub.data$site <- site_names[i]

  data_list[i] <- list(sub.data)
}

#stack all the data frame into one file 
all_data <- do.call(rbind.fill, data_list)

#fix date - it is a factor :/ 
all_data$date <- paste(all_data$Date, all_data$Time, sep = " ")

all_data$date <- as.POSIXct(all_data$date, format = "%d/%m/%Y %H:%M", tz = "Etc/GMT-10")


oeh_data <- subset(all_data, select = c("date", "CO", "RH", "NEPH", "NO", "NO2", "NOX", "OZONE", "PM10", "PM25", 
                                        "SO2", "TEMP", "WDR", "SD1", "WSP", "site" ))
##this removes the duplicates if any 


#check data 
timePlot(subset(oeh_data, site %in% "Lindfield"), pollutant = "PM10", date.format = "%b %Y")
timeVariation(subset(oeh_data, site %in% c("Earlwood", "Chullora")), pollutant = "WSP", group = "site")
timePlot(oeh_data, pollutant = "OZONE", type = "site")


##the data is now fine but I want to prepare the data so it is in the right format for the intercomparison 
#change the variable names to those used in the intercomparison 

names(oeh_data) <- c("date", "CO", "RH", "NEPH", "NO", "NO2", "NOX", "O3", "PM10", "PM2.5", 
                          "SO2", "temp", "wd", "SD1", "ws", "site" )


oeh_data <- transform(oeh_data, NO = NO*10, NO2 = NO2*10, NOX = NOX*10, O3 = O3*10, SO2 = SO2*10) 

timePlot(oeh_data, pollutant = "O3", type = "site")

### SPS1 select Feb 1 - March 7 2011
oeh_data_2011 <- oeh_data 
oeh_data_sps1 <- selectByDate(oeh_data_2011, start = "01/02/2011", end = "07/03/2011")

### SPS2 and MUMBA merge four 2012-2013 files

oeh_2012_2013_A_C <- oeh_data  #first file
oeh_2012_2013_C_M <- oeh_data  #second file
oeh_2012_2013_M_S <- oeh_data  #third file 
oeh_2012_2013_S_W <- oeh_data  #fourth file 

oeh_data_2012_2013 <- merge(oeh_2012_2013_A_C, oeh_2012_2013_C_M, all = T)
oeh_data_2012_2013 <- merge(oeh_data_2012_2013, oeh_2012_2013_M_S, all = T)
oeh_data_2012_2013 <- merge(oeh_data_2012_2013, oeh_2012_2013_S_W, all = T)

oeh_data_sps2 <- selectByDate(oeh_data_2012_2013, start = "15/04/2012", end = "13/05/2012")
oeh_data_mumba <- selectByDate(oeh_data_2012_2013, start = "20/12/2012", end = "16/02/2013")

oeh_data_2011_2013_long <-merge(oeh_data_2011, oeh_data_2012_2013, all = TRUE)

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison")
save(oeh_data_2011_2013_long, file = "oeh_data_2011_2013_long.RData")

save(oeh_data_sps1, oeh_data_sps2, oeh_data_mumba, file = "OEH_campaign_data")

load("OEH_campaign_data")

setwd("C:/Documents and Settings/eag873/My Documents/Students")
write.csv(oeh_data, "oeh_data.csv", row.names = F )
save(oeh_data, file = "oeh_data.RData")
