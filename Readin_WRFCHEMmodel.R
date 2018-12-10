#this script is to read in model output from Steve Utembe (WRF-Chem)

#load in packages
library(ncdf4)
library(stringi)
library(reshape2)


#assign variables
campaign <- c("MUMBA","SPS1", "SPS2")
#start_date <- c("2013-01-01 01:00 UTC","2011-02-07 01:00 UTC", "2012-04-16 01:00 UTC") 
#end_date <- c("2013-02-16 00:00 UTC","2011-03-07 00:00 UTC","2012-05-14 00:00 UTC") 
#had to modify those dates on the fly at CASANZ - not sure why it wasn't working 

#go to folder containing model output 
#setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/Sydney_wrf_corrected/")
#setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/wrf_chem/")
setwd("C:/Users/eag873/owncloud/wrf_chem/")

for (i in 1:length(campaign)) {
  
# fname <- paste0("wrf_chem_aer_d04_",campaign[i],".nc")
  fname <- paste0("wrf_chem_aer_met_d04_",campaign[i],"_bin_emis.nc")
  ncin <- nc_open(fname)
  
  #create a date vector 
  time <- ncvar_get(ncin,"time")
  time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
  date <- as.POSIXct(time, origin = "2000-01-01 00:00:00 UTC", tz = "UTC")
  
  #get site info variables in 
  site_name <- ncvar_get(ncin, "site_name")
  site_lon <- ncvar_get(ncin,"site_lon")
  site_lat <- ncvar_get(ncin,"site_lat")
  site_owner <- ncvar_get(ncin,"site_owner")
  
  #create a site vector containing the site names in a more usable format 
  #remove white spaces to the right
  site_name <- stri_trim_right(site_name, pattern = "\\P{Wspace}")
  #replace white spaces between words by "_"
  site_name <- stri_replace_all_charclass(site_name, "\\p{WHITE_SPACE}", "_")
  #Replace MUMBA_University_site by UOW  
  #site_name[c(2)] <- "UOW"
  site_name[c(2,29)] <- c("UOW", "Warrawong")
  site <- site_name
  
  #do a similar thing for site owner 
  site_owner <- stri_trim_right(site_owner, pattern = "\\P{Wspace}")
  site_owner <- stri_replace_all_fixed(site_owner, pattern = "BoM", "BOM")
  site_owner <- stri_replace_all_fixed(site_owner, pattern = "UoW", "UOW")
  
  #create a site_info dataframe 
  site_info <- data.frame(site, site_lat, site_lon, site_owner)
  str(site_info)
  
  #Create dataframe containing model observations 
  #Create a list containing all the variable names 
  list_var <- row.names(summary(ncin$var))
  #'Get' the first variable in manually, and create the dataframe "data". The loop will then add to that dataframe. 
  #This should ensure that the dataframe has the right length 
  v  <- ncvar_get(ncin, list_var[9]) #start at nine because this is where the observations start 
  v <- data.frame(v[,2,2,])
  names(v) <- site
  data <- data.frame(date, v)
  data <- melt(data, id.vars = "date")
  names(data)[c(1,2,3)] <- c("date", "site", list_var[9])
  
  
  #then, use loop to add all the others 
  for (j in 10:length(list_var)) { #start at nine because the first 8 ones are not in a 3 x 3 format, and the 9th one is in data already 
    
    v <- ncvar_get(ncin, list_var[j])
    v <- data.frame(v[,2,2,])
    names(v) <- site
    v <- melt(v)
    data <- data.frame(data, v[,2])
  }
  names(data)[-c(1,2,3)] <- list_var[10:length(list_var)]
  
  #add data_source 
  data$data_source <- "W-UM2"
  names(data)[c(3,4,5,6,8,10,11,9)] <- c("pblh","wd","ws","u10", "v10", "temp", "pres", "prcp")
  #make prcp in mm 
  #data$prcp <- data$prcp*10 #data is already in mm, not cm 
  data$prcp[data$prcp < 0] <- NA #to remove negative values close to spin up periods 
  data$temp <- data$temp - 273.15 
  data$pres <- data$pres / 100
  
  #calculate water mixing ratio
  es <- 6.112*exp((17.67*data$temp)/(data$temp+243.5))
  e <- es * (data$RH/100.0)
  #q <- (0.622*e)/(data$pres - (0.378*e)) #specific humidity in kg/kg
  q <- (0.622*e)/(data$pres - (e))
  # I want w: grams of vapor per kg of dry air
  data$W <- q*1000
  
  data$NH4 <- data$nh4ai + data$nh4aj 
  data$NIT <- data$no3ai + data$no3aj 
  data$SO4 <- data$so4ai + data$so4aj
  data$EC <-  data$eci + data$ecj 
  
  #add campaign tag
  data$campaign <- campaign[i]
  #cut data to length
  #data <- subset(data, date >= start_date[i] & date <= end_date[i])
  
  #add site info to dataframe
  data <- merge(data, site_info, by = "site")
  
  #save the dataframe as something else 
  dataframe_name <- paste0("wrf_chem_",campaign[i]) 
  assign(dataframe_name,data)
  #timePlot(data, pollutant = "PM2.5", type = "site")
}


#create one large dataframe containing all three files 
wrf_chem <- rbind(wrf_chem_SPS1,wrf_chem_SPS2,wrf_chem_MUMBA)

#set directory and save all dataframes 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(wrf_chem_SPS1,wrf_chem_SPS2,wrf_chem_MUMBA,wrf_chem, file = "WRFCHEM_model_output_final.RData")

#library(openair)
#timePlot(wrf_chem_SPS2, pollutant = c("PM2.5", "SO4"), type = "site")
