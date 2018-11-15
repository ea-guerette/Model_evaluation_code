#this script is to read in radon model output from Jeremy Silver (WRF-CMAQ)

#load in packages
library(ncdf4)
library(stringi)
library(reshape2)
library(openair)

#assign variables
campaign <- c("MUMBA","SPS1", "SPS2")
#start_date <- c("2012-12-31 14:00 UTC","2011-02-06 14:00 UTC", "2012-04-15 14:00 UTC") 
#end_date <- c("2013-02-15 13:00 UTC","2011-03-06 13:00 UTC","2012-05-13 13:00 UTC") 
#go to folder containing model output 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/CMAQ_rn/")


for (i in 1:length(campaign)) {
  
    fname <- paste0("cmaq_rn_d04_",campaign[i],".nc")
    ncin <- nc_open(fname)
    #print(ncin)
    #create a date vector 
    time <- ncvar_get(ncin,"time")
    time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
    date <- as.POSIXct(time, origin = "2000-01-01 00:00:00", tz = "UTC")
    
    #get site info variables in 
    site_name <- ncvar_get(ncin, "site_name")
    site_lon <- ncvar_get(ncin,"site_lon")
    site_lat <- ncvar_get(ncin,"site_lat")
    #site_owner <- ncvar_get(ncin,"site_owner")
    
    #create a site vector containing the site names in a more usable format 
    #remove white spaces to the right
    site_name <- stri_trim_right(site_name, pattern = "\\P{Wspace}")
    #replace white spaces between words by "_"
    site_name <- stri_replace_all_charclass(site_name, "\\p{WHITE_SPACE}", "_")
    
    site <- site_name
    
    #create a site_info dataframe 
    site_info_rn <- data.frame(site, site_lat, site_lon)
    str(site_info_rn)
    
    #Create dataframe containing model observations 
    #Create a list containing all the variable names 
    list_var <- row.names(summary(ncin$var))
    #'Get' the first variable in manually, and create the dataframe "data". The loop will then add to that dataframe. 
    #This should ensure that the dataframe has the right length 
    v  <- ncvar_get(ncin, list_var[8]) #start at nine because this is where the observations start 
    v <- data.frame(v[,2,2,])
    names(v) <- site
    data <- data.frame(date, v)
    data <- melt(data, id.vars = "date")
    names(data)[c(1,2,3)] <- c("date", "site", list_var[8])
    
    #add data_source 
    data$data_source <- "W-UM1"
   
    
    #add campaign tag
    data$campaign <- campaign[i]
    #cut data to length
    #data <- subset(data, date >= start_date[i] & date <= end_date[i])
    
    #add site info to dataframe
    data <- merge(data, site_info_rn, by = "site")
    
    #save the dataframe as something else 
    dataframe_name <- paste0("cmaq_rn_",campaign[i]) 
    assign(dataframe_name,data)
    timePlot(data, pollutant = "Rn", type = "site")
  }


#create one large dataframe containing all three files 
cmaq_rn <- rbind(cmaq_rn_SPS1,cmaq_rn_SPS2,cmaq_rn_MUMBA)

#set directory and save all dataframes 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(cmaq_rn_SPS1,cmaq_rn_SPS2,cmaq_rn_MUMBA, cmaq_rn, file = "CMAQ_radon_model_output_new.RData")
save(site_info_rn, file = "site_info_rn.Rdata")

#load("CMAQ_model_output_new.RData")

