#this script is to read in model output from Jeremy Silver (WRF-CMAQ)

#load in packages
library(ncdf4)
library(stringi)
library(reshape2)


#assign variables
campaign <- c("SPS1", "SPS2")#, "MUMBA")

#go to folder containing model output 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/WASPSS_CMAQ_WRFCHEMemis/")


for (i in 1:length(campaign)) {
  
    fname <- paste0("cmaq_chem_aer_met_d04_",campaign[i],".nc")
    ncin <- nc_open(fname)
    
    #create a date vector 
    time <- ncvar_get(ncin,"time")
    time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
    date <- as.POSIXct(time, origin = "2000-01-01 00:00:00", tz = "UTC")
    
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
    site_name[c(2)] <- "UOW"
    #site_name[c(2,29)] <- c("UOW", "Warrawong")
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
    data$data_source <- "CMAQ"
    names(data)[c(3,4,5,6,7,10,11,12)] <- c("pblh","wd","ws","u10", "v10", "temp", "pres", "prcp")
    #make prcp in mm 
    data$prcp <- data$prcp*10
    
    #add campaign tag
    data$campaign <- campaign[i]
    
    #add site info to dataframe
    data <- merge(data, site_info, by = "site")
    
    #save the dataframe as something else 
    dataframe_name <- paste0("cmaq_",campaign[i]) 
    assign(dataframe_name,data)
    
  }


#create one large dataframe containing all six files 
cmaq <- rbind(cmaq_SPS1,cmaq_SPS2)#,cmaq_MUMBA)

#set directory and save all dataframes 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(cmaq_SPS1,cmaq_SPS2, cmaq, file = "CMAQ_model_output.RData")

load("CMAQ_model_output.RData")