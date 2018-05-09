#this code is to read in Yang's model output 
#there are three files per site per campaign

library(ncdf4)

library(plyr)

model <- c("ROMS", "WRFCHEM")
model_names <- c("W-NC2", "W-NC1")
campaign <- c("MUMBA","SPS1", "SPS2")
output <- c("meteo", "gas", "pm")
start.date <- c("2012-12-19","2011-01-30", "2012-04-09")
len <- c(1465, 913, 889)
  
#go to folder containing model output 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/YZ/")

for (j in 1:length(model)){
    #go to folder containing model output 
    dir <- paste0("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/YZ/", model[j], "/")
    for (i in 1:length(campaign)) {
    setwd(paste0(dir, campaign[i], "/"))
      Sys.setenv(TZ = "UTC")
      date = seq(as.POSIXct(start.date[i]), by = "hours", length = len[i] , tzone = "UTC")
      site_names <- levels(as.factor(sub("\\..*", "", list.files())))
      data_list <- list()
      for (k in 1:length(site_names)){     
      fname <- list.files(pattern = paste0("^",site_names[k],"\\."))
      v = list()
      
        for (l in 1:length(fname)) {
        ncin <- nc_open(fname[l])
        list_var <- row.names(summary(ncin$var))
        v[[l]]  <- sapply(list_var, function(x) ncvar_get(ncin, x))
        nc_close(ncin)
        }
      data <- data.frame(do.call(cbind, v))
      data$date <- date
      data$site <- site_names[k]
      data_list[[k]] <- data
      }
      data <- do.call(rbind, data_list)
      
      #fix a few things: 
      data <- data[,-grep("\\.", names(data))]
      data$site <- gsub("Warrawong_radon", "Warrawong", data$site)
      data$site <- gsub("MUMBA_Uni", "UOW", data$site)
      
      #change units of gas species from ppm to ppb
      data[,1:13] <- data[,1:13]*1000
      #change units of pressure to hPa (from Pa), temperature to C (from K)
      data$PSFC <- data$PSFC /100
      data$T2 <-  data$T2 - 273.15
      
      es <- 6.112*exp((17.67*data$T2)/(data$T2+243.5))
      e <- es * (data$RH/100.0)
      q <- (0.622*e)/(data$PSFC - (0.378*e)) #specific humidity in kg/kg
      # I want w: grams of vapor per kg of dry air
      data$W <- q*1000
      
      names(data)[1:33] <- c("CO", "HCHO", "C5H8", "IsopRxnProd", "Methanol", "NH3", 
                       "NO", "NO2", "O3", "SO2", "Terpenes", "Toluene", "Xylenes",
                       "site_lat", "site_lon", "SWR", "pblh", "pres", "temp", "u10", "v10",
                       "RH", "wd", "ws", "prcp", "PM10", "PM2.5", "EC", "NH4", "NO3", "OC", "P25", "SO4")
      data$NOx <- data$NO + data$NO2
      
      data$campaign <- campaign[i]
      data$data_source <- model_names[j]
      
      data_name <- paste(model[j], campaign[i], sep = "_")
      assign(data_name, data)
    }  
    }

yz_mod <- rbind(ROMS_MUMBA,ROMS_SPS1,ROMS_SPS2, WRFCHEM_MUMBA, WRFCHEM_SPS1, WRFCHEM_SPS2)
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
save(yz_mod, ROMS_MUMBA,ROMS_SPS1,ROMS_SPS2, WRFCHEM_MUMBA, WRFCHEM_SPS1, WRFCHEM_SPS2, file = "YZ.RData")

#library(openair)
#timePlot(WRFCHEM_SPS1, pol = "C5H8", type = "site")
#timeVariation(yz_mod, pollutant = "O3", group = "data_source", type = "campaign")

