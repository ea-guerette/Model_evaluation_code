#calculate pblh
library(ncdf4)
#for ANSTO (no Td, get e from RH)
theta_v_from_RH <- function(RH, pres, temp) {
  es <- 6.112*exp((17.67*temp)/(temp+243.5))
  e = es * (RH/100.0)
  q <- (0.622*e)/(pres - (0.378*e))
  theta_v <- (1 + 0.61*q)*(temp+273.15)*((pres[1]/pres)^0.286)   
}

## Richardson number - this function is from Jeremy Silver's
RI <- function(thetav_z, thetav_s, z, z_s, u_z, u_s, v_z, v_s){
  ## denom <- (u_z - u_s)^2 + (v_z - v_s)^2 = WS2_z + WS2_s -2* (u_z*u_s + v_z*v_s)
  ## WS2_z <- u_z^2 + v_z^2
  ## WS2_s <- u_s^2 + v_s^2
  g <- 9.81
  ## jitter (tiny amount of random noise) added to prevent division by zero
  (g/thetav_s) * (thetav_z - thetav_s) * (z - z_s) / ((jitter(u_z) - u_s)^2 + (jitter(v_z) - v_s)^2)
}

#read in all three vertical profile files 
setwd("C:/Users/eag873/ownCloud_uow/vertical_profiles/ANSTO")

Sys.setenv(TZ = "UTC")
period <- c("MUMBA", "SPS2", "SPS1")
#period_start <-c("2013-01-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
#period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")
period_start <-c("2012-12-31 14:00:00", "2012-04-15 14:00:00", "2011-02-06 14:00:00")
period_end <- c("2013-02-15 13:00:00", "2012-05-13 13:00:00","2011-03-06:00 13:00")

#period <- c("MUMBA", "SPS2", "SPS1")
#period_start <-c("2013-01-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
#period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")
init <- c("2012-11-25 00:00:00", "2012-04-11 00:00:00","2011-02-01 00:00:00")

for (j in 1:length(period)) {
  fname <- paste0("ANSTO-", period[j], "-vertical-profiles_config-11.nc")
  ncin <-nc_open(fname)
  print(ncin)
  
  #create a date vector 
  #get date
  time <- ncvar_get(ncin,"Time")
  time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
  date <- as.POSIXct(time, origin = init[j], tz = "UTC")
  head(date)
  
  list_var <- row.names(summary(ncin$var))
  var_names <- list_var[c(4,7:11,13)]
  for (i in 1:length(var_names)){
    y <- ncvar_get(ncin, var_names[i])
    assign(var_names[i],y)
  }
  THTV <- theta_v_from_RH(rh, pressure, (temp-273.15))
  
  #output_date_ansto <- c()
  output_zPBL_ansto <- c()
  
  for (i in 1:length(date)){
    RIx <- RI(THTV[,i], THTV[1,i],height_agl[,i],height_agl[1,i],u[,i],u[1,i], v[,i],v[1,i])
    iPBL <- head(which(RIx > 0.25),1) 
    if (length(iPBL ==1)) {
      zPBL <- height_agl[iPBL-1, i] + (height_agl[iPBL, i] - height_agl[iPBL-1, i])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
      
      #  output_date_ansto[i] <- as.POSIXct(date[i])
      output_zPBL_ansto[i] <- zPBL
    }
  }
  output_ansto <- data.frame(date = date, pblh = output_zPBL_ansto)
  output_ansto <-subset(output_ansto, date >= period_start[j] & date <=period_end[j] )
  output_ansto$campaign <- period[j]
  output_ansto$data_source <- "W-A11"
  dataframe_name <- paste0("ansto_pblh_",period[j]) 
  assign(dataframe_name, output_ansto)
}

ansto_pblh <- rbind(ansto_pblh_SPS1, ansto_pblh_SPS2, ansto_pblh_MUMBA) 
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Model output")
save(ansto_pblh, ansto_pblh_MUMBA, ansto_pblh_SPS1, ansto_pblh_SPS2, file = "ansto_pblh_v2.RData")

####################

library(openair) 
timeVariation(ansto_pblh, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10")
