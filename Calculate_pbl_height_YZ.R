#calculate pblh from YZ's vertical profiles 
library(ncdf4)

#for YZ (no Td, get e from RH)
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


#read in vertical profile files 
setwd("C:/Users/eag873/ownCloud_uow/vertical_profiles/YZ/")

#period <- c("MUMBA", "SPS2", "SPS1")
#period_start <-c("2013-01-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
#period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")

Sys.setenv(TZ = "UTC")
period <- c("MUMBA", "SPS2", "SPS1")
period_start <-c("2012-12-31 14:00:00", "2012-04-15 14:00:00", "2011-02-06 14:00:00")
period_end <- c("2013-02-15 13:00:00", "2012-05-13 13:00:00","2011-03-06:00 13:00")


start.date <- c("2012-12-19", "2012-04-09","2011-01-30")
len <- c(1465, 889, 913)


model <- c("ROMS", "WRFCHEM")
mn <- c("W-NC2", "W-NC1")


for (k in 1:length(model)){ 
for (j in 1:length(period)) {
  fname <- paste0("Sydney_Airport.mlevels.", model[k], ".3km.", period[j], ".nc")
  ncin <-nc_open(fname)
  print(ncin)
  
  #create a date vector 
  #get date
#  time <- ncvar_get(ncin,"Time")
#  time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
  date = seq(as.POSIXct(start.date[j], tz = "UTC"), by = "hours", length = len[j] )
  head(date)
  
  
  list_var <- row.names(summary(ncin$var))
  var_names <- list_var[c(1:6)]
  for (i in 1:length(var_names)){
    y <- ncvar_get(ncin, var_names[i])
    assign(var_names[i],y)
  }
  THTV <- theta_v_from_RH(RH, P/100, (T-273.15) ) 
  
  for (n in 2:33) {
    Z[n-1,] <- Z[n,] - ((Z[n,]- Z[n-1,])/2)
  }

  Z <- Z[c(1:32),] #exclude 33rd height 
  
  output_zPBL_yz <- c()
  
  for (i in 1:length(date)){
    RIx <- RI(THTV[,i], THTV[1,i],Z[,i],Z[1,i],U[,i],U[1,i], V[,i],V[1,i])  #need to check this 
    iPBL <- head(which(RIx > 0.25),1) 
    if (length(iPBL ==1)) {
      zPBL <- Z[iPBL-1,i] + (Z[iPBL,i] - Z[iPBL-1,i])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
      
      
      output_zPBL_yz[i] <- zPBL
    }
  }
  output_yz <- data.frame(date = date, pblh = output_zPBL_yz)
  output_yz <-subset(output_yz, date >= period_start[j] & date <=period_end[j] )
  output_yz$campaign <- period[j]
  output_yz$data_source <- mn[k]
  dataframe_name <- paste0(model[k],"_pblh_",period[j]) 
  assign(dataframe_name, output_yz)
}
  
  
}
  
yz_pblh <- rbind(ROMS_pblh_MUMBA, ROMS_pblh_SPS1, ROMS_pblh_SPS2, WRFCHEM_pblh_MUMBA, WRFCHEM_pblh_SPS1, WRFCHEM_pblh_SPS2)
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Model output")
save(yz_pblh, ROMS_pblh_MUMBA, ROMS_pblh_SPS1, ROMS_pblh_SPS2, WRFCHEM_pblh_MUMBA, WRFCHEM_pblh_SPS1, WRFCHEM_pblh_SPS2, file = "yz_pblh_v2.RData")


library(openair)
timeVariation(yz_pblh, pollutant = "pblh", type = "campaign", group = "data_source", local.tz = "Etc/GMT-10")
