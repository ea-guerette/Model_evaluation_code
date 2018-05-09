#calculate pblh
#for CMAQ (no Td, get e from RH)
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
setwd("C:/Users/eag873/ownCloud/vertical_profiles")

period <- c("MUMBA", "SPS2", "SPS1")

for (j in 1:length(period)) {
  fname <- paste0("cmaq_profile_met_d02_", period[j], ".nc")
  ncin <-nc_open(fname)
  print(ncin)
  
  #create a date vector 
  #get date
  time <- ncvar_get(ncin,"time")
  time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
  date <- as.POSIXct(time, origin = "2000-01-01 00:00:00", tz = "UTC")
  head(date)
  
  list_var <- row.names(summary(ncin$var))
  var_names <- list_var
  for (i in 9:length(list_var)){
    y <- ncvar_get(ncin, list_var[i])
    assign(var_names[i],y)
  }
  THTV <- theta_v_from_RH(RH, PRES, TENP)
  
  output_date_cmaq <- c()
  output_zPBL_cmaq <- c()
  
  for (i in 1:length(date)){
    RIx <- RI(THTV[i,], THTV[i,1],ZH[i,],ZH[i,1],U[i,],U[i,1], V[i,],V[i,1])
    iPBL <- head(which(RIx > 0.25),1) 
    if (length(iPBL ==1)) {
      zPBL <- ZH[i,iPBL-1] + (ZH[i,iPBL] - ZH[i,iPBL-1])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
      
      #  output_date_cmaq[i] <- as.POSIXct(date[i])
      output_zPBL_cmaq[i] <- zPBL
    }
  }
  output_cmaq <- data.frame(date = date, pblh = output_zPBL_cmaq)
  output_cmaq <-subset(output_cmaq, date >= period_start[j] & date <=period_end[j] )
  output_cmaq$campaign <- period[j]
  dataframe_name <- paste0("cmaq_pblh_",period[j]) 
  assign(dataframe_name, output_cmaq)
}

cmaq_pblh <- rbind(cmaq_pblh_SPS1, cmaq_pblh_SPS2, cmaq_pblh_MUMBA) 
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Model output")
save(cmaq_pblh, cmaq_pblh_MUMBA, cmaq_pblh_SPS1, cmaq_pblh_SPS2, file = "cmaq_pblh.R")

####################