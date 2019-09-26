#calculate pblh from vertical profiles - WRF-Chem (Steve Utembe)
setwd("C:/Users/eag873/ownCloud_uow/vertical_profiles/vertical_prof/")
library(ncdf4)

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

windcomponents <- function(ws,wd) {
  rad = pi/180.
  return(list(u = -ws*sin(rad*wd), v = -ws*cos(rad*wd)))
}

#Steve provided 3 files for MUMBA, one for SPS1, and one for SPS2 

files <- c("MUMBA_Jan", "MUMBA_Feb", "SPS2", "SPS1")

m <- c(5,5,11,11)
Sys.setenv(TZ = "UTC")
#period_start <-c("2013-01-01 00:00",  "2013-02-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
#period_end <- c("2013-02-02 00:00","2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")


period_start <-c("2012-12-31 14:00:00","2013-01-31 14:00:00", "2012-04-15 14:00:00", "2011-02-06 14:00:00")
period_end <- c("2013-01-31 13:00","2013-02-15 13:00:00", "2012-05-13 13:00:00","2011-03-06:00 13:00")


#Steve also provided fixed heights
Z <- c(0,59.1475,144.05,255.115,393.052,567.685,789.989,1054.55,1456.95,1877.04,2314.33,2768.89,3618.25,4470.23,5319.09,6167.33,7018.9,7869.61,8716.83,9560.35,10400.1,11237,12075.1,12922.1,13791.9,14664.4,15533.1,16383,17220.4,18058.8,18904.6,19757.8,20619.5) 

for (n in 2:33) {
  Z[n-1] <- Z[n] - ((Z[n]- Z[n-1])/2)
}

ZH <- Z[1:32] #exclude 33rd height - These are the heights at mid-level 



for (j in 1: length(files)){
fname <- paste0("wrf_chem_aer_met_d04_", files[j], "_vert_prof.nc")
ncin <-nc_open(fname)
#print(ncin)

time <- ncvar_get(ncin,"time")
time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
date <- as.POSIXct(time, origin = "2000-01-01 00:00:00", tz = "UTC")
head(date)
date <- date[1:(length(date)-m[j])]

list_var <- row.names(summary(ncin$var))
var_names <- list_var[9:13]

for (k in 1:length(var_names)){
  y <- ncvar_get(ncin, var_names[k])
  y <- y[,2,2,]
 assign(var_names[k],y)   
       }

#need theta_v 
THTV <- theta_v_from_RH(RH, PRES/100, (T-273.25) )

#need u, v
UV <- windcomponents(WS,WD)
U <- UV$u
V <- UV$v


output_zPBL_wrf_chem <- c()

for (i in 1:length(date)){
  RIx <- RI(THTV[i,], THTV[i,1],ZH,ZH[1],U[i,],U[i,1], V[i,],V[i,1])
  iPBL <- head(which(RIx > 0.25),1) 
  if (length(iPBL ==1)) {
    zPBL <- ZH[iPBL-1] + (ZH[iPBL] - ZH[iPBL-1])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
    
    #  output_date_cmaq[i] <- as.POSIXct(date[i])
    output_zPBL_wrf_chem[i] <- zPBL
  }
}

  output_wrf_chem <- data.frame(date = date, pblh = output_zPBL_wrf_chem)
  output_wrf_chem <-subset(output_wrf_chem, date >= period_start[j] & date <=period_end[j] )
  output_wrf_chem$campaign <- files[j]
  output_wrf_chem$data_source <- "W-UM2"
  dataframe_name <- paste0("wrf_chem_pblh_",files[j]) 
  assign(dataframe_name, output_wrf_chem)
}


#need to merge MUMBA files and fix campaign name 

wrf_chem_pblh_MUMBA <- rbind(wrf_chem_pblh_MUMBA_Jan, wrf_chem_pblh_MUMBA_Feb)
wrf_chem_pblh_MUMBA$campaign <- "MUMBA"

wrf_chem_pblh <- rbind(wrf_chem_pblh_SPS1, wrf_chem_pblh_SPS2, wrf_chem_pblh_MUMBA) 
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Model output")
save(wrf_chem_pblh, wrf_chem_pblh_MUMBA,wrf_chem_pblh_SPS1, wrf_chem_pblh_SPS2, file = "wrf_chem_pblh_v2.RData")

####################
library(openair)
timeVariation(wrf_chem_pblh, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10")

