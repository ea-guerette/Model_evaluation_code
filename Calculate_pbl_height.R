#this code is to read in Sydney Airport sounding data and output a timeseries of pbl heights

library(ncdf4)

#some functions:
#from Bolton 1980, function to calculate theta_v from td, pres, temp
theta_v_from_td <- function(df, td, pres, temp) {
  e <- 6.112*exp((17.67*df[[td]])/(df[[td]]+243.5))
  q <- (0.622*e)/(df[[pres]] - (0.378*e))
  theta_v <- (1 + 0.61*q)*(df[[temp]]+273.15)*(1000/df[[pres]])^0.286
}

#function to calculate u and v from ws, wd - function adapted from Jeremy Silver's 
windcomponents <- function(df, ws,wd) {
  rad = pi/180.
  return(list(u = -df[[ws]]*sin(rad*df[[wd]]), v = -df[[ws]]*cos(rad*df[[wd]])))
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


data <- read.csv(file.choose())

date <- ISOdatetime(data$Year.1, data$Month.1, data$Day.1, hour = data$Hour.1,
                    min = data$Minute.in.Local.Standard.Time, sec = 0, tz="Etc/GMT-10")

bom <- data.frame(date = date, 
                  temp = data$Air.temperature.in.Degrees.C, 
                  td = data$Dew.point.temperature.in.Degrees.C, 
                  ws = data$Wind.speed.measured.in.km.h*0.277778, #conversion to m/s 
                  wd = data$Wind.direction.measured.in.degrees, 
                  pres = data$Pressure.in.hPa, #to calculate W
                  geo_height = data$Geopotential.height.in.gpm.to.nearest.0.1m)


bom$theta_v <- theta_v_from_td(bom, "td", "pres", "temp")


UV <- windcomponents(bom, "ws", "wd")
bom$u <- UV$u
bom$v <- UV$v

#select one profile at a time, apply pblh calculation to it 

date_list <- levels(as.factor(date))
output_date <- c()
output_zPBL <- c()

for (i in 1:length(date_list)) {
  x <- subset(bom, date %in% as.POSIXct(paste(date_list[i], "GMT-10"), tz = "ETc/GMT-10"))
  x <- na.omit(x) #this should make the zPBL calculation work most of the time, but introduces more uncertainty
  RIx <- RI(x$theta_v, x$theta_v[1], x$geo_height, x$geo_height[1], x$u, x$u[1], x$v, x$v[1])
  #if (mean(RIx, na.rm = T) != 0) {
  iPBL <- head(which(RIx > 0.25),1)
  if (length(iPBL ==1)) {
  zPBL <- x$geo_height[iPBL-1] + (x$geo_height[iPBL] - x$geo_height[iPBL-1])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
  
  output_date[i] <- paste(date_list[i], "GMT-10")
  output_zPBL[i] <- zPBL
  }
}

#}

output <- data.frame(date = as.POSIXct(output_date, tz = "Etc/GMT-10"), pblh = output_zPBL)
new_out <- na.omit(output)

#save data for campaign periods
period <- c("MUMBA", "SPS2", "SPS1")
period_start <-c("2013-01-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")

for (p in 1:3){
  bom_data_subset <-subset(new_out, date >= period_start[p] & date <=period_end[p] )
  bom_data_subset$campaign <- period[p] 
  dataframe_name <- paste0("BOM_pblh_",period[p]) 
  assign(dataframe_name, bom_data_subset)
}

BOM_pblh <- rbind(BOM_pblh_SPS1,BOM_pblh_SPS2,BOM_pblh_MUMBA)
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Campaign data")
save(BOM_pblh, BOM_pblh_MUMBA, BOM_pblh_SPS1, BOM_pblh_SPS2, file = "BOM_pblh.RData")

####################### end of code - OBS


summary(as.factor(format(BOM_pblh_SPS1$date, "%H")))
summary(as.factor(format(BOM_pblh_SPS2$date, "%H")))
summary(as.factor(format(BOM_pblh_MUMBA$date, "%H")))
#more data seems better, so keeping the new approach 

timePlot(BOM_pblh_MUMBA, pollutant = "pblh") #picking up the hot days, 13 afternoon profiles 
timePlot(BOM_pblh_SPS1, pollutant = "pblh") #there is a gap in the middle still
timePlot(BOM_pblh_SPS2, pollutant = "pblh") #looks quite complete

#######################



