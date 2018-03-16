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

#once the data is in, select one profile at a time, apply pbl calculations to it 

date_list <- levels(as.factor(date))
output_date <- c()
output_zPBL <- c()

for (i in 1:length(date_list)) {
  x <- subset(bom, date %in% as.POSIXct(paste(date_list[i], "GMT-10"), tz = "ETc/GMT-10"))
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

output <- data.frame(date = as.POSIXct(output_date), pblh = output_zPBL)
new_out <- na.omit(output)

#######################
library(openair)
timePlot(new_out, pollutant = "pblh")
a <- timeVariation(new_out, pollutant = "pblh", plot.type = "p", ci = F)

test <- a$data$hour
plot(test$hour, test$Mean, type = "b")
#######################

#for CMAQ (no Td, get e from RH)
theta_v_from_RH <- function(RH, pres, temp) {
  es <- 6.112*exp((17.67*temp)/(temp+243.5))
  e = es * (RH/100.0)
  q <- (0.622*e)/(pres - (0.378*e))
  theta_v <- (1 + 0.61*q)*(temp+273.15)*((pres[1]/pres)^0.286)   
}

fname <- file.choose(new = F)
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
new_out_cmaq <- na.omit(output_cmaq)



library(openair)
timePlot(new_out_cmaq, pollutant = "pblh")
timeVariation(new_out_cmaq, pollutant = "pblh")

load(file.choose())
timeVariation(subset(cmaq_SPS1, site %in% "Sydney_Airport"), pollutant = "pblh")
Syd_cmaq <- subset(cmaq_SPS1, site %in% "Sydney_Airport")


plot(cmaq$date, cmaq$pblh, col = "red", type = "b")
points( new_out_cmaq$pblh ~new_out_cmaq$date )

PRES[1,]
plot(THTV-273.145, TENP)
head(new_out_cmaq$date)
