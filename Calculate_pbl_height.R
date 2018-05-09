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
period_start <-c("2012-12-21 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")

for (p in 1:3){
  bom_data_subset <-subset(new_out, date >= period_start[p] & date <=period_end[p] )
  bom_data_subset$campaign <- period[p] 
  dataframe_name <- paste0("BOM_pblh_",period[p]) 
  assign(dataframe_name, bom_data_subset)
}

BOM_pblh <- rbind(BOM_pblh_SPS1,BOM_pblh_SPS2,BOM_pblh_MUMBA)
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Campaign data")
save(BOM_pblh, BOM_pblh_MUMBA, BOM_pblh_SPS1, BOM_pblh_SPS2, file = "BOM_pblh.R")

####################### end of code - OBS


summary(as.factor(format(BOM_pblh_SPS1$date, "%H")))
summary(as.factor(format(BOM_pblh_SPS2$date, "%H")))
summary(as.factor(format(BOM_pblh_MUMBA$date, "%H")))
#more data seems better, so keeping the new approach 

timePlot(BOM_pblh_MUMBA, pollutant = "pblh") #picking up the hot days, 13 afternoon profiles 
timePlot(BOM_pblh_SPS1, pollutant = "pblh") #there is a gap in the middle still
timePlot(BOM_pblh_SPS2, pollutant = "pblh") #looks quite complete

#######################

#for CMAQ (no Td, get e from RH)
theta_v_from_RH <- function(RH, pres, temp) {
  es <- 6.112*exp((17.67*temp)/(temp+243.5))
  e = es * (RH/100.0)
  q <- (0.622*e)/(pres - (0.378*e))
  theta_v <- (1 + 0.61*q)*(temp+273.15)*((pres[1]/pres)^0.286)   
}

#read in all three vertical profile files 
setwd("C:/Users/eag873/ownCloud/vertical_profiles")

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

################################# end of code - model
######################

library(openair)
timePlot(cmaq_pblh_SPS1, pollutant = "pblh", local.tz = "Etc/GMT-10")
############################

#to make an example plot - load in model and OBS
load("cmaq_pblh.R")
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Campaign data")
load("BOM_pblh.R")

#also load in pblh included in surface output  
load(file.choose())
Syd_cmaq <- subset(cmaq, site %in% "Sydney_Airport")
#this has not been cut to length 

#cut to length
mumba_mod <- subset(Syd_cmaq, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-20 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
sps1_mod <- subset(Syd_cmaq, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(Syd_cmaq, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")

Syd_cmaq <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 

##############

#use timeVariation on each data set to get mean values for each hour of the day 
a <- timeVariation(BOM_pblh, pollutant = "pblh", type = "campaign")
bom_summary <- a$data$hour
bom_summary <- na.omit(bom_summary)
bom_summary <- bom_summary[-3,]

b<- timeVariation(cmaq_pblh, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10" ) #local tz does work
cmaq_summary <- b$data$hour

for_stats <- merge(bom_summary,cmaq_summary, by = c("variable", "hour", "campaign"), suffixes = c(".obs", ".mod"))
names(for_stats)[2] <-"HOD"
for_stats$HOD <- as.factor(for_stats$HOD)
stats_pblh <- modStats(for_stats, mod = "Mean.mod", obs = "Mean.obs", type = c("campaign", "HOD"))
  
#OK, now plot all three 
bom_summary$data_source <- "OBS"
cmaq_summary$data_source <- "CMAQ"

c <- timeVariation(Syd_cmaq, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10")
Syd_cmaq_summary <- c$data$hour
Syd_cmaq_summary$data_source <- "CMAQ TKE"

#use mean hourly values to make xy.plots 
hourly_pblh <- rbind(bom_summary, cmaq_summary, Syd_cmaq_summary)


library(lattice)

original.settings <- trellis.par.get()
my.settings <- trellis.par.get()
names(my.settings)
my.settings$superpose.line$col = c("#1B9E77","dark gray", "black" )  
my.settings$superpose.line$lty = c(1,3,1)
my.settings$superpose.line$lwd = c(1,1,1)
my.settings$superpose.polygon$col = c( "#1B9E77","dark gray", "black") 
my.settings$superpose.symbol$col = c( "#1B9E77","dark gray", "black") 
my.settings$superpose.symbol$pch = c(20:20:16)
my.settings$strip.background$col <- "white"
#my.settings$dot.symbol$pch <- c(20:21)
#my.settings$plot.line <- c("l", "p")

xyplot(Mean ~hour|campaign, data = hourly_pblh, groups = data_source, 
      ylab ="pblh (m)", 
      auto.key = list(column = 2, space = "top"), 
      par.settings = my.settings
       )



####################
##############
