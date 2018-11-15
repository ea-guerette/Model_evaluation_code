#radon vertical profiles 

#calculate pblh
library(ncdf4)

setwd("C:/Users/eag873/ownCloud/vertical_profiles/ANSTO")

period <- c("MUMBA", "SPS2", "SPS1")
period_start <-c("2013-01-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")
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
  var_names <- list_var[c(4,5,7:11,13)]
  for (i in 1:length(var_names)){
    y <- ncvar_get(ncin, var_names[i])
    assign(var_names[i],y)
    
  }
}  

xyplot( height_agl[,880] ~ rn_1_activity[,880], pch = 20, type = "b", main = paste("Radon profile", date[880]))

#run pblh calc 
#reload model: wrf_11_MUMBA 
syd <- subset(wrf_11_MUMBA, site %in% "Sydney_Airport")

i=4
xyplot(height_agl[,i] ~ rn_1_activity[,i], pch = 20, type = "b", main = paste("Radon profile", date[i]),
        xlab = "radon Bq/m3", ylab = "Height above ground (m)", ylim = c(0,5000),
        panel =function(...){  
          panel.xyplot(...);
          panel.abline(h = c(ansto_pblh_MUMBA$pblh[i],syd$pblh[i]), col = c("black","red"), lty = 3)
        }
       )
#

dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
load(paste0(dir_obs,"/BOM_pblh.RData"))

ids <- which(date %in% BOM_pblh_MUMBA$date)


i = 1829
xyplot(height_agl[,i] ~ rn_1_activity[,i], pch = 20, type = "b", main = paste("Radon profile", date[i]),
       xlab = "radon Bq/m3", ylab = "Height above ground (m)", ylim = c(0,4000),
       panel =function(...){  
         panel.xyplot(...);
         panel.abline(h = c(BOM_pblh_MUMBA$pblh[52],ansto_pblh_MUMBA$pblh[i],syd$pblh[i]), col = c("black","red", "purple"), lty = 1)
       }
)


