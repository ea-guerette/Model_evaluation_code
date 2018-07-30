#calculate pblh from OEH vertical profiles 

#library(reshape2)
library(stringi)

RI <- function(thetav_z, thetav_s, z, z_s, u_z, u_s, v_z, v_s){
  g <- 9.81
  ## jitter (tiny amount of random noise) added to prevent division by zero
  (g/thetav_s) * (thetav_z - thetav_s) * (z - z_s) / ((jitter(u_z) - u_s)^2 + (jitter(v_z) - v_s)^2)
}

period <- c("MUMBA", "SPS2", "SPS1")
period_start <-c("2013-01-01 00:00", "2012-04-16 00:00", "2011-02-07 00:00")
period_end <- c("2013-02-16 00:00", "2012-05-14 00:00","2011-03-07 00:00")

dir <- "C:/Users/eag873/ownCloud/vertical_profiles/OEH/"

for (j in 2:length(period)){ 
  
  data <- read.csv(paste0(dir, "Sydney_Airport_AMO-CCAM-allLevels_",period[j], ".csv"), header = T)
 
  date <- stri_replace_all_fixed(data$Date, "+00:00", "")
  date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  data$Date <- date
 
  date_list <- levels(as.factor(data$Date)) 

  
  
  output_zPBL <- c()
  
 for (k in 1:length(date_list)) {
   ids <- which(as.character(data$Date) %in% date_list[k])
   dat <- data[ids,]
   
   #calculate levels heights (in m) 
   z <- c()
   z[1] <- 0 + (286.8875/9.81)*dat$temp[1] * log(1/dat$lev[1])
   
   for (n in 2:35){
     z[n] <- z[n-1] + (286.8875/9.81)*dat$temp[n] * log(dat$lev[n-1]/dat$lev[n])
   }
   
   #calculate theta_v from mixr and lev
   theta_v <- (1 + 0.61*dat$mixr)*dat$temp*(1/dat$lev)^0.286
  
   u <- dat$u
   v <- dat$v
   
   RIx <- RI(theta_v, theta_v[1], z, z[1], u, u[1], v, v[1])
   
   iPBL <- head(which(RIx > 0.25),1)
   if (length(iPBL ==1)) {
     zPBL <- z[iPBL-1] + (z[iPBL] - z[iPBL-1])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
     
     output_zPBL[k] <- zPBL
   }
   
 }

  output_oeh <- data.frame(date = as.POSIXct(date_list, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), pblh = output_zPBL)
  output_oeh <-subset(output_oeh, date >= period_start[j] & date <=period_end[j] )
  output_oeh$campaign <- period[j]
  output_oeh$data_source <- "O-CTM"
  dataframe_name <- paste0("oeh_pblh_",period[j]) 
  assign(dataframe_name, output_oeh)


}

oeh_pblh <- rbind(oeh_pblh_SPS1, oeh_pblh_SPS2, oeh_pblh_MUMBA) 
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Model output")
save(oeh_pblh, oeh_pblh_MUMBA, oeh_pblh_SPS1, oeh_pblh_SPS2, file = "oeh_pblh.RData")
