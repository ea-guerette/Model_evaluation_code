#calculate pblh from CSIRO vertical profiles 
library(reshape2)

RI <- function(thetav_z, thetav_s, z, z_s, u_z, u_s, v_z, v_s){
  g <- 9.81
  ## jitter (tiny amount of random noise) added to prevent division by zero
  (g/thetav_s) * (thetav_z - thetav_s) * (z - z_s) / ((jitter(u_z) - u_s)^2 + (jitter(v_z) - v_s)^2)
}

period <- c("MUMBA", "SPS2", "SPS1")
dir <- "C:/Users/eag873/ownCloud/vertical_profiles/"

for (j in 1:length(period)){ 
setwd(paste0(dir, period[j], "_vertical_profiles/"))
filenames <- list.files()

output_date <- c()
output_zPBL <- c()

for (i in 1:length(filenames)) {
dat <- read.csv(filenames[i], header = F)

#get the date from the file_name
date <- sub(".csv","", filenames[i])

data <- t(dat)
sig <- as.numeric(data[,1]) #pres/pres[0]
q <-  as.numeric(data[,2])
tempK <- as.numeric(data[,3]) #in degree K 
u <- as.numeric(data[,4])
v <- as.numeric(data[,5])

#calculate levels heights (in m) 
z <- c()
z[1] <- 0 + (286.8875/9.81)*tempK[1] * log(1/sig[1])

for (n in 2:36){
  z[n] <- z[n-1] + (286.8875/9.81)*tempK[n] * log(sig[n-1]/sig[n])
}

#calculate theta_v from q and sig 
theta_v <- (1 + 0.61*q)*tempK*(1/sig)^0.286

#calculate Ri, using the function  
RIx <- RI(theta_v, theta_v[1], z, z[1], u, u[1], v, v[1])
#if (mean(RIx, na.rm = T) != 0) {
iPBL <- head(which(RIx > 0.25),1)
if (length(iPBL ==1)) {
  zPBL <- z[iPBL-1] + (z[iPBL] - z[iPBL-1])*(0.25 - RIx[iPBL-1])/(RIx[iPBL]-RIx[iPBL-1])
  output_date[i] <- date
  output_zPBL[i] <- zPBL
  }
}

output_csiro <- data.frame(date = as.POSIXct(output_date, format = "%Y%m%d_%H", tz = "UTC"), pblh = output_zPBL)

output_csiro$campaign <- period[j]
dataframe_name <- paste0("csiro_pblh_",period[j]) 
assign(dataframe_name, output_csiro)
} 

csiro_pblh <- rbind(csiro_pblh_SPS1, csiro_pblh_SPS2, csiro_pblh_MUMBA) 
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Model output")
save(csiro_pblh, csiro_pblh_MUMBA, csiro_pblh_SPS1, csiro_pblh_SPS2, file = "csiro_pblh.R")


library(openair)
timePlot(csiro_pblh_MUMBA, pollutant = "pblh")
timePlot(csiro_pblh_SPS1, pollutant = "pblh")
timePlot(csiro_pblh_SPS2, pollutant = "pblh")

#compare to obs 
setwd("C:/Users/eag873/Documents/R_Model_Intercomparison/Campaign data")
load("BOM_pblh.R")

a <- timeVariation(BOM_pblh, pollutant = "pblh", type = "campaign")
bom_summary <- a$data$hour
bom_summary <- na.omit(bom_summary)
bom_summary <- bom_summary[-3,]

b<- timeVariation(csiro_pblh, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10" ) #local tz does work
csiro_summary <- b$data$hour

bom_summary$data_source <- "OBS"
csiro_summary$data_source <- "CSIRO recalc"

load(file.choose())
Syd_csiro <- subset(csiro, site %in% "Sydney_Airport")

c <- timeVariation(Syd_csiro, pollutant = "pblh", type = "campaign", local.tz = "Etc/GMT-10")
Syd_csiro_summary <- c$data$hour
Syd_csiro_summary$data_source <- "CSIRO Ri = 0.3"

hourly_pblh <- rbind(bom_summary, csiro_summary, Syd_csiro_summary)

library(lattice)

original.settings <- trellis.par.get()
my.settings <- trellis.par.get()

my.settings$superpose.line$col = c("#1B9E77","dark grey", "black" )  
my.settings$superpose.line$lty = c(1,3,1)
my.settings$superpose.line$lwd = c(1,1,1)
my.settings$superpose.polygon$col = c( "#1B9E77","dark grey",  "black") 
my.settings$superpose.symbol$col = c( "#1B9E77","dark grey", "black") 
my.settings$superpose.symbol$pch = c(20,16,20)
my.settings$strip.background$col <- "white"

xyplot(Mean ~hour|campaign, data = hourly_pblh, groups = data_source, 
       ylab ="pblh (m)", 
       auto.key = list(column = 2, space = "top"), 
       par.settings = my.settings
)










