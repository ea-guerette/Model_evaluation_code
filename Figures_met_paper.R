library(openair)
library(plyr)
library(lattice)
library(latticeExtra)

target_list <- c("temp", "RH", "ws", "u10", "v10", "prcp")
target_names <- c("temperature", "RH (%)", "wind speed (m/s)", "u wind", "v wind", "precip")
date_start <- c("21/12/2012","01/02/2011", "01/04/2012")
date_end <- c("15/02/2013","07/03/2011","17/05/2012")

for (i in 1:length(target_list)) {

  d <- timeVariation(wrf, pollutant = target_list[i], group = "data_source", type = "campaign", ci = T, ylab = target_names[i])
  print(d, subset = "hour")
  
  for (j in 1:length(date_start)){
  scatterPlot(selectByDate(wrf, start = date_start[j], end = date_end[j]), x = "date", y = target_list[i], ylab = target_names[i], group = "data_source", type = "campaign", plot.type = "l")
  }
}
#the above will work for showing all models, averaged across all sites separately for each campaign 
#will need to reduce the number of sites to include only the BOM sites! 
 


#this will require wide data... 
TaylorDiagram(sps2, obs = "WS.obs", mod = "WS.mod", group = "data_source.mod")
stats <- modStats(sps2, obs = "WS.obs",mod = "WS.mod", type = "data_source.mod")

#what about spatial analysis? - need to compute stats first - wide format 

means <- ddply(wrf, .(site,campaign, data_source), numcolwise(mean), na.rm = TRUE)

#strip = function(...) strip.default(style=1, ...) 
#strip.left = function(...) strip.custom(style = 1, horizontal = F, strip.left = T)

strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)
a1 <- GoogleMapsPlot(means, latitude = "site_lat", longitude = "site_lon", pollutant = "ws",
                     maptype = "roadmap", col = "jet", cex = 2, main = "mean wind speed", 
                     key.footer = "(m/s)", xlab = "lon", ylab = "lat", type = c( "campaign", "data_source"), strip.left = my.strip.left)
#this does not work - function is broken (cannot find function "strip")
useOuterStrips(a1$plot)



GoogleMapsPlot(stats_ws_sps1_sites, latitude = "site_lat", longitude = "site_lon", pollutant = "r", type = "data_source.mod",
               maptype = "roadmap", col = "jet", cex = 3, main = "SPS1 - correlation - Wind speed")





#so how do I loop through these plots? 
mod_list <- levels(as.factor(wrf$data_source))
stat_list <- c("r", "RMSE", "MB")

