#this code is to read in and save MSWEP precipitation data in R format 
# also has some analysis code at the end 

library(ncdf4)
library(stringi)
library(reshape2)
library(openair)
campaign <- c("MUMBA","SPS1", "SPS2")
start_date <- c("2012-12-31 14:00 UTC","2011-02-06 14:00 UTC", "2012-04-15 14:00 UTC") 
end_date <- c("2013-02-15 13:00 UTC","2011-03-06 13:00 UTC","2012-05-13 13:00 UTC") 


setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/MSWEPv1.2/")

for (i in 1:length(campaign)) {
  fname <- paste0("MSWEP_V1.2_3hourly_",campaign[i],".nc")
  ncin <- nc_open(fname)

  print(ncin)

#looks like the other files 
#create a date vector 
time <- ncvar_get(ncin,"time")
time = time*3600 #*3600 because R expect time in seconds since, not hours since (but is this conversion OK?)
date <- as.POSIXct(time, origin = "2000-01-01 00:00:00", tz = "UTC")

#get site info variables in 
site_name <- ncvar_get(ncin, "site_name")
site_lon <- ncvar_get(ncin,"site_lon")
site_lat <- ncvar_get(ncin,"site_lat")
site_owner <- ncvar_get(ncin,"site_owner")

#create a site vector containing the site names in a more usable format 
#remove white spaces to the right
site_name <- stri_trim_right(site_name, pattern = "\\P{Wspace}")
#replace white spaces between words by "_"
site_name <- stri_replace_all_charclass(site_name, "\\p{WHITE_SPACE}", "_")
site_name[c(2)] <- "UOW"

site_owner <- stri_trim_right(site_owner, pattern = "\\P{Wspace}")
site_owner <- stri_replace_all_fixed(site_owner, pattern = "BoM", "BOM")
site_owner <- stri_replace_all_fixed(site_owner, pattern = "UoW", "UOW")

site <- site_name
site_info_prcp <- data.frame(site, site_lat, site_lon, site_owner)

list_var <- row.names(summary(ncin$var))

v  <- ncvar_get(ncin, list_var[9]) #start at nine because this is where the observations start 
v <- data.frame(v[,2,2,])
names(v) <- site_name
data <- data.frame(date, v)
data <- melt(data, id.vars = "date")
names(data)[c(1,2,3)] <- c("date", "site", list_var[9])

#timePlot(data, pollutant = "PRCP", type = "site")

data$data_source <- "MSWEP"
names(data)[3] <- "prcp"
#make prcp in mm 
data$prcp <- data$prcp*10

#add campaign tag
data$campaign <- campaign[i]
#cut to length 
data <- subset(data, date >= start_date[i] & date <= end_date[i])
#add site info to dataframe
data <- merge(data, site_info_prcp, by = "site")
#data_subset <-subset(data, date >= start_date[i] & date <=end_date[i] )

#save the dataframe as something else 
dataframe_name <- paste0("mswep_",campaign[i]) 
assign(dataframe_name,data)
#assign(dataframe_name,data_subset)
timePlot(data, pollutant = "prcp", type = "site")
}

mswep <- rbind(mswep_SPS1,mswep_SPS2,mswep_MUMBA)
save(mswep, file = "MSWEP.RData")


#compare to point observations - BOM sites  
library(plyr)

setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
load("BOM_data_updated.RData")
BOM <- bom_data_all_campaigns
site_list <- levels(as.factor(BOM$site))

grid_prcp <- subset(mswep, site %in% site_list)

#grid_SPS1 <- selectByDate(grid_SPS1, start = "07/02/2011", end = "06/03/2011")

BOM$data_source <- "OBS"

prcp_ln <- rbind.fill(BOM, grid_prcp)

campaign <- c("MUMBA","SPS1", "SPS2")
date_start <- c("21/12/2012","07/02/2011", "16/04/2012") 
date_end <- c("15/02/2013","06/03/2011","13/05/2012") 

scatterPlot(prcp_ln, x = "date", y = "prcp", 
            ylab = "PRCP (mm)", group = "data_source", type = "site", plot.type = "l")
for ( j in 1:length(date_start)){
scatterPlot(selectByDate(prcp_ln, start = date_start[j], end = date_end[j]), x = "date", y = "prcp", 
            ylab = "PRCP (mm)", group = "data_source", type = "site", plot.type = "l", main = campaign[j])
}


#add models 
#go to Figures_met_abalysis.R to read in data 
#model_met_SPS1 <- subset(model_met, campaign %in% "SPS1")
model_met_3hr <- timeAverage(model_met, avg.time = "3 hour", statistic = "sum", type = c("data_source", "site","campaign")) 
model_met_daily <- timeAverage(model_met, avg.time = "1 day", statistic = "sum", type = c("data_source", "site","campaign")) 

prcp_ln_daily <- timeAverage(prcp_ln, avg.time = "1 day", statistic = "sum", type = c("data_source", "site","campaign"))         

#combine both 
prcp_daily <- rbind.fill(prcp_ln_daily, model_met_daily)

#scatterPlot(prcp_daily, x = "date", y = "prcp", 
#            ylab = "PRCP (mm)", group = "data_source", type = "site", plot.type = "l")
for ( j in 1:length(date_start)){
  scatterPlot(selectByDate(prcp_daily, start = date_start[j], end = date_end[j]), x = "date", y = "prcp", 
              ylab = "PRCP (mm)", group = "data_source", type = "site", plot.type = "l", main = campaign[j])
}

##all data_source need to contain same number of days before summing!! Thanks Yang
###should not be needed anymore since I am cutting the data as I import it in 
#prcp_daily_sps1 <- subset(prcp_daily, campaign %in% "SPS1")
#prcp_daily_sps2 <- subset(prcp_daily, campaign %in% "SPS2")
#prcp_daily_mumba <- subset(prcp_daily, campaign %in% "MUMBA")

#cut them to length 
#start_date <- c("2013-01-01","2011-02-07", "2012-04-16") 
#end_date <- c("2013-02-15","2011-03-06","2012-05-13") 

#prcp_daily_sps1 <-subset(prcp_daily_sps1, date >= start_date[2] & date <= end_date[2] )
#prcp_daily_sps2 <-subset(prcp_daily_sps2, date >= start_date[3] & date <= end_date[3] )
#prcp_daily_mumba <-subset(prcp_daily_mumba, date >= start_date[1] & date <= end_date[1] )

#prcp_daily_updated <- rbind.fill(prcp_daily_sps1,prcp_daily_sps2,prcp_daily_mumba)
#write.csv(prcp_daily_updated, file ="daily_prcp.csv", row.names = F)
sums <- ddply(prcp_daily, .(site, campaign, data_source), numcolwise(sum), na.rm = TRUE)
total_prcp <- subset(sums, select = c("site", "campaign", "data_source", "prcp"))

myColours_prcp <- c("grey80","#000000","#1B9E77", "#386CB0", "#FF7F00", "#F42E3C", "#7570B3")
mystrip <- strip.custom(bg ="white")
library(lattice)
library(latticeExtra)

setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
png(filename = "Total_prcp_w_MSWEP.png", width = 12 * 300, height = 7 * 300, res = 300)#, type = "windows")
#trellis.par.set(my.settings) 
mystrip <- strip.custom(bg ="white")
b1 <- barchart(total_prcp$prcp~total_prcp$site|total_prcp$campaign, group = total_prcp$data_source,
               col=myColours_prcp,
               superpose.polygon=list(col= myColours_prcp),
               ylab = "Total precipitaion (mm)", #ylim = c(0, 150),
               #strip.left = strip.custom(style=1, horizontal = F),
               auto.key = list(column = 3, space = "top"), 
               par.strip.text=list(cex=0.8), scales =list(cex = 0.8, rot = c(40,0), alternating = 2))
#print(useOuterStrips(b1, strip = mystrip, strip.left = mystrip)) #useOuterStrips ignores specified strip parameters... 
plot(b1, strip = mystrip)
dev.off() 
trellis.par.set(original.settings)

##statistical analysis 
total_prcp_obs <- subset(total_prcp, data_source %in% "OBS")
total_prcp_models <- subset(total_prcp, data_source != "OBS") #incl. MSWEP
total_prcp_wide <- merge(total_prcp_obs, total_prcp_models, by = c("site", "campaign"), suffixes = c(".obs", ".mod", all = T))

stats <- modStats(total_prcp_wide, mod = "prcp.mod", obs = "prcp.obs", type = c("data_source.mod","site", "campaign"))

stats_name <- "stats_total_prcp_OBS"
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
assign(stats_name,stats)

#stats comparing models to MSWEP 
total_prcp_MSWEP <- subset(total_prcp, data_source %in% "MSWEP")
total_prcp_models <- subset(total_prcp, data_source != "MSWEP") #incl. obs 
total_prcp_wide2 <- merge(total_prcp_MSWEP, total_prcp_models, by = c("site", "campaign"), suffixes = c(".obs", ".mod", all = T))

stats <- modStats(total_prcp_wide2, mod = "prcp.mod", obs = "prcp.obs", type = c("data_source.mod","site", "campaign"))


stats_name <- "stats_total_prcp_MSWEP"
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Stats/met_analysis")
write.csv(stats, file = paste0(stats_name, ".csv"), row.names =F)
assign(stats_name,stats)

#worth plotting the google plots now - coastal effects seem present! 
stats <- merge(stats, site_info_prcp, by = "site")
stat_list_2 <- c("MB", "NMB")

setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
mystrip <- strip.custom(bg ="white")
strip = function(...) strip.default(...)
strip.left = strip.custom(style=1, horizontal = F)
for (m in 1:length(stat_list_2)) {
  a1 <- GoogleMapsPlot(stats, latitude = "site_lat", longitude = "site_lon", pollutant = stat_list_2[m],
                       maptype = "roadmap", col = "jet", cex = 1, main = paste("Total precipitation", "-", stat_list_2[m]),
                       key.footer = stat_list_2[m], xlab = "lon", ylab = "lat", type = c( "campaign", "data_source.mod"))
  png(filename = paste("Total_precipitation", stat_list_2[m],"map_OBS.png", sep = '_'), width = 8 * 300, height = 8 * 300, res = 300)
  print(useOuterStrips(a1$plot,strip = mystrip, strip.left = mystrip))
  dev.off()
}

