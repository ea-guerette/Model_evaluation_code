#PM2.5 analysis 

#Set directories 
dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
dir_figures <- "C:/Users/eag873/Documents/CAUL/Model intercomparison/paper drafts/AQ/Figures"

#load in OEH observations  
load(paste0(dir_obs,"OEH_obs.RData")) 

#load in prepared model data
load(paste0(dir_mod,"/models.RData"))
#load site lat lon
load(paste0(dir_mod,"/site_info.RData"))

aq_models <- subset(models, data_source != "W-A11")

#assign variables #should I recycle OBS or use something else? 
OBS <- oeh_obs
site_list_pm <- c("Chullora","Earlwood", "Liverpool", "Richmond", "Wollongong")
site_list_pm_SPS2 <- c("Chullora","Earlwood", "Wollongong") #Liverpool only has a few days of data, Richmond has a gap

#separate SPS2, select sites 
SPS2_OBS <- subset(OBS, campaign %in% "SPS2")
SPS2_OBS <- subset(SPS2_OBS, site %in% site_list_pm_SPS2)

PM_OBS <- subset(OBS, campaign != "SPS2")
PM_OBS <- subset(OBS, site %in% site_list_pm)

PM_OBS <- rbind(PM_OBS, SPS2_OBS )

species_list_pm <-  c("PM2.5") #what else?
param_list_pm <- c("date", "site", "campaign", "data_source", species_list_pm)  #complete list of things to keep from model runs 

model_pm <- aq_models[,param_list_pm] #I think this var name is free 
#need to select sites here too
model_pm_SPS2 <- subset(model_pm, campaign %in% "SPS2" )
model_pm_SPS2 <- subset(model_pm_SPS2, site %in% site_list_pm_SPS2)
model_pm_oth <- subset(model_pm, campaign != "SPS2" )
model_pm_oth <- subset(model_pm_oth, site %in% site_list_pm)
model_pm <- rbind(model_pm_oth, model_pm_SPS2)

pm <- merge(PM_OBS, model_pm, by = c("date", "site", "campaign"), suffixes = c(".obs", ".mod"), all = TRUE)
#pm is already taken... OK, change pm to pmc in PM25_components 


PM_OBS$data_source <- "OBS"
pm_ln <- rbind.fill(PM_OBS, model_pm)


#make daily averages 
#one _per_site the other one _per_domain (for plots)

pm_daily <- data.frame(timeAverage(pm, avg.time = "1 day", type = c("site", "data_source", "campaign"))) #for stats
daily_pm <- data.frame(timeAverage(pm, avg.time = "1 day", type = c("data_source", "campaign")))

pm_ln_daily <- data.frame(timeAverage(pm_ln, avg.time = "1 day", type = c("site", "data_source", "campaign")))
daily_pm_ln <- data.frame(timeAverage(pm_ln, avg.time = "1 day", type = c("data_source", "campaign"))) #for overall daily timeseries

#make hourly panel, daily panel 
#calc stats
#same code as for met parameters (Khalia's stuff)
#for plotting

source(paste0(dir_code,"/lattice_plot_settings_aq.R"))
#source(paste0(dir_code, "/mod_TaylorDiagram.R"))
source(paste0(dir_code, "/GoogleMaps_support.R"))
myKey_aq <- list(column = 4, space = "top", text = list(c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), lines = list(lty =mylineTypes, col = myColours_aq))
resolution = 600

#for stats 

source(paste0(dir_code,"/makeStats_functions_MSE.R"))


species = "PM2.5"
i=1
y.lab1 <- "PM2.5"
y.lab3 <- "Mean bias"
fig_name <- "PM25_panel"
#add_line <- c(5,5) #not sure if 2 is OK for NOx
#add_line <- c(0.15,0.15, 0.15, 0.15) #not sure if 15% NMB is reasonable for NOx, NO2 - looks awful 

setwd(dir_figures)

a <- timeVariation(pm_ln, pollutant = species[i], group = "data_source", type = "campaign")
temp_hour <- a$data$hour

t1 <-  xyplot(Mean ~hour|campaign, data = temp_hour, groups = ordered(temp_hour$variable, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")),
              ylab =y.lab1[i], type = "l", col = myColours_aq, par.settings = my.settings,
              auto.key = list(column = 4, space = "top", points = F, lines = T), 
              scales = list(x = list(alternating = 1)), layout = c(3,1)
              , aspect = 1
)
#taylor diagram 

t2 <- TaylorDiagram(pm, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours_aq, key = F,
                    annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)

###binned quantiles 

species_col <- match(paste0(species[i], ".obs"), names(pm))
bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 

x_test <- quantile(pm[,species_col], probs = bins, na.rm = T)
unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
x_test <- x_test[unik] ## the values 
bin_labels <- bin_labels[unik]

pm$bin <- cut(pm[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
stats_bin <- modStats(pm, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))

#plot:  

t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
             xlab = "" ,#species_names[1], 
             type = "l", 
             ylab = y.lab3[i],
             auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
             par.settings = my.settings,
             scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),# y = list(limits = c(-0.6,0.6))),
             layout = c(3,1),
             aspect = 1,
             panel =function(...){  
              panel.xyplot(...);
             # panel.abline(h = c(0,add_line[i],-(add_line[i])), col = c("black","grey60", "grey60"), lty = c(2,3,3))
               panel.abline(h = 0, col = "black", lty = 2)
             }
)

print(t3, auto.key = list(column = 4, space = "bottom", lines = T, points = F))



png(filename = paste0(fig_name[i], ".png"), width = 7 * resolution, height = 9 * resolution, res = resolution)

print(t1, position = c(0,2/3-1/24.5,1,1), more = TRUE)
print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE)
print(t3, position = c(0,0,1,1/3+1/65))

dev.off() 



#make daily panel

species_col1 <- match(species, names(daily_pm_ln))

for (i in 1:length(species)) {
  
  t1 <- xyplot(daily_pm_ln[, species_col1[i]] ~date|campaign, groups = ordered(daily_pm_ln$data_source, levels = c("C-CTM", "O-CTM", "W-NC1", "W-NC2", "W-UM1", "W-UM2", "OBS")), data = daily_pm_ln, 
               scales = list(x = list(relation = "free")), par.settings = my.settings, type = "l", key = myKey_aq, ylab = y.lab1[i], layout =c(3,1), aspect =1, between = list(x = 1) )
  t2 <- TaylorDiagram(daily_pm, obs = paste0(species[i], ".obs"),mod = paste0(species[i], ".mod"), group = "data_source", type = "campaign", col = myColours_aq, key = F,
                      annotate = "", rms.col = "gray60", normalise = T, layout = c(3,1), cex = 1.6)
  
  
  ###binned quantiles 
  
  species_col <- match(paste0(species[i], ".obs"), names(pm))
  bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100
  bin_labels <- c("Q1", "Q5", "Q10","Q10-Q25", "Q25-Q50", "Q50-Q75", "Q75-Q90", "Q90", "Q95", "Q99") 
  
  x_test <- quantile(daily_pm[,species_col], probs = bins, na.rm = T)
  unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
  x_test <- x_test[unik] ## the values 
  bin_labels <- bin_labels[unik]
  
  daily_pm$bin <- cut(daily_pm[,species_col], breaks = x_test, include.lowest = T)#, labels = bin_labels)
  stats_bin <- modStats(daily_pm, obs = paste0(species[i], ".obs"), mod = paste0(species[i], ".mod"), type = c("data_source", "campaign" ,"bin"))
  
  #plot:  
  
  t3 <- xyplot(MB ~ bin|campaign, data = stats_bin, groups = data_source, 
               xlab = "" ,#species_names[1], 
               type = "l", 
               ylab = y.lab3[i],
               auto.key = F, #list(column = 4, space = "bottom", lines = T, points = F), 
               #key = myKey,
               par.settings = my.settings,
               scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6, labels = bin_labels)),
               layout = c(3,1),
               aspect = 1,
               panel =function(...){  
                 panel.xyplot(...);
                 panel.abline(h = 0, col = "black", lty = 2)
                 #panel.abline(h = c(0,add_line[i],-(add_line[i])), col = c("black","grey60", "grey60"), lty = c(2,3,3))
               })
  
  
  png(filename = paste0(fig_name[i], "_daily.png"),  width = 7 * resolution, height = 9 * resolution, res = resolution)
  
  print(t1, position = c(0,2/3,1,1), more = TRUE) #c(0,2/3-1/24.5,1,1)
  print(t2$plot, trellis.par.set(my.settings), position = c(0,1/3,1,2/3), more = TRUE) #c(0,1/3,1,2/3)
  print(t3, position = c(0,0,1,1/3+1/65)) # c(0,0,1,1/3+1/65)
  
  
  dev.off() 
} 

#qq plot? 
pm_ln$data_type <- "OBS"
ids <- which(pm_ln$data_source != "OBS") #there was a typo here ids <- ids <- which()
pm_ln$data_type[ids] <- "MODEL"
pm_ln$data_type <- ordered(pm_ln$data_type, levels = c("OBS", "MODEL"))

model_list <- levels(as.factor(pm$data_source))
#rearrange data so that each model has a set of obs
Data <- pm_ln
Data2 <- NULL
for(m in 1:length(model_list)){
  Data2 <- rbind(Data2,
                 cbind(rbind(Data[Data$data_source == model_list[m],],Data[Data$data_source == 'OBS',]),
                       model_list[m]))
  
}
names(Data2)[21] <- "model"
#cols <- match(species_list_2, names(Data2))
#Data3 <- Data2[,c(1,2,12,13,14,17,18)]

#make the plots

species_col <- match("PM2.5", names(Data2))
d <- qq(data_type~Data2[,species_col]|model*campaign, data=Data2,
        as.table = T, col = rep(myColours_2_aq,3),
        aspect = 1, main = "", scales = list(alternating =1), par.settings = my.settings,# between = list(y =0.25, x = 0.25),
        panel=function(x, col=col,...){
          panel.qq(x,col=col[packet.number()],...) #gets color for each panel
        }
)
setwd(dir_figures)
png(filename = paste0(species[1], "_quantile_plot.png"), width = 10 *resolution, height = 8*resolution, res = resolution)
useOuterStrips(d)
dev.off()
