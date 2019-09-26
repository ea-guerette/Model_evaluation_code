library(lattice)
library(openair)
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"

load(paste0(dir_mod,"/OEH_model_output_newMET_revised_10m_temp_timeshift.RData")) #temporary - no AQ in this (3 Apr 2019) - still no AQ (29 May 2019)
timeshifted <- oeh_model_final_met
timeshifted$data_source <- "OEH-timeshifted"  

load(paste0(dir_mod,"/OEH_model_output_newMET_revised_10m_temp.RData")) #temporary - no AQ in this (3 Apr 2019) - still no AQ (29 May 2019)

comp <- rbind(timeshifted, oeh_model_final_met)

xyplot(temp ~ date|campaign, data = comp, groups = data_source,
       scales = list(x=list(relation = "free")))

timeVariation(comp, pollutant = "temp", group = "data_source", type = "campaign")
timeVariation(comp, pollutant = "ws", group = "data_source", type = "campaign")
timeVariation(comp, pollutant = "W", group = "data_source", type = "campaign")
timeVariation(comp, pollutant = "u10", group = "data_source", type = "campaign")

names(comp)

load(paste0(dir_mod,"/OEH_model_output_newMET_revised.RData")) #temporary - no AQ in this (3 Apr 2019) - still no AQ (29 May 2019)
oeh_model_final_met$data_source <- "OEH-2m_temp"

comp <- rbind.fill(comp, oeh_model_final_met)

names(comp)

o <- timeVariation(comp, pollutant = "temp", group = "data_source", type = "campaign")
print(o, subset = "hour")
timeVariation(comp, pollutant = "ws", group = "data_source", type = "campaign")
timeVariation(comp, pollutant = "W", group = "data_source", type = "campaign")
timeVariation(comp, pollutant = "u10", group = "data_source", type = "campaign")

load(paste0(dir_mod,"/OEH_model_output_newMET_revised.RData")) #temporary - no AQ in this (3 Apr 2019) - still no AQ (29 May 2019)


#try with csiro model 
load(paste0(dir_mod,"/CSIRO_model_output_final_W_10m_temp.RData"))
csiro_10m <- csiro
csiro_10m$data_source <- "C-CTM_10m"

load(paste0(dir_mod,"/CSIRO_model_output_final_W.RData"))

comp_csiro <- rbind.fill(csiro, csiro_10m)

c <- timeVariation(comp_csiro, pollutant = "temp", group = "data_source", type = "campaign")
print(c, subset = "hour")

