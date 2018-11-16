#Prepare a model Rdata file 
#should avoid mistakes as to which version of the model output I am using in my various analysis files 

dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"

#load in model data - This bit has to be right!!  
load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
#load(paste0(dir_mod,"/CSIRO_model_output_new_new_new_fixed.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_newTemp.RData"))
#load(paste0(dir_mod,"/OEH_model_output2.RData"))
load(paste0(dir_mod,"/OEH_model_output_newMET.RData"))
load(paste0(dir_mod, "/YZ.RData"))

#combine models 
models <- rbind.fill(wrf, cmaq, wrf_chem, csiro_newTemp, oeh_model_new_met, yz_mod)

#cut them to length 
mumba_mod <- subset(models, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
sps1_mod <- subset(models, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(models, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
models <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 



save(models, file = "models.RData")

