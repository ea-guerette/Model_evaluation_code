#Prepare a model Rdata file 
#should avoid mistakes as to which version of the model output I am using in my various analysis files 
Sys.setenv(TZ = "UTC")
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"

#load in model data - This bit has to be right!!  
load(paste0(dir_mod,"/ANSTO_model_output_final.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_final.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_final.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_final_W_10m_temp_timeshifted.RData"))
#load(paste0(dir_mod,"/OEH_model_output_final.RData"))
#load(paste0(dir_mod,"/OEH_model_output_newMET_revised_10m_temp_timeshift.RData")) #temporary - no AQ in this (3 Apr 2019) - still no AQ (29 May 2019)
load(paste0(dir_mod,"/OEH_model_output_revised_final.RData")) #has new CTM output 
load(paste0(dir_mod, "/YZ_final.RData"))

#combine models 
models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model, yz_mod)

#cut them to length 
mumba_mod <- subset(models, campaign %in% "MUMBA")
mumba_mod <- subset(mumba_mod, date >= "2012-12-31 14:00 UTC" & date <= "2013-02-15 13:00 UTC")
sps1_mod <- subset(models, campaign %in% "SPS1")
sps1_mod <- subset(sps1_mod, date >= "2011-02-06 14:00 UTC" & date <= "2011-03-06 13:00 UTC")
sps2_mod <- subset(models, campaign %in% "SPS2")
sps2_mod <- subset(sps2_mod, date >= "2012-04-15 14:00 UTC" & date <= "2012-05-13 13:00 UTC")
models <-rbind.data.frame(mumba_mod, sps1_mod,sps2_mod) 


setwd(dir_mod)
save(models, file = "models.RData")
#modified new CTM output to rename PM25 to PM2.5 (1 Aug 2019)
#added new CTM output from OEH (5 June 2019)
#changed assumed 1:24 to assumed 0:23 for OEH CCAM output
#reverted back to 10m temp for CCAM runs (29 May 2019)
#added W to C-CTM (Apr 19 2019)
#testing W calculation for O-CTM + C-CTM (Apr 18 2019)
#this version (Apr 03 2019) has new output from OEH (met only)
#this version (Dec 10 2018) should have right q, right label and newMET for both CSIRO and OEH (with time offset) + Bellambi fix
#this version (Nov 21 2018) has wrong q for most, wrong label for NIT, etc. 
