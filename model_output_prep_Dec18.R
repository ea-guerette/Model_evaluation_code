#prepare file for Kai/Yang 

#dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
#load(paste0(dir_mod,"/models.RData")) # this was wrong - cut to length 

dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"

#load in model data - This bit has to be right!!  
load(paste0(dir_mod,"/ANSTO_model_output_final.RData"))
load(paste0(dir_mod,"/CMAQ_model_output_final.RData"))
load(paste0(dir_mod,"/WRFCHEM_model_output_final.RData"))
load(paste0(dir_mod,"/CSIRO_model_output_final.RData"))
load(paste0(dir_mod,"/OEH_model_output_final.RData"))
load(paste0(dir_mod, "/YZ_final.RData"))

#combine models 
models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model, yz_mod)


#load(paste0(dir_mod,"/ANSTO_model_output_new.RData"))
#load(paste0(dir_mod,"/CMAQ_model_output_new.RData"))
#load(paste0(dir_mod,"/WRFCHEM_model_output_new.RData"))
#load(paste0(dir_mod,"/CSIRO_model_output_new_new_new_fixed.RData"))
#load(paste0(dir_mod,"/OEH_model_output2.RData"))

#models <- rbind.fill(wrf, cmaq, wrf_chem, csiro, oeh_model)

#xyplot(O3 ~ date|campaign*data_source, data = models)
#test<- subset(models, data_source %in% "W-UM1")
#levels(as.factor(models$site))


setwd("C:/Users/eag873/owncloud/for Yang Zhang/Model_output/")
write.csv(models, file = "updated_model_output_Jan2019.csv", row.names = F)

levels(as.factor(models$site))
cctm <- subset(models, data_source %in% "C-CTM")
levels(as.factor(cctm$site))
cctm_RR <- subset(cctm, site %in% "Richmond_RAAF")
xyplot(O3 ~date|campaign, data = cctm_RR, scales = list(x = list(relation = "free")), type = "l", col = "red")
