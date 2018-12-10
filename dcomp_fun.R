#script to decompose timeseries into its components using KZ filter
#this is the KZ package
library(kza)

#this is the function - returns 4 columns (BL,SY,DU,ID)
decomp_fun <- function(df, species) {
  BL <- kz(df[[species]], 103,5)
  t1 <- kz(df[[species]], 13,5)
  SY <- t1 - BL
  t2 <- kz(df[[species]], 3,3)
  DU <- t2 - t1 
  ID <- df[[species]] - t2
  
  dat <- as.data.frame(cbind(BL, SY,DU,ID))
  #return(dat)
}


#load models.RData 
dir_mod <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/"
load(paste0(dir_mod,"/models.RData"))
#this is already the right length, but contains all sites, all models, all campaigns


#I think it is better to decompose the timeseries at each site, for each model for each campaign 
campaigns_list <- c("MUMBA", "SPS1", "SPS2")
models_list <- levels(as.factor(models$data_source))
#sites_list <- levels(as.factor(models$site)) #this won't work if not all models have the same sites... 

#select the campaign, the model and the site:
#this will be the df you feed to the function 
#create a couple of list to save output to 
#dat_list <- list() #moved to loop in case not all models have the same number of sites 

species <- "PM2.5"  #make this a list ? O3, PM2.5, ws 


data_list <- list()
res_list <- list()



for (i in 1:length(campaigns_list)){
  campaign_data <- subset(models, campaign %in% campaigns_list[i])
    for (j in 1:length(models_list)) {
    model_data <- subset(campaign_data, data_source %in% models_list[j])
    sites_list <- levels(as.factor(model_data$site))
    dat_list <- list()
    for (k in 1:length(sites_list)) {
      site_data <- subset(model_data, site %in% sites_list[k]) #if a list, could use lapply instead of a loop 
      dat <- decomp_fun(site_data, species) #could replace this with a species list - #might need to make sure that dates are ordered 
      #need to add $site and then save it to a list
      dat$site <- sites_list[k]
      dat$date <- site_data$date
      dat_list[[k]] <- dat
            }
    #take the list and rowbind it, add data_source
    data <- do.call(rbind, dat_list)
    data$data_source <- models_list[j]  
    data_list[[j]] <- data  
    #data is one model, all sites, for one campaign - do I save that as a dataframe or to a list?
      }
  #combine data and add campaign, save it to a dataframe   
    res <-  do.call(rbind, data_list) 
    res$campaign <- campaigns_list[i]
    
    res_list[[i]] <- res 
   # names_res <- paste0("decomp_mod_", species, "_", campaigns_list[i])
   #assign(names_res, res)
}
names_res <- paste0("decomp_mod_", species)
mod_results <- do.call(rbind, res_list)
assign(names_res, mod_results)
rm(mod_results)

#obs at a bit trickier - two sets, and species not in both 

dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"

#for O3, PM2.5 - load OEH observations 

load(paste0(dir_obs,"OEH_obs.RData")) #why not "updated" ? check this 
#calculate each site separately for each campaign 
#obs_list <- list()
cam_res_list <- list()

for (i in 1:length(campaigns_list)){
  campaign_data <- subset(oeh_obs, campaign %in% campaigns_list[i])
  sites_list <- levels(as.factor(campaign_data$site))
  obs_list <- list() #moved here because not all campaigns had the same number of sites 
    for (k in 1:length(sites_list)) {
      site_data <- subset(campaign_data, site %in% sites_list[k]) #if a list, could use lapply instead of a loop 
      dat <- decomp_fun(site_data, species) #could replace this with a species list
      #need to add $site and then save it to a list
      dat$site <- sites_list[k]
      dat$date <- site_data$date
      obs_list[[k]] <- dat
    }
  #take the list and rowbind it, add data_source
  data <- do.call(rbind, obs_list)
  data$campaign <- campaigns_list[i]
  cam_res_list[[i]] <- data
  
}

obs_results <-  do.call(rbind, cam_res_list)
names_data <- paste0("decomp_obs_", species)
assign(names_data, obs_results)
rm(obs_results)


#for ws, temp?, W? - load BOM observations  
load(paste0(dir_obs,"/BOM_data_updated3.RData")) #will probably need to recalc q 

library(plyr)

cam_res_list <- list()

for (i in 1:length(campaigns_list)){
  campaign_data <- subset(bom_data_all_campaigns, campaign %in% campaigns_list[i])
  sites_list <- levels(as.factor(campaign_data$site))
  if (campaigns_list[i] == "SPS1") {
    sites_list <- sites_list[1:7] #because there is no data for Wollongong_Airport during SPS1
  }
  met_list <- list() #moved here because not all campaigns had the same number of sites 
  for (k in 1:length(sites_list)) {
    site_data <- subset(campaign_data, site %in% sites_list[k]) #if a list, could use lapply instead of a loop 
    if(nrow(site_data !=0)) {
    dat <- decomp_fun(site_data, species) #could replace this with a species list
    #need to add $site and then save it to a list
    dat$site <- sites_list[k]
    dat$date <- site_data$date }
    #if (nrow(site_data ==0)) {dat <- c()}
    met_list[[k]] <- dat #need to deal with the case nrow == 0 
  }
  #take the list and rowbind it, add data_source
  data <- do.call(rbind.fill, met_list)
  data$campaign <- campaigns_list[i]
  cam_res_list[[i]] <- data
  
}

met_results <-  do.call(rbind, cam_res_list)
names_data <- paste0("decomp_obs_", species)
assign(names_data, met_results)



###########################


#for analysis 
decomp_O3 <- merge(decomp_mod_O3, decomp_obs_O3, suffixes = c(".mod", ".obs"), by = c("date", "site", "campaign"))

dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
source(paste0(dir_code,"/makeStats_functions_MSE.R"))

stats <- makeStats2(decomp_O3, "ID")
barchart( bias + var +mMSE ~data_source |campaign, data= na.omit(stats), auto.key = TRUE, stack = T, horizontal = F)
#OK this works BUT, need to make stats for all 4 components, combine them, and plot BL, SY, DU, ID side-by-side, one row per campaign
#can you figure out colours related to R2 like in Solazzo et al 2016? 
#our synoptic error is much greater than AQMEII's, also ID!! 

#so interesting - boundary conditions are the same for wrf_chem and CMAQ, and NC1 and NC2, right? 
#so this does not explain the bias in BL component 

#look at how variable MSE_sum is - bubble plots - one per component, show only interesting one(s) 
#use makeStats3, site_info, etc. 

