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

species <- "NOx"  #make this a list ? O3, PM2.5, ws 


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
names_res <- paste0("decomp_mod")
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
names_data <- paste0("decomp_obs")
assign(names_data, obs_results)
rm(obs_results)

dir_obs <- "C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data/"
#for ws, temp?, W? - load BOM observations  
load(paste0(dir_obs,"/BOM_data_final.RData")) 

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
names_data <- paste0("decomp_obs")
assign(names_data, met_results)



###########################
#plot BL, etc. by site 


xyplot(BL ~ date|campaign, data = decomp_obs, groups = site, scales = list(x = list(relation = "free")))
xyplot(SY ~ date|campaign, data = decomp_obs, groups = site, scales = list(x = list(relation = "free")))
xyplot(DU ~ date|campaign, data = decomp_obs, groups = site, scales = list(x = list(relation = "free")))
xyplot(ID ~ date|campaign, data = decomp_obs, groups = site, scales = list(x = list(relation = "free")))
#for analysis 
#decomp_O3 <- merge(decomp_mod_O3, decomp_obs_O3, suffixes = c(".mod", ".obs"), by = c("date", "site", "campaign"))

#library(openair)
#dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
#source(paste0(dir_code,"/makeStats_functions_MSE.R"))

#stats <- makeStats2(decomp_O3, "ID")
#barchart( bias + var +mMSE ~data_source |campaign, data= na.omit(stats), auto.key = TRUE, stack = T, horizontal = F)
#OK this works BUT, need to make stats for all 4 components, combine them, and plot BL, SY, DU, ID side-by-side, one row per campaign
#can you figure out colours related to R2 like in Solazzo et al 2016? 
#our synoptic error is much greater than AQMEII's, also ID!! 

#so interesting - boundary conditions are the same for wrf_chem and CMAQ, and NC1 and NC2, right? 
#so this does not explain the bias in BL component 

#look at how variable MSE_sum is - bubble plots - one per component, show only interesting one(s) 
#use makeStats3, site_info, etc. 
#I made a series of similar functions to output stats 
library(openair)
dir_code <- "C:/Users/eag873/Documents/GitHub/Model_evaluation_code/"
dir_figures <- "C:/Users/eag873/ownCloud/Figures_and_stats_met_paper/newMET"


source(paste0(dir_code,"/makeStats_functions_MSE.R"))
source(paste0(dir_code,"/lattice_plot_settings.R"))

decomp <- merge(decomp_mod, decomp_obs, suffixes = c(".mod", ".obs"), by = c("date", "site", "campaign"))


#stats <- makeStats2(decomp_u10, "SY")
#barchart( bias + var +mMSE ~data_source |campaign, data= na.omit(stats), auto.key = TRUE, stack = T, horizontal = F)

#need stats for each component, then by component and by campaign (1 row of component per campaign)

component_list <- c("BL", "SY", "DU", "ID")
stat_res_list <- list()
for (c in (1: length(component_list))) {
  stats <- makeStats2(decomp,component_list[c])
  stats$comp <- component_list[c]
  stat_res_list[[c]] <- stats
}
stat_results <-  do.call(rbind, stat_res_list)
stat_results$comp <- ordered(stat_results$comp, levels = c("BL", "SY", "DU", "ID"))

library(latticeExtra)
p <- barchart( bias + var +mMSE ~data_source |comp + campaign , data= na.omit(stat_results), auto.key = TRUE, stack = T, horizontal = F, ylab = "MSE = bias + var +mMSE")
useOuterStrips(p,strip = mystrip, strip.left = mystrip)
#works! Can I get the colour for mMSE from r? 
#trellis.par.get()
#default cols are  "#CCFFFF" "#FFCCFF" "#CCFFCC" "#FFE5CC" "#CCE6FF" "#FFFFCC" "#FFCCCC"

q <- quantile(stat_results$r, na.rm = T)
cols_mMSE <- openColours("Greens", 4)

#not pretty, but make 4 columns, one per quantile 

stat_results$mMSE1 <- 0
ids <- which(stat_results$r <= q[2])
stat_results$mMSE1[ids] <- stat_results$mMSE[ids]
stat_results$mMSE2 <- 0
ids <- which(stat_results$r <= q[3] & stat_results$r > q[2])
stat_results$mMSE2[ids] <- stat_results$mMSE[ids]
stat_results$mMSE3 <- 0
ids <- which(stat_results$r <= q[4] & stat_results$r > q[3])
stat_results$mMSE3[ids] <- stat_results$mMSE[ids]
stat_results$mMSE4 <- 0
ids <- which(stat_results$r <= q[5] & stat_results$r > q[4])
stat_results$mMSE4[ids] <- stat_results$mMSE[ids]

setwd(dir_figures)
chartKey <- list(column = 3, space = "top", cex = 0.8, 
                 text = list(c("Bias", "Variance", "mMSE")), rectangles = list(col = c("#CCFFFF", "#FFCCFF", "#CCFFCC")))
resolution = 600

p <- barchart(bias + var + mMSE1+ mMSE2+mMSE3+mMSE4~data_source |comp + campaign , data= na.omit(stat_results), main = species,  scales = list(x = list(rot = 45)),# y = list(relation = "free")), 
         auto.key = F, stack = T, horizontal = F, ylab = "MSE = bias + var +mMSE", col = c("#CCFFFF", "#FFCCFF", cols_mMSE), 
         key = chartKey)


png(filename = paste0(species, "_decomp_barchart_newMET.png"), width = 10 *resolution, height = 8*resolution, res = resolution)
useOuterStrips(p, strip = mystrip, strip.left = mystrip)
dev.off()

#, col = col_vector,#[1:12],
#         scales = list(x = list(rot = 45)), 


#is it worth doing the same, for each site individually? 
#Not that hard, use makeStats3 instead of 2 and loop through sites to make barcharts 




#create colours for mMSE based on associated r value 
#try quantiles?
q <- quantile(stat_results$r)
cols_mMSE <- openColours("Greens", 4)
stat_results$col_mMSE <- cols_mMSE[1]
ids <- which(stat_results$r < q[5])
stat_results$col_mMSE[ids] <- cols_mMSE[4]
ids <- which(stat_results$r < q[4])
stat_results$col_mMSE[ids] <- cols_mMSE[3]
ids <- which(stat_results$r < q[3])
stat_results$col_mMSE[ids] <- cols_mMSE[2]

col_df <-  data.frame(bias_col = as.character(rep("#CCFFFF", length(stat_results$col_mMSE))), var_col = as.character(rep("#FFCCFF", length(stat_results$col_mMSE))), mMSE_cols = as.character(stat_results$col_mMSE ))



col_list <- list() 
for(l in 1:length(stat_results$col_mMSE)) {
  col_list[[l]] <- c("#CCFFFF", "#FFCCFF", stat_results$col_mMSE[l] )
}

col_vector <- do.call(cbind, col_list)

barchart(bias + var +mMSE ~data_source |comp + campaign , data= na.omit(stat_results), 
          auto.key = TRUE, stack = T, horizontal = F, ylab = "MSE = bias + var +mMSE", col = col_vector,#[1:12],
         scales = list(x = list(rot = 45)), 
         panel=function(x,y,  col=col,...){
           panel.barchart(y,x,col=col[[subscripts()]],...) #need a colour for each bit of each bar
         }
         )

#check panel.superpose, panel.groups 
     
 #col = c("#CCFFFF", "#FFCCFF","#CCFFCC", "blue", "red", "green" )) #c("#CCFFFF", "#FFCCFF", stat_results$col_mMSE) )
#does not work - all same colour - only accepts three colours - need panel function as in the qq plots


#what if I melt it? does not work, same as y1+y2+y3... 
library(reshape2)
sub_stat_results <- stat_results[,c(1,2,10, 19,20,21,23)]
melted_stat_results <- melt(sub_stat_results, id = c("data_source", "r","comp", "campaign"), value.name = "stat")

barchart(stat ~data_source |comp + campaign , data= na.omit(melted_stat_results), groups = variable, key = F,
         auto.key = TRUE, stack = T, horizontal = F, ylab = "MSE = bias + var +mMSE", col = col_list, #[1:12],
         panel=function(x,y,  col=col,...){
           panel.barchart(x,y, col=col[[subscripts()]],...) #need a colour for each bit of each bar
         }
)




#interaction? 
#not want I want 

myData <- data.frame(score = c(0,0,1,0,0,3,0,0,1,2,3), percent = c(100,80,20,100,100,100,100,36,9,18,36), marker = c("ER", "PAX", "PAX", "ER", "PAX", "ER", "PAX", "ER", "ER","ER", "ER"), cellType = c("B", "B", "B", "Br","Br","Bre", "Bre", "C", "C", "C", "C"), Malignat = c(T,T,T,F,F,T,T,T,T,T,T))
palette <- palette(gray(seq(0, 1,len=8)))
trellis.par.set(list(par.xlab.text=list(cex=0.85)
                                         , superpose.polygon=list(col=palette())
                                          , axis.text=list(cex=0.8)))


 barchart(percent~cellType|marker
                    , groups=score
                    , data=myData
                    , stack=TRUE
                    , xlab='N=Normal/Benign, M=Malignant'
                    , ylab='Percentage of Cores Staining'
                    , color=palette()
                    , auto.key = list(points = FALSE, rectangles = TRUE, space = "top")
                    , scales=list(x=list(rot=70))
                   , layout=c(1,2)) 
 
 barchart(percent~cellType|marker
          , groups= interaction(score, Malignat)
          , data=myData
          , stack=TRUE
          , xlab='N=Normal/Benign, M=Malignant'
          , ylab='Percentage of Cores Staining'
          # , color=palette() # not doing anything
          , auto.key = list(points = FALSE, rectangles = TRUE, space =
                              "top", columns = 2)
          , scales=list(x=list(rot=70))
          , layout=c(1,2)) 
 