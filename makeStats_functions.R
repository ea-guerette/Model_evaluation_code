#These functions create stats in the format we need for the paper 
#these would ideally be one function, but I could not figure out how to specify how to split the data easily 
#makeStats1 : overall stats per model (for paper)
#makeStats2 : stats per model per campaign
#makeStats3 : stats per model per campaign per site 

#makeStats1
#function to calculate stats by data_source only 
makeStats1 <- function(df, species) {
  means <- ddply(df, .(data_source), numcolwise(mean), na.rm = TRUE)
  sds <- ddply(df, .(data_source), numcolwise(sd), na.rm = TRUE)
  #run basic stats 
  stats <- modStats(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), type = "data_source")
  #add mean obs and modelled values, and rename columns 
  stats <- cbind(stats, means[,grep(pattern = paste0("^",species), names(means))])
  names(stats)[grep(pattern = ".obs", names(stats))] <- "mean.obs"
  names(stats)[grep(pattern = ".mod", names(stats))] <- "mean.mod"
  #add sd of obs and modelled values and rename columns 
  stats <- cbind(stats, sds[,grep(pattern = paste0("^",species), names(sds))])
  names(stats) <- gsub(paste0(species,".obs"), "sd.obs", names(stats))
  names(stats) <- gsub(paste0(species,".mod"), "sd.mod", names(stats))
  #calc cRMS
  stats <-within(stats, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*r))
  
  #remove missing values 
  #stats <- na.omit(stats)
  
  

  #add sd and cRMS - use TaylorDiagram output
#  t <- TaylorDiagram(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), type = "data_source")
  
#  t_data <- within(t$data, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*R))
#  stats <- cbind(stats, t_data[4:6])
  
  return(stats)
}

#function to calculate stats by data_source by campaign 
makeStats2 <- function(df, species) {
  means <- ddply(df, .(data_source, campaign ), numcolwise(mean), na.rm = TRUE)
  sds <- ddply(df, .(data_source, campaign ), numcolwise(sd), na.rm = TRUE)
  #run basic stats 
  stats <- modStats(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), type = c("data_source", "campaign"))
  #add mean obs and modelled values, and rename columns 
  stats <- cbind(stats, means[,grep(pattern = paste0("^",species), names(means))])
  names(stats)[grep(pattern = ".obs", names(stats))] <- "mean.obs"
  names(stats)[grep(pattern = ".mod", names(stats))] <- "mean.mod"
  #add sd of obs and modelled values and rename columns 
  stats <- cbind(stats, sds[,grep(pattern = paste0("^",species), names(sds))])
  names(stats) <- gsub(paste0(species,".obs"), "sd.obs", names(stats))
  names(stats) <- gsub(paste0(species,".mod"), "sd.mod", names(stats))
  #calc cRMS
  stats <-within(stats, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*r))
  
  #remove missing values 
  #stats <- na.omit(stats)
  
  #add sd and cRMS - use TaylorDiagram output
  #t <- TaylorDiagram(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), group = c("data_source"), type = "campaign")
  
  #t_data <- within(t$data, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*R))
  #stats <- cbind(stats, t_data[4:6])
  
  #return a file named after the variable 
  
 # return(write.csv(stats, file = paste0(species_list_2[k], "_dom_avg_stats_per_campaign.csv"), row.names = F ))
  return(stats)
}

#function to calculate stats by data_source by campaign by site  
makeStats3 <- function(df, species) {
  means <- ddply(df, .(data_source, campaign, site), numcolwise(mean), na.rm = TRUE)
  sds <- ddply(df, .(data_source, campaign, site), numcolwise(sd), na.rm = TRUE)
  
  #run basic stats 
  stats <- modStats(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), type = c("data_source", "campaign", "site"))
  #add mean obs and modelled values, and rename columns 
  stats <- cbind(stats, means[,grep(pattern = paste0("^",species), names(means))])
  names(stats)[grep(pattern = ".obs", names(stats))] <- "mean.obs"
  names(stats)[grep(pattern = ".mod", names(stats))] <- "mean.mod"
  #add sd of obs and modelled values and rename columns 
  stats <- cbind(stats, sds[,grep(pattern = paste0("^",species), names(sds))])
  names(stats) <- gsub(paste0(species,".obs"), "sd.obs", names(stats))
  names(stats) <- gsub(paste0(species,".mod"), "sd.mod", names(stats))
  #calc cRMS
  stats <-within(stats, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*r))#remove missing values 
  
  #stats <- na.omit(stats)
  
  #add sd and cRMS - use TaylorDiagram output
  #t <- TaylorDiagram(df, obs = paste0(species,".obs"), mod = paste0(species,".mod"), group = c("data_source", "site"), type = "campaign")
  
  #t_data <- within(t$data, cRMS <- sqrt(sd.obs^2 + sd.mod^2 - 2*sd.obs*sd.mod*R))
  #stats <- cbind(stats, t_data[4:6])
  
  #return a file named after the variable 
  
  # return(write.csv(stats, file = paste0(species_list_2[k], "_dom_avg_stats_per_campaign.csv"), row.names = F ))
  return(stats)
}

