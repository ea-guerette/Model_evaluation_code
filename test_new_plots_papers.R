#play around with melt to make a dataset that allow putting all variables on one panel of the TaylorDiagram


#I think I need to melt BOM first, then melt model_met, then combine them 
library(reshape2)
library(plyr)

melted_BOM <- melt(BOM, id = c("date", "site", "campaign"), value.name = "obs")

melted_model_met <- melt(model_met, id = c("date", "site", "campaign", "data_source"), value.name = "mod")

melted <- merge(melted_BOM, melted_model_met, by = c("date", "site", "campaign", "variable"))
setwd(dir_figures)
png(filename = "Taylor_all.png", width = 8 * 300, height = 12 * 300, res = 300)
TaylorDiagram(subset(melted, variable != "prcp" & variable != "wd" ), obs = "obs", mod = "mod", normalise = T, 
              group = "variable", type = c("campaign", "data_source"), cex = 0.95, 
              annotate = "", rms.col = "grey40")
dev.off()

#this works and looks reasonable - people liked it - but maybe grouping by model per parameter would be more informative  

#where is temp for WRF-11 SPS2? 
TaylorDiagram(subset(melted, data_source %in% "WRF_11" ), obs = "obs", mod = "mod", normalise = T, 
              group = "variable", type = "campaign", cex = 0.95)
#OK, hiding behing W 

png(filename = "Taylor_all_v2.png", width = 8 * 300, height = 12 * 300, res = 300)
TaylorDiagram(subset(melted, variable != "prcp" & variable != "wd" ), obs = "obs", mod = "mod", normalise = T, 
              group = "data_source", type = c("campaign", "variable"), cex = 0.95, 
              annotate = "", rms.col = "grey40", cols = "Dark2")
dev.off()



#now, try quantile to make binned MB plots 
#and fix other uglies 

species <- c("temp.obs", "ws.obs")
species_col <- match(species, names(met))
#species_col <- c(4,7,8)
obs_species <- c("temp.obs", "ws.obs")#, "wd.obs")
mod_species <- c("temp.mod","ws.mod")#,"wd.mod")
x_labels <- c(expression("observed temperature " ( degree*C)), expression("observed wind speed (m/s)")) #, "observed wind direction")
add_line <- c(1,1.5)#,30)
#exclude Bellambi 
met_sub <- met#subset(met, site!="Bellambi")
setwd("C:/Users/eag873/Documents/GitHub/Model_evaluation/Figures/met_analysis")
bins <- c(0,1,5,10,25,50,75,90,95,99,100)/100

for (t in 1:length(species_col)) {
  
  x_test <- quantile(met_sub[,species_col[t]], probs = bins, na.rm = T)
  unik <- !duplicated(x_test, fromLast = T)  ## logical vector of unique values
  x_test <- x_test[unik] ## the values 
  
  met_sub$bin2 <- cut(met_sub[,species_col[t]], breaks = x_test, include.lowest = T)#, labels = names(x_test)[1:length(x_test)-1])  #c(0,1,5,10,25,50,75,90,95,99)/100)
  stats_test2 <- modStats(met_sub, obs = obs_species[t], mod = mod_species[t], type = c("data_source", "campaign" ,"bin2"))
  #stats_test2$bin2 <- as.numeric(sub(pattern = "%",stats_test2$bin2, replacement = ""))/100
  
#  png(filename = paste(names(met_sub[species_col[t]]),"mb_by_quantile_v1.png", sep = "_"), width = 9 * 300, height = 5 * 300, res = 300)
  
#  b <- xyplot(MB ~ bin2|campaign, data = stats_test2, groups = data_source, 
#         xlab = x_labels[t],  
#         auto.key = list(column = 3, space = "top"), 
#         par.settings = my.settings,
#         scales = list(alternating = 1) , # x = list(at = bins[2:(length(bins)-1)], labels = bins[2:(length(bins)-1)], rot = c(40,0), cex = 0.6)),
#         panel =function(...){  
#           panel.xyplot(...);
#           panel.abline(h = c(0,add_line[t],-(add_line[t])), col = c("blue","grey", "grey"), lty = c(2,3,3))
#         })
#  print(b, auto.key = list(column = 3, space = "top"))
#  dev.off() 
  
#  stats_test2$bin2 <- as.numeric(sub(pattern = "%",stats_test2$bin2, replacement = ""))/100
  
  png(filename = paste(names(met_sub[species_col[t]]),"mb_by_quantile_v2.png", sep = "_"), width = 12 * 300, height = 5 * 300, res = 300)
  
  b <- xyplot(MB ~ bin2|campaign, data = stats_test2, groups = data_source, 
              xlab = x_labels[t],  
              auto.key = list(column = 3, space = "top"), 
              par.settings = my.settings,
              scales = list(alternating = 1, x = list(rot = c(40,0), cex = 0.6)), #at = bins[2:(length(bins)-1)], labels = bins[2:(length(bins)-1)]),
              panel =function(...){  
                panel.xyplot(...);
                panel.abline(h = c(0,add_line[t],-(add_line[t])), col = c("blue","grey", "grey"), lty = c(2,3,3))
              })
  print(b, auto.key = list(column = 3, space = "top"))
  dev.off() 
}

#to check bias by time of day - timeVariation on met, save output and run modStats on that 

a <- timeVariation(met, pollutant = "ws.obs", type =  "campaign", group = "data_source")
obsws <- a$data$hour  
b <- timeVariation(met, pollutant = "ws.mod", type =  "campaign", group = "data_source")
modws <- b$data$hour 
#combine
comb <- merge(obsws, modws, by = c("variable", "hour", "campaign"), suffixes = c(".obs", ".mod"))  
names(comb)[2] <- "HOD"  
comb[,2] <- as.factor(comb[,2])
stats <- modStats(comb, obs = "Mean.obs", mod = "Mean.mod", type = c("campaign", "variable", "HOD"))

xyplot(MB ~HOD|campaign, data = stats, groups = variable, type = "l", 
       scales = list(x = list(alternating = 1, at = c(0,3,6,9,12,15,18,21))),
       par.settings = my.settings,
       auto.key = list(column = 1, space = "right", points = T, lines = F),
       panel =function(...){  
         panel.xyplot(...);
         panel.abline(h = 0, col = "blue", lty = 2)
       })
#OK, this kind of works, but linetypes are wrong 




#quantile plots 

#wait up, lattice has a qq funciton 

qq(data_type ~ws|campaign, data = met_ln, groups = data_source, 
   layout = c(3,1), 
   par.settings = my.settings) #I don't think this is working  (or it might be, but it is not legible)


qq(data_source ~temp|campaign, data = met_ln, subset = (data_source == "OBS"| data_source == "W-A11"), 
   layout = c(3,1),scales = list(x = list(alternating = 1)), strip = mystrip)  
qq(data_source ~ws|campaign, data = met_ln, subset = (data_source == "OBS"| data_source == "C-CTM"), 
   layout = c(3,1), scales = list(x = list(alternating = 1)), strip = mystrip)  
#these both work - maybe need to write a loop to make them all 
#it would be nice to combine models onto one page for each parameter - maybe colour them by their assigned colour 
#I find the new colours really ugly 


xyplot(quantile(temp.obs, probs = c(0,1,5,10,25,50,75,90,95,99,100)/100, na.rm = T) ~ 
         quantile(temp.mod, probs = c(0,1,5,10,25,50,75,90,95,99,100)/100, na.rm = T)|data_source, data = met, 
       xlab = "modelled temperature", ylab = "observed temperature", 
       pch = 16,
       scales = list(x = list(alternating = 1)),
       panel=function(...){  
         panel.xyplot(...);
         panel.abline(b=1, a = 0, col = "black")
       })#xlab = "observed", ylab = "modelled")
#this does not quite work - to make it pretty will take time, unless I find a model somewhere 
#abline(0,1,col ="red")

#conditioning works if I don't compute the quantiles in the formula 
xyplot(temp.obs ~ 
         temp.mod|data_source+campaign, data = met, 
       xlab = "modelled temperature", ylab = "observed temperature", 
       pch = 16,
       scales = list(x = list(alternating = 1)),
       strip.left = T,
       panel=function(...){  
         panel.xyplot(...);
         panel.abline(b=1, a = 0, col = "black")
       })

#new plan: 
#hijack conditionalQuantiles from openair - only plot the histograms 









#barchart(MB ~ bin2, data = stats_test2, groups = data_source, 
#             xlab = x_labels[t],  
#             auto.key = list(column = 3, space = "top"), 
#             par.settings = my.settings,
#             scales = list(alternating = 1) , # x = list(at = bins[2:(length(bins)-1)], labels = bins[2:(length(bins)-1)], rot = c(40,0), cex = 0.6)),
#             panel =function(...){  
#             panel.xyplot(...);
#             panel.abline(h = c(0,add_line[t],-(add_line[t])), col = c("blue","grey", "grey"), lty = c(2,3,3))
#               })



##


  x_1 <- floor((max(met_sub[,species_col[t]], na.rm = T) - min(met_sub[,species_col[t]], na.rm = T))/10)
  x_max <- ceiling(max(met_sub[,species_col[t]], na.rm = T)/x_1) * x_1
  x_breaks <- c(0, seq(x_1, x_max, by = x_1))
  # a <- histogram(~ met[,species_col[t]]|campaign, data = met, #endpoints = c(0,ceiling(max(met[,species_col[t]], na.rm = T))),
  #             col = "grey90", xlab = names(met[species_col[t]]),
  #              breaks = x_breaks,
  #             scales = list(x = list(at = x_breaks)))
  c <- densityplot(~ met_sub[,species_col[t]]|campaign, data = met_sub, plot.points=FALSE, col = "grey", from = 0, to = x_max, xlab = x_labels[t])
  
  met_sub$bin <- cut(met_sub[,species_col[t]], breaks = x_breaks, labels = (seq(0, (x_max-x_1), by = x_1)))
  stats_test <- modStats(met_sub, obs = obs_species[t], mod = mod_species[t], type = c("data_source", "campaign", "bin"))
  stats_test$bin <- as.numeric(stats_test$bin)*x_1
  b <- xyplot(MB ~ bin|campaign, data = stats_test, groups = data_source, 
              xlab = x_labels[t],  
              auto.key = list(column = 3, space = "top"), 
              par.settings = my.settings,
              scales = list(alternating = 1),
              panel =function(...){  
                panel.xyplot(...);
                panel.abline(h = c(0,add_line[t],-(add_line[t])), col = c("blue","red", "red"), lty = c(2,3,3))
              })
  print((b, auto.key = list(column = 3, space = "top")))
  #update(b2, strip = mystrip)#, auto.key = list(column = 3, space = "top"))
  #update(b2, strip = mystrip)
  dev.off()
}
