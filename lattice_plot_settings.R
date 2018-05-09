#settings for lattice plotting - model inter-comparison 
require(lattice)
require(RColorBrewer)

#myColours <- c("#1B9E77", "#386CB0", "#000000","#FF7F00", "#F42E3C", "#7570B3") #trying a more vibrant orange (less yellow)
#myColours_2_aq <-  c("#1B9E77", "#386CB0", "#FF7F00", "#F42E3C")#, "#7570B3") #for when there is no obs... 
#myColours_aq <- c("#1B9E77", "#386CB0", "#000000","#FF7F00", "#F42E3C")#, "#7570B3") #exclude WRF colour

myColours2 <- c("#386CB0", "#FF7F00", "#7570B3", "#7FC97F", "#F0027F","#1B9E77","#F42E3C")

myColours <- c("#386CB0", "#FF7F00", "#7570B3", "#7FC97F", "#F0027F","#1B9E77","#F42E3C", "#000000")
  #c("#7FC97F", "#BEAED4", "#000000", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666")
  #c("#1B9E77", "#D95F02", "#000000", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
#myColours <- c("#1B9E77", "#386CB0", "#000000","#FF7F00", "#F42E3C", "#7570B3") 
#myColours_2 <-  c("#1B9E77", "#386CB0", "#FF7F00", "#F42E3C", "#7570B3") #for when there is no obs... 
mylineTypes <- c("dotted","dashed","dotdash","longdash","twodash", "dashed", "solid", "solid")
mylineWidths <- c(2,2,2,2,2,2,2,3)

original.settings <- trellis.par.get()
my.settings <- trellis.par.get()
names(my.settings)
my.settings$superpose.line$col = myColours  
my.settings$superpose.line$lty = mylineTypes
my.settings$superpose.line$lwd = mylineWidths
my.settings$superpose.polygon$col = myColours2
my.settings$superpose.symbol$col = myColours2
my.settings$superpose.symbol$pch = c(16:21)
my.settings$strip.background$col <- "white"
#trellis.par.set(my.settings) #need to include this inside the graphing device if I want white strips and the right colours 
#no good because it changes the default settings - but necessary for timeVariation... 

mystrip <- strip.custom(bg ="white")


