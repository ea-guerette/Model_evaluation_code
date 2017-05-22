#AQ analysis script 
#copy over the first part of the code from met analysis, but change the species list and species name, etc.  

library(openair)
library(plyr)
library(lattice)
library(latticeExtra)
library(stringi)

#load in data 
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Campaign data")
load("BOM_data_updated.RData")
setwd("C:/Documents and Settings/eag873/My Documents/R_Model_Intercomparison/Model output/")
load("ANSTO_model_output.RData")
load("CMAQ_model_output.RData")
load("WRFCHEM_model_output.RData")