# ------------------------------------------------------------------------
# Title             : R exercises for OLS regression, building & evaluating regressions
# Module            : Data science for agricultural systems
# Author            : James Hawkins
# Date              : 19/06/2024
# Last Updated      : 19/06/2024
# R Version used    : 4.2.2
#
# Description       : 
#
# Dependencies      : readxl, ggplot
#
# Notes             : 
# ------------------------------------------------------------------------


# Automatically define working directory based on where the file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Save .RData file so we can load our data in future sessions
todays.date <- Sys.Date()
save.data.file.name <- paste( 'OLS.regression.data.', todays.date, '.RData' , sep ='')
save.image(save.data.file.name )


# Dependencies # of not already installed, must install
library(readxl)
library(ggplot2)


# LOAD DATA
  reg.dat <- read_excel("OLS_reg_1_data.xlsx" , sheet = 'Data_set_1')
  reg.dat <- reg.dat[-nrow(reg.dat)  , ]
  
  View(reg.dat) # View contents of the dataframe
  
  colnames(reg.dat) # Display column names of data frame
  
  # -- EXERCISE BEGINS HERE -- #
  

  
  

