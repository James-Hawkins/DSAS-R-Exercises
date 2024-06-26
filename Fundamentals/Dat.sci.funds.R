#' ------------------------------------------------------------------------
#' Title             : R exercise for fundamentals of data science
#' Module            : Data science for agricultural systems
#' Author            : James Hawkins
#' Date              : 19/06/2024
#' Last Updated      : 19/06/2024
#' R Version used    : 4.2.2
#'
#' Description       : 
#'
#' Dependencies      : readxl
#'
#' Notes             : if the required packages are not installed, run 
#'                    'install.packages('package') to install it
# ------------------------------------------------------------------------


# Dependencies 
library(readxl)


# Automatically define working directory based on where the file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Save .RData file so we can load our data in future sessions
todays.date <- Sys.Date()
save.data.file.name <- paste( 'data.science.fundamentals.data.', todays.date, '.RData' , sep ='')
save.image(save.data.file.name )


# Load data frame
dat <- as.data.frame(read_excel("dat_sci_funds.xlsx" , sheet = 'All_data'))

View(dat) # See contents of data

colnames(dat) # See column names of dataframe

# -- EXERCISE BEGINS HERE -- #

