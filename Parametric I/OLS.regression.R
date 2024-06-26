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
main.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
getwd()

# Save .RData file so we can load our data in future sessions
todays.date <- Sys.Date()
save.data.file.name <- paste( main.dir , '/', 'OLS.regression.data.', todays.date, '.RData' , sep ='')
save.image(save.data.file.name )


# Dependencies # of not already installed, must install
library(readxl)
library(ggplot2)



# commands for lm
gmodel=lm(y~x) #fit a regression model
summary(regmodel) #get results from fitting the regression model
anova(regmodel) #get the ANOVA table fro the regression fit
plot(regmodel) #get four plots, including normal probability plot, of residuals
'''
confint(regmodel) #CIs for all parameters
predict.lm(regmodel, interval="confidence") #make prediction and give confidence interval for the mean response
predict.lm(regmodel, interval="prediction") #make prediction and give prediction interval for the mean response
newx=data.frame(X=4) #create a new data frame with one new x* value of 4
predict.lm(regmodel, newx, interval="confidence") #get a CI for the mean at the value x*
Tests for homogeneity of variance
bptest(regmodel) #get the Breusch-Pagan test (lmtest package must be installed)
levene.test(Y, groupvariable) #get the Levene test (lawstat package must be installed)
Tests for normalit
'''

# LOAD DATA
  reg.dat <- read_excel("OLS_reg_1_data.xlsx" , sheet = 'Data_set_1')
  reg.dat <- reg.dat[-nrow(reg.dat)  , ]
  
  View(reg.dat) # View contents of the dataframe
  
  colnames(reg.dat) # Display column names of data frame
  
  # -- EXERCISE BEGINS HERE -- #
  
  # PART 1 -- BASIC REGRESSION MODEL
  
  ols.1 <- lm( yield  ~ variety + fertiliser , data = reg.dat )
  
  ols.2 <- lm( yield  ~  variety + fertiliser + precip + temperature ,  data = reg.dat )
  
  ols.3 <- lm( yield ~  variety + fertiliser + precip + temperature + bulkdensity + ph , data = reg.dat )
  

  summary(ols.1)
  summary(ols.2)
  summary(ols.3)
  
  confint(ols.1)
  confint(ols.2)
  confint(ols.3)
  
  
  # fits=regmodel$fitted #store the fitted values in variable named "fits"
  #resids=regmodel$residuals #store the residual values in a varaible named "resids"
  #  beta1hat=regmodel$coeff[2] #assign the slope coefficient to the name "beta1hat"
  
  
  # PART 2 -- PLOTS
  plot.1 <- ggplot( data = reg.dat , aes( y = yield  , x = fertiliser)) +
    geom_point() + 
    geom_smooth(method='lm', se = FALSE ,  formula= y~x) +
    xlab('X variable') +
    ylab('Y variable') +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank() , 
      panel.background = element_rect( fill = 'white') , 
      panel.border = element_rect( color= 'black' , fill = NA , size = 1.2)
    )
  
  plot.2 <- ggplot( data =  reg.dat  , aes( y = yield  , x = fertiliser)) +
    geom_point() + 
    geom_abline(slope = coef(ols.3 )[['fertiliser']], 
                intercept = coef(ols.3 )[['(Intercept)']]) +
    xlab('X variable') +
    ylab('Y variable') +
    coord_cartesian(ylim=c(2000,4000) , xlim = c(0,70)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank() , 
      panel.background = element_rect( fill = 'white') , 
      panel.border = element_rect( color= 'black' , fill = NA , size = 1.2)
    )
  
  plot.1
  plot.2
  
  
  # 3 Variable manipulation
  
  reg.dat$squared.fertiliser <- (reg.dat$fertiliser)^2
  reg.dat$sq.rt.fertiliser <- (reg.dat$fertiliser)^0.5
  reg.dat$temperature.std <- reg.dat$temperature - mean(reg.dat$temperature)
  
  
  ols.4 <- lm( yield  ~ variety + squared.fertiliser + precip + temperature + bulkdensity + ph , data = reg.dat)
  ols.5 <- lm( yield  ~ variety + sq.rt.fertiliser  + precip + temperature.squared + bulkdensity + ph , data = reg.dat)
  ols.6 <- lm( yield  ~ variety + fertiliser + precip + temperature.std  + bulkdensity + ph , data = reg.dat)
  
  summary(ols.1)                
  summary(ols.4)
  summary(ols.5)
  summary(ols.6)      
  
  # 4 VARIABLE INTERACTIONS

  ols.7 <- lm( yield  ~  variety + row.space.cm * sq.rt.fertiliser + precip + temperature + bulkdensity + ph, data = reg.dat )
  
  ols.8 <- lm( yield  ~ variety + row.space.category : sq.rt.fertiliser + precip + temperature + bulkdensity + ph, data = reg.dat )
    
                
  summary(ols.7)
  summary(ols.8)
  
  # 5 MODEL SELECTION
  

  AIC(lm.4)
  
  BIC(lm.4)
  
  anova(lm.2)
  
  plot(lm.2)
  

  
  
  

