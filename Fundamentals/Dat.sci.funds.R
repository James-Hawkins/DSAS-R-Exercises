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

# PART 1 -- CLASSIFICATION

typeof(dat$stock_rate_group)
typeof(dat$stock_rate_value)

unique(dat$stock_rate_group)

ordered.stocking.densities <- c('low' , 'high')
dat$stock_rate_group <- factor(dat$stock_rate_group ,  levels = ordered.stocking.densities)
dat$stock_rate_group

# PART 2 -- DISTRIBUTION TESTS

qqnorm( dat$stock_rate_value, pch = 2)
qqline( dat$stock_rate_value, col = "blue", lwd = 2)

shapiro.test( dat$stock_rate_value)

qqnorm( dat$rainfall , pch = 2)
qqline( dat$rainfall  , col = "blue", lwd = 2)

shapiro.test( dat$rainfall  )

# PART 2 -- VARIABLE STANDARDISATION
dat$stock_rate_value_stand <- (dat$stock_rate_value - mean(dat$stock_rate_value)) / sd(dat$stock_rate_value) 

mean(dat$stock_rate_value_stand)
sd(dat$stock_rate_value_stand)



# PART 4 -- CORRELATION COEFFICIENTS

cor( dat$soil_c , dat$stock_rate_value , method = 'pearson'  )
cor( dat$soil_c , dat$stock_rate_value_stand , method = 'pearson'  )


cor( dat$soil_c , as.numeric(dat$stock_rate_group) , method = 'spearman'  )

# PART 5 -- HYPOTHESIS TESTS


# 5.1
# One sample t-test for population mean
# Is population mean different from stated value?
national.rainfall.mean <- 1350
national.rainfall.sd <- 420

t.test(dat$rainfall, mu = national.rainfall.mean , alternative = 'greater')

mean(dat[dat$site ==  "Montpellier" ,'rainfall'])
mean(dat[dat$site ==  "Orleans" ,'rainfall'])

# Exercise 3.1
# Two sample f-test for population variance
# Are population variances significantly different?
var.test(  rainfall  ~  site, data =dat ,
           alternative = 'greater'
)



# PART 6 -- ANOVA

#' 6.1 Simple anova considering only site (site) factors 
#' A One-way ANOVA
aov.1 <- aov(soil_c ~ site  , data = dat )

summary(aov.1)
print(aov.1)

attributes(aov.1)

aov.1$coefficients
aov.1$residuals
aov.1$effects
aov.1$rank
aov.1$fitted.values
aov.1$assign

#' 6.2 ANOVA consider site + stocking rate only  
#' A Two-way ANOVA
aov.2 <- aov(soil_c ~ site + stock_rate_group, data = dat )

summary(aov.2)
print(aov.2)

aov.2$coefficients
aov.2$residuals
aov.2$effects
aov.2$rank
aov.2$fitted.values
aov.2$assign



#' 6.2 ANOVA with all treatments controlled for
#' A 4-way ANOVA
aov.3 <- aov(soil_c ~ stock_rate_group + grass_species + grazing.system + site , data = dat )

summary(aov.3)
print(aov.3)


#' ANOVA with all factors measured, including sites, experimantal factors, and site specific climate variables
aov.4 <- aov(soil_c ~ site + stock_rate_group + grass_species + grazing.system + rainfall + temperature, data = dat )

summary(aov.4)
print(aov.4)

attributes(aov.4)

residuals(aov.4)

aov.4$coefficients
aov.4$residuals
aov.4$effects
aov.4$rank
aov.4$fitted.values
aov.4$assign


aov.4 <- aov(soil_c ~ site + stock_rate_group , data = dat )

summary(aov.4)
print(aov.4)
