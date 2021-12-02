############################################################################
###                         Al.I. Cuza University of Iași                ###
###            Faculty of Economics and Business Administration          ###
###       Department of Accounting, Information Systems and Statistics   ###
############################################################################
###
############################################################################
###             Data Processing/Analysis/Science with R                  ###
############################################################################
###
############################################################################
###                       09a Descriptive Statistics                     ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/09%20Exploratory%20Data%20Analysis/09%20Exploratory%20Data%20Analysis.pptx
############################################################################
## last update: 02.12.2021


############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


############################################################################

# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)


# needed packages
library(tidyverse)
library(tidymodels) # fot the `corrr` package

# install.packages('skimr')
library(skimr) # for summary statistics

# install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
library(scales) # for `scale_x_continuous` ... `pretty_breaks`
#install.packages("car")
library(car) # for correlation plots
#install.packages("GGally")
library(GGally) # for correlation plots
library(corrplot) # # for correlation plots


#######################################################################
###	                              Agenda                           ###	
#######################################################################
###	 I. Basic information about data (observations, variables)     ###     
###	 II. Data locality/spread/shape                                ###
###	 II.1 Data locality/spread/shape for interval/ratio variables  ###
###	 II.2 Data locality/spread/shape for ordinal variables         ###
###	 II.3 Data locality/spread/shape for nominal variables         ###
###	 III. Measure of association between variables                  ###
###	 III.1 Association between interval/ratio variables             ###
###	 III.2 Association between ordinal variables                    ###
###	 III.3 Association between nominal variables                    ###
#######################################################################


#######################################################################
###	 I. Basic information about data (observations, variables)     ###                           ###
#######################################################################

# nrow(), ncol()
# names()
# str()
# head()/tail()
# glimpse()
# sample()

#######################################################################
###                                Sales
load (file = 'sales.RData')

## Task:
## Inspect `invoice_detailed` data frame , extract:
## - the number of observations
## - the number of variables
## - variable names
## - variable types
## - a couple of values (first, last, a sample) from a given variable

## Solutions for getting...

# - the number of observations
nrow(invoice_detailed)

## - the number of variables
ncol(invoice_detailed)

## - variable names
names(invoice_detailed)

## - variable types
str(invoice_detailed)
# ..or
glimpse(invoice_detailed)

## - first six values from each variable
head(invoice_detailed)

## - first values from each variable
glimpse(invoice_detailed)

## - last six values from each variable
tail(invoice_detailed)

## - sample of 10 from `invoiceno` variable

# basic R solution
sample(invoice_detailed$invoiceno, 10)

# dplyr solution
invoice_detailed %>%
     select (invoiceno) %>%
     sample_n(10) %>%
     pull()



#######################################################################
###                      Fuel Economy dataset(s) 
fuel_economy_2018 <- read_tsv("all_alpha_18.txt") %>%
     mutate (cty_l100km = round(235.214583333333 / as.numeric(`City MPG`),2),
          hwy_l100km = round(235.214583333333 / as.numeric(`Hwy MPG`),2),
          combined_l100km = round(235.214583333333 / as.numeric(`Cmb MPG`),2)) %>%
     mutate (manufacturer = word(Model)) %>%
     mutate(manufacturer = case_when(
          manufacturer == 'ACURA' ~ 'HONDA',
          manufacturer == 'ASTON' ~ 'ASTON MARTIN',
          manufacturer == 'ALFA' ~ 'FIAT',
          manufacturer %in% c('BUICK', 'CADILLAC', 'CHEVROLET',
               'GMC') ~ 'GENERAL MOTORS',
          manufacturer %in% c( 'DODGE', 'JEEP', 'RAM') ~ 'CHRYSLER',
          manufacturer == 'GENESIS' ~ 'HYUNDAI',
          manufacturer == 'INFINITI' ~ 'NISSAN',
          manufacturer == 'JAGUAR' |  
               str_detect (manufacturer, '(^LAND|^RANGE)|ROVER') ~ 'TATA MOTORS',
          manufacturer == 'LEXUS' ~ 'TOYOTA',
          manufacturer == 'LINCOLN' ~ 'FORD',
          manufacturer == 'MINI' ~ 'BMW',
          manufacturer == 'SMART' ~ 'MERCEDES-BENZ',
          TRUE ~ manufacturer)
     )


## Task:
## Examine the names and types of variables in `fuel_economy_2018` data set
## When necessary, change the data type for further descriptive statistics
glimpse(fuel_economy_2018)

# Some numeric attributes were imported as characters;
# we convert them:
fuel_economy_2018 <- fuel_economy_2018 %>%
     mutate (displacement  = as.numeric(Displ), 
             n_of_cyl = as.numeric(Cyl),
             air_pollution = as.numeric(`Air Pollution Score`),
             greenhouse = as.numeric(`Greenhouse Gas Score`),
             combined_CO2 = as.numeric(`Comb CO2`)
             ) 



#######################################################################
###	                II. Data locality/spread/shape                 ###
#######################################################################


#######################################################################
###	 II.1 Data locality/spread/shape for interval/ratio variables  ###
#######################################################################

#######################################################################
###   Definition of:                                                ###
###   mean, median, mode, range, variance, standard deviation ...   ###

###
### mean
###
vector.1 <- c(123, 234, 43, 32, 23, 11, 2, 32)

sum(vector.1)
length(vector.1)
sum(vector.1) / length(vector.1)

mean(vector.1)


###
### trimmed mean
###
vector.1 <- c(123, 234, 43, 32, 23, 11, 2, 32)
mean(vector.1)
median(vector.1)
# 20% trimmed mean
mean( x = vector.1, trim = .2)
# 40% trimmed mean
mean( x = vector.1, trim = .4)


###
### median
###
vector.1 <- c(123, 234, 43, 32, 23, 11, 2)
vector.1 <- sort(vector.1)
vector.1
mean(vector.1)
median(vector.1)
vector.2 <- c(123, 234, 43, 32, 23, 11)
vector.2 <- sort(vector.2)
vector.2
median(vector.2)


###
### mode (no direct function, but there is package `modeest`)
vector.1 <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)

# the frequency of each value can be displayed with table()
table(vector.1)

# the most frequent value and its frequency (number of occurences) 
#  can bbe displayed by:
as.numeric(names(sort(table(vector.1), decreasing=TRUE)[1]))
# so the mode is 23, and it oocurs 4 times in "vector.1"



###
### min, max, range
###
vector.1 <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
max(vector.1)
min(vector.1)
max(vector.1) - min(vector.1)
range(vector.1)
range(vector.1)[2] - range(vector.1)[1]


###
### Interquartile range (IQR) 
###
vector.1 <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
# median is the 50th quantile
median(vector.1)
quantile( x = vector.1, probs = .5)

# the 25th and the 75th  quantiles are...
quantile( x = vector.1, probs = c(.25, .75))

# now the inter-quartile range:
IQR(vector.1)


###
### Min, Max, 1st and 3rd Quartile, Median and Mean
### with `summmary` functio 
summary (vector.1)


###
### mean absolute deviation
###
X <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
X.bar <- mean(X)
X.bar
absolute.deviation <- abs(X - X.bar)
absolute.deviation
mean.absolute.deviation <- mean(absolute.deviation)
mean.absolute.deviation


###
### sample variance
###
X <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
mean.X <- mean(X)
mean.X
squared.deviation <- (X - mean.X) ^ 2
squared.deviation
sample.variance <- mean(squared.deviation)
sample.variance


###
### unbiased estimator of population variance
###
options("scipen"=30, "digits"=5)
X <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
mean.X <- mean(X)
squared.deviation <- (X - mean.X) ^ 2
estim.of.pop.variance <- sum(squared.deviation) / (length(X) - 1)
estim.of.pop.variance

# function "var" computes unbiased estimator of population variance
var(X)

###
### sample standard deviation
###
X <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
mean.X <- mean(X)
squared.deviation <- (X - mean.X) ^ 2
sample.st.dev <- sqrt(mean(squared.deviation))
sample.variance
sample.st.dev


###
### unbiased estimator of population standard deviation
###
X <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
mean.X <- mean(X)
squared.deviation <- (X - mean.X) ^ 2
estim.of.pop.st.dev <- sqrt( sum(squared.deviation) / (length(X) - 1) )
estim.of.pop.st.dev

# function "sd" computes unbiased estimator of population standard deviation
sd(X)


###
### median absolute deviation
###
X <- c(23, 34, 43, 32, 23, 23, 32, 11, 43, 23)
median.X <- median(X)
median.X
absolute.deviation <- abs(X - median.X)
absolute.deviation
median.absolute.deviation <- median(absolute.deviation)
median.absolute.deviation



#######################################################################
###  Some descriptive statistics for `Sales` (`invoice_detailed`)
glimpse(invoice_detailed)

## Task:
## Given `invoice_detailed` data frame , compute the following
##        measures for each customer:
## - the number of invoices
## - mean invoice amount
## - 20% trimmed mean (of invoice amount)
## - minimum invoice value
## - maximum invoice value
## - the range of invoice amount
## - the inter-quartile range of invoice amount
## - the variance of invoice amount
## - the standard deviation of invoice amount

temp <- invoice_detailed %>%
     group_by(invoiceno, customername) %>%
     summarise (inv_amount = sum(amount)) %>%  # compute the invoice amount
     group_by(customername) %>%  # group by each customer
     summarise(
          n_of_invoices = n(),     # the number of invoices
          mean_invoice_amount = mean(inv_amount), # mean invoice amount
          trim_mean =   mean( x = inv_amount, trim = .2),   # 20% trimmed mean 
          median_invoice_amount = median(inv_amount), # median invoice value
          min_invoice_amount = min(inv_amount), # min invoice value
          max_invoice_amount = max(inv_amount), # max invoice value
          range_invoice_amount = 
               paste(range(inv_amount), collapse = '-'), # range of invoice value
          iqr_invoice_amount = IQR(inv_amount), # the inter-quartile range
          var_invoice_amount = var(inv_amount), # variance
          sd_invoice_amount = sd(inv_amount) # standard deviation
     )
View(temp)          

## descriptive statistics using `skim` function provided by package `skimr`
##  some of the statistics are provided (the most popular) 
## - number of missing values
## - number of non-missing values
## - total number of values
## - mean
## - standard deviation
## - min (p0), first quartile (p25), median (p50), 
##        third quartile (p75) and max (p100),
## - plus a tiny histogram!
## 
library(skimr)

# we can just visualize...
invoice_detailed %>%
     group_by(invoiceno, customername) %>%
     summarise (inv_amount = sum(amount)) %>%  # compute the invoice amount
     group_by(customername) %>%  # group by each customer
     skimr::skim()

# ... or save the results in a data frame
temp <- invoice_detailed %>%
     group_by(invoiceno, customername) %>%
     summarise (inv_amount = sum(amount)) %>%  # compute the invoice amount
     group_by(customername) %>%  # group by each customer
     skimr::skim()
View(temp)


#######################################################################
###  Some descriptive statistics for `fuel_economy_2018` dataset
### option `na.rm = TRUE`
glimpse(fuel_economy_2018)

## Task:
## For variable: `combined_l100km`
## compute the following statistics:
##   - the number of values
##   - number of distinct values
##   - number of NA values
##   - mean 
##   - min, 1st quartile, median, 3rd quartile and max
##   - standard deviation
summary(fuel_economy_2018$combined_l100km)

# Solution...
fuel_economy_2018 %>%
     summarise(
          n = n(),     # the number of values
          n_distinct = n_distinct(combined_l100km),   # the number of distinct values
          n_na = sum( if_else (is.na(combined_l100km), 1L, 0L)), # number of NA values
          min = min(combined_l100km), 
          first_quartile = quantile(combined_l100km, .25),
          mean = mean(combined_l100km), 
          median = median(combined_l100km), 
          third_quartile = quantile( combined_l100km, .75),
          max = max(combined_l100km), 
          sd = sd(combined_l100km) # standard deviation
     )
### ... does not work, since the variable `combined_l100km` has
### at least one NA value;
### we have to use `na.rm = TRUE`:
fuel_economy_2018 %>%
     summarise(
          n = n(),     # the number of values
          n_distinct = n_distinct(combined_l100km),   # the number of distinct values
          n_na = sum( if_else (is.na(combined_l100km), 1L, 0L)), # number of NA values
          min = min(combined_l100km, na.rm = TRUE), 
          first_quartile = quantile( combined_l100km, na.rm = TRUE, .25),
          mean = mean(combined_l100km, na.rm = TRUE), 
          median = median(combined_l100km, na.rm = TRUE), 
          third_quartile = quantile( combined_l100km, na.rm = TRUE, .75),
          max = max(combined_l100km, na.rm = TRUE), 
          sd = sd(combined_l100km, na.rm = TRUE) # standard deviation
     )


## with `skim` from package `skimr`, solution is considerably simpler,
## even if the number of distinct values is not provided
fuel_economy_2018 %>%
     select (combined_l100km) %>%
     skim()


## Task:
## For each of the variables: `cty_l100km`, `hwy_l100km`, `combined_l100km`
## compute the following statistics:
##   - the number of values
##   - number of distinct values
##   - number of NA values
##   - mean 
##   - min, 1st quartile, median, 3rd quartile and max
##   - standard deviation
## Get the result as a data frame (an observation will be associated 
##   to a variable)

fuel_economy_2018 %>%
     select (cty_l100km:combined_l100km) %>%
     gather(variable, value) %>%
     group_by(variable) %>%
     summarise(
          n = n(),     # the number of values
          n_distinct = n_distinct(value),   # the number of distinct values
          n_na = sum( if_else (is.na(value), 1L, 0L)), # number of NA values
          min = min(value, na.rm = TRUE), 
          first_quartile = quantile( value, na.rm = TRUE, .25),
          mean = mean(value, na.rm = TRUE), 
          median = median(value, na.rm = TRUE), 
          third_quartile = quantile( value, na.rm = TRUE, .75),
          max = max(value, na.rm = TRUE), 
          sd = sd(value, na.rm = TRUE) # standard deviation
     )


## with `skim` from package `skimr`, solution is considerably simpler,
## even if the number of distinct values is not provided
fuel_economy_2018 %>%
     select (cty_l100km:combined_l100km) %>%
     skim()



## Task:
## Compute the main descriptive statistics for all numeric 
## variables

# solution with `dplyr` package
fuel_economy_2018 %>%
     select_if(is.numeric) %>%
     summary()


# first solution with `dplyr` + `skimr` package
fuel_economy_2018 %>%
     select_if(is.numeric) %>%
     skim()

# second solution with `dplyr` + `skimr` package
temp <- fuel_economy_2018 %>%
     skim() %>%
     dplyr::filter(type == "numeric")
View(temp)




#######################################################################
###                           Skewness and Kurtosis                 ###

### 
### There are many packages that provide functions for computing
### skewness and kurtosis (`e1071`, `psych`, `moments`, `fbasics`,  
### `PerformanceAnalytics`,  `tidyquant`)
### 
### We will use `PerformanceAnalytics`, because it sounds professional :-) 
library(PerformanceAnalytics)

## Task: 
## Display as a density the distribution of combined 
##   fuel consumption variable (`cty_l100km`) and then
##   compute skewness and kurtosis
##   

# firstly, the chart...
ggplot(fuel_economy_2018 %>% filter (!is.na(combined_l100km)), 
          aes(x = combined_l100km)) + 
     geom_density(color = "white", fill = 'red', alpha = .5) +
	ggtitle("Density Plot of Combined Fuel Consumption") +
  	xlab("liters per 100 km") 

# ... and now, the statistics
fuel_economy_2018 %>%
     select (combined_l100km) %>%
     summarise(
          skewness = PerformanceAnalytics::skewness(combined_l100km), 
          kurtosis = PerformanceAnalytics::kurtosis(combined_l100km) 
          )


glimpse(fuel_economy_2018)
## Task: 
## 1) Display the histograms of the following variables:
##        - `displacement`
##        - `n_of_cyl`
##        - `air_pollution`
##        - `greenhouse`
##        - `combined_CO2`
##        - `combined_l100km`
##  2) Compute skewness and kurtosis for each      


# 1) - the faceted histograms
fuel_economy_2018 %>%
     select (displacement:combined_CO2, combined_l100km) %>%
     gather(variable, value) %>%
ggplot(., aes(x = value, fill = variable)) +
     geom_histogram() +
     facet_wrap( . ~ variable, scales = 'free') +
     theme(legend.position="none") 
     
# 2) compute skewness and kurtosis for each variable   
fuel_economy_2018 %>%
     select (displacement:combined_CO2, combined_l100km) %>%
     gather(variable, value) %>%
     group_by(variable) %>%
     summarise(
          skewness = skewness(value), 
          kurtosis = kurtosis(value) 
          )



#######################################################################
###	    II.2 Data locality/spread/shape for ordinal variables      ###
#######################################################################

### Compared to interval/ration data, some descriptive statististics
### are less relevant, even unrecommended:
### - mean is less revelant since the pool of averaged numbers is small
### - standard deviation is even less relevant
### - for small number of levels, the quartiles are not useful
### - also skewness and kurtosis are usually not computed
### - do not use density plots for ordinal variables (even if 
###       they look appealing)!


#######################################################################
###              Sales data set (`invoice_detailed`)
glimpse(invoice_detailed)

## Task:
## Display basic statistics about the number of rows (lines) in invoices

# summary()... 
summary(invoice_detailed$invoicerownumber)

# `skimr` package
invoice_detailed %>%
     select (invoicerownumber) %>%
     skim()


# similar to nominal variables a table of frequencies would be
#    more useful
table(invoice_detailed$invoicerownumber)


## Task:
## For each customer, get a table of frequencies with the number of 
## lines in its invoices

# base R solution:
table(invoice_detailed$customername, invoice_detailed$invoicerownumber)

# dplyr solution:
invoice_detailed %>%
     group_by(customername, invoicerownumber) %>%
     summarise(n = n()) %>%
     spread (invoicerownumber, n, fill = 0)



#######################################################################
###                      `fuel_economy_2018` dataset
glimpse(fuel_economy_2018)

## In the last chart/task (skewness/kurtosis), actually
## three of the variables seem to be ordinal:
##   - `n_of_cyl`
##   - `air_pollution`
##   - `greenhouse`
## Task:
## Plot the histogram only for these variables, displaying only 
##   integer values on x-axis

# the faceted histograms ()
fuel_economy_2018 %>%
     select (n_of_cyl:greenhouse) %>%
     gather(variable, value) %>%
ggplot(., aes(x = value, fill = variable, col = variable)) +
     geom_histogram(binwidth = 1, alpha = 0.5) +
     facet_wrap( . ~ variable, scales = 'free') +
     theme(legend.position="none") +
     scale_y_continuous(breaks = seq(0, 1300, 50)) +  
     scale_x_continuous(breaks = pretty_breaks())  ## this will display
                                                   ## only integers on the x-axis


#######################################################################
###	    II.3 Data locality/spread/shape for nominal variables      ###
#######################################################################

### For nominal variables, descriptive statistics is usually
### reduced to only frequency tables;
### 
### Barcharts are ok for describing nominal variables


#######################################################################
###              Sales data set (`invoice_detailed`)
glimpse(invoice_detailed)

## Task:
## Display some basic information about nominal variable `customername`

# `summary` provides no much information... 
summary(invoice_detailed$customername)

#... but `skimr` package do (notice the min and the max lengths and
# also the number of distinct values)
invoice_detailed %>%
     select (customername) %>%
     skim()

# for displaying the frequency table one can use...
table(invoice_detailed$customername)
# ...or 
invoice_detailed %>%
     select (customername) %>%
     table()



## Task:
## Display, for each customer, the product frequency (number of 
##   occurences in customer's invoices)

# base R solution:
table(invoice_detailed$customername, invoice_detailed$productname)

# dplyr solution:
invoice_detailed %>%
     group_by(customername, productname) %>%
     summarise(n = n()) %>%
     spread (productname, n, fill = 0)




#######################################################################
###	          III. Measure of association between variables        ###
#######################################################################

## See also:
# Correlation and Covariance in R (R Tutorial 4.9)
# https://www.youtube.com/watch?v=XaNKst8ODEQ&index=33&list=PLqzoL9-eJTNBDdKgJgJzaQcY6OXmsXAHU


## Correlation
# Correlation coefficients are used to describe linear relationships among 
#  quantitative variables.
# The sign ± indicates the direction of the relationship (positive or 
#  inverse) and the magnitude indicates the strength of the linear 
#   relationship (ranging from 0 for  no relationship to 1 for a 
#   perfectly predictable relationship).

# Three correlation coefficients:
#    - The Pearson product moment correlation assesses the degree of 
#  linear relationship between two quantitative variables. 
#    - Spearman’s Rank Order correlation coefficient assesses 
#   the degree of relationship between two rank-ordered 
#   variables (nonparametric). 
#	- Kendall’s Tau is also a nonparametric measure of rank correlation.
# (see http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient)
#


#######################################################################
###        III.1 Association between interval/ratio variables       ###
#######################################################################


#######################################################################
###                           `Faithful` data set        
### (http://www.r-tutor.com/elementary-statistics/quantitative-data(

# This is a built-in data frame named faithful containing a collection 
#  of observations of the Old Faithful geyser in the USA 
#   Yellowstone National Park. 
head(faithful)
# `eruptions`  stores the duration of the geyser eruptions
# `waiting` is the length of waiting period until the next eruption

##  Task:
##  Is there any correlation between `eruptions` and `waiting`?
##  1) Compute the correlation coeficient(s)
##  2) Visualize the correlation  


##  1) Compute the correlation coeficient(s)

# The Pearson product moment correlation assesses the degree of 
#  linear relationship between two quantitative variables. 
cor(faithful$eruptions, faithful$waiting)

# Spearman’s Rank Order correlation coefficient assesses 
#   the degree of relationship between two rank-ordered 
#   variables (nonparametric). 
cor(faithful$eruptions, faithful$waiting, method="spearman")

#	Kendall’s Tau is also a nonparametric measure of rank correlation.
# http://en.wikipedia.org/wiki/Kendall_tau_rank_correlation_coefficient
cor.test(faithful$eruptions, faithful$waiting, 
         method="kendall", alternative="two.side")


##  2) Visualize the correlation  

# Solution with `scatterplotMatrix` function in `car` package. 
# `scatterplotMatrix` provides scatter plots of the variables 
#    with each other in the off-diagonals and 
#    superimposes smoothed (loess) and linear fit lines on these plots; 
# the principal diagonal contains density and rug plots for each variable.
car::scatterplotMatrix(faithful, spread=FALSE, lty.smooth=2, 
	main="Scatter Plot Matrix")

# Solution with `ggpairs` function from package `GGally`
GGally::ggpairs(faithful)


#######################################################################
###                      `fuel_economy_2018` dataset
###  Similar example with the script `08c`, section `III.2`                     
glimpse(fuel_economy_2018)

## Task: 
## 1) Compute and display the correlations between all pairs of the 
##   following variables:
## - `cty_l100km`
## - `hwy_l100km`
## - `combined_l100km`
## - `displacement`
## - `combined_CO2`
## 2) Visualize association/correlation among these variables


## 1) Compute correlation between each pair from above variables:

# Base R Solution - `Pearson` coefficient:
cor (fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 'combined_l100km',
                          'displacement', 'combined_CO2')], 
     use = "pairwise.complete.obs")

# Base R Solution - `Spearman` coefficient:
cor (fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 
          'combined_l100km', 'displacement', 'combined_CO2')],
     method = 'spearman',
     use = "pairwise.complete.obs")

# Base R Solution - `Kendall` coefficient:
cor (fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 
          'combined_l100km', 'displacement', 'combined_CO2')],
     method = 'kendall',
     use = "pairwise.complete.obs")


# `correlate` function in package `corrr` (part of the tidymodels ecosystem)
corrr::correlate (fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 
          'combined_l100km', 'displacement', 'combined_CO2')],
     method = 'kendall',
     use = "pairwise.complete.obs",
     diagonal = NA,
     quiet = TRUE
)



##  2) Visualize the correlation  

# Solution with `scatterplotMatrix` (it can took a while...)
car::scatterplotMatrix(fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 
          'combined_l100km', 'displacement', 'combined_CO2')], 
          spread=FALSE, lty.smooth=2, main="Scatter Plot Matrix")

# Solution with `ggpairs` (it can took another while...)
GGally::ggpairs(fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 
          'combined_l100km', 'displacement', 'combined_CO2')])


# First solution based on `corrplot` package 
fuel_economy_2018 %>%
     select (cty_l100km:combined_l100km, displacement, combined_CO2) %>%
     filter(complete.cases(.)) %>% # remove all observations with NA values
cor(.) %>%
corrplot::corrplot.mixed(.)

# Second solution based on `corrplot` package 
fuel_economy_2018 %>%
     mutate (displacement  = as.numeric(Displ), 
             n_of_cyl = as.numeric(Cyl),
             air_pollution = as.numeric(`Air Pollution Score`),
             greenhouse = as.numeric(`Greenhouse Gas Score`),
             combined_CO2 = as.numeric(`Comb CO2`)
             ) %>%
     select (cty_l100km:combined_l100km, displacement:combined_CO2) %>%
     filter(complete.cases(.)) %>% # remove all observations with NA values
cor(.) %>%
corrplot(., method = 'number', type = 'lower')


# Third solution based on `corrplot` package 
fuel_economy_2018 %>%
     mutate (displacement  = as.numeric(Displ), 
             n_of_cyl = as.numeric(Cyl),
             air_pollution = as.numeric(`Air Pollution Score`),
             greenhouse = as.numeric(`Greenhouse Gas Score`),
             combined_CO2 = as.numeric(`Comb CO2`)
             ) %>%
     select (cty_l100km:combined_l100km, displacement:combined_CO2) %>%
     filter(complete.cases(.)) %>% # remove all observations with NA values
cor(., method = "spearman") %>%
corrplot(., order="hclust",
         method = "number", type = "upper")



#######################################################################
###	          III.2 Association between ordinal variables          ###
#######################################################################

## For ordinal data, two of the three correlation coefficients are
## recommended:
#    - Spearman’s Rank Order correlation coefficient assesses 
#   the degree of relationship between two rank-ordered 
#   variables (nonparametric). 
#	- Kendall’s Tau is also a nonparametric measure of rank correlation.
#
#  For visualisation, use mosaic plots and heatmaps (script 08b, 
#  section V)

#######################################################################
###                      `fuel_economy_2018` dataset
###  Similar example with the script `08c`, section `III.2`                     
glimpse(fuel_economy_2018)

## Task: 
## Compute correlations between all pairs of the 
##   following variables:
##   - `n_of_cyl`
##   - `air_pollution`
##   - `greenhouse`

# Base R Solution - `Spearman` coefficient:
cor (fuel_economy_2018[c('n_of_cyl', 'air_pollution', 
          'greenhouse')],
     method = 'spearman',
     use = "pairwise.complete.obs")

# Base R Solution - `Kendall` coefficient:
cor (fuel_economy_2018[c('n_of_cyl', 'air_pollution', 
          'greenhouse')],
     method = 'kendall',
     use = "pairwise.complete.obs")


#######################################################################
###	         III.3 Association between nominal variables           ###
#######################################################################

#    In order to estimate the strength of the relationships between
#    nominal variables, some association coefficients were proposes, 
#    such as:
#    - phi coefficient
#    - contingency coefficient
#    - Cramer’s V coefficient
#    
 #       Interpretation of the Phi coefficient (similar to correlations):
# -1.0 to -0.7 strong negative association.
# -0.7 to -0.3 weak negative association.
# -0.3 to +0.3 little or no association.
# +0.3 to +0.7 weak positive association.
# +0.7 to +1.0 strong positive association.


#######################################################################
###	                         `Arthritis` dataset 
library (vcd)
glimpse(Arthritis)

## Task:
## Estimate the strength of the association between `Treatment` and
##   `Improved`

# Solution based on `assocstats` function provided by package `vcd`
vcd::assocstats(xtabs(~ Treatment + Improved, data = Arthritis))

## we can compare the results with a non-parametric test after 
## converting the variables into ordinal (numeric) ones 
## notice: this transformation is statistically debatable 

#... with Spearman coefficient
Arthritis %>%
     transmute (treatment_num = if_else(Treatment == "Placebo", 0, 1), 
                result_num = case_when(
                     Improved == 'None' ~ 0, 
                     Improved == 'Some' ~ 1, 
                     TRUE ~ 2 )) %>%
cor(., method = "spearman") 

#... with Kendall coefficient
Arthritis %>%
     transmute (treatment_num = if_else(Treatment == "Placebo", 0, 1), 
                result_num = case_when(
                     Improved == 'None' ~ 0, 
                     Improved == 'Some' ~ 1, 
                     TRUE ~ 2 )) %>%
cor(., method = "kendall") 



