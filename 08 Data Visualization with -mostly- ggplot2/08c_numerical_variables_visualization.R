###############################################################################
### Document partially supported by research project: POC/398/1/1 nr. 124759 -
### „Research As A Service – Iasi (RaaS-IS)”
###############################################################################

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
###              8c. Numerical Data Visualization with ggplot2          ####
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/08%20Data%20Visualization%20with%20-mostly-%20ggplot2/08_ggplot2.pptx
############################################################################
## last update: 06.06.2021

library(tidyverse)
library(readxl)
library(lubridate)
options(scipen = 999)

############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and
# set the directory where you dowloaded the data files as the
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


#######################################################################
###  Agenda:                                                        ###
###	I. Visualizing Single Numeric Variable                         ###
###		I.1 Histograms                                            ###
###		I.2 Density plots                                         ###
###       I.3 Single Variable Boxplots                              ###
###	II. Superimposing/Faceting Two Numeric Variables               ###
###		II.1 Superimposed and Faceted Histograms                  ###
###		II.2 Superimposed and Faceted Density Curves              ###
###		II.3 Boxplots of multiple variables                       ###
###	III. Relationship Between Two Numeric Variables                ###
###		III.1 Scatterplots                                        ###
###	     III.2 Correlation plots (without ggplot2)                 ###
#######################################################################


#######################################################################
###		     I. Visualizing Single Numeric Variable               ###
#######################################################################

#######################################################################
###		                I.1 Histograms                            ###
#######################################################################

#######################################################################
###	    `Arthritis` dataset (see description in script `08b...`)
#install.packages('vcd')
library (vcd)
glimpse(Arthritis)

## Task:
## Display as histogram the distribution of `Age` values

# Solution 1: default `binning`
ggplot(Arthritis %>% filter (!is.na(Age)),
          aes(x = Age)) +
     geom_histogram(color = "white") +
	ggtitle("Histogram of Age (`Arthritis` data set)")


# Solution 2: set each bin to cover five years
ggplot(Arthritis %>% filter (!is.na(Age)),
          aes(x = Age)) +
	geom_histogram( binwidth = 5, # a bin will cover five years
	                alpha = .6) + # make the bins more transparent
	ggtitle("Histogram of Age (`Arthritis` data set)")


## Task:
## Display as histogram the distribution of `Age` values;
## New: add a vertical line showing the average age

# `geom_vline` added
ggplot(Arthritis %>% filter (!is.na(Age)),
          aes(x = Age)) +
	geom_histogram( binwidth = 5, # a bin will cover five years
	                alpha = .6) + # make the bins more transparent
     geom_vline( aes (xintercept = mean(Age, na.rm=T)),
               color="red", linetype="dashed", size= .5) +
	ggtitle("Histogram of Age (with Mean Age)")


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
glimpse(fuel_economy_2018)


## Task:
## Display as histogram the distribution of city
##   fuel consumption variable (`cty_l100km`)
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km)),
          aes(x = cty_l100km)) +
     geom_histogram(color = "white", fill = 'red') +
	ggtitle("Histogram of City Fuel Consumption") +
  	xlab("liters per 100 km")


## Task:
## Display as histogram the distribution of highway
##   fuel consumption variable (`hwy_l100km`)
ggplot(fuel_economy_2018 %>% filter (!is.na(hwy_l100km)),
          aes(x = hwy_l100km)) +
     geom_histogram(color = "white") +
	ggtitle("Histogram of Highway Fuel Consumption") +
  	xlab("liters per 100 km")


############################################################################
###            Dragos Cogean's Data Set (compare two cloud
###                 database services, MongoDB and MySQL)

# import only the data on `read` operations for MongoDB
ReadMongoALL <- read_tsv("DragosCogean__ReadMongo_ALL.txt") %>%
     mutate (latency_secs = latenta / 1000)
glimpse(ReadMongoALL)

## Task:
## Display the overall histogram for `latency_secs` variable

# initial solution:
ggplot(ReadMongoALL,
	     aes(x = latency_secs)) +
	geom_histogram() +
	ggtitle("Read Latency for MongoDB") +
     xlab('Latency (in seconds)')


# now, try again
ggplot(ReadMongoALL,
	     aes(x = latency_secs)) +
	geom_histogram() +
	ggtitle("Read Latency for MongoDB") +
     xlab('Latency (in seconds)')


## Task:
## Display the histogram for `latency_secs` variable
## only for tests of type (`testtype`) "100SEC"
ggplot(ReadMongoALL %>% filter(testtype == "100SEC"),
	     aes(x = latency_secs)) +
	geom_histogram() +
	ggtitle("100SEC Test Read Latency for MongoDB") +
     xlab('Latency (in seconds)')


#######################################################################
###		                I.2 Density plots                        ###
#######################################################################

#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)

## Task:
## Display as a density the distribution of city
##   fuel consumption variable (`cty_l100km`)
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km)),
          aes(x = cty_l100km)) +
     geom_density(color = "white", fill = 'red', alpha = .5) +
	ggtitle("Density Plot of City Fuel Consumption") +
  	xlab("liters per 100 km")


## Task:
## Superimpose the histogram and the density line
##  of highway fuel consumption variable (`hwy_l100km`)
ggplot(fuel_economy_2018 %>% filter (!is.na(hwy_l100km)),
          aes(x = hwy_l100km)) +
     geom_histogram(aes(y = ..density..), alpha = .5) +
     geom_density(alpha = .5) +
	ggtitle("Superimposed Histogram and Density Curve of \nHighway Fuel Consumption") +
  	xlab("liters per 100 km")

## Task:
## To the previous plot, add a vertical line showing the mean
##   consumption
ggplot(fuel_economy_2018 %>% filter (!is.na(hwy_l100km)),
          aes(x = hwy_l100km)) +
     geom_histogram(aes(y = ..density..), alpha = .5) +
     geom_density(alpha = .5) +
     geom_vline( aes (xintercept = mean(hwy_l100km, na.rm=T)),
               color="red", linetype="dashed", size=.5)  +
	ggtitle("Superimposed Histogram and Density Curve of \nHighway Fuel Consumption") +
  	xlab("liters per 100 km")



#######################################################################
###		         I.3 Single Variable Boxplots                     ###
# (not very impressive - see `08d` script for more relevant examples) #
#######################################################################

#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)


## Task:
## Display as a boxplot the distribution of city
##   fuel consumption variable (`cty_l100km`)

# as we display the boxplot of the single variable,
# we have to declare an arbitrary x (such as 0, 1, ...)
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km)),
          aes(x = 0, y = cty_l100km)) +
     geom_boxplot() +
	ggtitle("Boxplot of City Fuel Consumption")

# as text on the x axis is completely irrelevant, it must
# be removed
# we also rescale the y-axis
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km)),
          aes(x = 0, y = cty_l100km)) +
     geom_boxplot() +
	ggtitle("Boxplot of City Fuel Consumption") +
  	ylab("liters per 100 km") + xlab("") +
     theme(axis.text.x = element_blank()) +
     scale_y_continuous(breaks = seq(0, 30, 2))


#######################################################################
###	        II. Superimposing/Faceting Two Numeric Variables       ###
#######################################################################

#######################################################################
###		      II.1 Superimposed and Faceted Histograms            ###
#######################################################################

#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)

## Task:
## Display two SUPERIMPOSED histograms for:
## - city fuel consumption variable (`cty_l100km`) and
## - highway fuel consumption variable (`hwy_l100km`)

# Solution 1:
# the main `aes` clause will provide the settings for the
# first histogram (`cty_l100km`); for the second histogram
# (`hwy_l100km`) we'll declare the aestetics directly in
# its `geom_histogram` clause
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km) &
               !is.na(hwy_l100km)),
          aes(x = cty_l100km)) +    # global `aes`; used for first histogram
     geom_histogram(col = "red", fill = "red", alpha = 0.4) +
     geom_histogram(aes(x = hwy_l100km), col = "yellow", # the second histogram needs
                    fill = "yellow", alpha = 0.4) +      #  its own `aes`
  	xlab("liters per 100 km") +
	ggtitle("Superimosed Histograms of City (red) and Highway (yellow) \nFuel Consumption")

# Solution 2: we transform the data set (from `wide` to `long` format),
# use a single `geom_histogram` clause, and use `fill` and `col` options
# for plotting two histograms
ggplot(fuel_economy_2018 %>%
            filter (!is.na(cty_l100km) & !is.na(hwy_l100km)) %>%
            mutate (row_num = row_number()) %>%
            select (row_num, cty_l100km, hwy_l100km) %>%
            gather (parameter, value, -row_num),
          aes(x = value, fill = parameter, col = parameter)) +
     geom_histogram(alpha = .4) +
  	xlab("liters per 100 km") +
	ggtitle("Superimosed Histograms of City and Highway \nFuel Consumption")


## Task: Display two FACETED histograms for of city
##   fuel consumption variable (`cty_l100km`) and highway
##   fuel consumption variable (`hwy_l100km`)

# we'll use the last solution, adding `facet_wrap`
ggplot(fuel_economy_2018 %>%
            filter (!is.na(cty_l100km) & !is.na(hwy_l100km)) %>%
            mutate (row_num = row_number()) %>%
            select (row_num, cty_l100km, hwy_l100km) %>%
            gather (parameter, value, -row_num),
          aes(x = value, fill = parameter)) +
     geom_histogram( alpha = 0.5) +
  	xlab("liters per 100 km") +
	ggtitle("Faceted Histograms of City and Highway \nFuel Consumption") +
     facet_wrap(~ parameter) +
	theme(legend.position = "none")



#######################################################################
###		  II.2 Superimposed and Faceted Density Curves            ###
#######################################################################


#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)

## Task:
## Display two SUPERIMPOSED density curves for of city
##   fuel consumption variable (`cty_l100km`) and highway
##   fuel consumption variable (`hwy_l100km`)

# Solution 1:
# the same as for the histogram, but with `geom_density`
#    instead of `geom_histogram`
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km) &
               !is.na(hwy_l100km)),
          aes(x = cty_l100km)) +    # global `aes`; used for first density line
     geom_density(col = "red", fill = "red", alpha = 0.4) +
     geom_density(aes(x = hwy_l100km), col = "yellow", # the second density line needs
                    fill = "yellow", alpha = 0.4) +      #  its own `aes`
  	xlab("liters per 100 km") +
	ggtitle("Superimosed Density Curves of City (red) and \nHighway (yellow) Fuel Consumption")

# Solution 2: identical to histogram: we transform the data set (from
# `wide` to `long` format), use a single `geom_density` clause,
# and use `fill` and `col` options for plotting two density lines
ggplot(fuel_economy_2018 %>%
            filter (!is.na(cty_l100km) & !is.na(hwy_l100km)) %>%
            mutate (row_num = row_number()) %>%
            select (row_num, cty_l100km, hwy_l100km) %>%
            gather (parameter, value, -row_num),
          aes(x = value, fill = parameter, col = parameter)) +
     geom_density(alpha = .5) +
  	xlab("liters per 100 km") +
	ggtitle("Superimosed Density Curves of City and \nHighway Fuel Consumption")


## Task: Display two FACETED density curves for of city
##   fuel consumption variable (`cty_l100km`) and highway
##   fuel consumption variable (`hwy_l100km`)

# we'll use the last solution, adding `facet_wrap`
ggplot(fuel_economy_2018 %>%
            filter (!is.na(cty_l100km) & !is.na(hwy_l100km)) %>%
            mutate (row_num = row_number()) %>%
            select (row_num, cty_l100km, hwy_l100km) %>%
            gather (parameter, value, -row_num),
          aes(x = value, fill = parameter)) +
     geom_density( alpha = 0.4) +
  	xlab("liters per 100 km") +
	ggtitle("Faceted Density Curves of City and \nHighway Fuel Consumption") +
     facet_wrap(~ parameter) +
	theme(legend.position = "none")


############################################################################
###            Dragos Cogean's Data Set (compare two cloud
###                 database services, MongoDB and MySQL)
glimpse(ReadMongoALL)
table(ReadMongoALL$testtype)

## Task:
## Display two SUPERIMPOSED density curves for:
## - the latency or read operations of type `100SEC`
## - the latency or read operations of type `LONG`

# Solution (note the `data` clause for both `geom_density`es)
ggplot (ReadMongoALL, aes(x = latency_secs)) + # the data won't be used
                                               # by the `geom_density` options
     geom_density(data = ReadMongoALL %>% filter ( testtype == "100SEC"),
          col = "red", fill = "red", alpha = 0.2) +
     geom_density(data = ReadMongoALL %>% filter ( testtype == "LONG"),
          col = "yellow", fill = "yellow", alpha = 0.5) +
	ggtitle("Superimposed Density Curves for the Latency of READ operations \nof type `100SEC` (red) and `LONG` (yellow) in MongoDB")



#######################################################################
###		          II.3 Boxplots of multiple variables             ###
#######################################################################

#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)

## Task:
## Display, on a single chart, three boxplots showing
## the distribution of three variables:
##   - city fuel consumption  (`cty_l100km`)
##   - highway fuel consumption (`hwy_l100km`)
##   - combined (city and highway) fuel consumption (`combined_l100km`)


# Solution 1:
# with three `geom_boxplot`s
ggplot(fuel_economy_2018 %>% filter (!is.na(cty_l100km) &
               !is.na(hwy_l100km)),
          aes(x = '1 - city', y = cty_l100km)) +  # global `aes`; used for first boxplot
     geom_boxplot() + # this box takes the settings from the global `aes`
     geom_boxplot(aes(x = '2 - highway', y = hwy_l100km)) +  #  own `aes`
     geom_boxplot(aes(x = '3 - combined', y = combined_l100km)) + #  own `aes`
  	ylab("liters per 100 km") + xlab("type of consumption") +
	ggtitle("City, Highway, and Combined Fuel Consumption")

# Solution 2:
# transform the data set (from `wide` to `long` format),
# and use a single `geom_boxplot` clause
ggplot(fuel_economy_2018 %>%
            filter (!is.na(cty_l100km) & !is.na(hwy_l100km)) %>%
            mutate (row_num = row_number()) %>%
            select (row_num, `1 - city` = cty_l100km,
                    `2 - highway` = hwy_l100km,
                    `3 - combined` = combined_l100km) %>%
            gather (parameter, value, -row_num),
          aes(x = parameter, y = value)) +
     geom_boxplot() +
  	ylab("liters per 100 km") + xlab("type of consumption") +
	ggtitle("City, Highway, and Combined Fuel Consumption")



#######################################################################
###	        III. Relationship Between Two Numeric Variables        ###
#######################################################################

#######################################################################
###		                 III.1 Scatterplots                       ###
#######################################################################


#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)

## Task:
## Examine through a scatterplot the relationship between the
## combined (city and highway) fuel consumption (on the y-axis) and the
## engine displacement (on x-axis)

# Note: both variables seem continuous, but the engine displacement
# is rather discrete, so expect some vertical `stripes`

# Solution 1 ingredients:
# - filter `Displ` removing NA and `N/A` values and convert it
#    into numbers
# - `geom_point` will draw the scatterplot
# - `scale_x_continuous` and `scale_y_continuos` will set the
#    values displayes on the axes
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km)) +
     geom_point(alpha = 0.2) +
     xlab("engine displacement (thousands of cubic centimetres)") +
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45,
               vjust = 1, hjust = 1 )) +
     scale_y_continuous(breaks = seq(0, 25, 1))  +
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))


# Solution 2 use `position_jitter` inside `geom_point` for
# showing the concentration
# of values (very useful for discrete variables)
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km)) +
     geom_point (position = position_jitter(), alpha = 0.2) +
     xlab("engine displacement (thousands of cubic centimetres)") +
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45,
               vjust = 1, hjust = 1 )) +
     scale_y_continuous(breaks = seq(0, 25, 1))  +
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))


# Solution 3 use `geom_jitter` for showing the concentration
# of values
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km)) +
     geom_jitter (alpha = 0.2) +
     xlab("engine displacement (thousands of cubic centimetres)") +
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45,
               vjust = 1, hjust = 1 )) +
     scale_y_continuous(breaks = seq(0, 25, 1))  +
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))


# Solution 4 adds marginal rugs on exes (see `geom_rug`)
# A rug plot is a compact visualisation designed to supplement
# a 2d display with the two 1d marginal distributions.
# Rug plots display individual cases so are best used
# with smaller datasets.
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km)) +
     geom_jitter (alpha = 0.2) +
     geom_rug() +
     xlab("engine displacement (thousands of cubic centimetres)") +
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45,
               vjust = 1, hjust = 1 )) +
     scale_y_continuous(breaks = seq(0, 25, 1))  +
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))


# Solution 5 adds a regression line for showing the
# nature of the relationship between two variable
#
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km)) +
     geom_jitter (alpha = 0.3) +
     geom_rug() +
     xlab("engine displacement (thousands of cubic centimetres)") +
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45,
               vjust = 1, hjust = 1 )) +
     scale_y_continuous(breaks = seq(0, 25, 1))  +
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))  +
     geom_smooth()    # Add a (non-linear) regression line,
                      # including a confidence region for the curve



# Solution 6 uses a hexbin chart
# A hexbin plot is like a two-dimensional histogram.
# The data is divided into bins, and the number of data points
#  in each bin is represented by color or shading (lighter = more crowded)
# install.packages("hexbin")
library(hexbin)

fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km)) +
     geom_hex() +
     xlab("engine displacement (thousands of cubic centimetres)") +
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45,
               vjust = 1, hjust = 1 )) +
     scale_y_continuous(breaks = seq(0, 25, 1))  +
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))  +
     geom_smooth()    # Add a (non-linear) regression line,
                      # including a confidence region for the curve


#######################################################################
###	         III.2 Correlation plots (without ggplot2)             ###
#######################################################################
# install.packages('corrplot')
library(corrplot)

#######################################################################
###                      Fuel Economy dataset(s)
glimpse(fuel_economy_2018)

## Task:
## Display the correlations between all pairs of the following variables:
## - cty_l100km
## - hwy_l100km
## - combined_l100km
## - displacement
## - number of cyllinders
## - `Air Pollution Score`
## - `Greenhouse Gas Score`
## - `Comb CO2`

# first solution based on `corrplot` package (not ggplot)
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
corrplot.mixed(., number.cex=0.75, tl.cex=0.6 )


# another of of displaying the correlations
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
corrplot(., method = 'number', type = 'lower', number.cex=0.75, tl.cex=0.6)
