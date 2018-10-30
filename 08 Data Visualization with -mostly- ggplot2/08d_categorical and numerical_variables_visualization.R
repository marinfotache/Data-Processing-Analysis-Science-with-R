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
###   8d. Both Categorical and Numerical Data Visualization with ggplot2 ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/08%20Data%20Visualization%20with%20-mostly-%20ggplot2/08_ggplot2.pptx
############################################################################
## last update: 30.10.2018

library(tidyverse) 
library(readxl)
library(scales) # need for thousands separation
library(lubridate)

############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')

# giving up scientific notation (1.6e+07)
#options("scipen"=30, "digits"=14)


#######################################################################
###	                              Agenda                           ###	
#######################################################################
###	 I. Groups of Barcharts (of Values)                            ###
###	 II. Line charts                                               ###
###	 III. Histograms and density curves with multiple groups       ###
###	 IV. Boxplot with multiple groups                              ###
###	 V. Scatter plots with multiple groups                         ###
#######################################################################


#######################################################################
###	              I. Groups of Barcharts (of Values)               ###
#######################################################################

#######################################################################
###                                Sales
load (file = 'sales.RData')
glimpse(invoice_detailed)

## Task:
## Display the overall product sales
invoice_detailed %>%
     group_by(productname) %>%
     summarise (sales = sum(amount)) %>%
ggplot(., aes(x = productname, y = sales, fill = productname)) +
     geom_bar(stat="identity") +
     xlab("product") + ylab("sales") +
     ggtitle("Product Sales") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     scale_y_continuous(labels = comma)    # separate thousands with comma


## Task:
## Display the product sales for each year
invoice_detailed %>%
     group_by(year = year (invoicedate), productname) %>%
     summarise (sales = sum(amount)) %>%
ggplot(., aes(x = productname, y = sales, fill = productname)) +
     geom_bar(stat="identity") +
     xlab("product") + ylab("sales") +
     ggtitle("Yearly Sales, by Product") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45, 
          vjust = 1, hjust = 1 )) + 
     scale_y_continuous(labels = comma)  +  # separate thousands with comma
     facet_wrap( ~ year)


## Task:
## Display the yearly sales for each product 

# solution 1: with faceting
invoice_detailed %>%
     group_by(year = year (invoicedate), productname) %>%
     summarise (sales = sum(amount)) %>%
ggplot(., aes(x = year, y = sales, fill = factor(year))) +
     geom_bar(stat="identity") +
     xlab("year") + ylab("sales") +
     ggtitle("Product Sales, by Year") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(labels = comma)  +  # separate thousands with comma
     facet_wrap( ~ productname)

# solution 2 - stacked bars
invoice_detailed %>%
     group_by(year = factor(year (invoicedate)), productname) %>%
     summarise (sales = sum(amount)) %>%
ggplot(., aes(x = productname, y = sales, fill = year)) +
     geom_bar(stat="identity") +
     xlab("year") + ylab("sales") +
     ggtitle("Product Sales, by Year") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
          vjust = 1, hjust = 1 )) + 
	scale_y_continuous(labels = comma)    # separate thousands with comma


## Task:
## Display monthly (within each year) product sales  

# solution: `facet_wrap` on both year and month (notice `labeller`)
invoice_detailed %>%
     group_by(year = year (invoicedate), month = month (invoicedate), 
              productname) %>%
     summarise (sales = sum(amount)) %>%
ggplot(., aes(x = productname, y = sales, fill = productname)) +
     geom_bar(stat="identity") +
     xlab("product") + ylab("sales") +
     ggtitle("Monthly (Within Each Year) Product Sales ") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 45, 
          vjust = 1, hjust = 1 )) + 
	scale_y_continuous(labels = comma)  +  # separate thousands with comma
     facet_wrap( year ~ month, labeller = label_both) 



#######################################################################
###	                    II. Line charts                            ###
#######################################################################

#  For line graphs, the data points must be grouped so that it knows 
#  which points to connect. For all points to be connected, 
#  use `group = 1` 

#######################################################################
###                                Sales
glimpse(invoice_detailed)

## Task:
## Display the evolution of daily sales

# Variable mappings:
#   `orderdate`: x-axis
#   "daily_sales": y-axis

invoice_detailed %>%
     group_by(date = factor(invoicedate)) %>%
     summarise (daily_sales = sum(amount)) %>%
ggplot(., aes(x = date, y = daily_sales, group = 1)) +
     geom_line(colour="red", linetype="dotted", size=0.5) +
     geom_point() + # Add points to the extremities
     ggtitle("Daily Sales") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     theme(axis.text.x = element_text(angle = 60, 
          vjust = 1, hjust = 1 )) + 
	scale_y_continuous(labels = comma) 


## Task:
## Display the sales monthly evolution within each yar 

# Solution:
#  To draw multiple lines, the points must be grouped by a variable; 
#   otherwise all points will be connected by a single line. 
#  In this case, we want them to be grouped by sex.

invoice_detailed %>%
     group_by(year = factor(year (invoicedate)), 
                   month = factor(month (invoicedate))) %>%
     summarise (sales = sum(amount)) %>%
ggplot(., aes(x = month, y = sales, group = year, 
                   colour = year, shape = year)) +
     geom_line() +
     geom_point() + # Add points to the extremities
     ggtitle("Monthly Sales Evolution, by Year") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 0, 
               vjust = 1, hjust = 0.5 )) + 
     scale_y_continuous(labels = comma) 


## Task:
## Display the sales monthly evolution for each product 

# Solution:
#  To draw multiple lines, the points must be grouped by a variable; 
#   otherwise all points will be connected by a single line. 
#  In this case, we want them to be grouped by `productname`

invoice_detailed %>%
     group_by(year = factor(year (invoicedate)), 
                   month = factor(month (invoicedate)), 
              productname) %>%
     summarise (sales = sum(amount)) %>%
     mutate(year_month = paste(year, 
                               str_pad(month, 2, side ='left' ,
                                       pad = "0") , sep = '-')) %>%
ggplot(., aes(x = year_month, y = sales, 
              group = productname, colour = productname, shape = productname)) +
     geom_line() +
     geom_point() + # Add points to the extremities
     ggtitle("Product Monthly Sales Evolution") +
     xlab("year-month") + ylab("sales") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(labels = comma) 



#######################################################################
###	    III. Histograms and density curves with multiple groups    ###
#######################################################################


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
## Display the combined fuel consumption for 100 Km (`combined_l100km`)
## as supeimposed (interleaved) density curvers
## for all vehicle classes (`Veh Class`) that have at least 50 models
fuel_economy_2018 %>%
     filter (!is.na(combined_l100km)) %>%
     group_by(`Veh Class`) %>%
     summarise(n = n()) %>%
     filter (n >= 50) %>%
     inner_join(fuel_economy_2018) %>%
ggplot(., aes(x = combined_l100km, fill = `Veh Class`, col = `Veh Class`)) + 
	geom_density(alpha=.3, position="identity") +
     ggtitle("Combined Fuel Consumption, by Vehicle Class") 
     

## Task:
## Display the histograms of combined fuel consumption for 
## 100 Km (`combined_l100km`) for all vehicle classes (`Veh Class`) 
## that have at least 50 models;
## this time, every histogram will be displayes in a separate panel
fuel_economy_2018 %>%
     filter (!is.na(combined_l100km)) %>%
     group_by(`Veh Class`) %>%
     summarise(n = n()) %>%
     filter (n >= 50) %>%
     inner_join(fuel_economy_2018) %>%
ggplot(., aes(x = combined_l100km, fill = `Veh Class`, col = `Veh Class`)) + 
	geom_histogram(alpha=.3, position="identity") +
     ggtitle("Combined Fuel Consumption, by Vehicle Class") +
     facet_wrap (. ~ `Veh Class`, ncol = 1) + # display on a single column
                                              # for easying the comparison
     theme(legend.position="none") 
     

############################################################################
###            Dragos Cogean's Data Set (compare two cloud 
###                 database services, MongoDB and MySQL)

# import the data on `read` operations for MongoDB and 
ReadMongoDB <- read_tsv("DragosCogean__ReadMongo_ALL.txt") %>%
     mutate (dbserver = 'MongoDB', latency_secs = latenta / 1000)
ReadMySQL <- read_tsv("DragosCogean__ReadMySQL_ALL.txt") %>%
     mutate (dbserver = 'MySQL', latency_secs = latenta / 1000)

AllRead <- bind_rows(ReadMongoDB, ReadMySQL)
rm(ReadMongoDB, ReadMySQL)

glimpse(AllRead)


## Task (identical a task in script 08c, section II.2): 
## Display two SUPERIMPOSED density curves for: 
## - the latency or read operations of type `100SEC`
## - the latency or read operations of type `LONG`
## only for MongoDB

# New solution 
ggplot (AllRead, aes(x = latency_secs, fill = testtype )) + 
     geom_density( alpha = .3) +
	ggtitle("Superimposed Density Curves for the Latency of \nREAD operations \nof type `100SEC` and `LONG` in MongoDB") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="top") +
     guides(fill = guide_legend(title = "Read Test Type")) # change the legend title

# ... another way of changing the legend title
ggplot (AllRead, aes(x = latency_secs, fill = testtype )) + 
     geom_density( alpha = .3) +
	ggtitle("Superimposed Density Curves for the Latency of \nREAD operations \nof type `100SEC` and `LONG` in MongoDB") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="top") +
     scale_fill_discrete(name="Read Test Type") # another way to change the legend title


## Task : 
## Display in two separate panels, one for read operations of type `100SEC`
## and the other for read operations of type `LONG`, 
## the recorded latencies for the two database servers (as interleaved
## density plots)

ggplot (AllRead, aes(x = latency_secs, fill = dbserver )) + 
     geom_density( alpha = .3) +
	ggtitle("Superimposed Density Curves for the Latency of \nREAD operations \nof type `100SEC` and `LONG` in MongoDB") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="top") +
     scale_fill_discrete(name="database\nserver") + # change the legend title 
     facet_wrap( . ~ testtype)


## Task : 
## Display in four separate panels, the density plots of latency 
## for read operations by both servers and test types
ggplot (AllRead, aes(x = latency_secs, fill = dbserver )) + 
     geom_density( alpha = .3) +
	ggtitle("Latency of READ Pperations \nby Server and Test Type") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     scale_fill_discrete(name="database\nserver") + # change the legend title 
     facet_wrap( dbserver ~ testtype)


# with `facet_grid`, the chart is a bit more elegant
ggplot (AllRead, aes(x = latency_secs, fill = dbserver )) + 
     geom_density( alpha = .3) +
	ggtitle("Latency of READ Pperations \nby Server and Test Type") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(legend.position="none") +
     scale_fill_discrete(name="database\nserver") + # change the legend title 
     facet_grid( dbserver ~ testtype)



#######################################################################
###	               IV. Boxplot with multiple groups                ###
#######################################################################


############################################################################
###            Dragos Cogean's Data Set (compare two cloud 
###                 database services, MongoDB and MySQL)

## Task:
## Compare with boxplots the distribution of latency for read operations
## of type `100SEC` - Mongo vs. MySQL
ggplot(subset(AllRead, testtype=="100SEC"), 
	aes(x = dbserver, y = latency_secs)) +
	geom_boxplot() +
	ggtitle("Latency of `READ 100SEC` Operations \nMongo vs. MySQL") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
	xlab("DB Server") + ylab("Latency (seconds)")
	

## Task:
# Compare using boxplots the distribution of latency for both types
#  of insert operations - Mongo vs. MySQL
#  on each boxplot, add a `+` signalling the mean
ggplot(subset(AllRead), 
	aes(x = dbserver, y = latency_secs)) +
	geom_boxplot() +
	ggtitle("Latency of Read Operations \n by Server and Test Type") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
	xlab("DB Server")+ylab("Latency (seconds)") +
	facet_grid(testtype ~ .) +
	stat_summary(fun.y=mean, geom="point", shape=3, size=1, color = "red")
     


#######################################################################
###	          V. Scatter plots with multiple groups                ###
#######################################################################

#######################################################################
###                      Fuel Economy dataset(s) 
glimpse(fuel_economy_2018)

## Task: 
## Examine through a scatterplot the relationship between the 
## combined (city and highway) fuel consumption (on the y-axis) and the 
## engine displacement (on x-axis), by the number of cylinders

# Variables mapping:
# - x-axis: `Disp`
# - y-axis: `combined_l100km`
# - colour: Cyl


# Solution 1 is based on solution in script 08c, adding
# `color`  parameter in global aestetics
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ), Cyl = factor(Cyl)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km, color = Cyl)) +
     geom_point() +              
     xlab("engine displacement (thousands of cubic centimetres)") + 
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 25, 1))  +  #
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))    #
     

# Solution 2 use `position_jitter` 
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ), Cyl = factor(Cyl)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km, color = Cyl)) +
     geom_point (position = position_jitter()) +              
     xlab("engine displacement (thousands of cubic centimetres)") + 
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 25, 1))  +  
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))   


# Solution 3 use `geom_jitter` 
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ), Cyl = factor(Cyl)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km, color = Cyl)) +
     geom_jitter () +              
     xlab("engine displacement (thousands of cubic centimetres)") + 
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 25, 1))  +  
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))   


# Solution 4 adds marginal rugs on axes
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ), Cyl = factor(Cyl)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km, color = Cyl)) +
     geom_jitter () + 
     geom_rug() +
     xlab("engine displacement (thousands of cubic centimetres)") + 
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 25, 1))  +  
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))   


# Solution 5 adds a regression line for each
# group associated to a number of cylinders
# `color`  parameter in global aestetics
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ), Cyl = factor(Cyl)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km, color = Cyl)) +
     geom_jitter () + 
     geom_rug() +
     xlab("engine displacement (thousands of cubic centimetres)") + 
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 25, 1))  +  
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))  + 
     geom_smooth(method = lm)    # Add a (linear) regression line, 
                      # including a confidence region for the curve


## Task:
## Examine through a scatterplot the relationship between the 
## combined (city and highway) fuel consumption (on the y-axis) and the 
## engine displacement (on x-axis), by the number of cylinders, 
## includin only the groups (built on number of cylinders) with 
## at least 50 observations

# Apply a filter to the last solution's data set 
fuel_economy_2018 %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     group_by(Cyl) %>%
     tally() %>%
     filter (n >= 50) %>%
     inner_join(fuel_economy_2018) %>%
     filter (!is.na(cty_l100km) & !is.na(hwy_l100km) &
                  !is.na(Displ) & Displ != 'N/A') %>%
     mutate (Displ = as.numeric(Displ), Cyl = factor(Cyl)) %>%
ggplot(. , aes(x = Displ, y = combined_l100km, color = Cyl)) +
     geom_jitter () + 
     geom_rug() +
     xlab("engine displacement (thousands of cubic centimetres)") + 
     ylab("liters per 100 Km") +
     ggtitle("Combined Fuel Consumption vs. Engine Displacement") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 25, 1))  +  
     scale_x_continuous(breaks = seq(0.5, 8, 0.5))  + 
     geom_smooth(method = lm)    # Add a (linear) regression line, 
                      # including a confidence region for the curve




