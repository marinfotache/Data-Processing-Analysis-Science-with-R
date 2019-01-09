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
###                            09b Data Exploration                      ###
### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 30.10.2018

library(tidyverse) 
library(readxl)
library(scales) # need for thousands separation
library(lubridate)
library(ggrepel)

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


# http://r-statistics.co/ggplot2-Tutorial-With-R.html
# http://sharpsightlabs.com/blog/r-package-think-about-visualization/

# Myfanwy Johnston on graphing with ggplot
# https://www.youtube.com/watch?v=SaJCKpYX5Lo

#https://www.youtube.com/watch?v=rsG-GgR0aEY

# http://www.sthda.com/english/wiki/be-awesome-in-ggplot2-a-practical-guide-to-be-highly-effective-r-software-and-data-visualization#at_pco=smlre-1.0&at_si=56e91c075a4d017c&at_ab=per-2&at_pos=2&at_tot=4

# https://github.com/slowkow/ggrepel/blob/master/vignettes/ggrepel.md


#######################################################################
###	                              Agenda                           ###	
#######################################################################

###   I. Descriptive statistics & graphics
###   Visualizing mean, median on different types of chart


###   II. Estimation and graphical represention of association
###   (numeric vs numeric/other, ordinal vs. ordinal/other, nominal vs... )

###   III. Visualizing groups of variables

###   IV. Missing, wrong and extreme values


## III
## Task:
## Given a dataset, display:
## - barplots for all char/factor variables (see programming )
## - histogram for discrete (or low number of values) variables
## - density plots for continuos variables

#######################################################################




###   I. Descriptive statistics & graphics
###   Visualizing mean, median on different types of chart


## Task:
## Display as boxplots invoice amount distribution for each
## customer;
## New; Mark within the box the position of the mean
## (average) invoice amount

invoice_detailed %>%
     group_by(invoiceno, customername) %>%
     summarise (inv_amount = sum(amount)) %>%  # compute the invoice amount
     group_by(customername) %>%  # group by each customer
ggplot(., 
       aes(x = customername, y = inv_amount)) +
	geom_boxplot() +
	ggtitle("Distribution of Invoice Amount, by Customer",
	        subtitle = "(`the red `+` sign indicates the mean" ) +
     theme(plot.title = element_text(hjust = 0.5), 
           plot.subtitle = element_text(hjust = 0.5)) +  # center the title
	xlab("customer")+ylab("invoice amount") +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 800000, 50000), labels = comma)  +  #
	stat_summary(fun.y=mean, geom="point", shape=3, size=3, color = "red")


# not only the position, but also the value of the mean will be displayed
invoice_detailed %>%
     group_by(invoiceno, customername) %>%
     summarise (inv_amount = sum(amount)) %>%  # compute the invoice amount
     group_by(customername) %>%  # group by each customer
ggplot(., 
       aes(x = customername, y = inv_amount)) +
	geom_boxplot() +
     geom_text_repel(data = invoice_detailed %>%
               group_by(invoiceno, customername) %>%
               summarise (inv_amount = sum(amount)) %>%  
               group_by(customername) %>%
               summarise(mean_invoice = round(mean(inv_amount),2)),  
          aes( label = mean_invoice, y = mean_invoice), col = "red", size = 3) +
	ggtitle("Distribution of Invoice Amount, by Customer",
	        subtitle = "(`the red `+` sign indicates the mean" ) +
     theme(plot.title = element_text(hjust = 0.5), 
           plot.subtitle = element_text(hjust = 0.5)) +  # center the title
	xlab("customer")+ylab("invoice amount") +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     scale_y_continuous(breaks = seq(0, 800000, 50000), labels = comma)  +  #
	stat_summary(fun.y=mean, geom="point", shape=3, size=2, color = "red")



#######################################################################
###	              III. Visualizing groups of variables               ###
#######################################################################

## Task:
## Given a dataset, display:
## - barplots for all char/factor variables (see programming )
## - histogram for discrete (or low number of values) variables
## - density plots for continuos variables


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
          TRUE ~ manufacturer) %>%
     mutate (displacement  = as.numeric(Displ), 
             n_of_cyl = as.integer(Cyl),
             air_pollution = as.integer(`Air Pollution Score`),
             greenhouse = as.numeric(`Greenhouse Gas Score`),
             combined_CO2 = as.numeric(`Comb CO2`)
             )          
     ) 

glimpse(fuel_economy_2018)

# change the name
names(fuel_economy_2018) <- str_replace_all(
     str_replace_all(names(fuel_economy_2018), ' ', '_'), 
          '`', '')
glimpse(fuel_economy_2018)



## Task:
## Given a dataset, display:
## - barplots for all char/factor variables (see programming )
## - histogram for discrete (or low number of values) variables
## - density plots for continuos variables


## first, the barplot
## select only the nominal variables (strings/factors) - 
## non-numeric with distinct values fewer than 20 
## ( for graphics lisibility)
fuel_economy_2018 %>%
     select_if(negate(is.numeric)) %>%  # select non-numeric columns
     select_if(~ n_distinct(.) < 20 ) %>%  # select columns with not-so-large
                                        # distinct values
     gather (Variable, Value) %>% # convert 
ggplot (., aes (x = Value)) +     
     geom_bar() +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     facet_wrap( . ~ Variable, scale = 'free')     


## second, the histogram
## select only the numeric variables of type integers 
## ( for graphic lisibility)
fuel_economy_2018 %>%
     select_if(is.numeric) %>%  # select non-numeric columns
     select_if(~ n_distinct(.) < 50 ) %>%  # select columns with not-so-large
                                        # distinct values
     gather (Variable, Value) %>% # convert 
ggplot (., aes (x = Value)) +     
     geom_histogram() +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     facet_wrap( . ~ Variable, scale = 'free')     


## third, the density plot
## select only the numeric variables of type integers 
## ( for graphic lisibility)
fuel_economy_2018 %>%
     select_if(is.numeric) %>%  # select non-numeric columns
     select_if(~ n_distinct(.) >= 50 ) %>%  # select columns with not-so-large
                                        # distinct values
     gather (Variable, Value) %>% # convert 
ggplot (., aes (x = Value)) +     
     geom_density() +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, hjust = 1 )) + 
     facet_wrap( . ~ Variable, scale = 'free')     




?
>>> older

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




#####################################################################################
## 				other cases

## evolution of enrolled students in the first year of study 
##    for three master programmes
#  with legend and the name of the program as text labels
load("students/mpi.rda")
mpi
# text labels as number of enrollments
ggplot (data=subset(mpi, yearofstudy==1), aes(x=academicyear, y=nofstuds)) +
	geom_line(aes(colour = factor(program), group=program)) +
	scale_color_discrete(name="Programme") +
	labs(title="Students Enrolled in the First Year for \nThree Master Programmes",
		x="Academic Year", y="Number of enrolled students") +
	geom_text(aes(x=academicyear, y=nofstuds, label=nofstuds) )
# text labels as the name of the programme	
ggplot (data=subset(mpi, yearofstudy==1), aes(x=academicyear, y=nofstuds)) +
	geom_line(aes(colour = factor(program), group=program)) +
	scale_color_discrete(name="Programme") +
	labs(title="Students Enrolled in the First Year for \nThree Master Programmes",
		x="Academic Year", y="Number of enrolled students") +
	geom_text(aes(x=academicyear, y=nofstuds, label=program) )




# Montly sales for each year on different panels arranged vertically
ggplot (data=monthly_sales, aes(x=month, y=sales)) +
	geom_line(aes(colour = factor(year), group=factor(year))) +
	scale_color_discrete(name="Year") +
	labs(title="Montly sales for \neach year",
		x="Month", y="Sales") +
	scale_y_continuous(labels=comma) +
	facet_wrap(~ year)

# Montly sales for each year on different panels arranged horizontally
ggplot (data=monthly_sales, aes(x=month, y=sales)) +
	geom_line(aes(colour = factor(year), group=factor(year))) +
	scale_color_discrete(name="Year") +
	labs(title="Montly sales for \neach year",
		x="Month", y="Sales") +
	scale_y_continuous(labels=comma) +
	facet_grid( year ~ .)





# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization


#######################################################################
###	    III. Histograms and density curves with multiple groups    ###
#######################################################################


#  Histogram and density plots with multiple groups
head(FuelEfficiency2)

ggplot(FuelEfficiency2, aes(x = cty_l100km, 
		fill = factor(cyl))) + 
	geom_histogram(binwidth=.5, alpha=.5, position="identity")

# Interleaved histograms
ggplot(FuelEfficiency2, aes(x=cty_l100km, 
		fill = factor(cyl))) + 
	geom_histogram(binwidth=1, position="dodge")

# Density plots
ggplot(FuelEfficiency2, aes(x=cty_l100km, colour=factor(cyl))) + 
	geom_density()

# Density plots with semi-transparent fill
ggplot(FuelEfficiency2, aes(x=cty_l100km, colour=factor(cyl))) + 
	geom_density(alpha=.3)


# density kernel of latency for insert operation (100SEC) in Mongo DB
load("DragosCogean/Dragos.Cogean.2.RData")
head(AllInserts)

ggplot(subset(AllInserts, testtype="100SEC" & dbserver=="Mongo"), 
	aes(x = latenta)) + 
	geom_density() +
	ggtitle("Density kernel of latency for insert operation (100SEC) in Mongo DB") 

# superimposed density kernels of latency for insert operation (100SEC) 
#   in Mongo vs. MySQL
ggplot(subset(AllInserts, testtype="100SEC"), aes(x=latenta)) + 
    	geom_density( data = subset(AllInserts, testtype="100SEC" & dbserver=="Mongo"),
		fill = "red", alpha = 0.2) +
    	geom_density( data = subset(AllInserts, testtype="100SEC" & dbserver=="MySQL"),
		fill = "green", alpha = 0.2) +
	ggtitle("Superimposed density kernels of latency for insert operation (100SEC) \n
in Mongo vs. MySQL") 
# or (also changing legend position)
ggplot(subset(AllInserts, testtype="100SEC"), 
	aes(x=latenta, color = dbserver)) + 
	geom_density(  alpha = 0.2) +
	theme(legend.position = "bottom", 
		legend.direction = "horizontal")


# separate panels (vertically) density kernels of latency for 
# insert operation (100SEC) in Mongo vs. MySQL
ggplot(subset(AllInserts, testtype="100SEC"), aes(x=latenta)) + 
    	geom_density() +
	ggtitle("Density kernels of latency for insert operation (100SEC) \n
in Mongo vs. MySQL") +
	facet_wrap(~ dbserver)

# separate panels (horizontally) density kernels of latency for 
# insert operation (100SEC) in Mongo vs. MySQL
ggplot(subset(AllInserts, testtype="100SEC"), aes(x=latenta)) + 
    	geom_density() +
	ggtitle("Density kernels of latency for insert operation (100SEC) \n
in Mongo vs. MySQL") +
	facet_grid( dbserver ~ .)

# display latency for all groups (dbserver~testtype) - 1
ggplot(AllInserts, aes(x = latenta)) + geom_density() + 
	facet_wrap(dbserver~testtype)

# display latency for all groups (dbserver~testtype) - 1
ggplot(AllInserts, aes(x = latenta)) + geom_density() + 
	facet_grid(dbserver~testtype)



# or (also changing legend position)
ggplot(subset(AllInserts, testtype="100SEC"), 
	aes(x=latenta, color = dbserver)) + 
	geom_density(  alpha = 0.2) +
	theme(legend.position = "bottom", 
		legend.direction = "horizontal")


# separate panels (vertically) density kernels of latency for 
# insert operation (100SEC) in Mongo vs. MySQL
ggplot(subset(AllInserts, testtype="100SEC"), aes(x=latenta)) + 
    	geom_density() +
	ggtitle("Density kernels of latency for insert operation (100SEC) \n
in Mongo vs. MySQL") +
	facet_wrap(~ dbserver)

# separate panels (horizontally) density kernels of latency for 
# insert operation (100SEC) in Mongo vs. MySQL
ggplot(subset(AllInserts, testtype="100SEC"), aes(x=latenta)) + 
    	geom_density() +
	ggtitle("Density kernels of latency for insert operation (100SEC) \n
in Mongo vs. MySQL") +
	facet_grid( dbserver ~ .)

# display latency for all groups (dbserver~testtype) - 1
ggplot(AllInserts, aes(x = latenta)) + geom_density() + 
	facet_wrap(dbserver~testtype)

# display latency for all groups (dbserver~testtype) - 1
ggplot(AllInserts, aes(x = latenta)) + geom_density() + 
	facet_grid(dbserver~testtype)




#####################################################################################
###						Boxplots	
#####################################################################################

# The same kind of boxplot displayed with...
boxplot(FuelEfficiency2$cty_l100km, 
	main="A Boxplot (for variable liters_for_100km in cities)",
	ylab="Number of necessary liters", las=1)
# ... can be displayed with:
ggplot(FuelEfficiency2, aes(x = 0, y = cty_l100km)) +
	geom_boxplot() +
	ggtitle("A Boxplot (for variable liters_for_100km in cities)") +
	ylab("Number of necessary liters")


# compare with boxplots the distribution of latency for insert
#  (100SEC) operations - Mongo vs. MySQL
ggplot(subset(AllInserts, testtype=="100SEC"), 
	aes(x = dbserver, y = latenta/1000)) +
	geom_boxplot() +
	ggtitle("Compared latency (seconds) of insert operations (100SEC) \n for Mongo and MySQL") +
	xlab("DB Server") + ylab("Latency (seconds)")
	

# compare using boxplots the distribution of latency for both types
#  of insert operations - Mongo vs. MySQL
ggplot(subset(AllInserts), 
	aes(x = dbserver, y = latenta/1000)) +
	geom_boxplot() +
	ggtitle("Compared latency (seconds) of insert operations (100SEC) \n for Mongo and MySQL") +
	xlab("DB Server")+ylab("Latency (seconds)") +
		facet_grid(testtype ~ .)


head(FuelEfficiency2)
ggplot(FuelEfficiency2, aes(x = factor(cyl), y = cty_l100km)) +
	geom_boxplot() +
	ggtitle("Boxplots - number of liters for 100km in cities \n grouped by number of cylinders") +
	ylab("Number of necessary liters")


# Add a diamond at the mean, and make it larger
ggplot(FuelEfficiency2, aes(x = factor(cyl), y = cty_l100km)) +
	geom_boxplot() +
	ggtitle("Boxplots - number of liters for 100km in cities \n grouped by number of cylinders") +
	ylab("Number of necessary liters") +
	stat_summary(fun.y=mean, geom="point", shape=5, size=4)


#######################################################################
###	 V. Scatter plots with multiple groups                         ###
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




