#######################################################################
###                    Al.I. Cuza University of Iași                ###
###       Faculty of Economics and Business Administration          ###
###   Department of Accounting, Information Systems and Statistics  ###
#######################################################################
###
#######################################################################
###        Data Processing/Analysis/Science with R                  ###
#######################################################################
###
#######################################################################
###         8b. Categorical Data Visualization with ggplot2        ####
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/08%20Data%20Visualization%20with%20-mostly-%20ggplot2/08_ggplot2.pptx
#######################################################################
## last update: 02.04.2020

#install.packages('vcd')
library (vcd)
library(tidyverse) 
library(readxl)
library(lubridate)

#######################################################################
###           Download the necesary data sets for this script       ###
#######################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

# check if the current directory is ok
getwd()
#######################################################################
# giving up scientific notation (1.6e+07)
#options("scipen"=30, "digits"=14)

#######################################################################
###                                Agenda                           ###
#######################################################################
###	  I. Basic barcharts                                           ###	
###       I.1 Barcharts of values (frequencies are pre-computed)    ###
###   	I.2 Barcharts of counts                                   ###
###   	I.3  Barcharts with two categorical variables             ###
###           (side-by-side bars vs. faceting) 	                  ###
###       I.4  Barcharts with more categorical variables            ###
###       I.5  Barcharts with numeric x-axis                        ###
###	 II. More on barcharts                                         ###	
###		II.1 Bars and Text Justification                          ###
###		II.2 Bars and Conditional Text Justification              ###	
###		II.3 Changing the bar order                               ###	
###	III. (A Sort of) Dotcharts                                     ###
###	 IV. Piecharts                                                 ###	
###    V. Association between categorical variables                 ###	
###		V.1. Mosaic plots                                         ###	
###		V.2. Heat maps                                            ###	
#######################################################################
###		

#######################################################################
###		               I. Basic barcharts                         ###	
#######################################################################

#  Basic graphs with discrete x-axis

# With bar graphs, there are two different things that the heights of bars 
#  commonly represent:
#  * The count of cases for each group -- typically, each x value represents 
#    one group. This is done with "stat_bin", which calculates the number 
#    of cases in each group (if x is discrete, then each x value is a group; 
#    if x is continuous, then all the data is automatically in one group, 
#      unless you specifiy grouping with group=xx).
#  * The value of a column in the data set. This is done with "stat_identity", 
#    which leaves the y values unchanged.

# In ggplot2, the default is to use "stat_bin", so that the bar height 
#   represents the count of cases.


#######################################################################
###   		   I.1 Simple barcharts of values                    ###
#######################################################################

#  In these examples, the height of the bar will represent the value 
#   in a column of the data frame. This is done by using 
#  stat="identity" instead of the default, stat="bin".


#######################################################################
###       (Anonymized) FEAA students for 2014-2015 academic year
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
## display the data frame structure
glimpse(studs)

## Task:
## Display the undergraduate student structure, by programmes

# ... first, we aggregate the data (removing the `Commomon courses`
# which is not a programme)
data <- studs %>%
     filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")) %>%
     dplyr::group_by(PROGRAMME) %>%
     summarise(n_of_studs = n())
data

# Variable mappings:
# `PROGRAMME`: x-axis (and color fill in some further examples)
# `n_of_studs`: y-axis

## Very basic bar graph
ggplot(data = data , aes(x = PROGRAMME, y = n_of_studs)) + 
	geom_bar(stat="identity")

##... Map the programme to different fill colors. 

# Both next two ggplots have the same result...
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
	geom_bar(stat="identity")
# ...and
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs)) + 
	geom_bar( aes(fill = PROGRAMME), stat="identity")

# Add a black outline for each bar
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
	geom_bar(colour="black", stat="identity")

# Remove the legend, since in this case it is redundant
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE)

# Add title, narrow the bars, use a gray fill, and change axis labels
# notice that `fill` in `geom_bar` overwrites `fill` in the main aestetics
# clause
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
    geom_bar(colour="black", fill="#DD8888", 
             width=.7, stat="identity") + 
    guides(fill=FALSE) +
    xlab("Undergraduate Programme") + ylab("Number of students") +
    ggtitle("Undergraduate Students Structure")


## Problem: the x axis labels are not visible; we'll change the angle on x-axis
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
    geom_bar(stat="identity") +
    guides(fill=FALSE) +
    xlab("Undergraduate Programme") + ylab("Number of students") +
    ggtitle("Undergraduate Students Structure") +
    theme(axis.text.x = element_text(angle = 90, # (not so good for the neck)
               vjust = .5, # vertical justification (position near to bar center)
               hjust = 1 )) # horizontal justification (align towards the bar)


# try with another angle and center the title
ggplot(data = data, aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
    geom_bar(stat="identity") +
    guides(fill=FALSE) +
    xlab("Undergraduate Programme") + ylab("Number of students") +
    ggtitle("Structure of the Undergraduate Students") +
    theme(axis.text.x = element_text(angle = 60, # (better for the neck)
               vjust = 1, # it was changes vertical justification (position near to bar center)
               hjust = 1 )) + # that remains unchanged
    theme(plot.title = element_text(hjust = 0.5))  # center the title

# rotate the entire graph with 90 degrees
ggplot(data = data, 
     aes(x = PROGRAMME, y = n_of_studs, 
                        fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, 
               hjust = 1 )) 


#######################################################################
###                                Sales
load (file = 'sales.RData')
glimpse(invoice_detailed)

## Task:
## Display how often each product appears in invoices/sales
invoice_detailed %>%
     group_by(productname) %>%
     summarise (freq = n()) %>%
ggplot(., aes (x = productname, y = freq, fill = productname)) +
     geom_bar(stat="identity") +
     guides(fill=FALSE) + # no legend
     xlab("product name") + ylab("Number of occurences (frequency)") +
     ggtitle("Product Frequency (Overall)") +
     theme(plot.title = element_text(hjust = 0.5))  # center the title
     

## Task:
## Display how often each product appears in invoices/sales
# New: change the y-axis labels (make the numbers appear like
# that: 0, 5, 10, 15, ..., 80)
invoice_detailed %>%
     group_by(productname) %>%
     summarise (freq = n()) %>%
ggplot(., aes (x = productname, y = freq, fill = productname)) +
     geom_bar(stat="identity") +
     guides(fill=FALSE) + # no legend
     xlab("product name") + ylab("Number of occurences (frequency)") +
     ggtitle("Product Frequency (Overall)") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     scale_y_continuous( breaks = seq(0, 80, 5)) +
     theme_bw()
     


#######################################################################
###   			I.2 Simple barcharts of counts                  ###
#######################################################################

#  In these examples, the height of the bar will represent 
#   the count of cases. This is done by using 
#   stat="count" (which is the default).


#######################################################################
###	                         `Arthritis` dataset 
#install.packages('vcd')
library (vcd)
#
## `Arthritis` dataset is included in the `vcd` package 
## (taken from the book `R in action` (by R. Kabacoff))
## 
## Data (Kock & Edward, 1988) - represent a double-blind clinical trial 
##  of new treatments for rheumatoid arthritis
glimpse(Arthritis)
## each observation describes one person

## There are two explanatory factors: "Treatment" and "Sex". 
## "Age" is a covariate, and "Improved" is the response (an 
##   ordered factor, with levels None < Some < Marked). 
## Excluding "Age", we would have a 2 x 2 x 3 contingency table for 
##   "Treatment", "Sex" and "Improved".

## "Treatment" (whose possible values are Placebo and Treated), 
##   "Sex" (Male, Female), and "Improved" (None, Some, Marked) 
##     are all categorical factors.

# order the factor `Improved`
# 
Arthritis <- Arthritis %>%
     mutate (Improved  = factor(Improved, 
	          levels=c("None", "Some", "Marked")))
glimpse(Arthritis)


## Task: 
## Display the values frequency for attribute `Improved` 
table(Arthritis$Improved)

ggplot(Arthritis, 
     aes(x = factor(Improved), fill = "yellow2")) + 
	geom_bar(width = 0.7, color="white") +
	ggtitle("Result of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Result") +
	guides(fill=FALSE)  # no legend

## Task: 
## Visualize basic information (frequencies) about attribute `Sex`
table(Arthritis$Sex)

ggplot(Arthritis, aes(x = factor(Sex), fill = "yellow2")) + 
	geom_bar(width = 1, color="white") +
	ggtitle("Sex of the Patients included in the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Treatment Groups") +
	guides(fill=FALSE)  # no legend


#######################################################################
###        (Anonymized) FEAA students for 2014-2015 academic year
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
glimpse(studs)

# we'll display an identical chart (with that in section I.1), but
#    there's no need to pre-aggregate the data
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = PROGRAMME)) +
     geom_bar(stat="count") +
     coord_flip()  +   # horizontal bars
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, 
               hjust = 1 )) 


#######################################################################
###                                Sales
glimpse(invoice_detailed)

## Task: 
## Get an identical chart wth that in section I.1 
## (display how often each product appears in 
## invoices/sales) 

# Another solution:
ggplot(invoice_detailed, 
     aes (x = productname, fill = productname)) +
     geom_bar() +
     guides(fill=FALSE) + # no legend
     xlab("product name") + ylab("Number of occurences (frequency)") +
     ggtitle("Product Frequency (Overall)") +
     theme(plot.title = element_text(hjust = 0.5)) + # center the title
     scale_y_continuous( breaks = seq(0, 80, 5)) # display denser grid lines
                                                 #  on y axis


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
## Display the number of models for each manufacturer
ggplot(fuel_economy_2018, 
     aes (x = manufacturer, fill = manufacturer)) +
     geom_bar() +
     coord_flip() + # there are many manufactures, so we'll use horizontal bars
     guides(fill=FALSE) + # no legend
     xlab("manufacturer") + ylab("number of car models") +
     ggtitle("Number of Car Models, by Manufacturer") +
     theme(plot.title = element_text(hjust = 0.5)) + # center the title
     scale_y_continuous( breaks = seq(0, 400, 25)) # display denser grid lines
                                                 #  on y axis


#######################################################################
###   	   I.3  Barcharts with two categorical variables          ###
###                 (side-by-side bars vs. faceting) 	             ###
#######################################################################

#######################################################################
###        (Anonymized) FEAA students for 2014-2015 academic year
glimpse(studs)

## Task:
## Display the same undergraduate student stucture (as horizontal bars); 
## for each programme, visualize the proportion of `state support` vs.
## `tuition fee` students (variable `FINANCIAL_SUPPORT`)
# 
#    Variable mappings:
# `PROGRAMME`: x-axis
# `FINANCIAL_SUPPORT`: color fill
# count: y-axis.

ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar() +
     coord_flip()  +   # horizontal bars
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, 
               vjust = 1, 
               hjust = 1 ))  
# the legend fills too much space!...

## move the legend at the bottom of the chart
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar() +
     coord_flip()  +   # horizontal bars
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     theme(legend.position="bottom")

# here, the legend could appear truncatedly (as the graph title)
# we can simply resize the figure using the mouse

## use side-by-side bars (instead of stacked bars)
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar(position=position_dodge()) + # side-by-side bars 
     coord_flip()  +   # horizontal bars
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     theme(legend.position="bottom")


## polish the chart:
# change the legend title, choose a simplified theme
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar(position = position_dodge()) + # side-by-side bars 
     coord_flip()  +   # horizontal bars
     theme_bw() +   # a new theme (must be inserted before the other `themes`)
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Undergraduate Programme Students and Financial Support") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     theme(legend.position="bottom") +
     scale_fill_hue(name="Financial support")       # Set legend title

     
## instead of side-by-side barchart, one can use a `faceted` graph
## no need for legend
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar(position = position_dodge()) + # side-by-side bars 
     coord_flip()  +   # horizontal bars
     theme_bw() +   # a new theme (must be inserted before the other `themes`)
     xlab("Undergraduate Programme") + ylab("Number of students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     ggtitle("Financial Support of Undergraduate \nStudents, by Programme") +
          # notice the title wraping
     facet_wrap( ~ FINANCIAL_SUPPORT) +
     theme(legend.position="none") # remove the legend
     

#######################################################################
###                                Sales
glimpse(invoice_detailed)
##
## Task:
## Display the frequency of each product on its customers

# between `side-by-side` bars ...
ggplot(invoice_detailed, aes (x = productname, fill = customername)) +
     geom_bar(position=position_dodge()) + # side-by-side bars 
     xlab("product name") + ylab("Number of occurences (frequency)") +
     ggtitle("Customer Frequency for Each Product") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     scale_y_continuous( breaks = seq(0, 80, 5))

# ... and faceting...
ggplot(invoice_detailed, aes (x = customername, fill = customername)) +
     geom_bar(position=position_dodge()) + # side-by-side bars 
     xlab("product name") + ylab("Number of occurences (frequency)") +
     ggtitle("Customer Frequency for Each Product") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     scale_y_continuous( breaks = seq(0, 80, 5)) +
     facet_wrap(~ productname) +
     theme(legend.position="none") # remove the legend

# ... I would choose the latter.


## Task:
## Display the frequency of products bought by each customers
## (we'll just switch customers with products)

# solution with `side-by-side` bars
ggplot(invoice_detailed, aes (x = customername, fill = productname)) +
     geom_bar(position=position_dodge()) + # side-by-side bars 
     xlab("customer") + ylab("Number of occurences (frequency)") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     ggtitle("Product Frequency (for Each Customer)") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     scale_y_continuous( breaks = seq(0, 80, 5))

# solution with faceting
ggplot(invoice_detailed, aes (x = productname, fill = productname)) +
     geom_bar(position=position_dodge()) + # side-by-side bars 
     xlab("product") + ylab("Number of occurences (frequency)") +
     ggtitle("Product Frequency (for Each Customer)") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     scale_y_continuous( breaks = seq(0, 80, 5)) +
     facet_wrap(~ customername) +
     theme(legend.position="none") # remove the legend


#######################################################################
###                      Fuel Economy dataset(s) 
glimpse((fuel_economy_2018))  

## Task:
## Show contribution of each manufactures in terms of number of cars
##   in every class

# solution with `side-by-side` bars
ggplot(fuel_economy_2018, aes (x = manufacturer, fill = `Veh Class`)) +
     geom_bar(position = position_dodge()) + # side-by-side bars 
     coord_flip()  +   # horizontal bars
     xlab("manufacturer") + ylab("number of car models") +
     ggtitle("Car Category Frequency for Each Manufacturer") +
     theme(plot.title = element_text(hjust = 0.5))   # center the title

# solution with faceting
ggplot(fuel_economy_2018, aes (x = `Veh Class`, fill = `Veh Class`)) +
     geom_bar(position=position_dodge()) + # side-by-side bars 
     xlab("manufacturer") + ylab("number of car models") +
     ggtitle("Car Category Frequency for Each Manufacturer") +
     theme(plot.title = element_text(hjust = 0.5)) +  # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     facet_wrap(~ manufacturer) +
     theme(legend.position="none") # remove the legend



#######################################################################
###   I.4  Barcharts with three (or more) categorical variables     ###
#######################################################################

#######################################################################
###        (Anonymized) FEAA students for 2014-2015 academic year
glimpse(studs)

## Task: 
## Undergraduate students must be compared on each programme
## by two variables
##   (1) the financial support
##   (2) attendance (Regular or Distance Learning (DL))
## 
## the graph will be `faceted` by two variables (notice `labeler`)
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar(position = position_dodge()) + # side-by-side bars 
     coord_flip()  +   # horizontal bars
     theme_bw() +   # a new theme (must be inserted before the other `themes`)
     xlab("Undergraduate Programme") + ylab("Number of students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     ggtitle("Financial Support of Undergraduate \nStudents, by Programme and Attendance") +
          # notice the title wraping
     facet_wrap( FINANCIAL_SUPPORT ~ ATTENDANCE, labeller = label_both) +
     theme(legend.position="none") # remove the legend


## in this case, with `facet_grid` the chart is slightly more lisible 
## than `facet_wrap`
ggplot(data = studs %>%
            filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")), 
     aes(x = PROGRAMME, fill = FINANCIAL_SUPPORT)) +
     geom_bar(position = position_dodge()) + # side-by-side bars 
     coord_flip()  +   # horizontal bars
     theme_bw() +   # a new theme (must be inserted before the other `themes`)
     xlab("Undergraduate Programme") + ylab("Number of students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) + 
     ggtitle("Financial Support of Undergraduate \nStudents, by Programme and Attendance") +
          # notice the title wraping
     facet_grid( FINANCIAL_SUPPORT ~ ATTENDANCE, labeller = label_both) +
     theme(legend.position="none") # remove the legend



#######################################################################
###	                         `Arthritis` dataset 
library (vcd)
glimpse(Arthritis)

## Task: 
## Visualize treatment results (`Improved`) on 
##   treatment group (`Treatment`) by sex (`Sex`) 
## 
table(Arthritis$Improved, Arthritis$Treatment, Arthritis$Sex)

ggplot(Arthritis, 
     aes(x = Improved, fill = Treatment)) + 
	geom_bar(width = 1) +
	ggtitle("Result of the Treatment, by Genre", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Result") +
     facet_wrap( ~ Sex) +
     theme(legend.position="bottom") 
     


#######################################################################
###                I.5  Barcharts with numeric x-axis               ###
#######################################################################

#  When the variable on the x-axis is numeric, it is sometimes useful 
#  to treat it as continuous, and sometimes useful to treat it 
#  as categorical. That could entail some differences in the result


#######################################################################
###                                Sales
glimpse(invoice_detailed)
library(lubridate)

## Task:
## Display the yearly frequency of products sold (number of invoice
##   rows/items for each year)

## We'll use  `side-by-side` bars. Notice the differences among
## the three version of the graph

# in the first version, `year` is declared on the x axis, as it is 
#    (number); also in the `fill` argument
ggplot(invoice_detailed %>% mutate (year = year(invoicedate)),
     aes (x = year, fill = year )) +
     geom_bar() +
     xlab("year") + ylab("Number of invoice rows") +
     ggtitle("Yearly Item Frequency (Number of Invoice Rows)") +
     theme(plot.title = element_text(hjust = 0.5))   # center the title

# in the second version, `year` is declared as factor on the x axis, 
#    and as it is (number) in the `fill` argument
#  (notice that here the legend appears with intriguing values)
ggplot(invoice_detailed %>% mutate (year = year(invoicedate)),
     aes (x = factor(year), fill = year )) +
     geom_bar() +
     xlab("year") + ylab("Number of invoice rows") +
     ggtitle("Yearly Item Frequency (Number of Invoice Rows)") +
     theme(plot.title = element_text(hjust = 0.5))   # center the title


# in the third version, `year` is declared as factor on both the x axis, 
#    and in the `fill` argument
 #  (notice that here the legend appears with proper values)
ggplot(invoice_detailed %>% mutate (year = year(invoicedate)),
     aes (x = factor(year), fill = factor(year))) +
     geom_bar() +
     xlab("year") + ylab("Number of invoice rows") +
     ggtitle("Yearly Item Frequency (Number of Invoice Rows)") +
     theme(plot.title = element_text(hjust = 0.5))   # center the title

# third version, without legend
ggplot(invoice_detailed %>% mutate (year = year(invoicedate)),
     aes (x = factor(year), fill = factor(year))) +
     geom_bar() +
     xlab("year") + ylab("Number of invoice rows") +
     ggtitle("Yearly Item Frequency (Number of Invoice Rows)") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(legend.position="none") # remove the legend

          
## Task:
## Display the monthly frequency of products sold (number of invoice
##   rows/items for each month), by years

ggplot(invoice_detailed %>% 
            mutate (year = year(invoicedate), month = month(invoicedate)),
     aes (x = factor(month), fill = factor(month))) +
     geom_bar() +
     xlab("month") + ylab("Number of invoice rows") +
     ggtitle("Monthly Item Frequency, by Years") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(legend.position="none") + # remove the legend
     facet_wrap (~ year)



#######################################################################
###		               II. More on barcharts                      ###	
#######################################################################

#######################################################################
###		           II.1 Bars and Text Justification               ###	
#######################################################################

#######################################################################
###       (Anonymized) FEAA students for 2014-2015 academic year

## Task:
## Display the structure of undergraduate students, by programme 
## (like in section I.1); this time, for each programme, add
## a text showing the number the student enrolled in the programme

# we'll add `geom_text` option to a previous ggplot command
# notice the `geom_text` own `aes` clause 
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")) %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, y = n_of_studs, label = n_of_studs), 
               hjust = 0, size = 3) + # display the text outside the bar 
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) 


# the same solution, but now the text is placed inside the bar
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")) %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, y = n_of_studs, label = n_of_studs), 
               hjust = 1.1, size = 3) + # display the text inside the bar 
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) 


# if the data is not pre-aggregated, in `geom_text` one must change
# two options:
#    - stat = "count"
#    - aes(x = PROGRAMME, y = ..count.., label = ..count..)
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses")) , 
     aes(x = PROGRAMME, fill = PROGRAMME)) +
     geom_bar() +
     coord_flip()  +   # horizontal bars
     geom_text(stat = "count", 
               aes(x = PROGRAMME, y = ..count.., label = ..count..), 
               hjust = 1.1, size = 3) + # display the text inside the bar 
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) 


#######################################################################
###                                Sales
glimpse(invoice_detailed)
library(lubridate)

## Task:
## Display the yearly frequency of products sold (number of invoice
##   rows/items for each year)

# `geom_text` added
ggplot(invoice_detailed %>% mutate (year = year(invoicedate)),
     aes (x = factor(year), fill = factor(year))) +
     geom_bar() +
     geom_text(stat = "count", 
               aes(x = factor(year), y = ..count.., label = ..count..), 
               vjust = 1.2, size = 5) + # display the text inside the bar 
     xlab("year") + ylab("Number of invoice rows") +
     ggtitle("Yearly Item Frequency (Number of Invoice Rows)") +
     theme(plot.title = element_text(hjust = 0))  + # center the title
     theme(legend.position="none") # remove the legend

          
## Task:
## Display the monthly frequency of products sold (number of invoice
##   rows/items for each month), by years

# `geom_text` added
ggplot(invoice_detailed %>% 
            mutate (year = year(invoicedate), month = month(invoicedate)),
     aes (x = factor(month), fill = factor(month))) +
     geom_bar() +
     geom_text(stat = "count", 
               aes(x = factor(month), y = ..count.., label = ..count..), 
               vjust = 1.2, size = 4) + # display the text inside the bar 
     xlab("month") + ylab("Number of invoice rows") +
     ggtitle("Monthly Item Frequency, by Year") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(legend.position="none") + # remove the legend
     facet_wrap (~ year)




#######################################################################
###	                         `Arthritis` dataset 
library (vcd)
glimpse(Arthritis)

## Task:
## Visualize information about attribute `Improved`, showing the 
## frequencies inside the bars
table(Arthritis$Improved)

# first solution - without pre-aggregation
ggplot(Arthritis, 
     aes(x = factor(Improved), fill = "yellow2")) + 
	geom_bar(width = 1, color="white") +
	geom_text(stat="count", color="black", hjust=.5, vjust=1.2, size=6,
		aes(y=..count.., label=..count..)) + 
	ggtitle("Result of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Result") +
	guides(fill=FALSE)  # no legend


# second solution - wit pre-aggregation and horizontar bars
ggplot(Arthritis %>%
            group_by(Improved) %>%
            tally(), 
       aes(x = Improved, fill = Improved)) +
  	geom_bar(stat="identity", aes(y = n), position="dodge") +
  	geom_text(aes(x = Improved, y= n, label = Improved), hjust = 1.1,
  		size = 5.5, position = position_dodge(width=1), show.legend = F) +
  	scale_y_continuous(labels = waiver()) + 
     coord_flip() +
     	ggtitle("Result of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
     theme(axis.text.y = element_blank(),    # here we remove the text on the Y-axis
		text=element_text(size=12)) +      
   	ylab("Number of cases") + xlab("Result") +
	guides(fill=FALSE)  # no legend



#######################################################################
###		    II.2 Bars and Conditional Text Justification          ###	
#######################################################################

#######################################################################
###       (Anonymized) FEAA students for 2014-2015 academic year

## Task:
## Display the structure of undergraduate students enrolled with
## regular attendance (remove distance learning studs), by programme 
## (similar to the example section I.1); this time, 
## for each programme, add a text showing the number 
## of students enrolled in the programme;
## additionally, the name of the programe will appear inside 
## the bar (if there is enough space) or ouside the bar (if
## the bar is too small or the title too large)

## we'll add another `geom_text` option for the program title
## giving up the axis label


# first try...
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, 
                   y = n_of_studs, 
                   label = PROGRAMME), 
               hjust = 1.1, size = 3) + # display the text inside the bar 
     geom_text(aes(x = PROGRAMME, y = n_of_studs, label = n_of_studs), 
               hjust = 0, size = 3) + # display the text outside the bar 
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) +
     theme(axis.text.y = element_blank()) 

# ... problem with the small bars (and long program titles)


# first solution: write programme names outside the bar and 
#    the number of students inside the bar 
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, 
                   y = n_of_studs, 
                   label = PROGRAMME), 
               hjust = 0, size = 3) + # display the text outside the bar 
     geom_text(aes(x = PROGRAMME, y = n_of_studs, label = n_of_studs), 
               hjust = 1.1, size = 3) + # display the text inside the bar 
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) +
     theme(axis.text.y = element_blank()) 

# not ok for the programmes with long titles and large number of students,
# but one can extend the y-axis
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, 
                   y = n_of_studs, 
                   label = PROGRAMME), 
               hjust = -0.01, size = 3.5) + # display the text outside the bar 
     geom_text(aes(x = PROGRAMME, y = n_of_studs, label = n_of_studs), 
               hjust = 1.1, size = 3.5) + # display the text inside the bar 
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) +
     theme(axis.text.y = element_blank()) +
     expand_limits(y = 800)


# second solution: display both the programme title and the 
# number of  inside the bar only if the program 
# has at least 300 students;
#   notice ` hjust=if_else( n_of_studs > 300, 1, 0)` INSIDE the
#   geom_text's aestetics 
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, 
                   y = n_of_studs, 
                   label = paste0(PROGRAMME, ' (', n_of_studs, ')' ), 
                   hjust = if_else( n_of_studs > 300, 1, 0) # !!!!!
                   ), 
               size = 3.5) +
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )) +
     theme(axis.text.y = element_blank()) 
     


#######################################################################
###		          II.3 Changing the bar order                     ###	
#######################################################################

#######################################################################
###       (Anonymized) FEAA students for 2014-2015 academic year

## Task:
## Display the structure of undergraduate students enrolled with
## regular attendance, by programme; showing the number 
## of students enrolled in the programme;
## New: the programme bars will be ordered by the number of
## students

# solution: in main aestetics (aes) clause, we'll use `reorder` function
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = reorder(PROGRAMME, n_of_studs), # this does the reordering
         y = n_of_studs, fill = PROGRAMME)) +
     geom_bar(stat="identity") +
     coord_flip()  +   # horizontal bars
     geom_text(aes(x = PROGRAMME, 
                   y = n_of_studs, 
                   label = paste0(PROGRAMME, ' (', n_of_studs, ')' ), 
                   hjust = if_else( n_of_studs > 300, 1, 0) # !!!!!
                   ), 
               size = 3.5) +
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5 )) +
     theme(axis.text.y = element_blank()) 
     

#######################################################################
###                                Sales
glimpse(invoice_detailed)
library(lubridate)

## Task:
## Display the monthly frequency of products sold (number of invoice
##   rows/items for each month), by years
##   New: Label the months with name (not number) 

# we'll not change the ggplot, but rather the data subject to
# plotting, using `factor` and `recode` 
ggplot(invoice_detailed %>% 
            mutate (year = year(invoicedate), 
                    month = factor(month(invoicedate)),
                    month_name = recode (month, `7` = 'Jul', 
                         `8` = 'Aug', `9` = 'Sep', `10` = 'Oct')),
     aes (x = month_name, fill = month_name)) +
     geom_bar() +
     geom_text(stat = "count", 
               aes(x = month_name, y = ..count.., label = ..count..), 
               vjust = 1.2, size = 4) + # display the text inside the bar 
     xlab("month") + ylab("Number of invoice rows") +
     ggtitle("Monthly Item Frequency, by Year") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(legend.position="none") + # remove the legend
     facet_wrap (~ year)



#######################################################################
###		          III. (A Sort of) Dotcharts                      ###
#######################################################################

#######################################################################
###       (Anonymized) FEAA students for 2014-2015 academic year

## Task:
## Display the structure of undergraduate students enrolled with
## regular attendance, by programme; showing the number 
## of students enrolled in the programme;
## New: instead of bars, use dots

# solution 1: use `geom_point` instead of `geom_bar`, remove
# `coord_flip` and x-axis labels
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME)) +
     geom_point() +
     geom_text(aes(x = PROGRAMME, 
                   y = n_of_studs, 
                   label = paste0(PROGRAMME, ' (', n_of_studs, ')' ) ), 
               size = 4) +
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_blank()) 
     
# is does not look too good.
# package `ggrepel` to the rescue
# see
# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
library(ggrepel)

# solution: instead of `geom_text` we will use `geom_text_repel`
ggplot(data = studs %>%
          filter (LEVEL_OF_STUDY == 'undergraduate' & !is.na(PROGRAMME) &
                  !str_detect(PROGRAMME, "^Common courses") &
                       ATTENDANCE == "Regular") %>%
          group_by(PROGRAMME) %>%
          summarise(n_of_studs = n()), 
     aes(x = PROGRAMME, y = n_of_studs, fill = PROGRAMME) ) +
     geom_point() +
     geom_text_repel(aes(x = PROGRAMME, # geom_text_repel !!!
                   y = n_of_studs, 
                   label = paste0(PROGRAMME, ' (', n_of_studs, ')' )),       
               size = 3.5) +
     guides(fill=FALSE) +
     xlab("Undergraduate Programme") + ylab("Number of students") +
     ggtitle("Structure of the Undergraduate Students") +
     theme(plot.title = element_text(hjust = 0.5))  + # center the title
     theme(axis.text.x = element_blank()) 
     


#######################################################################
###		                     IV. Piecharts                        ###	
#######################################################################
## Not very popular among statisticians and data scientists
## see
## https://www.businessinsider.com/pie-charts-are-the-worst-2013-6
## or
## http://varianceexplained.org/r/improving-pie-chart/


#######################################################################
###	                         `Arthritis` dataset 
library (vcd)
glimpse(Arthritis)

## Task:
## Visualize as a piechart the value frequencies for 
## attribute `Improved`
table(Arthritis$Improved)


# solution 1 - `geom_bar` + `coord_polar(theta = "y")`
ggplot(Arthritis, aes(x = Improved, fill = Improved)) + 
	geom_bar(width = 1) +
	coord_polar(theta = "y") + 
	ggtitle("Result of the Treatment" ) +
     theme(legend.position = "none") # remove the lengend

# solution 2 - `geom_bar` + `coord_polar(theta = "x")`
ggplot(Arthritis, aes(x = Improved, fill = Improved)) + 
	geom_bar(width = 1) +
	coord_polar(theta = "x") + 
	ggtitle("Result of the Treatment" ) +
     theme(legend.position = "none") # remove the lengend

# solution 3 - `geom_bar` + `coord_polar(theta = "x")`
ggplot(Arthritis, aes(x = factor(1), fill = Improved)) + 
	geom_bar(width = 1) +
	coord_polar(theta = "y") + 
	ggtitle("Result of the Treatment" ) +
     xlab(NULL) + ylab(NULL) # remove axes labels

# solution 4:
# - pre-compute the frequency
# - show text with label and percent inside the pie slices
Arthritis %>%
     group_by(result = Improved) %>%
     summarise (n = n(), percent = floor(n() / nrow(Arthritis) * 100)) %>%
ggplot(., aes(x = "", y = n, fill = result))   +  
     geom_bar(width = 1, stat = "identity")  +
     coord_polar("y", start=0) +
     geom_text(aes(y = percent, label = paste(result, '\n', percent, '%')),
               size = 6)


#######################################################################
###		   V. Association between categorical variables           ###	
#######################################################################


#######################################################################
###		                    V.1. Mosaic plots                     ###	
#######################################################################

# https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
## install.packages('ggmosaic')
library(ggmosaic)


#######################################################################
###	                         `Arthritis` dataset 
library (vcd)
glimpse(Arthritis)

## Task:
##   Is the treatment effective in fighting with the disease?

# The visual solution (for statistical tests - see script 11a  )
# is based on `ggplot` and `ggmosaic` packages
library(ggmosaic)
ggplot(data = Arthritis) +
     geom_mosaic(aes(weight = 1, x = product(Improved, Treatment), 
                   fill=factor(Improved)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=0, hjust= 0.5, size = 12)) + 
     labs(x = "Treatment", y = "Result", title='Treatment vs. Result') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5)) +
     theme(legend.position = "none") # remove the lengend


#######################################################################
###                      Fuel Economy dataset(s) 
glimpse((fuel_economy_2018))  

## Task:
## Examine visually if there is an association between
## variables `Drive` (2WD/4WD) and `Trans` (transmission)

# Solution 1 - consider all observations:
ggplot(data = fuel_economy_2018) +
     geom_mosaic(aes(weight = 1, x = product(Drive, Trans), 
                   fill=factor(Drive)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=-90, hjust= .1)) + 
     labs(x="transmission type", 
          title='Association between variables `Drive` and `Trans`') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     guides(fill=guide_legend(title = "Drive", reverse = TRUE))


# Solution 2 - recode the trasmission type, by fusioning subcategories
# of transmission type (we'll extract the first word from `Trans`):
table(fuel_economy_2018$Trans)

test <- fuel_economy_2018 %>%
     mutate (transmission_categ = word(Trans, 1, sep='-')) 
table(test$transmission_categ)

fuel_economy_2018 %>%
     mutate (transmission_categ = word(Trans, 1, sep='-')) %>%
ggplot(.) +
     geom_mosaic(aes(weight = 1, x = product(Drive, transmission_categ), 
                   fill=factor(Drive)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle = 45, hjust= 1, vjust = 1)) + 
     labs(x="transmission type", y = 'Drive',
          title='Association between `Drive` and `Transmission Type`') + 
     theme (plot.title = element_text (colour="black", size=14, hjust = 0.5))+
     guides(fill=guide_legend(title = "Drive", reverse = TRUE)) +
     theme(legend.position = "none") # remove the lengend     


## Task:
## Examine visually if there is an association between
## variables `Drive` (2WD/4WD) and `Veh Class` (class of the car)
## Note: we will regroup the class of the vehicle into larger
## categories

table(fuel_economy_2018$`Veh Class`)

fuel_economy_2018  %>%   
     mutate (vehicle_class = case_when(
          str_detect(`Veh Class`, 'SUV') ~ 'SUV',
          `Veh Class` %in% c('large car', 'station wagon') ~ 'large car',
          `Veh Class` %in% c('minivan', 'pickup', 'special purpose',
               'van') ~ 'van/pickup',
          TRUE ~ `Veh Class`)) %>% 
ggplot(.) +
     geom_mosaic(aes(weight = 1, x = product(Drive, vehicle_class), 
                   fill=factor(Drive)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle = 45, hjust= 1, vjust = 1)) + 
     labs(x="vehicle category", y = 'Drive',
          title='Association between Drive and Main Vehicle Category') + 
     theme (plot.title = element_text (colour="black", size=14, hjust = 0.5))+
     guides(fill=guide_legend(title = "Drive", reverse = TRUE)) +
     theme(legend.position = "none") # remove the lengend     




#######################################################################
###		                     V.2. Heatmaps                        ###	
#######################################################################


#######################################################################
###	                         `Arthritis` dataset 
library (vcd)
glimpse(Arthritis)

## Task:
## Visualize with a heatmap the association
## between `Treatment` and `Result`

# Solution with `ggplot2` package - notice `geom_tile`
Arthritis %>%
     group_by(treatment_group = Treatment, result = Improved) %>%
     summarise(frequency = n()) %>%
ggplot(., aes (x = treatment_group, y = result, 
               fill = frequency)) +
     geom_tile() +
     labs(x = "Treatment", y = "Result", title='Treatment vs. Result') +
     theme (plot.title = element_text (colour="black", size=14, hjust = 0.5)) 
     

# Same solution, but with better colours
Arthritis %>%
     group_by(treatment_group = Treatment, result = Improved) %>%
     summarise(frequency = n()) %>%
ggplot(., aes (x = treatment_group, y = result, 
               fill = frequency)) +
     geom_tile() +
     labs(x = "Treatment", y = "Result", title='Treatment vs. Result') +
     theme (plot.title = element_text (colour="black", size=14, hjust = 0.5)) +
     scale_fill_gradient(low = "white", high = "steelblue")



#######################################################################
###                      Fuel Economy dataset(s) 
glimpse(fuel_economy_2018) 

## Task:
## Examine visually if there is an association between
## variables `Drive` (2WD/4WD) and `Trans` (transmission)

# In this case, hhe reatmap is better than the mosaic plot 
fuel_economy_2018 %>%
     group_by(weel_drive = Drive, transmission = Trans) %>%
     summarise(frequency = n()) %>%
ggplot(., aes (x = weel_drive, y = transmission, fill = frequency)) +
     geom_tile() +
     labs(title='Association between `Drive` and `Transmission Type`') + 
     theme (plot.title = element_text (colour="black", size=14, hjust = 0.5)) +
     scale_fill_gradient(low = "white", high = "steelblue")


## Task:
## Examine visually f there is an association between
## variables `Trans` (transmission) and `Veh Class`

# solution
fuel_economy_2018 %>%
     group_by(vehicle_class = `Veh Class`, transmission = Trans) %>%
     summarise(frequency = n()) %>%
ggplot(., aes (x = vehicle_class, y = transmission, fill = frequency)) +
     geom_tile() +
     labs(title='Association between Vehicle Class and Transmission Type') + 
     theme (plot.title = element_text (colour="black", size=14, hjust = 0.5)) +
     theme(axis.text.x=element_text(angle = 45, hjust= 1, vjust = 1)) + 
     scale_fill_gradient(low = "white", high = "steelblue")






