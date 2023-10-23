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
###    3b. Data processing with the tidy-verse: tibble, dplyr, tidyr    ####
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/03%20Data_Processing/03b_Tidy-verse.pptx
############################################################################
## last update: 23.10.2023

library(tidyverse) 
library(readxl)

############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

# check if the current directory is ok
getwd()
############################################################################


########################################################################
###                    I. `dplyr` package                            ### 
########################################################################

# one can install `dlplyr` separatedly...
# install.packages("dplyr")
# library(dplyr)

#... but, since it would also load automatically when loading the `tidyverse`
#    package
library(tidyverse)

# dplyr provides five basic data manipulation verbs that work on a single table: 
#    filter(), 
#	arrange(), 
#	select(), 
#	mutate() (and transmute())
#	summarise(). 


########################################################################
###                   I.1 Basic `dplyr` verbs                        ### 
########################################################################

# dplyr provides five basic data manipulation verbs that work on a single table: 
#    filter(), 
#	arrange(), 
#	select(), 
#	mutate() 
#	summarise(). 


#######################################################################
###                           mutate()
#######################################################################
###   mutate() serves for creating new variables in a data frame
###            but also for removing or recoding variables 


#######################################################################
###                mutate() for adding variables

             
##########################################################
# (Anonymized) FEAA students for 2014-2015 academic year
getwd()
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)

## display the data frame structure
glimpse(studs)

## add a new variable (column) called `FULL_NAME` by concatenating the variables
##   `FAMILY_NAME`, `MIDDLE_NAME` and `FIRST_NAME`
studs <- mutate(studs, FULL_NAME  = paste(studs$FAMILY_NAME, 
          studs$MIDDLE_NAME, studs$FIRST_NAME))
glimpse(studs)
studs


############################
# Fuel Economy dataset(s) 
fuel_economy_2018 <- read_tsv("all_alpha_18.txt") 
glimpse(fuel_economy_2018)

## Add two new variables for expressing fuel consumption in
##    `liters per 100 kilometers` (miles per gallon is difficult to
##         grasp for Europeans)
## The formula is:   
## liters per 100 kilometers = 235.214583333333 ÷ (mile per gallon)
##
## Also add a variable about the (`approximate`) manufacturer
fuel_economy_2018 <- fuel_economy_2018 %>%
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


############################
###       Sales
load (file = 'sales.RData')
## ... taken the data frame `invoice_detailed`
glimpse(invoice_detailed)
## extract year, month, day, the day of the week and the invoice line VAT for each 
## invoice in separate columns/variables; 
## install.packages('lubridate')
library(lubridate)
invoice_detailed <- mutate (invoice_detailed, 
     year = year(invoicedate),
     month = month(invoicedate),
     day = day(invoicedate),
     weekDay = wday(invoicedate, label=T),
     rowVAT = amount - amountwithoutvat
          )
glimpse(invoice_detailed)


#######################################################################
###                mutate() for removing variables

## The simplest way is just to set the variable on the NULL (meta)value
## Example:
## In data frame `fuel_economy_2018` remove variables `Stnd`, `Stnd Description`
##   and `Underhood ID`
##   

glimpse(fuel_economy_2018)
fuel_economy_2018 <- mutate (fuel_economy_2018, Stnd = NULL, 
          `Stnd Description` = NULL, `Underhood ID` = NULL)
# see the structure after the variables were removed
glimpse(fuel_economy_2018)


#######################################################################
###                mutate() for recoding variables

###  FEAA students
glimpse(studs)
## display variabile `YEAR_OF_STUDY` values
studs$YEAR_OF_STUDY

## task: change the roman numbers (I, II, III) with the arabic equivalents (1, 2 or 3)
studs <- mutate(studs, 
     YEAR_OF_STUDY = as.numeric(if_else(YEAR_OF_STUDY == 'I', 1, 
          if_else(YEAR_OF_STUDY == 'II', 2, 
          if_else(YEAR_OF_STUDY == 'III', 3, as.numeric(YEAR_OF_STUDY))))))

studs$YEAR_OF_STUDY



#######################################################################
###                           transmute()
#######################################################################
###   mutate() add newly created variables to the existing ones;
###   by contrast, transmute() remove any variable that is left
###   outside (non-declared)

file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
## display the data frame structure
glimpse(studs)

## notice the difference between...
studs.mutate <- mutate(studs, FULL_NAME  = paste(studs$FAMILY_NAME, 
          studs$MIDDLE_NAME, studs$FIRST_NAME))
glimpse(studs.mutate)

## ... and
studs.transmute <- transmute(studs, FULL_NAME  = paste(studs$FAMILY_NAME, 
          studs$MIDDLE_NAME, studs$FIRST_NAME))
glimpse(studs.transmute)


#######################################################################
###      transmute() for selecting variables (and removing 
###            unselected variables)

## Extract all the rows of the data frame `studs`, but only columns
## `FAMILY_NAME` and `FIRST_NAME`
temp <- transmute(studs, FAMILY_NAME, FIRST_NAME)
temp


#######################################################################
###      transmute() for changing the order of the columns 
names(contacts)
## Task: change the variables (columns) position in the data frame:
## instead of ("personalcode", "customerid",  "position"), the order must be:
##   ("customerid",  "position", "personalcode")
contacts <-  transmute(contacts, 
     customerid,  position, personalcode)


#######################################################################
###     transmute() for renaming variables (not quite recommended...)

## Task: Rename the variable `YEAR_OF_STUDY` (in data frame `studs`) into `STUDY_YEAR`
# First, display the variable name in the dataframe
names(studs)

# now, rename wihin `transmute`
studs <- transmute(studs,
     LEVEL_OF_STUDY, ATTENDANCE, STUDY_YEAR = YEAR_OF_STUDY, 
     PROGRAMME, LOCATION, LECTURE_GROUP, LAB_GROUP, FINANCIAL_SUPPORT, FAMILY_NAME, 
     MIDDLE_NAME, FIRST_NAME, STUD_ID) 

# check the new names
names(studs)


#######################################################################
###                          select()
#######################################################################
###   select() extracts from a data frame only the nominated
###   columns (variables) 

#######################################################################
###     selecting() for selecting variables (and removing 
###            unselected variables) in a specific order

## Extract the following columns/variables (in this order) 
##   from the data frame `studs`
##   STUD_ID, FAMILY_NAME, MIDDLE_NAME, FIRST_NAME, LEVEL_OF_STUDY, 
#$   ATTENDANCE, STUDY_YEAR, PROGRAMME
View(studs)
names(studs)
temp <- select(studs, STUD_ID, FAMILY_NAME:FIRST_NAME, 
               LEVEL_OF_STUDY:PROGRAMME)
temp


#######################################################################
###       select() for renaming variables 

# reload FEAA students
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
# display the data frame structure
glimpse(studs)

## Task: Rename the variable `YEAR_OF_STUDY` (in data frame `studs`) into `STUDY_YEAR`
studs <- select(studs,
     LEVEL_OF_STUDY:ATTENDANCE, STUDY_YEAR = YEAR_OF_STUDY, 
     PROGRAMME:STUD_ID) 

glimpse(studs)


#######################################################################
###                           arrange()
#######################################################################
###   arrange() orders the rows in the result (like ORDER BY in SQL)

## sort people by their last name

people_ord1 <- arrange(people, lastname)
people
people_ord1

## sort people by their genre (descending) and their last name (ascending)
people_ord2 <- arrange(people, desc(genre), lastname)
people
people_ord2


#######################################################################
###                           filter()
#######################################################################
###   filter() extracts from a data frame only the rows 
###   matching a criteria

## extract only males in data frame `people` who do not own a mobile
##   phone (apparently)
females_no_phones <- filter(people, genre == 'B' & is.na(mobilephone))
females_no_phones


#######################################################################
###                           summarise()
#######################################################################
### summarise() is useful for computing aggregate values

# Count the number of observations in data frame `people`
summarise(people, n_of_people = n())

# Compute total sales (in data frame `invoice_detailed`)
summarise(invoice_detailed, sales = sum(amount))



########################################################################
###                 I.2 Other useful `dplyr` verbs                   ### 
########################################################################

########################################################################
##                       distinct()
## extract unique values

# Display the levels of study in data frame `studs`
distinct(studs, LEVEL_OF_STUDY)

# Display available programmes for each level of study in data frame `studs`
distinct(studs, PROGRAMME, LEVEL_OF_STUDY)


########################################################################
##                       sample_n()
## extract randomly entire rows from a data frame

# Extact a random sample of 5 invoices (from all invoices)
sample_5invoices.1 <- sample_n(invoices, 5, replace=FALSE)
sample_5invoices.1




########################################################################
###         I.3 Pipelining (chaining) operations with `dplyr`        ### 
########################################################################
#    Pipelining is the key ingredient in explaining the success 
#  of `dplyr`  package; similar to SQL SELECT, pipelining allows
#  writing complex queries in a single statetement
#  Nevertheless, pipeline logic is different; the result of an 
#    operation (step) is passed, through `%>%` operator, as argument
#    for the next step (operation/verb) 
#


## Task: Extract variables `lastname`, `firstname` and `address` only for the females
## in base R the solution would be:
## people [people$genre == 'F', c('lastname', 'firstname', 'address')]

## With `dplyr` the solution is non necessarily shorter, but 
##   easier to grasp
glimpse(people)
people %>% 
     filter (genre == 'F') %>% 
     select (lastname:address)

people %>% 
     filter (genre == 'F') %>% 
     select (lastname:address) -> temp

temp <- people %>% 
     filter (genre == 'F') %>% 
     select (lastname:address)
temp


## Extract all columns except `email` for females
## subset(people, genre == 'F', select =  -c( email ))
people %>%
     filter (genre == 'F') %>%
     select ( - email)

## Extract, for females, all columns except the three phone numbers  
# subset(people, genre == 'F', select =  -c(homephone, officephone, mobilephone ))
# ... or
# subset(people, genre == 'F', select = personalcode:email)
people %>%
     filter (genre == 'F') %>%
     select ( -c(homephone, officephone, mobilephone ))
# ...or
people %>%
     filter (genre == 'F') %>%
     select ( personalcode:email)


########################################################################
###                    New examples 
# reload FEAA students
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
# display the data frame structure
glimpse(studs)

##    Display, in alphabetical order, all the master programmes at FEAA 

# SQL solution:
#    SELECT DISTINCT PROGRAMME
#    FROM studs
#    WHERE LEVEL_OF_STUDY = 'master'
#    ORDER BY PROGRAMME DESC

temp <- studs %>%
     filter (LEVEL_OF_STUDY == 'master') %>%
     select (PROGRAMME) %>%
     distinct (PROGRAMME) %>%
     arrange (desc(PROGRAMME))
temp
View(temp)

# one can give up `select`...
temp <- studs %>%
     filter (LEVEL_OF_STUDY == 'master') %>%
     distinct (PROGRAMME) %>%
     arrange (desc(PROGRAMME))
temp
View(temp)



##    Extract students enrolled in the second year of study of the
##    undegraduate programme `Business Informatics`
temp <- studs %>%
     filter (LEVEL_OF_STUDY == 'undergraduate' & 
                  PROGRAMME == 'Business Informatics'
             & YEAR_OF_STUDY == 'II') %>%
     transmute (FULL_NAME = paste(FAMILY_NAME, MIDDLE_NAME, FIRST_NAME)) %>%
     arrange (FULL_NAME)
temp     
View(temp)


##    How many lab groups are there in the second year of study of the
##    `Business Informatics` undegraduate programme ?
studs %>%
     filter (LEVEL_OF_STUDY == 'undergraduate' & 
          PROGRAMME == 'Business Informatics'
             & YEAR_OF_STUDY == 'II') %>%
     distinct(LAB_GROUP) %>%
     summarise (n_of_groups = n())


# the same result, with `tally()` instead of `n()`
studs %>%
     filter (LEVEL_OF_STUDY == 'undergraduate' & 
          PROGRAMME == 'Business Informatics'& YEAR_OF_STUDY == 'II') %>%
     distinct(LAB_GROUP) %>%
     tally()



########################################################################
###               I.4 Group operations with `dplyr`                 ### 
########################################################################
## group_by() verb states how to break a dataset down into groups of rows


##    Display the number of enrolled students for each level of study

# solution with `n()` (equivalent to SQL's COUNT(*))
studs %>%
     group_by(LEVEL_OF_STUDY) %>%
     summarise (n_of_studs = n())

# solution with `tally` (the agrregate colum name is `n`)
studs %>%
     group_by(LEVEL_OF_STUDY) %>%
     tally()


##    Display, for each level of study, the number of programmes and 
##    the number enrolled students (on that study level)
studs %>%
     group_by(LEVEL_OF_STUDY) %>%
     summarise (
          n_of_progs = n_distinct(PROGRAMME), 
          n_of_studs = n()
          )


##    Given tha data frame `invoice_detailed`...
glimpse(invoice_detailed)
## ... Display for each year, the first and the last day 
##         with sales (for that year) 
library(lubridate)   

invoice_detailed %>%
     group_by(year = year(invoicedate)) %>%
     summarise (
          first_date_with_sales = min(invoicedate), 
          last_date_with_sales = max(invoicedate)
          ) %>%
     arrange (year)


##    Compute the monthly sales
glimpse(invoice_detailed) 
invoice_detailed %>%
     group_by(year  = year(invoicedate), month = month(invoicedate)) %>%
     summarise ( sales = sum(amount) ) %>%
     arrange (year, month)


##    Extract the lab groups for the second year 
##    of study of the `Business Informatics` undegraduate programme ?
##    with at least 26 studs
glimpse(studs)
studs %>%
     filter (LEVEL_OF_STUDY == 'undergraduate' & 
                  PROGRAMME == 'Business Informatics'
             & YEAR_OF_STUDY == 'II') %>%
     group_by(LAB_GROUP) %>%
     summarise (n_of_studs = n()) %>%
     filter (n_of_studs >= 26)


##    Display the largest number of students in a lab group for the second year 
##    of study of the `Business Informatics` undegraduate programme ?
studs %>%
     filter (LEVEL_OF_STUDY == 'undergraduate' & 
                  PROGRAMME == 'Business Informatics'
             & YEAR_OF_STUDY == 'II') %>%
     group_by(LAB_GROUP) %>%
     summarise (n_of_studs = n()) %>%
     summarise(n_max = max(n_of_studs))


##    Display the total number of students for each master programme
temp <- studs %>%
     filter (LEVEL_OF_STUDY == 'master') %>%
     group_by(PROGRAMME) %>%
     summarise (n_of_studs = n())
temp


## Display, for each programme, the study year with the largest number
##   of enrolled students

# notice two group operations and `slice` function
temp <- studs %>%
     group_by(LEVEL_OF_STUDY, PROGRAMME, YEAR_OF_STUDY) %>%
     summarise (n_of_studs = n()) %>%
     ungroup() %>%
     arrange(LEVEL_OF_STUDY, PROGRAMME, desc(n_of_studs)) %>%
     group_by(LEVEL_OF_STUDY, PROGRAMME) %>%
     slice(1)

temp



##    Display top 5 master programmes as total number of students
temp <- studs %>%
     filter (LEVEL_OF_STUDY == 'master') %>%
     group_by(PROGRAMME) %>%
     summarise (n_of_studs = n()) %>%
     arrange (desc(n_of_studs)) %>%
     top_n(5, n_of_studs )
temp


##    Display the master programmes with at least the total 
##    number of students as the programme `Business Information Systems`
temp <- studs %>%
     filter (LEVEL_OF_STUDY == 'master') %>%
     group_by(PROGRAMME) %>%
     summarise (n_of_studs = n()) %>%
     mutate(n_of_studs_BIS = if_else(PROGRAMME == 'Business Information Systems',
          n_of_studs, 0L)) %>%
     filter (n_of_studs >= max(n_of_studs_BIS)) %>%
     select (PROGRAMME, n_of_studs) %>%
     arrange(desc(n_of_studs))
View(temp)





########################################################################
###                            I.5 Joins                             ### 
########################################################################
### The are four joining-related functions in the `dplyr` package:
#  inner_join(x, y, by = NULL, copy = FALSE, ...): return all rows from x 
#         where there are matching values in y, and all columns from x and y
#  left_join(x, y, by = NULL, copy = FALSE, ...): return all rows from x, 
#         and all columns from x and y
#  semi_join(x, y, by = NULL, copy = FALSE, ...): return all rows from x 
#         where there are matching values in y, keeping just columns from x.
#  anti_join(x, y, by = NULL, copy = FALSE, ...): return all rows from x 
#         where there are not matching values in y, keeping just columns from x

## Note that the right outer join (df1, df2) is computed by simply reversing
## the order
## left_join(df2, df1)

# see also:
# http://stat545.com/bit001_dplyr-cheatsheet.html#semi_joinsuperheroes-publishers

# clear the environment
rm(list = ls())


########################################################################
###        Joining data frames by attributes having identical names 

# load `Chinook` database tables saved as data frames
load('chinook.RData')

## Extract the artist names ordered alphabetically
names(artist)
View(artist)
temp <- artist %>% 
     select (name) %>% 
     arrange (name)
     
## Display the albums released by `Creedence Clearwater Revival`
View(album)
temp <- artist %>%
     filter (name == 'Creedence Clearwater Revival') %>%
     inner_join(album)
temp

#...or
temp <- artist %>%
     inner_join(album) %>%
     filter (name == 'Creedence Clearwater Revival') 
temp


########################################################################
###                 Join by specifying the join attributes 

## Display the tracks on each album of Led Zeppelin?
View(track)
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track)
View(temp)
## Not good!!!!!
## Source of the problem:
#   both data frames `artist` and `track` contain variable `name`, 
#   so the inner join between `artist`-`album` and `track` is 
#   performed on both `name` and `albumid` columns, which is wrong.
# Solutions idea: the inner_join with `track` must by done only by `albumid`
# attribute 
#   
# Solution 1: we change the column `name` in `artist` data frme into
#   `artist_name`  
temp <- artist %>%
     select (artistid, artist_name = name) %>%
     filter (artist_name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(
          track %>% 
               select (trackid, track_name = name, albumid:unitprice)) %>%
     select (album_title=title, track_name) %>%
     arrange(album_title)
temp

# Solution 2: we force the inner_join with `track` to be performed only 
#   on attribute `albumid`  
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track, by = 'albumid')
temp


## How many tracks are contained on each album of Led Zeppelin band?
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track, by = 'albumid') %>%
     group_by(title) %>%
     summarise( n_of_tracks = n())
temp


## Which Led Zeppelin album (or albums, if there are more with the 
## same numeber) contain the largest number of tracks?
# 
# Solution 1 is based on `top_n(1)`:
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track, by = 'albumid') %>%
     group_by(title) %>%
     summarise( n_of_tracks = n()) %>%
     top_n(1, n_of_tracks)
temp

# Solution 2 is based on `filter` and `max`:
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track, by = 'albumid') %>%
     group_by(title) %>%
     summarise( n_of_tracks = n()) %>%
     filter (n_of_tracks == max(n_of_tracks))
temp


## Which Led Zeppelin album (or albums, if there are more with the 
## same number) contains the second largest number of tracks?
# 
# Solution 1:
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track, by = 'albumid') %>%
     group_by(title) %>%
     summarise( n_of_tracks = n()) %>%
     top_n(2, n_of_tracks) %>%
     filter(n_of_tracks != max(n_of_tracks))
temp

# Solution 2:
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     inner_join(track, by = 'albumid') %>%
     group_by(title) %>%
     summarise( n_of_tracks = n()) %>%
     filter (n_of_tracks < max(n_of_tracks)) %>%
     filter (n_of_tracks == max(n_of_tracks))
temp


########################################################################
###           Joining tables by attributes with different names

names(customer)
View(customer)
View(employee)

# Display the employees who "take care" of customers from Brazil 
# Solution: in table `customer` atribute `supportrepid` shows the employee
#   who takes care of each customer;
#  we'll join tables `customer` and `amployee` by
#   `customer.supportrepid = employee.employeeid
temp <- customer %>%
     filter (country == 'Brazil') %>%
     inner_join(employee, by = c('supportrepid' = 'employeeid')) %>%
     select (lastname.y, firstname.y) %>%
     distinct (lastname.y, firstname.y)
temp

names(temp)


########################################################################
###                             Self join
###            (joining two copies of the same table/data frame)

## Display the name of each employee's boss

# Solution: table `employee` has attribute `reportsto` which points
#   to the id of current employee's boss;
#   we'll join two copies of table `employee` by condition
#         `subordinate.reportsto = boss.employeeid` (actually there is not
#         such an alias `boss`, but that's the basic idea)
temp <- employee %>%
     select (subordinate_employeeid = employeeid, 
               subordinate_lastname = lastname, 
               subordinate_firstname = firstname, 
               subordinate_reportsto = reportsto) %>%
     inner_join(employee, by = c('subordinate_reportsto' = 'employeeid')) %>%
     select (subordinate_employeeid:subordinate_reportsto, 
             boss_lastname = lastname, boss_firstname = firstname)
temp


########################################################################
###                             Outer joins

load(file = 'sales.RData')
library(lubridate)

## display, for each product, three separate variables (columns) containing
##  sales for 2016, 2017 and 2018

# solution 1: without outer join
temp <- invoice_detailed %>%
     mutate(
          i.d.2016 = if_else(year(invoicedate)==2016, invoice_detailed$amount, 0),
          i.d.2017 = if_else(year(invoicedate)==2017, invoice_detailed$amount, 0),
          i.d.2018 = if_else(year(invoicedate)==2018, invoice_detailed$amount, 0) ) %>%
     group_by(productname) %>%
     summarise(
          sales.2016 = sum(i.d.2016, na.rm=TRUE),
          sales.2017 = sum(i.d.2017, na.rm=TRUE),
          sales.2018 = sum(i.d.2018, na.rm=TRUE) )    


# solution 2: with outer joins
temp <- products %>%
     select (productid:category) %>%
     left_join(
          invoice_detailed %>% 
               filter (year(invoicedate) == 2016) %>%
               group_by( productid) %>%
               summarise(sales2016 = sum(amount, na.rm=TRUE))) %>%
     left_join(
          invoice_detailed %>% 
               filter (year(invoicedate) == 2017) %>%
               group_by( productid) %>%
               summarise(sales2017 = sum(amount, na.rm=TRUE))) %>%
     left_join(
          invoice_detailed %>% 
               filter (year(invoicedate) == 2018) %>%
               group_by( productid) %>%
               summarise(sales2018 = sum(amount, na.rm=TRUE))) %>%
     transmute (productid, productname, category, 
             sales2016 = ifelse(is.na(sales2016), 0, sales2016), 
             sales2017 = ifelse(is.na(sales2017), 0, sales2017), 
             sales2018 = ifelse(is.na(sales2018), 0, sales2018)
             )
     


########################################################################
###                             Semi joins

## Display the title of all Led Zeppelin albums 
# 
# Solution 1: without semi-join
temp <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     select (albumid, title)
temp


# Solution 1: with semi-join
temp <- album %>%
     semi_join(artist %>% filter (name == 'Led Zeppelin'))
temp


########################################################################
###                             Anti joins

## Display products sold in August 2017 which were not sold in September 2017
temp <- invoice_detailed %>%
     filter (year(invoicedate) == 2017 & month(invoicedate) == 8) %>%
     select (productid, productname) %>%
     anti_join(
          invoice_detailed %>%
               filter (year(invoicedate) == 2017 & month(invoicedate) == 9) %>%
               select (productid, productname)
     ) %>%
     distinct(productid, productname)
temp



##############################################################################
###                       Cross-join (cartesian product with magritte)
##############################################################################
# solution taken from 
# http://stackoverflow.com/questions/35406535/cross-join-in-dplyr-in-r
#
a <- tibble(x = 1:10, y = LETTERS[1:10])
b <- a
a
b
# one line:
temp <- (a %>% mutate(foo = 1)) %>% 
        full_join(mutate(b, foo=1), by = 'foo') %>% 
        select(-foo)
temp


## We want to generate a data seta with people by combining all the
##   original students family name with all students first name

# reload FEAA students ...
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)

# ...display the data frame structure
glimpse(studs)

# Take care! The results will contain more than 42,000,000 rows
# When data set is large, the cross join may block the system
temp <- (studs %>% select (FAMILY_NAME) %>% mutate(foo = 1)) %>% 
        full_join(mutate(studs %>% select (FIRST_NAME), foo=1), by = 'foo') %>% 
        select(-foo)

# so, if you work on not-so-powerful system, if might be a good idea, to 
# previously extract a sample or rows and thew to cross-join them
temp <- (studs %>% select (FAMILY_NAME) %>% sample_n(100, replace=F) %>% 
              mutate(foo = 1)) %>% 
        full_join(mutate(studs %>% select (FIRST_NAME) %>% sample_n(100, replace=F), 
                    foo=1), by = 'foo') %>% 
        select(-foo)
# now the result has 10,000 rows



########################################################################
###                        I.6 Unions/Binds                          ### 
########################################################################
# clear the environment
rm(list = ls())


########################################################################
###                            bind_rows()                          ### 


## Aggregate all the data sets in DragosCogean directory into two
##  data frames, `all_inserts` and `all_reads`
all_inserts <- bind_rows(
     read_tsv("DragosCogean__InsertMongo_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MongoDB'), 
     read_tsv("DragosCogean__InsertMySQL_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MySQL'))

all_reads <- bind_rows(
     read_tsv("DragosCogean__ReadMongo_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MongoDB'), 
     read_tsv("DragosCogean__ReadMySQL_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MySQL'))



## Given `studs` data frame (FEAA students)....
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)

## ... Display number of studs for each master programme and year of study,
##  then a subtotal with the overall number of students for each programme

# Solution: there are two types of records in the result:
#   - one for each programme and year of study 
#   - another for each programme
# `bind_rows` will `paste` the two types of rows
temp <- bind_rows (
     studs %>%   # extract rows for programmes and years of study
     filter (LEVEL_OF_STUDY == 'master') %>%
     group_by(PROGRAMME, YEAR_OF_STUDY) %>%
     summarise (n_of_studs = n()) %>%
     select (PROGRAMME, YEAR_OF_STUDY, n_of_studs), 
          studs %>%   # extract rows just for programmes 
          filter (LEVEL_OF_STUDY == 'master') %>%
          group_by(PROGRAMME) %>%
          summarise (n_of_studs = n()) %>%
          transmute (PROGRAMME, YEAR_OF_STUDY = 'subtotal', n_of_studs)
                          ) %>%
     arrange (PROGRAMME, YEAR_OF_STUDY)
temp

# Add to the previous report a `Total` row displaying the number of 
#   master students 
temp2 <- bind_rows (
     temp, 
          studs %>%   # extract the total row
          filter (LEVEL_OF_STUDY == 'master') %>%
          summarise (n_of_studs = n()) %>%
          transmute (PROGRAMME = 'TOTAL', YEAR_OF_STUDY = ' ', n_of_studs)
                          ) %>%
     arrange (PROGRAMME, YEAR_OF_STUDY)
temp2

# There could be a problem: the total row might be not displayed 
# at the end of the report 
# Solution: we might add a column showing the aggregation level:
temp <- bind_rows (
     studs %>%   # extract rows for programmes and years of study
     filter (LEVEL_OF_STUDY == 'master') %>%
     mutate (LEVEL = 1) %>%     # this is the regular type of row
     group_by(LEVEL, PROGRAMME, YEAR_OF_STUDY) %>%
     summarise (n_of_studs = n()) %>%
     select (LEVEL, PROGRAMME, YEAR_OF_STUDY, n_of_studs), 
          studs %>%   # extract rows just for programmes 
          filter (LEVEL_OF_STUDY == 'master') %>%
          mutate (LEVEL = 1) %>%   # this is subtotal type of row
          group_by(LEVEL, PROGRAMME) %>%
          summarise (n_of_studs = n()) %>%
          mutate (YEAR_OF_STUDY = 'prog. subtotal') %>%
          select (LEVEL, PROGRAMME, YEAR_OF_STUDY, n_of_studs), 
     studs %>%   # extract just the total row
     filter (LEVEL_OF_STUDY == 'master') %>%
     summarise (n_of_studs = n()) %>%
     transmute (LEVEL = 2, PROGRAMME = 'TOTAL', YEAR_OF_STUDY = '', n_of_studs) ) %>%
     arrange (LEVEL, PROGRAMME, YEAR_OF_STUDY)
View(temp)


#  Actually, there is a slightly simpler solution:
temp <- bind_rows (
     bind_rows(
          studs %>%   # extract rows for programmes and years of study
          filter (LEVEL_OF_STUDY == 'master') %>%
          group_by(PROGRAMME, YEAR_OF_STUDY) %>%
          summarise (n_of_studs = n()), 
               studs %>%   # extract rows just for programmes 
               filter (LEVEL_OF_STUDY == 'master') %>%
               group_by(PROGRAMME) %>%
               summarise (n_of_studs = n()) %>%
               mutate (YEAR_OF_STUDY = 'prog. subtotal') ) %>%
     arrange (PROGRAMME, YEAR_OF_STUDY), 
          studs %>%   # extract just the total row
          filter (LEVEL_OF_STUDY == 'master') %>%
          summarise (n_of_studs = n()) %>%
          mutate (PROGRAMME = 'TOTAL') )
temp



#######################################################################
###                            bind_cols()                          ### 

load(file = 'sales.RData')
library(lubridate)
## display, for each product, three separate variables (columns) containing
##  sales for 2016, 2017 and 2018 and column for product overall sales

# solution 1: without outer join and bind_cols()
temp <- invoice_detailed %>%
     mutate(
          sales2016 = ifelse(year(invoicedate)==2016, invoice_detailed$amount, 0),
          sales2017 = ifelse(year(invoicedate)==2017, invoice_detailed$amount, 0),
          sales2018 = ifelse(year(invoicedate)==2018, invoice_detailed$amount, 0),
          sales_2016_2018 = ifelse(year(invoicedate) %in% c(2016, 2017, 2018), 
               invoice_detailed$amount, 0)) %>%
     group_by(productname) %>%
     summarise(
          sales.2016 = sum(sales2016, na.rm=TRUE),
          sales.2017 = sum(sales2017, na.rm=TRUE),
          sales.2018 = sum(sales2018, na.rm=TRUE),
          sales.2016_2018 = sum(sales_2016_2018, na.rm=TRUE)          )    


# solution 2: with outer joins and cbind
temp <- bind_cols(
     products %>%
     select (productid:category) %>%
     left_join(
          invoice_detailed %>% 
               filter (year(invoicedate) == 2016) %>%
               group_by( productid) %>%
               summarise(sales2016 = sum(amount, na.rm=TRUE))) %>%
     left_join(
          invoice_detailed %>% 
               filter (year(invoicedate) == 2017) %>%
               group_by( productid) %>%
               summarise(sales2017 = sum(amount, na.rm=TRUE))) %>%
     left_join(
          invoice_detailed %>% 
               filter (year(invoicedate) == 2018) %>%
               group_by( productid) %>%
               summarise(sales2018 = sum(amount, na.rm=TRUE))) %>%
     transmute (productid, productname, category, 
             sales2016 = ifelse(is.na(sales2016), 0, sales2016), 
             sales2017 = ifelse(is.na(sales2017), 0, sales2017), 
             sales2018 = ifelse(is.na(sales2018), 0, sales2018)
             ),
     products %>%
          select (productid:category) %>%
          left_join(
               invoice_detailed %>% 
               filter (year(invoicedate) %in% c(2016, 2017, 2018)) %>%
               group_by( productid) %>%
               summarise(sales2016_2018 = sum(amount, na.rm=TRUE))) %>%
               select (sales2016_2018)
     ) %>%
     mutate (sales2016_2018 = ifelse(is.na(sales2016_2018), 0, sales2016_2018))



########################################################################
###                      I.7 OLAP functions                          ### 
########################################################################
###                          (just a few)                            ###


########################################################################
###                           row numbering                          ### 

# load data frames loaded from the `chinook` database tables
load('chinook.RData')

## display the order number of each artist's album, by `albumid` 
temp <- artist %>%
     inner_join(album) %>%
     arrange(name, albumid) %>%
     group_by(name) %>%
     mutate (album_no_within_artist = row_number())


## display the order number for each album of each artist, 
## by `albumid`; also add a general order number (for artists + albums)

# sol. 1 (notice the `ungroup()`)
temp <- artist %>%
     inner_join(album) %>%
     arrange(name, albumid) %>%
     group_by(name) %>%
     mutate (album_number_within_artist = row_number()) %>%
     ungroup() %>%
     mutate (album_number_general = row_number())
     
# sol. 2 (notice the `ungroup()`)
temp <- artist %>%
     inner_join(album) %>%
     arrange(name, albumid) %>%
     group_by(name) %>%
     mutate (album_number__within_artist = rank(albumid)) %>%
     ungroup() %>%
     mutate (artist_and_album = paste(name, albumid)) %>%
     mutate (album_number__general = rank(artist_and_album))


# sol. 3 (notice the `ungroup()`)
temp <- artist %>%
     inner_join(album) %>%
     arrange(name, albumid) %>%
     group_by(name) %>%
     mutate (album_number__within_artist = dense_rank(albumid)) %>%
     ungroup() %>%
     mutate (artist_and_album = paste(name, albumid)) %>%
     mutate (album_number__general = dense_rank(artist_and_album))


########################################################################
###                               lag/lead                          ### 

load(file = 'sales.RData')
## display, for each product the yearly sales and the difference 
## in sales between current and the previous year
glimpse(invoice_detailed)

invoice_detailed %>%
     group_by(productname, year = year(invoicedate)) %>%
     summarise(crt_year_sales = sum(amount)) %>%
     mutate (prev_year_sales = lag(crt_year_sales, order_by = year, default = 0), 
             sales_diff = crt_year_sales - prev_year_sales)


########################################################################
###                               cumsum                             ### 

load(file = 'sales.RData')

## display, for each product the yearly sales and the sum of sales
## for all the prevoius years until (including) the current year
glimpse(invoice_detailed)

invoice_detailed %>%
     group_by(productname, year = year(invoicedate)) %>%
     summarise(crt_year_sales = sum(amount)) %>%
     mutate (cumulative_sales = cumsum(crt_year_sales))



########################################################################
###                  I.8 Other types of subqueries                   ### 
########################################################################


## extract the tracks from the same album(s) with track `Dazed and Confused`

# sol. 1
temp <- track %>%
     filter (name == "Dazed and Confused") %>%
     distinct (albumid) %>%
     inner_join(track) %>%
     select (albumid, name) %>%
     inner_join(album)

# sol. 2
track %>%
     inner_join(track %>%
                    filter (name == "Dazed and Confused") %>%
                    distinct (albumid)
                ) %>%
     select (albumid, name) %>%
     inner_join(album)


# sol. 3
track %>%
     select (albumid, name) %>%
     inner_join(album) %>%
     filter (albumid %in% (track %>%
                              filter (name == "Dazed and Confused") %>%
                              distinct (albumid) %>%
                              pull() ) )
# sol. 4
track %>%
     select (albumid, name) %>%
     inner_join(album) %>%
     filter (albumid %in% (track %>%
                              filter (name == "Dazed and Confused") %>%
                              distinct (albumid)  ) [['albumid']] )
     
# sol. 5
track %>%
     select (albumid, name) %>%
     inner_join(album) %>%
     filter (albumid %in% (track %>%
                              filter (name == "Dazed and Confused") %>%
                              distinct (albumid) %>%
                              .$albumid  )  )




########################################################################
###                 II.  Restructuring/Reshaping Data                ###
########################################################################
###       Two options: 
###       - `dplyr` for simple pivoting
###       - `tidyr` advanced pivoting
########################################################################
# clear the environment
rm(list = ls())


load(file = 'sales.RData')
library(lubridate)


########################################################################
###      II.1   Reshaping/pivoting data with package `dplyr`          ### 
########################################################################

## display, for each product, three separate variables (columns) containing
##  sales for 2016, 2017 and 2018
invoice_detailed %>%
     mutate(
          i.d.2016 = ifelse(year(invoicedate)==2016, invoice_detailed$amount, 0),
          i.d.2017 = ifelse(year(invoicedate)==2017, invoice_detailed$amount, 0),
          i.d.2018 = ifelse(year(invoicedate)==2018, invoice_detailed$amount, 0) ) %>%
     group_by(productname) %>%
     summarise(
          sales.2016 = sum(i.d.2016, na.rm=TRUE),
          sales.2017 = sum(i.d.2017, na.rm=TRUE),
          sales.2018 = sum(i.d.2018, na.rm=TRUE)          )
     

## display, for each product, three separate variables (columns) containing
##  sales for 2016, 2017 and 2018; also display a grand total row
rbind(
invoice_detailed %>%
     mutate(
          i.d.2016 = ifelse(year(invoicedate)==2016, invoice_detailed$amount, 0),
          i.d.2017 = ifelse(year(invoicedate)==2017, invoice_detailed$amount, 0),
          i.d.2018 = ifelse(year(invoicedate)==2018, invoice_detailed$amount, 0) ) %>%
     group_by(productname) %>%
     summarise(
          sales.2016 = sum(i.d.2016, na.rm=TRUE),
          sales.2017 = sum(i.d.2017, na.rm=TRUE),
          sales.2018 = sum(i.d.2018, na.rm=TRUE)   )     
, 
data.frame( productname =  'total',  
invoice_detailed %>%
     mutate(
          i.d.2016 = ifelse(year(invoicedate)==2016, invoice_detailed$amount, 0),
          i.d.2017 = ifelse(year(invoicedate)==2017, invoice_detailed$amount, 0),
          i.d.2018 = ifelse(year(invoicedate)==2018, invoice_detailed$amount, 0) ) %>%
     group_by(productname) %>%
     dplyr::summarise(
          sales.2016 = sum(i.d.2016, na.rm=TRUE),
          sales.2017 = sum(i.d.2017, na.rm=TRUE),
          sales.2018 = sum(i.d.2018, na.rm=TRUE) ) %>%
     summarise(
          sales.2016 = sum(sales.2016, na.rm=TRUE),
          sales.2017 = sum(sales.2017, na.rm=TRUE),
          sales.2018 = sum(sales.2018, na.rm=TRUE) ) )  
)
     





########################################################################
###      II.2     Reshaping/pivoting data with package `tidyr`       ### 
########################################################################

### Tidy data: Every column in a data frame represents a variable and 
###    every row represents an observation. 
###    This is also referred to as long format (as opposed to wide format).

# one can install `tidyr` separatedly...
# install.packages("tidyr")
# library(tidyr)

#... but, ir loads automatically when loading the `tidyverse` package
library(tidyverse)


########################################################################
###                          gather() / pivot_longer()
# with gather() data can be converted from a `wide` format to a 
#  `long` format

## Ex: Assessement "Databases 2" course taught in 2013-2014 at SIA 
##  master program is in a `wide` format
getwd()
library(readxl)
file.name <- "BD2_2013_SIA1_v2.xlsx"
assessment <- read_excel(file.name, sheet = 'centralizator', 
          col_names = TRUE, skip = 0) %>%
          mutate_if (str_detect(names(.), "\\."), as.numeric)
glimpse(assessment)
View(assessment)

## task: extract separately the general data about students (not important 
## for conversion, but for a better visibility)
assess_summary <- assessment %>%
     select (nr:echipa, nota, catalog)
View(assess_summary)

## now, we want that all assessment parts (`nota.t.1`, `bonus.t.1`, `nota.t.2`, ...)
## to be stored as rows (not as columns, as currently they are); 
## the column with name of assessment part will be `asess_part`
## the column with the (assessment) value will be `grade`
## conversion will be done only for columns between `nota.t.1` and `nota.pr.2`  
assess_details_long <- assessment %>%
     select (nr, nota.t.1:nota.pr.2) %>%
     gather (assess_part, grade, nota.t.1:nota.pr.2) %>%
     arrange (nr, assess_part)
glimpse(assess_details_long)
View(assess_details_long)


# recent versions of `tidyr` propose `pivot_longer` function
assess_details_longer <- assessment %>%
     select (nr, nota.t.1:nota.pr.2) %>%
     pivot_longer (-nr, names_to = "assess_part", values_to = "grade") %>%
     arrange (nr, assess_part)


# test if the result is identical
identical(assess_details_long, assess_details_longer)


##  Import montly earnings from the (Romanian) National Institute for Statistics
# install.packages ('htmltab')
library(htmltab)
url <- 'http://www.insse.ro/cms/ro/content/castiguri-salariale-din-1991-serie-lunara'
net_earning <- htmltab(doc = url, which = 1, encoding = "UTF-8")
glimpse(net_earning)
head(net_earning)
names(net_earning)[1] <- 'Year'


# backup this data frame, in case of the link will no longer be accesible
#save (net_earning, file = 'net_earning.RData')

# we have to remove the dots (separating the thousands) from the values
# we alse have to divide by 10000 all the values before 2005 (when
# `old` LEU was replaced by the new one, by cutting 4 zeros)

net_earning <- net_earning %>%
     mutate_all( ~ str_replace_all(., '\\.', '')) %>%
     mutate_all (as.numeric)
glimpse(net_earning)



# convert the data into the long format
net_earning_long <- net_earning %>%
     gather(Month, Net_Earn, -Year)
head(net_earning_long)
View(net_earning_long)

# now, just for playing, convert (Romanian) month names in integers
month_recoding <-  net_earning_long %>%
     distinct (Month) %>%
     mutate (Month_No = row_number() )
View(month_recoding)     


# now add a column with the month number
net_earning_long2 <- net_earning_long %>%
     inner_join(month_recoding) %>%
     arrange(Year, Month_No) %>%
     mutate (Net_Earn = if_else(coalesce(Year, 2018) < 2005, Net_Earn / 10000, Net_Earn))

glimpse(net_earning_long2)
View(net_earning_long2)


#... with `pivot_longer` instead of `gather`
net_earning_longer2 <- net_earning %>%
     pivot_longer (-Year, names_to = "Month", values_to = "Net_Earn") %>%
     inner_join(month_recoding) %>%    
     arrange(Year, Month_No) %>%
     mutate (Net_Earn = if_else(coalesce(Year, 2018) < 2005, Net_Earn / 10000, Net_Earn))
     

glimpse(net_earning_longer2)
View(net_earning_longer2)



###  Import the exchange rates published by National Bank of Romania (BNR)
# install.packges ('htmltab')
library(htmltab)
url <- 'http://www.bnr.ro/Exchange-rates-15192.aspx'
exchange_rates <- htmltab(doc = url, which = 1, encoding = "UTF-8")
head(exchange_rates)
View(exchange_rates)
names(exchange_rates)[1] <- 'Date'

# backup this data frame, in case of the link will no longer be accesible
# save (exchange_rates, file = 'exchange_rates.RData')


# Now, convert to the `long format`:
exchange_rates_long <- exchange_rates %>%
     gather(currency, exchange_rate, -Date)
View(exchange_rates_long)


#... with `pivot_longer` instead of `gather`
exchange_rates_longer <- exchange_rates %>%
     pivot_longer (-Date, names_to = "currency", values_to = "exchange_rate") 
     
View(exchange_rates_longer)




########################################################################
### Task:
### Display the number of NA values for each column of a data frame
### Ex: data frame `customer` (`chinook` data base)
########################################################################
# load data frames imported from the `chinook` database tables
load('chinook.RData')


# Note: I wouldn't recommend this solution when the data frame is very
# large, singe pivot_longer will get `n` x `m` rows
temp <- customer %>%
     mutate_all (as.character) %>%    # convert all variables into strings
     pivot_longer(everything(),
                  names_to = "variable", values_to = "value") %>%
     group_by(variable) %>%   # 
     summarise( n_of_NA = sum(is.na(value)), n_of_non_NA = sum(!is.na(value)))



########################################################################
###                      spread() / pivot_wider()
### with spread() data can be converted from a `long` format to a 
###  `wide` format; very useful for pivoting


## Display, for each product, three separate variables (columns) containing
##  sales for 2016, 2017 and 2018
View(invoice_detailed)
library(lubridate)

# solution using `spread`
sales_prods_2016_2018_spread <- invoice_detailed %>%
     filter (year(invoicedate) %in% c(2016, 2017, 2018)) %>%
     mutate (year = year(invoicedate)) %>%
     group_by(productname, year) %>%
     summarise (sales = sum(amount)) %>%
     spread(year, sales)
View(sales_prods_2016_2018_spread)


# solution using `pivot_wider`
sales_prods_2016_2018_wider <- invoice_detailed %>%
     filter (year(invoicedate) %in% c(2016, 2017, 2018)) %>%
     mutate (year = year(invoicedate)) %>%
     group_by(productname, year) %>%
     summarise (sales = sum(amount)) %>%
     pivot_wider(names_from = year, values_from = sales)     



## Display, for each product, separate variables (columns) containing
##  monthly sales for 2017  

# with `spread`
montly_sales_prods_2017_spread <- invoice_detailed %>%
     filter (year(invoicedate) == 2017) %>%
     mutate (month = month(invoicedate)) %>%
     group_by(productname, month) %>%
     summarise (sales = sum(amount)) %>%
     spread(month, sales, fill = 0)     # notice `fill = 0`

View(montly_sales_prods_2017_spread)

# with `pivot_wider`
montly_sales_prods_2017_wider <- invoice_detailed %>%
     filter (year(invoicedate) == 2017) %>%
     mutate (month = month(invoicedate)) %>%
     group_by(productname, month) %>%
     summarise (sales = sum(amount)) %>%
     pivot_wider(names_from = month, 
                 values_from = sales, values_fill = list(sales = 0))    # notice `values_fill = 0` 
     
View(montly_sales_prods_2017_wider)



## Display, for 2016, a pivot table with product sales  by each client
prod_client_sales_2016 <- invoice_detailed %>%
     filter (year(invoicedate) == 2016) %>%
     group_by(productname, customername) %>%
     summarise (sales = sum(amount)) %>%
     spread(customername, sales, fill = 0)     # notice `fill = 0`
prod_client_sales_2016



##  Display for each year starting with 2006 the monthly earnings, but with 
##     months displayed as numbers (not labels) 

#  We'll use the above created `net_earning_long2` data frame
View(net_earning_long2)
net_earning_wide2 <- net_earning_long2 %>%
     filter (Year >= 2006) %>%
     select (Year, Month_No, Net_Earn) %>%
     spread (Month_No, Net_Earn, fill = 0)
View(net_earning_wide2)


## For the exchange rates published by National Bank of Romania (BNR)
##   currently in data frame `exchange_rates_long`...
exchange_rates_long

##  Display the recent evolution of exchange rate for each currency 
exchange_rates_wide <- exchange_rates_long %>%
     spread (Date, exchange_rate)
exchange_rates_wide


########################################################################
###                     spread() and totals
### see `rowSum()`, `colSum`, `.` and `bind_rows` in the next queries 

## Display, for each product, three separate variables (columns) containing
##  sales for 2016, 2017 and 2018; add a total column and a total row
library(lubridate)
sales_prods_2016_2018_totals <- invoice_detailed %>%
     filter (year(invoicedate) %in% c(2016, 2017, 2018)) %>%
     mutate (year = paste0('year', year(invoicedate))) %>%
     group_by(productname, year) %>%
     summarise (sales = sum(amount)) %>%
     ungroup() %>%       # ungroup is necessary for `rowSums` and `colSums` to work
     spread(year, sales) %>%
     mutate(Total = rowSums(.[2:4])) %>%  # this the the column with totals
     bind_rows(.,      # `.` refers to the current result (data frame)
                       # to the current result content a total row will be added
          tibble(productname="Total", t(colSums(.[2:5]))))  # the total row
               # is build as a (one-row) data frame; `t` stands for `transpose`
View(sales_prods_2016_2018_totals)



## Display, for each product, separate variables (columns) containing
##  the monthly sales for 2017; add total row and column  
montly_sales_prods_2017_totals <- invoice_detailed %>%
     filter (year(invoicedate) == 2017) %>%
     mutate (month = paste0('month_', substring(100+month(invoicedate), 2) )) %>%
     group_by(productname, month) %>%
     summarise (sales = sum(amount)) %>%
     ungroup() %>%
     spread(month, sales, fill = 0)  %>%     
     mutate(Total = rowSums(.[2:ncol(.)])) %>%  # to total column
     bind_rows(., 
          data.frame(productname="Total",     # the total row
               t(colSums(.[2:ncol(.)])))) 

View(montly_sales_prods_2017_totals)


## Display, for 2016, a pivot table with product sales  by each client
library(stringr) # needed for function `str_replace_all`
prod_client_sales_2016_totals <- invoice_detailed %>%
     filter (year(invoicedate) == 2016) %>%
          # we'll replace spaces in the customer name with underscores
          # that's necessary because the customer names will be column names
          # in the final result
     mutate (customername = str_replace_all(customername, ' ', '_')) %>%
     group_by(productname, customername) %>%
     summarise (sales = sum(amount)) %>%
     ungroup() %>%
     spread(customername, sales, fill = 0) %>%   
     mutate(Total = rowSums(.[2:ncol(.)])) %>%
     bind_rows(., 
          data.frame(productname="Total", 
               t(colSums(.[2:ncol(.)])))) 

View(prod_client_sales_2016_totals)



########################################################################
###                             separate()
### separate() takes values inside a column and separates them.


assess_details_long <- assessment %>%
     select (nr, nota.t.1:nota.pr.2) %>%
     gather (assess_part, grade, nota.t.1:nota.pr.2) %>%
     arrange (nr, assess_part)
glimpse(assess_details_long)
assess_details_long


assess_details_even_longer <- assess_details_long %>%
     separate(assess_part, into = c("assess_type", 
                                    "hmw_proj", "part_no"), sep = "\\.")
              
View(assess_details_even_longer)


##  Exchange rates published by National Bank of Romania (BNR)
# install.packges ('htmltab')
library(htmltab)
url <- 'http://www.bnr.ro/Exchange-rates-15192.aspx'
## ... convert the data into the long format
exchange_rates_longer <- htmltab(doc = url, which = 1, encoding = "UTF-8") %>%
     gather(currency, exchange_rate, -Data) %>%
     separate(Data, into = c("day", "month", "year"), sep = "\\.")
     
View(exchange_rates_longer)




######################################################################
###                 III. (Slightly) more advanced stuff:           ###
###                 working with lists (in tidyverse)              ###
######################################################################

# clear the environment
rm(list = ls())


# load data frames loaded from the `chinook` database tables
load('chinook.RData')



######################################################################
###       An equivalent of SQL `list_agg` function                 ###

## Display, as a string, the track names for each Led Zeppelin album
album_track_list_LZ <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     select (albumid, title) %>%
     inner_join(track, by = 'albumid') %>%
     arrange (trackid) %>%
     group_by(title) %>%
     summarise( track_list = paste(name, collapse = '; '))
View(album_track_list_LZ)



######################################################################
###                    unnest() function                           ###

## Given data frame `album_track_list_LZ`, get a data frame in 
##     tidy format (title, track_no, track_name )

tidy_album_track_list_LZ <- album_track_list_LZ %>%
     # first, split in track list (track separator is `; `)
     mutate (track_list2 = str_split (track_list, '; ')) %>%
     # second, convert the list into a data frame row with `unnest`
     unnest (track_list2) %>%
     # add the track number for each album by a combination of
     #  `group_by` and `mutate`
     group_by(title) %>%
     mutate(track_no = row_number()) %>%
     # finally extract only variables of interest
     select (title, track_no, track_name = track_list2)



View(tidy_album_track_list_LZ)


######################################################################
###                    rowwise() option                           ###
###  (to be sure that a function applies on rows, not on columns  ###


## Display on separate column the first three tracks on each Led Zeppelin album
first3tracks_each_album_LZ <- album_track_list_LZ %>%
     # first, split in track list (track separator is `; `)
     mutate (track_list2 = str_split (track_list, '; ')) %>%
     rowwise() %>%
     summarise(title = title, first_track = track_list2[1], 
          second_track = track_list2[2], third_track = track_list2[3] )

View(first3tracks_each_album_LZ)



######################################################################
###                 list-column of data frames                     ###
###               nest() and unnest() functions                    ###
######################################################################


######################################################################
##  example taken from:
# https://blog.rstudio.org/2015/01/09/dplyr-0-4-0/
qs <- mtcars %>%
  group_by(cyl) %>%
  summarise(y = list(quantile(mpg)))

qs

# display a list-column content
list1 <- qs[1,]$y
list1

## Unnest input to collpase into rows
qs %>% tidyr::unnest(y)

## To extract individual elements into columns, wrap the result in rowwise()
## then use summarise()
qs %>% 
  rowwise() %>% 
  summarise(q25 = y[2], q75 = y[4])





######################################################################
###  get a data frame where each row describes an album,
###       and, for each album there is a column of type
###       data frame containing the album tracks (their name,
###       composes and duration)

# pay attention to `nest()` function
album_tracks__list <- artist %>%
     filter (name == 'Led Zeppelin') %>%
     inner_join(album) %>%
     select (albumid, title) %>%
     inner_join(track, by = 'albumid') %>%
     arrange (trackid) %>%
     transmute(title, name, composer, milliseconds) %>%
     group_by(title) %>%
     # add a track number
     mutate(track_no = row_number()) %>%
     nest(track_df = c(track_no, name, composer, milliseconds)) %>%
     ungroup()

# display (the data frame) tracks from the first album
album_tracks__list$track_df[1][[1]]    
     

# extract the track list (as data frame) for album 'IV'
temp <- album_tracks__list %>%
     filter (title == 'IV') %>%
     unnest(track_df)
View(temp)


     
     
     
### Note:
# list processing is considerably easier with `purrr` package   
# (`map` family ) - see subsequent script/presentation on functional
# programming   

