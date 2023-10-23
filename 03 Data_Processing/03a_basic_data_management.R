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
########         3a. Data Management: Basic operations in base R      ######
### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 23.10.2023

library(tidyverse) # for `glimpse` and `read_tsv`
library(readxl)
library(lubridate)


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


#####################################################################################
###                          O. ...Before you begin...                            ###
#####################################################################################

# Some of the following operations (e.g. adding, renaming, selecting columns)
#  could be easier achieved using the package 
#    `dplyr` - see script 03b...


#####################################################################################
###                      I. Data processsing for SQL users                        ###
#####################################################################################

# you can use SQL queries not only on data stored in relational/SQL databases, 
#   but also on data frames (imported from .csv, .xlsx, ... files)
# package needed: `sqldf` (sql in data frames)
# install.packages("sqldf")
library(sqldf)
# as PostgreSQL driver creates lots of headaches, we'll use SQLite driver 
#    (even if you do not have installed SQLite on our computers)
options(sqldf.driver = "SQLite")
# options(sqldf.driver = "PostgreSQL")


############################################################################
# Fuel Economy dataset(s) - data about fuel consumption, pollution etc
#    for car models (updated regularly) - see
# `https://www.fueleconomy.gov/feg/download.shtml`
#  
# first, we downloaded the 2018 report as a text file...
#  https://www.fueleconomy.gov/feg/EPAGreenGuide/txt/all_alpha_18.txt
# into the current working directory ;
# now, we'll import into r
fuel_economy_2018 <- read_tsv("all_alpha_18.txt") 
names(fuel_economy_2018)
glimpse(fuel_economy_2018)


## Extract information about gas consumption and pollution parameters
##   for `AUDI A6` model
audi_a6 <- sqldf(
     "select * from fuel_economy_2018 where Model = 'AUDI A6'") 
View(audi_a6)

## Extract information about gas consumption and pollution parameters
##   for all `HONDA` models
honda <- sqldf(
     "select * from fuel_economy_2018 where Model LIKE 'HONDA%' ORDER BY Model") 
View(honda)




#####################################################################################
###                 II. Basic operations with variables (rows) in base R          ###
#####################################################################################

# In this section we deal mainly with some of the most frequent operations with data:
# - add columns to a data frame
# - remove columns in a data frame
# - recode (change the values of a subset or observations) a column
# - change the names of the data frame columns
# - change the order of the data frame columns


#####################################################################################
###                        II.a. Adding columns/variables                         ###
#####################################################################################
# first, clear the session environment
rm(list = ls())


############################################################################
#              (Anonymized) FEAA students for 2014-2015academic year
getwd()
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- readxl::read_excel(file, sheet = 1, col_names = TRUE, skip = 0)

# display the data frame structure
str(studs)
glimpse(studs)

# display the first rows of the data frame
head(studs)

# add a new variable (column) called `FULL_NAME` by concatenating the variables
#   `FAMILY_NAME`, `MIDDLE_NAME` and `FIRST_NAME`
studs$FULL_NAME <- paste(studs$FAMILY_NAME, studs$MIDDLE_NAME, studs$FIRST_NAME)
glimpse(studs)

############################################################################
# Fuel Economy dataset(s) - data about fuel consumption, pollution etc
#    for car models (updated regularly) - see
# `https://www.fueleconomy.gov/feg/download.shtml`
#  
# first, we downloaded the 2018 report as a text file...
#  https://www.fueleconomy.gov/feg/EPAGreenGuide/txt/all_alpha_18.txt
# into the current working directory ;
# now, we'll import into r
fuel_economy_2018 <- read_tsv("all_alpha_18.txt") 
names(fuel_economy_2018)
glimpse(fuel_economy_2018)

# Add two new variables for expressing fuel consumption in
#    `liters per 100 kilometers` (miles per gallon is difficult to
#         grasp for Europeans)
# 
# The formula is:   
# liters per 100 kilometers = 235.214583333333 ÷ (mile per gallon)
fuel_economy_2018$cty_l100km <- round(235.214583333333 / 
     as.numeric(fuel_economy_2018$`City MPG`),2) 
fuel_economy_2018$hwy_l100km <-  round(235.214583333333 / 
    as.numeric(fuel_economy_2018$`Hwy MPG`),2)
fuel_economy_2018$combined_l100km <- round(235.214583333333 / 
     as.numeric(fuel_economy_2018$`Cmb MPG`),2)

glimpse(fuel_economy_2018)



#####################################################################################
###                               Sales
load (file = 'sales.RData')

# ... taken the data frame `invoice_detailed`
glimpse(invoice_detailed)

# extract year, month, day, the day of the week and the invoice line VAT for each 
# invoice in separate columns/variables; 
# for that we need package `lubridate` (for functions `year`, `month`, `day`,
#   `week`)
# install.packages('lubridate')
library(lubridate)
invoice_detailed$year <- year(invoice_detailed$invoicedate)
invoice_detailed$month <- month(invoice_detailed$invoicedate)
invoice_detailed$day <- day(invoice_detailed$invoicedate)
invoice_detailed$weekDay <- wday(invoice_detailed$invoicedate, label=T)
invoice_detailed$rowVAT <- invoice_detailed$amount - invoice_detailed$amountwithoutvat

glimpse(invoice_detailed)


#####################################################################################
###                        II.b. Removing columns/variables                       ###
#####################################################################################

# The simplest way is just to set the variable on the NULL (meta)value

# Example:
# In data frame `fuel_economy_2018` remove variables `Stnd`, `Stnd Description`
#   and `Underhood ID`
#   
glimpse(fuel_economy_2018)
fuel_economy_2018$Stnd <- NULL
fuel_economy_2018$`Stnd Description` <- NULL
fuel_economy_2018$`Underhood ID` <- NULL
# see the structure after the variables were removed
glimpse(fuel_economy_2018)


#####################################################################################
###                        II.c. Recoding columns/variables                       ###
#####################################################################################

#####################################################################################
###                           FEAA students
glimpse(studs)
# display variabile `YEAR_OF_STUDY` values
studs$YEAR_OF_STUDY

# task: change the roman numbers (I, II, III) with the arabic equivalents (1, 2 or 3)
studs$YEAR_OF_STUDY <- ifelse(studs$YEAR_OF_STUDY == 'I', 1, studs$YEAR_OF_STUDY)
studs$YEAR_OF_STUDY <- ifelse(studs$YEAR_OF_STUDY == 'II', 2, studs$YEAR_OF_STUDY)
studs$YEAR_OF_STUDY <- ifelse(studs$YEAR_OF_STUDY == 'III', 3, studs$YEAR_OF_STUDY)

studs$YEAR_OF_STUDY

# convert the variable data type into `integer`
studs$YEAR_OF_STUDY <- as.integer(studs$YEAR_OF_STUDY)
glimpse(studs)


#####################################################################################
###                            II.d.  Renaming variables                          ###
#####################################################################################

# Task: Rename the variable `YEAR_OF_STUDY` (in data frame `studs`) into `STUDY_YEAR`
 
# First, display the variable name in the dataframe
names(studs)
# `YEAR_OF_STUDY` is the 3rd variable of the data frame

# now, proceed to the name changing
names(studs) [3] <- 'STUDY_YEAR' 

# check the new names
names(studs)


#####################################################################################
###                       II.e.  Change order of the columns                      ###
#####################################################################################

names(contacts)
head(contacts)

# Task: change the variables (columns) position in the data frame:
# instead of ("personalcode", "customerid",  "position"), the order must be:
#   ("customerid",  "position", "personalcode")
contacts <- contacts [c("customerid",  "position", "personalcode")]
head(contacts)



#####################################################################################
###                III. Subsetting, sorting, sampling in base R                   ###
#####################################################################################

# In this section we deal mainly with some of the most frequent operations with data:
# - filter the rows (observations, records) of a data frame
# - sort the observations
# - extract distinct values of a column
# - extract a random sample of a variable values

#####################################################################################
###          III.1. Subsetting data frames using `[ ]` by rows/columns index        ###
#####################################################################################

# Given the data frame `people`:
str(people)
people

# Extract the first row (observation) in the data frame
people[1, ] # this is extract only the first row, but all the columns

# Extract the first three rows (observations) in the data frame
people[1:3, ] 

# Extract observations 1, 3, 4 and 7 in the data frame
people[c(1, 3, 4, 7), ] 

# Extract all observation except the first two
people [ -(1:2), ]

# Extract all observation except the first two and the fifth
people [ - c(1:2, 5), ]

# Extract the first variable values, as a vector
temp <- people[, 1]
temp
class(temp)

# Extract the first variable values, as a data frame column
temp <- people[, 1, drop = F]
temp
class(temp)


## When extracting more then one column, the result will be a data frame

# Extract variables 2 and 3 in the data frame
temp <- people [, 2:3]
temp
class(temp)

# Extract variables 2, 3, and 5 for the first five rows (observations)
people [1:5, c(2:3, 5)]



#####################################################################################
###     III.2. Subsetting data frames with `[ ]` by variable names and filters    ###
#####################################################################################

# Extract as vector the values of variable `lastname` for the first five rows (observations)
people [1:5, ]$lastname
# ...or
people [1:5, 'lastname']

# Extract as a (single columned) data frame the values of variable `lastname` 
#    for the first five rows (observations)
people [1:5, 'lastname', drop = F]

# Extract variables `lastname`, `firstname` and `address` for the first five rows (observations)
people [1:5, c('lastname', 'firstname', 'address')]

# Display the row numbers of observations describing females
which(people$genre == 'F')

# Display the row content of observations describing females
people[which(people$genre == 'F'), ]

# Extract variables `lastname`, `firstname` and `address` only for the females
people [people$genre == 'F', c('lastname', 'firstname', 'address')]


# Extract variables `lastname`, `firstname` and `address` only for the females
# for which the office phone is known
people [people$genre == 'F' & !is.na(people$officephone), 
        c('lastname', 'firstname', 'address')]

# Extract, for females, only the variables whose name contains `phone`
library(stringr) # we need this package for `str_detect` function
people[ people$genre == 'F', str_detect(names(people), 'phone')]



#####################################################################################
###            III.3. Subsetting data frames using `subset` function              ###
#####################################################################################

# Extract variables `lastname`, `firstname` and `address` only for the females
subset(people, genre == 'F', select = c('lastname', 'firstname', 'address'))

# Extract all columns except `email` for females
subset(people, genre == 'F', select =  -c( email ))

# Extract, for females, all columns except the three phone numbers 
subset(people, genre == 'F', select =  -c(homephone, officephone, mobilephone ))
# ... or
subset(people, genre == 'F', select = personalcode:email)

# Extract, for females, only the variables whose name contains `phone`
library(stringr) # we need this package for `str_detect` function
subset(people, genre == 'F', select = c(str_detect(names(people), 'phone')))



#####################################################################################
###                             III.4. Sorting data                               ###
#####################################################################################

# sort people by their last name
people_ord1 <- people[(order(people$lastname)),]
people
people_ord1

# sort people by their genre and their last name
people_ord2 <- people[(order(people$genre, people$lastname)),]
people
people_ord2


#####################################################################################
###                      III.5. Identical/distinct values                         ###
#####################################################################################

# Check if there are columns with identical names
duplicated(colnames(invoice_detailed))

# Check if the two columns are identical (they have the some values in all of the rows)
identical(invoice_detailed$invoicerownumber, invoice_detailed$productid)

# Check if there are duplicated rows in data frame "invoice_detailed"
invoice_detailed[duplicated(invoice_detailed), ]

# Check if there are duplicated values of productname in data frame "invoicerownumbers"
invoice_detailed$invoicerownumber
duplicated(invoice_detailed$invoicerownumber)

# Remove the duplicate values
invoice.numbers <- invoice_detailed$invoiceno
invoice.numbers
invoice.numbers <- unique(invoice.numbers)
invoice.numbers
# ...or
invoice.numbers <- invoice_detailed$invoiceno
invoice.numbers
invoice.numbers <- invoice.numbers[!duplicated(invoice.numbers)]
invoice.numbers



#####################################################################################
###         III.6. Extract sample (random) values from data frames                ###
#####################################################################################

# Extract randomly a `countyname`  value (data frame `counties`)
sample(counties$countyname, 1)
sample(counties$countyname, 1)
sample(counties$countyname, 1)

# Extract randomly five customers without repetition (one customer cannot be extracted
#   more than once)
sample(customers$customername, 5, replace = FALSE)

# Extract randomly five customers with possible repetitions (one customer can be extracted
#   more than once)
sample(customers$customername, 5, replace = TRUE)


# Extact a random sample of 5 invoices (from all invoices)
sample_5invoices <- invoices[sample(1:nrow(invoices), 5, replace=FALSE),]
sample_5invoices


