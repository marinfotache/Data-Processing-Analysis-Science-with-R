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
###                    2a. Data Input from Most Common Sources           ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/02%20Basic%20Data%20Import%20and%20Export/02a_basic_data_input_output.pptx
############################################################################
## last update: 2022-11-05


sessionInfo()

# install.packages('tidyverse')
library(tidyverse)


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




## remove objects in the current session environment.
rm(list = ls())



############################################################################
###            1. Load data previously saved as .RData file 		   ###
############################################################################

# load the file containing all the data frames associated to `chinook` database tables
getwd()
load(file='chinook.RData')


# to choose interactively the file to be loaded...
load(file.choose())

## remove (almost) everything in the working environment.
rm(list = ls())



############################################################################
###              		2. Data input from the keyboard			   ###
############################################################################

# create a data frame from scratch
df1 <- data.frame(age=numeric(0), gender=character(0), weight=numeric(0), 
          stringsAsFactors = F)
class(df1)

#... or (preferred solution)
library(tidyverse)
df2 <- tibble(age=numeric(0), gender=character(0), weight=numeric(0))
class(df2)

# you can create an emply data frame / tibble with `tibble` function
#  in `tibble` package (package `tibble` is included in package `tidyverse`)
df3 <- tibble()


# edit interactively the data set
df1 <- edit(df1)
# or
fix(df1)

# display data frame content
df1

# remove the data frame from the workspace
rm(df1)

# see what's happen in the case of...
df3 <- edit(df3)


## new (2020-08) - package `DataEditR`
## https://github.com/DillonHammill/DataEditR
#install.packages('devtools')
#library(devtools)
#install_github("DillonHammill/DataEditR")

# since 2021 the package is available on CRAN
#install.packages('DataEditR')

# Load required package
library(DataEditR)

# Save output to R object & csv file
mtcars_new <- data_edit(mtcars,
                        save_as = "mtcars_new.csv")




############################################################################
###                	3. Data input from clipboard				        ###
############################################################################
# 
# If you've got just a small section of data already in a table -- a spreadsheet, 
# say, or a Web HTML table -- you can copy (Control+C ) those data to your Windows 
# clipboard and import them into R.


# On Windows systems the command below handles clipboard data with a header 
# row that is separated by tabs, and stores the data in a data frame (x):
x <- read.table(file = "clipboard", sep="\t", header=TRUE)

# On a Mac, the pipe ("pbpaste") function will access data you've 
#  copied with command-c, so this will do the equivalent of the previous 
#  Windows command:
# copy without header
x <- read.table(pipe("pbpaste"), sep="\t", header=FALSE)
x
# copy with header
y <- read.table(pipe("pbpaste"), sep="\t", header=TRUE)
y

# clear the environment (delete objects in the current session workspace)
rm(list = ls())



############################################################################
###               4. Data input from local delimited text files		   ###
############################################################################

# package `readr` is part of the `tidyverse`
# require(readr)

############################################################################
##   Data frame "births2006" is contained in the text file "births2006.txt" 
##   which was created based on package `nutshell`
#    Delimitator in the source file is Tab (\t)

## Two functions (of package `readr`) can be used
# `read_delim`...
births2006 <- readr::read_delim('births2006_package_nutshell.txt', '\t', 
     progress = interactive())

# display the data frame content
View(births2006)

# display the data frame stucture
str(births2006)

# display the data frame stucture with `glimpse` (tidyverse)
glimpse(births2006)


head(births2006)

#  as the text file is tab-delimited, one can use `read_tsv`` 
births2006 <- read_tsv('births2006_package_nutshell.txt', progress = interactive())
glimpse(births2006)
head(births2006)

# copy the attribute names into a vector
df_names <- names(births2006)
print(df_names)
class(df_names)

# display some of the attribute names
df_names[1]
df_names[5] <- "TBO"
df_names
names(births2006)[5] <- "TBO"

# display the number of attributes
length(df_names)

# ... this is equivalent to
ncol(births2006)

# ...playing around with vector indices
df_names[length(df_names)]
df_names[1:3]
df_names[(length(df_names)-2):length(df_names)]
df_names[c(1:3, 7, (length(df_names)-2):length(df_names))]

# Note: progress = interactive() display the progress bar that might be not
#    visible, as the data set is relatively small


############################################################################
# Import a dataset from Dragos Cogean's Ph.D. thesis which compares two cloud 
#  database services, Mongo and MySQL; data sources are the tab delimited files 

# suggested solution - using function  `read_tsv` from package `readr`
InsertMongoALL <- read_tsv("DragosCogean__InsertMongo_ALL.txt") 
InsertMySQLALL <- read_tsv("DragosCogean__InsertMySQL_ALL.txt") 
ReadMongoALL <- read_tsv("DragosCogean__ReadMongo_ALL.txt") 
ReadMySQLALL <- read_tsv("DragosCogean__ReadMySQL_ALL.txt") 


# clear the environment (delete objects in the current session workspace)
ls()
rm(list = ls())
ls()


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



############################################################################
###        5. Data import from delimited text files on the web   	   ###
############################################################################

# clear the environment
rm(list = ls())


############################################################################
# Fuel Economy dataset 

# instead of dowloading the text file, then importing it (as we did
#    a few line above), we can import the text file directly into R

fuel_economy_2018 <- read_tsv("https://www.fueleconomy.gov/feg/EPAGreenGuide/txt/all_alpha_18.txt") 

#  take care, some web links might be `dead` after a while (hopefully it is not
#  the case of `fueleconomy.gov`);
#  when you want to keep a copy of imported web files, you have to save it locally;
#  see section `13. Export R Data in other formats`



############################################################################
###     6. Data input from local comma separated values (CSV)  files     ###
############################################################################

## clear the memory
rm(list = ls())

## 

############################################################################
#                             Romanian cities & towns 
#   This data set contains data about Romanian cities/towns/villages, their
#   county and region, geo-coordinates and the number of inhabitants in 2002
#  we could not identify the author of this file, but we keep the original 
#  file name `orase_cipy_ro(autor neidentificat).txt`

# function `read_csv` from package `readr` deals with .csv files

city_town_village_2002 <- read_csv("orase_cipy_ro(autor neidentificat).txt")

# function `read_delim` from package `readr` deals also with .csv files
city_town_village_2002 <- read_delim("orase_cipy_ro(autor neidentificat).txt", delim = ',')


############################################################################
#    Import one dataset from Irina Dan's Ph.D. thesis concerning a study 
#        of using e-documents in companies;  
#   The data source is a CSV (Comma Separated Values) file - "companyinfo.csv";
#         but, instead of "," (comma), the values are separated by ";" (semicolon)

# function `read_delim` from package `readr` deals alsoo with .csv files
comp <- readr::read_delim("IrinaDan__companyinfo.csv", ";")

# display some information about the data frame
str(comp)
# display first six observations
head(comp)
# summary
summary(comp)

# glimpse (from tidyverse)
glimpse(comp)

## clear the memory
rm(list = ls())



############################################################################
###          7.    Data import from CSV files on the web   	             ###
############################################################################

############################################################################
#                        Smoking data set
# http://www.cyclismo.org/tutorial/R/_static/smoker.csv 
# origin: Moore and McCabe, Introduction to the Practice of Statistics
# 356 people have been polled on their smoking status (Smoke) and their 
# socioeconomic status (SES). 
# For each person it was determined whether or not they are:
#    * current smokers, 
#    * former smokers, or 
#    *have never smoked. 
# Also, for each person their socioeconomic status was determined 
#    * low, 
#    * middle, or 
#    * high. 
smoker <- readr::read_csv("http://www.cyclismo.org/tutorial/R/_static/smoker.csv")
head(smoker)
glimpse(smoker)



############################################################################
#         Import web files in two steps: first download and then read
# when a data set is large, instead of the direct import: ...
dat.csv <- read_csv("http://www.cyclismo.org/tutorial/R/_static/smoker.csv")
head(dat.csv)
glimpse(dat.csv)

# ... one can proceed in two steps:
# 1. download the file
download.file("http://www.cyclismo.org/tutorial/R/_static/smoker.csv", destfile="data.csv")
# 2. read the downloaded file
dat.csv <- read_csv("data.csv")
head(dat.csv)
glimpse(dat.csv)

# see also
# http://cran.r-project.org/web/packages/downloader/index.html


## A more relevant example, since the file size is larger (56MB):
## Romanian Baccalaureate's results - summer 2017 (national level)
# 1. download the file
download.file("http://data.gov.ro/dataset/cb54fa0b-4d8c-4cef-b0d9-fe80e0c99743/resource/2b8d2567-2633-422a-a98b-4fb6cd9c0b09/download/2017-09-25-date-deschise-2017-i.csv", 
               destfile="bac_ro_2017.csv")
# 2. read the downloaded file
bac_ro_2017 <- read_csv("bac_ro_2017.csv")

## clear the memory
rm(list = ls())



############################################################################
### 	                     8. Read data from Excel files           	   ###
############################################################################
# the best package (fast, whithout dependencies) seems to be "readxl"
#install.packages('readxl')
# http://blog.rstudio.org/2015/04/15/readxl-0-1-0/
library(readxl)


############################################################################
# Fuel Economy dataset(s) - data about fuel consumption, pollution etc
#    for car models (updated regularly) - see
# `https://www.fueleconomy.gov/feg/download.shtml`
#  
# Data are also available as `.xlsx` files.
# First, we downloaded the 2018 report as a .xlsx file...
#  https://www.fueleconomy.gov/feg/EPAGreenGuide/txt/all_alpha_18.xlsx
# into the current working directory ;
# now, we'll import into r
fuel_economy <- read_excel("all_alpha_18.xlsx", sheet = 1, col_names = TRUE, skip = 0)
names(fuel_economy)
glimpse(fuel_economy)



############################################################################
#    Import the FEAA students for 2014-2015 (anonymized) academic year
getwd()
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
str(studs)
glimpse(studs)
head(studs)



##################################################################################
#              Import multiple sheets of the same .xlsx file
# all the tables in the famous `Northwind` database are worksheets in 
#    a `northwind.xlsx` files (stored into the current working directory)
file_name <- "northwind.xlsx"

# read `categories` worksheet (which is the first in the file)
categories <- read_excel(file_name, sheet = 1, col_names = TRUE, skip = 0)

# if we do not remember the worksheet position, we can refer an worksheet
#    by its name
orders <- read_excel(file_name, sheet = "orders", col_names = TRUE, skip = 0)

# display the worksheets names in a .xls(x) file
excel_sheets(file_name)

# it is possible to read all the worksheets in a single step, using "lapply"
#  the result will be stored in a list
#  (we'll discuss about the `apply family` later in the course)
big_list <- lapply(excel_sheets(file_name), read_excel, path = file_name)

categories_v2 <- big_list[[1]]


## clear the working environment.
rm(list = ls())



#################################################################################
###                  9. Import data from PostgreSQL databases			   ###
#################################################################################

# this section commands work only if you previously created and populated the 
#     databases on the PostgreSQL server installed on your system
#     (of course, connection data could vary in case of remote database servers
#     or other databases, tables and attributes)  

# install.packages('RPostgres')
library(RPostgres)

#################################################################################
##              Import data from PostgreSQL database "sales" 

#    Notice: the name of the database varies on each of your computers;
#              you must change database name, user name and password accordingly;
#          All the databases were created on the local server (laptop)    

# if you want to create and populate the `sales` database in PostgreSQL 
#    on your laptop, you can download and run the following two scripts:
#    https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/02%20Basic%20Data%20Import%20and%20Export/00a_1_creating_tables__sales_PostgreSQL.sql
#    https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/02%20Basic%20Data%20Import%20and%20Export/00a_2_populating_tables__sales_PostgreSQL.sql
# Pay attention to the server properties (Hostname/address, Port, Username)
#    and the database name (should be `sales``)


###  A. Open a connection (after loading the package and the driver)


## On Windows systems, PostgreSQL database service must already be started
con <- dbConnect(RPostgres::Postgres(), dbname="sales", user="postgres", 
                 host = 'localhost', password="postgres")

# On Mac OS
con <- dbConnect(RPostgres::Postgres(), host='localhost', port='5434', 
                 dbname='sales', user='postgres', password='postgres')


###  B. Display the table names in PostgreSQL database 

tables <- dbGetQuery(con, 
     "select table_name from information_schema.tables where table_schema = 'public'")
tables


###  C. Import, one by one, entire tables from PostgreSQL database
counties <- dbReadTable(con, "counties" )
str(counties)
head(counties)

postcodes <- dbReadTable(con, "postcodes" )
customers <- dbReadTable(con, "customers" )
products <- dbReadTable(con, "products" )
invoices <- dbReadTable(con, "invoices" )
invoice_details <- dbReadTable(con, "invoice_details" )
receipts <- dbReadTable(con, "receipts" )
receipt_details <- dbReadTable(con, "receipt_details" )


###  D. Query (from R) the PostgreSQL database and import the query result as a data frame
invoice_detailed <- dbGetQuery(con, 
     "SELECT i.invoiceNo, invoiceDate, i.customerId, 
          customerName, place, countyName, region,
          comments, invoiceRowNumber, i_d.productId,
	     productName, unitOfMeasurement, category, 
	     quantity, unitPrice, quantity * unitPrice AS amountWithoutVAT, 
	     quantity * unitPrice * (1 + VATPercent) AS amount
     FROM invoices i 
	     INNER JOIN invoice_details i_d ON i.invoiceNo = i_d.invoiceNo
	     INNER JOIN products p ON i_d.productId = p.productId
	     INNER JOIN customers c ON i.customerid = c.customerid
	     INNER JOIN postcodes pc ON c.postCode = pc.postCode
	     INNER JOIN counties ON pc.countyCode =counties.countyCode
     ORDER BY i.invoiceNo, invoiceRowNumber")

# display the result structure
View(invoice_detailed)

# display first rows of the result
head(invoice_detailed,5)

# two other query results
invoices_1 <- dbGetQuery(con, 
     "WITH inv AS (
          SELECT invoiceNo, TRUNC(SUM(quantity * unitPrice),2) AS amountWithoutVAT, 
		     TRUNC(SUM(quantity * unitPrice * (1 + VATPercent)),2) AS amount
	     FROM invoice_details i_d 
		     INNER JOIN products p ON i_d.productId = p.productId
	     GROUP BY invoiceNo
	     ORDER BY invoiceNo)
     SELECT i.invoiceNo, invoiceDate, amountWithoutVAT, amount,
	     i.customerId, customerName, place, countyName, region,
	     comments
     FROM invoices i 
	     INNER JOIN inv ON i.invoiceNo = inv.invoiceNo
	     INNER JOIN customers c ON i.customerid = c.customerid
	     INNER JOIN postcodes pc ON c.postCode = pc.postCode
	     INNER JOIN counties ON pc.countyCode =counties.countyCode
ORDER BY 1")
View(invoices_1)

daily_sales <- dbGetQuery(con, 
     "WITH inv AS (
          SELECT invoiceNo, TRUNC(SUM(quantity * unitPrice),2) AS amountWithoutVAT, 
     	     TRUNC(SUM(quantity * unitPrice * (1 + VATPercent)),2) AS amount
	     FROM invoice_details i_d 
		     INNER JOIN products p ON i_d.productId = p.productId
	     GROUP BY invoiceNo
	     ORDER BY invoiceNo)
     SELECT invoiceDate, SUM(amountWithoutVAT) AS salesWithoutVAT, 
          SUM(amount) AS sales
     FROM invoices i 
	     INNER JOIN inv ON i.invoiceNo = inv.invoiceNo
     GROUP BY invoiceDate
     ORDER BY 1"
     )
View(daily_sales)


## clear the working environment and restore the connection to the database
rm(list = ls())
drv <- dbDriver("PostgreSQL")
# On Mac OS
con <- dbConnect(drv, host='localhost', port='5433', dbname='sales',
                 user='postgres', password='postgres')
tables <- dbGetQuery(con, 
     "select table_name from information_schema.tables where table_schema = 'public'")

# re-compute `invoice_detailed`
invoice_detailed <- dbGetQuery(con, 
     "SELECT i.invoiceNo, invoiceDate, i.customerId, 
          customerName, place, countyName, region,
          comments, invoiceRowNumber, i_d.productId,
	     productName, unitOfMeasurement, category, 
	     quantity, unitPrice, quantity * unitPrice AS amountWithoutVAT, 
	     quantity * unitPrice * (1 + VATPercent) AS amount
     FROM invoices i 
	     INNER JOIN invoice_details i_d ON i.invoiceNo = i_d.invoiceNo
	     INNER JOIN products p ON i_d.productId = p.productId
	     INNER JOIN customers c ON i.customerid = c.customerid
	     INNER JOIN postcodes pc ON c.postCode = pc.postCode
	     INNER JOIN counties ON pc.countyCode =counties.countyCode
     ORDER BY i.invoiceNo, invoiceRowNumber")

###  E. Import (as a data frame) each PostgreSQL database table with 
###       just a loop
for (i in 1:nrow(tables)) {
     # extract data from table in PostgreSQL
     temp <- dbGetQuery(con, paste("select * from ", tables[i,1], sep=""))
     # create the data frame
     assign(tables[i,1], temp)
}     


###  F. close all PostgreSQL connections 
for (connection in dbListConnections(drv) ) {
  dbDisconnect(connection)
}

###  G. Frees all the resources on the driver
dbUnloadDriver(drv)

###  H. Save all of the data frames in the current session/environment as 
###  an .RData file
###  

# # remove non-useful objects (keep only the datavase tables)
rm(con, drv, temp, i, tables, connection )

getwd()
# now, save all the data frames corresponding the the database tables
save.image(file = 'sales.RData')


## clear the memory
rm(list = ls())




#################################################################################
###       10. Data input from other statistical packages: SPSS and Stata 	   ###
#################################################################################

#################################################################################
## 	"classical" package for importing data from various sources was  `foreign`
#install.packages("foreign")
library(foreign)

## import a Stata file
## prior I downloaded a Stata data file (states.dta) into the working directory
states <- read.dta("states.dta")
head(states)


## import a SPSS file
# file p004.sav downloaded from "http://www.ats.ucla.edu/stat/spss/examples/chp/chpspss_dl.htm"
spss1 <- read.spss("p004.sav", 
                   use.value.labels = TRUE, to.data.frame = TRUE)
head(spss1)


#################################################################################
#    Package "haven" 
# http://www.r-bloggers.com/haven-0-1-0/
# install.packages('haven')
library(haven)
# Functions:
#    * "read_dta" or "read_stata" reads and writes Stata DTA files.
#    * "read_sas" reads SAS files.
#    * "read_spss" reads SPSS files.

## import a SPSS file
spss1 <- read_spss("p004.sav")
head(spss1)

# import a Stata file...
# ...with `read_dta` 
states <- read_dta("states.dta")
head(states)
# ... or `read_stata`
states <- read_stata("states.dta")
head(states)

## import a SAS file 
sas1 <- read_sas('music.sas7bdat')


#################################################################################
#   Package "rio" (also since 2015)
#     http://cran.r-project.org/web/packages/rio/vignettes/rio.html
# install.packages('rio')
library(rio)

# install_formats()

# the package is extremely rich in supported formats, including Excel (.xlsx files) 

## import a SAS file in "DataSets/foreign"
sas1 <- import('music.sas7bdat')

## read a SPSS file
spss1 <- import("p004.sav")
head(spss1)

# immport a Stata file
states <- import("states.dta")
head(states)


## remove (almost) everything in the working environment.
rm(list = ls())



#################################################################################
###                   11. Data input from dBase/FoxPro files 	             ###
#################################################################################
## .DBF files, created with dBase or FoxPro, RBase, ... can be imported
## with packages `foreign`
library(foreign)

# a DBF file dowloaded from `http://colectaredate.insse.ro`
siruta_t <- read.dbf("SIRUTA_T.DBF")
head(siruta_t)

## package "haven" cannot import .dbf files...
##
## ... but package `rio` can
library(rio)
siruta_t <- import("SIRUTA_T.DBF")


## remove (almost) everything in the working environment.
rm(list = ls())



#################################################################################
###       12.	Saving/exporting data in R format (.RData files)  		   ###
#################################################################################

#################################################################################
#               Save objects in the R binary format - fully optimized; 
# also in a single file one can save multiple datasets and R objects


# we already saw how to save all the objects in the current session/workspace
#   as an `.RData` file
# save.image(file = 'sales.RData')

# in subsequent R sessions, all the objects saved in the `.RData` file will be 
# loaded at once with `load` function
# load("sales.RData")



#################################################################################
#       Save all the current session objects in the R binary format 
#                   in a single file 
save.image(file = paste0('all_objects_', Sys.time(), '.RData'))



#################################################################################
###                        13. Export R Data in other formats 
#################################################################################

#################################################################################
##        Save R data as a delimited text files or a .csv files

#  One of the fastest way to save a delimited text file or a .csv file
#         is with function `write_delim` in package `readr`

library(readr)

# we previously dowloaded the fuel economy data set as a tab-delimited text file...
fuel_economy_2018 <- read_tsv("https://www.fueleconomy.gov/feg/EPAGreenGuide/txt/all_alpha_18.txt") 

# now, we want to save it as a tab delimited file
readr::write_tsv(fuel_economy_2018, 'fuel2018.tsv')

# now, we want to save it as a text delimited file, but with `;` as delimiter
readr::write_delim(fuel_economy_2018, 'fuel2018.txt', delim = '|')

# For saving as `.csv` file, we can use `readr` package...
readr::write_csv(fuel_economy_2018, 'fuel2018.csv')

 # ..or with function `export` in package `rio`
require(rio)
rio::export(fuel_economy_2018, file = "fuel2018v2.csv", format = 'csv')

# examine the .csv files created by the functions above


#################################################################################
##             Save R data in Stata, SPSS, SAS, ... formats

## package `foreign` provides function for both importing (see section 10 above)
##   and exporting data from/to Stata, SPSS and SAS
require(foreign)

# we imported the SPSS file
# p004.sav (downloaded from "http://www.ats.ucla.edu/stat/spss/examples/chp/chpspss_dl.htm")
spss1 <- read.spss("p004.sav", use.value.labels = TRUE, to.data.frame = TRUE)

# now export the data frame `spss1` as an SPSS file
write.dta(spss1, file = "p004_copy.sav")

# we already import a Stata file
states <- read.dta("states.dta")
head(states)
# now, export a data frame to a Stata file
write.dta(states, file = "states_copy.dta")


## package `rio` provides function `export` which quite versatile
require(rio)

# save a data frame as a tab delimited text file
export(spss1, file = "p004_copy2.txt", sep = "\t")

### save a dataframe as a dta file 
export(spss1, file = "spss1_2.dta")



#################################################################################
##             Save an R data frame as an Excel (.xlsx) file
## remove (almost) everything in the working environment.
rm(list = ls())

load("sales.RData")


### Export each data frame in the current environment as a separate .xlsx file

# First solution: package `writexl`
#install.packages('writexl')
library(writexl)
writexl::write_xlsx(invoice_detailed, "invoice_detailed.xlsx", col_names = TRUE)

# Second solution: package `rio`
rio::export(invoice_detailed, file = "invoice_detailed.xlsx", 
            format = "xlsx", sheetName = "invoice_details")


ls()

#################################################################################
##    Save a bulk of R data frames as worksheets in a single Excel (.xlsx) file

# If you want to include all the data frames in a single `.xlsx` file, 
#    `writexl` and `rio` are no use
#  Packages such as `openxls`, xlsx`, or `XLConnect` do the task
#  

## Write all the dataframes stored in the `sales.RData` file as a separate
## sheets in a single `sales.xlsx` file 

# store the table names
rm(list = ls())
load("sales.RData")
tables <- ls()


## solution with `openxlsx` package
# `openxlsx` is installed automatically when installing `rio` package
library(openxlsx)

## Create a new workbook
wb <- createWorkbook("sales")

for (sheet in tables) {
     addWorksheet(wb, sheet)
     writeData(wb, sheet = sheet, get(sheet))
     
}

## Save workbook
saveWorkbook(wb, "sales.xlsx", overwrite = TRUE)



#################################################################################
###               14. Convert files directly from one format to another
#################################################################################

# Package "rio" - function "convert"

# convert a .dbf file into a .xlsx file
convert("SIRUTA_T.DBF", "SIRUTA_T.xlsx")

# convert a sppss file into a json file
convert("p004.sav", 'spss1.json')

# ...


#################################################################################
###         15. Write R data frames as tables in PostgreSQL databases
#################################################################################
## clear the memory
rm(list = ls())

# install.packages('RPostgres')
library(RPostgres)



## On Windows systems, PostgreSQL database service must already be started
con <- dbConnect(RPostgres::Postgres(), dbname="covid220925",
                 host = 'localhost', port='5432', user="postgres", password="postgres")

# On Mac OS
con <- dbConnect(RPostgres::Postgres(), host='localhost', port='5433',
                 dbname='covid220925', user='postgres', password='postgres')


# get the table names as a vector using function `dbListTables(con)` 
dbListTables(con)

library(readr)
# read the following four text files (as previously presented)
InsertMongoALL <- read_tsv("DragosCogean__InsertMongo_ALL.txt") 
InsertMySQLALL <- read_tsv("DragosCogean__InsertMySQL_ALL.txt") 
ReadMongoALL <- read_tsv("DragosCogean__ReadMongo_ALL.txt") 
ReadMySQLALL <- read_tsv("DragosCogean__ReadMySQL_ALL.txt") 

# write the four data frames as tables (with the same name) in the `test` database
dbWriteTable(con, "InsertMongoALL", InsertMongoALL, overwrite = T)
dbWriteTable(con, "InsertMySQLALL", InsertMySQLALL, overwrite = T)
dbWriteTable(con, "ReadMongoALL", ReadMongoALL, overwrite = T)
dbWriteTable(con, "ReadMySQLALL", ReadMySQLALL, overwrite = T)

# now query the PostgreSQL database
dbListTables(con)

# import one of the newly created tables as a separate data frame
# (pay attention to quotes and apostrophes)
insMongo_pg <- dbGetQuery (con, 'select * from "InsertMongoALL"')

# List fields in a table
dbListFields(con, "ReadMySQLALL") 




