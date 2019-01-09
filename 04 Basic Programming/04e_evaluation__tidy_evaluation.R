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
###                      04d. Evaluation. Tidy evaluation                ###     
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/04%20Basic%20Programming/04_Programming_UDFs_eval_tidyeval.pptx
############################################################################
## last update: 27.11.2018

# needed packages
library(tidyverse)
library(rlang)
library(purrr)
library(readxl)
library(rio)
library(skimr)

############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
###  I. `assign` function                                             ###
###  II. `get` function                                               ###
###  III. Introduction to tidy evalualuation                          ###
###       III.a. dynamic groups in `dplyr`                            ###
###       III.b. quosures                                             ###
###            - function `quo`                                       ###
###            - operator `!!`                                        ###
###            - function `rlang::sym`                                ###
###            - function `enquo`                                     ###
###            - function  `quos`                                     ###
###            - operator `!!!`                                       ###
###            - function `rlang::syms`                               ###
###  IV. `ggplot`and tidy evaluation                                  ###
#########################################################################


#########################################################################
###                      I. `assign` function                         ###
#########################################################################
###  We'll use `assign` to storing the result in a dynamically        ###
###  qualified (named) object (data frame)                            ###
#########################################################################


#########################################################################
##        Example 1 - import all tables from a `PostgreSQL` database
## 
## We already used `assign` function (script `02a`) when importing all 
## the PostgreSQL tables in a database (subschema) as R data frames 
## with identical name

## Reminder: PostgreSQL - see script `02a` 
##   It works if you have already created and populated the PostgreSQL 
##   database on your system !
## You have to change the `host`, `port`, `dbname`, `user`, 
##   and `postgres` accordingly

# taking the example of `northwind` database
library(RPostgreSQL)
## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# open connection - Mac OS syntax
con <- dbConnect(drv, host='localhost', port='5433', dbname='chinook',
                 user='postgres', password='postgres')
# get the table names
tables <- dbGetQuery(con, 
     "select table_name from information_schema.tables where table_schema = 'public'")
tables

#i <- 1
# import each of the table as a data frame 
for (i in 1:nrow(tables)) {
     # extract data from table in PostgreSQL
     temp <- dbGetQuery(con, 
          paste("select * from ", tables[i,1], sep=""))
     
     # create the data frame: `assign` allows naming dynamically the data frame 
     assign(tables[i,1], temp)
}

# close all PostgreSQL connections 
for (connection in dbListConnections(drv) ) {
  dbDisconnect(connection)
}
## Frees all the resources on the driver
dbUnloadDriver(drv)



#########################################################################
##  Example 2 - Import as data frames all the worksheets in an Excel file 

## 'northwind.xlsx'
## 

## remove (almost) everything in the working environment.
rm(list = ls())

file_name <- "northwind.xlsx"

# display the worksheets names in a .xls(x) file
ws <- readxl::excel_sheets(file_name)

i <- 2
# now, loop through the worksheets
for (i in 1:length(ws)) {
     # read the current worksheet
     temp <- readxl::read_excel(file_name, i)
     
     # copy `temp` data frame into a data frame with the name of the
     #   worksheet
     assign(ws[i], temp)
} 


#########################################################################
##  Example 3 - Import as data frames the first worksheet of all 
##  Excel (`.xlsx`) files) in the current directory 

## clear the working environment.
rm(list = ls())


# create a vector with the name and extension of all .xls files in current directory 
files <- list.files(pattern = "*.xlsx") 
# or
files <- dir(pattern = "*.xlsx") 

i <- 3
# now, loop through the files, but remove the `.xlsx` suffix in the data 
#   frame name
for (i in 1:length(files)) {
     # read the current worksheet
     temp <- readxl::read_excel(files[i], 1)
     
     # copy `temp` data frame into a data frame with the name of the
     #   worksheet (remove `.xlsx`)
     assign(substr(files[i], 1, (nchar(files[i])-5)), temp)
} 

# there is a shorter version of the loop
for (i in 1:length(files))
     assign(substr(files[i], 1, (nchar(files[i])-5)), 
            readxl::read_excel(files[i], 1))




#########################################################################
###                       II. `get` function                          ###
#########################################################################
###  We'll use `get` to read a dynamically qualified (named)          ###
###  object (data frame)                                              ### 
#########################################################################

## remove (almost) everything in the working environment.
rm(list = ls())

# load the data frames
load("chinook.RData")
rm(i, drv, con, connection, temp)

# create a subdirectory in the current directory
getwd()
main_dir <- getwd()

if (!dir.exists( file.path(main_dir, 'to_be_deleted')))
     dir.create(file.path(main_dir, 'to_be_deleted'))


### Export each data frame in the current environment as a separate .xlsx file
tables <- ls()
for (i in 1:length(tables)) {
     # here we used `get` for naming dynamically the data frame from which
     # the data is taken
     temp <- get(tables[i])
     if (is.data.frame(temp) & nrow(data.frame(temp)) > 0)
          rio::export(temp, file = paste0('to_be_deleted', '/', tables[i], ".xlsx"), 
                 format='xlsx', sheetName=tables[i])
}    




#########################################################################
###                 III. Introduction to tidy evalualuation           ###
#########################################################################


#########################################################################
###                 III.a dplyr - grouping dynamically                ###
#########################################################################
## remove (almost) everything in the working environment.
rm(list = ls())


#########################################################################
## (Anonymized) FEAA students for 2014-2015 academic year 
## example taken from script `03b_tidy-verse.R`
getwd()
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
## display the data frame structure
glimpse(studs)

###################################################################
##                       Task no. 1
# Print the frequency (of the values) for the following nominal
#    variables: `LEVEL_OF_STUDY`, `ATTENDANCE`, `YEAR_OF_STUDY`, 
#    `PROGRAMME`, `LOCATION`, and `FINANCIAL_SUPPORT`

# Currently, with `dplyr` we know to work with static variables,
# ... such as...
result <- studs %>%
               group_by(LEVEL_OF_STUDY) %>%
               summarise (frequency = n())
print(result)

studs %>%
     group_by(LEVEL_OF_STUDY) %>%
     tally()


#
# We have to do the same for all the variables of interes
#    (which is bearable only when the number of variables is small)
# ...
# ...
# ...


##        Alternatively, we can loop using `tidy evaluation`
##   which was introduced in `dplyr` 0.7.0
variables <- c('LEVEL_OF_STUDY', 'ATTENDANCE', 'YEAR_OF_STUDY', 
               'PROGRAMME', 'LOCATION', 'FINANCIAL_SUPPORT')
# variable <- variables[1]

for (variable in variables) {
     result <- studs %>%
               group_by( value = .data[[variable]] ) %>%     ## notice `.data[[variable]]`
               summarise (frequency = n())
     cat('', quote = FALSE, row.names = FALSE)
     print(variable)
     print(result)
     cat('', quote = FALSE, row.names = FALSE)
}     


# as some values were lost (because the way tibbles are displayed),
# we'll save the result as a tibble instead or printing
final_result <- tibble()
for (variable in variables) {
     final_result <- bind_rows(final_result, 
          tibble(variable = variable) %>%
               mutate (foo = 1) %>%
               inner_join(
                    studs %>%
                         group_by( value = .data[[variable]] ) %>%     ## notice `.data[[variable]]`
                         summarise (frequency = n()) %>%
                         ungroup() %>%
                    mutate (foo = 1)     
          ) %>%
          select (-foo)  )   
}     
View(final_result)




#########################################################################
###                           III.b. quosures                         ###
#########################################################################

### A quosure is a data structure that stores both an expression 
### and an environment

### Some functions and operators:
###
###  -    `quo()` - works like `"`: it quotes its input rather than 
###            evaluating it; it returns a quosure
###  -    `!!` operator unquotes an input so that it’s evaluated, 
###            not quoted. 
###  -    `rlang::sym()` takes strings as input and turn them into symbols                                ###
###  -    `enquo()` examines the argument, see what the user typed, 
###            and return that value as a quosure.
###  -    `quos()`` captures all the `...` as a list of arguments/formulas      
###  -    `!!!` operator splices the arguments


#########################################################################
##                       Fuel Economy dataset(s)                       ##
## example taken from script `03b_tidy-verse.R`
fuel_economy_2018 <- read_tsv("all_alpha_18.txt") 
glimpse(fuel_economy_2018)

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
     ) %>%
     mutate (displacement  = as.numeric(Displ), 
             n_of_cyl = as.numeric(Cyl),
             air_pollution = as.numeric(`Air Pollution Score`),
             greenhouse = as.numeric(`Greenhouse Gas Score`),
             combined_CO2 = as.numeric(`Comb CO2`)
             ) 


####################################################################
##                            Task 1:
## Create a function for computing the mean of `combined_l100km`
##   (combined (city + highway) fuel consumption for running 100Km)
## for the levels of a given variable (column) of type char
## 

## the initial function does not work
f_mean <- function(the_column) {
     fuel_economy_2018 %>%
          group_by(the_column) %>%
          summarise(mean = mean(combined_l100km))
}

f_mean(Drive)
# Error in grouped_df_impl(data, unname(vars), drop) : 
#  Column `the_column` is unknown
  
f_mean('Drive')
# Error in grouped_df_impl(data, unname(vars), drop) : 
#  Column `the_column` is unknown


## the function needs `!!` operator
f_mean <- function(the_column) {
     fuel_economy_2018 %>%
          group_by(!! the_column) %>%
          summarise(mean = mean(combined_l100km, na.rm = TRUE))
}

# this call will not compute the summary properly
f_mean('Drive')

# instead, we need `quo`
f_mean(quo(Drive))

# we can apply the function for every nominal variable of interest
f_mean(quo(manufacturer))


# using a variable as a parameter requires `rlang::sym` function
var <- 'Drive'
var
rlang::sym(var)
f_mean(rlang::sym(var))



## If we want to call the function without invoking it with `quo`, 
##   one must include function `enquo` 
f_mean2 <- function(the_column) {
     # `enquo`
     the_column <- enquo(the_column)
     
     # nothing changes here
     fuel_economy_2018 %>%
          group_by(!! the_column) %>%
          summarise(mean = mean(combined_l100km, na.rm = TRUE))
}

# new, we simply include the column name 
f_mean2(Drive)
f_mean2(manufacturer)


####################################################################
##                            Task 2:
## Create a function for computing the mean of `combined_l100km`
##   (combined (city + highway) fuel consumption for running 100Km)
## for any combination (group) of one, two or more (mainly)
## nominal variables
## 

# as the function will have a variable number of parameters, 
# we'll use `...` in its definition 
f_mean3 <- function(...) {
     
     # Use quos() to capture all the `...` as a list of formulas.
     group_vars <- quos(...)

     # notice the `!!!` operator that splices the arguments
     # `!!!` is needed in `select`, but not when grouping due
     # to `group_by_at` 
     fuel_economy_2018 %>%
          select (!!! group_vars, combined_l100km) %>% 
          group_by_at(., group_vars )  %>%
          summarise(mean = mean(combined_l100km, na.rm = TRUE))
}


# test the function with a two-variable group
result <- f_mean3(c('Displ', 'Cyl'))

# test the function with a four-variable group
result <- f_mean3(c('Cyl', 'Trans', 'Drive', 'Fuel'))


# using a variable requires `rlang::syms` function
# inside the `f_means`
f_mean4 <- function(...) {
     
     # Use `rlang::syms(...)`` to capture all the `...` as a list of formulas.
     group_vars <- rlang::syms(...)

     # notice the `!!!` operator that splices the arguments
     # `!!!` is used also in `group_by`
     fuel_economy_2018 %>%
          select (!!! group_vars, combined_l100km) %>% 
          group_by(!!!group_vars )  %>%
          summarise(mean = mean(combined_l100km, na.rm = TRUE))
}


# using a variable requires `rlang::syms` function
vars <- list('Cyl', 'Trans', 'Drive', 'Fuel')
result <- f_mean4(vars)





## If we want to call the function wihout including column names
## wihin ` or ",  one must include function `enquos` 
f_mean5 <- function(...) {
     
     # Use quos() to capture all the `...` as a list of formulas.
     group_vars <- enquos(...)

     # notice the `!!!` operator that splices the arguments
     # `!!!` is needed in `select`, but not when grouping due
     # to `group_by_at` 
     fuel_economy_2018 %>%
          select (!!! group_vars, combined_l100km) %>% 
          group_by_at(., group_vars )  %>%
          summarise(mean = mean(combined_l100km, na.rm = TRUE))
}


# test the function with a two-variable group
result <- f_mean5(c(Displ, Cyl))

# test the function with a four-variable group
result <- f_mean5(c(Cyl, Trans, Drive, Fuel))





###################################################################
###                           Task 3:
### Given the `stud` data frame:
### Extract as a tibble all the colums of type `char` that have at
### least ten distinct values
### (for a better version based on `purrrr` package, see script `05b`)

glimpse(studs)

## first, we create a function for doing grouping and summary
## for a single column
f_frequencies <- function(a_df, a_column) {
     a_df %>%
          select(the_column  = !! a_column) %>%
          summarise( n_distinct = n_distinct(the_column, na.rm = TRUE)) %>%
          mutate (the_column = !! a_column)
}


variable = 'LEVEL_OF_STUDY'
# when invoking the function, we can use `quo` 
f_frequencies(studs, quo(variable))

# in this case, function `quo` is not compusory, so we can ommit it:
f_frequencies(studs, variable)


## the main function will loop trough character columns and will build
## incrementally the resulting tibble
result <- tibble()
for (column in studs %>% select_if(is.character) %>% names()) {
     result <- bind_rows(result, 
                 f_frequencies(studs, column))
}
View(result)

# filter the result
result %>%
     filter (n_distinct >= 10)



###################################################################
##                        Task no. 4
### Given the `stud` data frame:

# Display the frequency table for the following pairs of nominal
#         variables 
#    - (`LEVEL_OF_STUDY`, `ATTENDANCE`)
#    - (`PROGRAMME`, YEAR_OF_STUDY')
#    - (`PROGRAMME`, `ATTENDANCE`)


pair_list <- list (
     pair1 = c('LEVEL_OF_STUDY', 'ATTENDANCE'),
     pair2 = c('PROGRAMME', 'YEAR_OF_STUDY'),
     pair3 = c('PROGRAMME', 'ATTENDANCE')
     )


## first, we create a function for doing grouping and summary
## for a pair of columns

first_column <- 'LEVEL_OF_STUDY'
second_column <- 'ATTENDANCE'
table(studs$LEVEL_OF_STUDY)
table(studs$ATTENDANCE)

studs %>%
     select(column_a  = !! first_column, column_b = !! second_column) %>%
     group_by(value_column_a = column_a, value_column_b = column_b) %>%
     summarise( frequency = n()) %>%
     ungroup() %>%
     transmute (column_a  = !! first_column, value_column_a,
             column_b = !! second_column, value_column_b,
             frequency
             ) 


## now, the function
f_frequencies2 <- function(a_df, first_column, second_column) {
     a_df %>%
          select(column_a  = !! first_column, column_b = !! second_column) %>%
          group_by(value_column_a = column_a, value_column_b = column_b) %>%
          summarise( frequency = n()) %>%
          ungroup() %>%
          transmute (column_a  = !! first_column, value_column_a,
             column_b = !! second_column, value_column_b,
             frequency
             ) 
}

# test the function
first_column <- 'LEVEL_OF_STUDY'
second_column <- 'ATTENDANCE'
f_frequencies2(studs, first_column, second_column)


# main loop
result <- tibble()
for (i in 1:length(pair_list)) {
     result <- bind_rows(result, 
          f_frequencies2(studs, pair_list[[i]][1], pair_list[[i]][2] ))
}
View(result)


###################################################################
##                        Task no. 5
### Given the `stud` data frame:

# Display the frequency table for a list of :
#    * nominal variables, 
#    * couples of nominal variables,
#    * tuples of nominal variables
#    

# example
list_freq_table <- list (
     'LEVEL_OF_STUDY', 
     'ATTENDANCE',     
     'PROGRAMME',
     c('LEVEL_OF_STUDY', 'ATTENDANCE'),
     c('PROGRAMME', 'YEAR_OF_STUDY'),
     c('PROGRAMME', 'ATTENDANCE'),
     c('LEVEL_OF_STUDY', 'YEAR_OF_STUDY', 'ATTENDANCE'),
     c('LEVEL_OF_STUDY', 'YEAR_OF_STUDY', 'ATTENDANCE', 'LOCATION')
     )


## now, the function that uses `...` in the definition so that it 
#    can accept any number of arguments.
f_frequencies3 <- function(a_df, ...) {
     
     # Use quos() to capture all the `...` as a list of formulas.
     group_vars <- quos(...)

     a_df %>%
          select ( !!! group_vars) %>%
          group_by_all( . ) %>%
          summarise( frequency = n()) 

}

# test the function
result <- f_frequencies3(studs, 
     c('LEVEL_OF_STUDY', 'YEAR_OF_STUDY', 'ATTENDANCE', 'LOCATION'))



# main part of the solution
result <- tibble()
for (i in 1:length(list_freq_table)) {
     result <- bind_rows(result, 
          f_frequencies3(studs, list_freq_table[[i]]) )
}
    
View(result)



#########################################################################
###                 IV. `ggplot`and tidy evaluation                   ###
#########################################################################
     
# for ggplot2  - see section `08` on GitHub:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/08%20Data%20Visualization%20with%20-mostly-%20ggplot2


###################################################################
##                        Task no. 1

# Display the frequency (of the values) for the following nominal
#    variables: `LEVEL_OF_STUDY`, `ATTENDANCE`, `YEAR_OF_STUDY`, 
#    `PROGRAMME`, `LOCATION`, and `FINANCIAL_SUPPORT`

variables <- c('LEVEL_OF_STUDY', 'ATTENDANCE', 'YEAR_OF_STUDY', 
               'PROGRAMME', 'LOCATION', 'FINANCIAL_SUPPORT')


f_barplot <- function(df, column) {
     column <- enquo(column)
     # `dplyr::quo_name` changes the quosure into a string
     title_ <- paste("Value Frequency for Variable", quo_name(column))
     
     ggplot(df, 
          aes(x = !!column)) +
          geom_bar() +
          ggtitle(title_) +
          theme(axis.text.x = element_text(angle = 45, # text angle on x-axis
               vjust = 1, # vertical justification (position near to bar center)
               hjust = 1 )) # horizontal justification (align towards the bar)
          
     # save the plot into the current working directory     
     ggsave(file = paste0(title_, ".pdf"))
}

# test the function
f_barplot(studs, LEVEL_OF_STUDY)

# using a variable as a parameter requires `rlang::sym` function
var <- 'LEVEL_OF_STUDY'
rlang::sym(var)
var_transf <- rlang::sym(var) 
f_barplot(studs, !!var_transf)


# main section
for (i in 1:length(variables)) {
     var = rlang::sym(variables[i])       
     f_barplot(studs, !!var)  
}



###################################################################
##                        Task no. 2

# Given the `fuel_economy_2018` data set (imported above): 
glimpse(fuel_economy_2018)

# 2a. Display the histograms for the following variables: 
#    `n_of_cyl`, `air_pollution`, and `greenhouse`

# 2b. Display the density curves for the following variables: 
#    `displacement`, `combined_CO2`, `cty_l100km`, `hwy_l100km`,
#    and `combined_l100km`


## 2a.

# create the variable list for histograms
list_histogram <- list (
     'n_of_cyl', 
     'air_pollution',     
     'greenhouse'
     )

list_density <- list (
     'displacement', 
     'combined_CO2',     
     'cty_l100km',
     'hwy_l100km',
     'combined_l100km'
     )

# create the function for displaying the either the histogram of a 
#    density plot for a given variable
f_histogram_or_density <- function(what_to_plot, df, column) {
     column <- enquo(column)
     # `dplyr::quo_name` changes the quosure into a string
     title_ <- paste(what_to_plot, "for Variable",
                     paste0('`', quo_name(column), '`'))
     
     graph <- ggplot(df, 
          aes(x = !!column)) 
     
     if (what_to_plot == 'Histogram')
          graph <- graph + geom_histogram()
     else
          graph <- graph + geom_density()
          
     graph <- graph +
          ggtitle(title_) +
          theme(axis.text.x = element_text(angle = 45, # text angle on x-axis
               vjust = 1, # vertical justification (position near to bar center)
               hjust = 1 )) # horizontal justification (align towards the bar)
     
     print(graph)
          
     # save the plot into the current working directory     
     ggsave(file = paste0(str_replace_all(title_, '`', '_'), ".pdf"))
}

# test the function
f_histogram_or_density('Histogram', fuel_economy_2018, n_of_cyl)  

var = rlang::sym(list_histogram[[1]][1])       
var
f_histogram(fuel_economy_2018, !!var)  


f_histogram_or_density('Density', fuel_economy_2018, combined_l100km)  

var = rlang::sym(list_density[[1]][1])       
var
f_histogram_or_density('Density', fuel_economy_2018, !!var)  



## main section

# first, for the histograms... 
for (i in 1:length(list_histogram)) {
     var = rlang::sym(list_histogram[[i]][1])       
     f_histogram_or_density('Histogram', fuel_economy_2018, !!var)  
}

# second, for the Density plots... 
for (i in 1:length(list_density)) {
     var = rlang::sym(list_density[[i]][1])       
     f_histogram_or_density('Density', fuel_economy_2018, !!var)  
}



###################################################################
##                        Task no. 3
## Given the `fuel_economy_2018` data set (imported above): 
glimpse(fuel_economy_2018)
## Display the scatter plot for any given pair of (numeric) variables 

# create the list with pairs of variables for the scatterplots 
list_scatterplots <- list (
     c('displacement', 'n_of_cyl'),
     c('displacement', 'combined_CO2'),
     c('displacement', 'combined_l100km'),
     c('combined_CO2', 'combined_l100km')
     )

# # create the function for displaying the either the histogram of a 
#    density plot for a given variable
f_scatterplot <- function(df, x, y) {
     x <- enquo(x)
     y <- enquo(y)
     
     # `dplyr::quo_name` changes the quosure into a string
     title_ <- paste("Scatterplot:",
          paste0('`', quo_name(y), '`'),
          '~ ',
          paste0('`', quo_name(x), '`'))

     ggplot(df, 
          aes(x = !!x, y = !!y)) +
          geom_point() +
          ggtitle(title_) +
          theme(axis.text.x = element_text(angle = 45, # text angle on x-axis
               vjust = 1, # vertical justification (position near to bar center)
               hjust = 1 )) # horizontal justification (align towards the bar)

     # save the plot into the current working directory     
     ggsave(file = paste0(str_replace_all(title_, '`|~| |:', '_'), ".pdf"))
}

# test the function
x = rlang::sym(list_scatterplots[[1]][1])       
y = rlang::sym(list_scatterplots[[1]][2])       
f_scatterplot(fuel_economy_2018, !!x, !!y)  


## main section
for (i in 1:length(list_scatterplots)) {
     x = rlang::sym(list_scatterplots[[i]][1])       
     y = rlang::sym(list_scatterplots[[i]][2])       
     f_scatterplot(fuel_economy_2018, !!x, !!y)  
}



