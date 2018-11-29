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
###                    05a.  Group operation. `apply` family             ###   

### See also the presentation:
### xxxxxxxx
############################################################################
## last update: 29.11.2018


# needed packages
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
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
###  I.   `apply`                                                     ###
###  II.  lapply`                                                     ###
###  III. `sapply`                                                    ###
###  IV.  `tapply`                                                    ### 
#########################################################################


#########################################################################
###                           Import the data sets                    ###
#########################################################################

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
     
                 


#########################################################################
###                                I. `apply`                         ###
#########################################################################

### The `apply()` function is usually used to apply a function to 
### the rows or columns (margins) of matrices or data frames. 

## Syntax for `apply()`:
##
## apply(x, MARGIN, FUN, ...)
##
##   - x is the matrix, dataframe or array
##   -  MARGIN is a vector giving the subscripts which 
##        the function will be applied over. 
##        E.g., for a matrix 1 indicates rows, 2 indicates columns, 
##             c(1, 2) indicates rows and columns.
##   - FUN is the function to be applied
##   - ... is for any other arguments to be passed to 
##        the function syntax of apply function



#########################################################################
###                                Task 1                             ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)

### Display
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')

# compute the mean for given variables and display
result <- apply(fuel_economy_2018[vars], 2, mean, na.rm = TRUE)
View(result)

# `result` is a named vector; if we want to get the resut as a 
# data frame/tible in the long format...

#... then one of the following solution will do the task 
#
apply(fuel_economy_2018[vars], 2, mean, na.rm = TRUE) %>%
     as.data.frame() %>%
     set_names('mean') %>%
     mutate(variable = rownames(.))

#
apply(fuel_economy_2018[vars], 2, mean, na.rm = TRUE) %>%
     tibble(variable = names(.), mean = .)


# if you prefer the long format, then one of the following solutions
# might be appropriate

# 
apply(fuel_economy_2018[vars], 2, mean, na.rm = TRUE) %>%
     tibble(variable = names(.), mean = .) %>%
     spread(variable, mean)

apply(fuel_economy_2018[vars], 2, mean, na.rm = TRUE) %>%
     t() %>%
     as.tibble()



#########################################################################
##                  Task 2: (taken from script `04a`)                 ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################

# this is a on-step solution based on `apply`
result <- apply (fuel_economy_2018 %>% select_if(is.numeric), 2,
                 sd, na.rm = TRUE) %>%
     tibble(variable = names(.), sd = .) %>%
     top_n(1, sd)
     


#########################################################################
##                     Task 3: (taken from script `04a`)              ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)

## ... and the UDF for descriptive statistics written in script `04b`...                         ###
descr_stats <- function(x, na.omit=FALSE) {
     if (na.omit)
          x <- x[!is.na(x)]
          min = min(x, na.rm=T)
          q_25 = quantile(x, names =F, na.rm=T)[2]
          med = median(x, na.rm=T)
          q_75 = quantile(x, names =F, na.rm=T)[4]
          max = max(x, na.rm=T)	
          n = length(x)
          mean = mean(x, na.rm=T)
          st_dev = sd(x, na.rm=T)
          skew = PerformanceAnalytics::skewness(x)
          kurt = PerformanceAnalytics::kurtosis(x)
     return(tibble(n=n, min=min, first_quartile=q_25, median=med, 
          third_quartile=q_75, max=max,  
          mean=mean, st_dev=st_dev, skew=skew, kurtosis=kurt))
}

## Get, as a list, all the variables statistics

# `result` will be a list
result <- apply(fuel_economy_2018 %>% select_if(is.numeric), 2, 
                descr_stats)
View(result)



#########################################################################
###                           II. `lapply`                            ###
#########################################################################

### The lapply() function does the following simple series of operations:
###
### - it loops over a list, iterating over each element in that list
### - it applies a function to each element of the list (a function 
###       that you specify) and returns a list (the l is for “list”).

### Syntax of lapply function
###         lapply(x, FUN, ...)

###   - x is the list
###   - FUN is the function to be applied
###   - ... is for any other arguments to be passed to the function



#########################################################################
###                    Task 1: (taken from scripts `02a`)             ###
###                    
### Import all text files whose names start with 
### `DragosDragosCogean__`
### and gather them (as they share a common stucture) into a single 
### data frame
getwd()

# get all the file names into a vector
file_names <- list.files(pattern = "DragosCogean__.+txt")

# import all the text files into a single list
the_list <- lapply(file_names, readr::read_tsv) %>%
     set_names(file_names)

# merge all the list elements into a data frame
the_df <- bind_rows(the_list) 

# add a column with the name of the source file
the_df$file_name <- rep(names(the_list), lapply(the_list, nrow))


# add a column for the db_server
the_df <- the_df %>%
     mutate (db_server = if_else( str_detect(file_name, 'Mongo'), 
               'MongoDB', 'MySQL'))
     


#########################################################################
###                                Task 2:                            ###
###                    
### After importing (as a list) all text files whose names start with 
### `DragosDragosCogean__`
##    extract/display first 10 elements in each subset

## solution with lapply

## we "borrow" `the_list` from above
short_list <- lapply(the_list, "[", 1:10,)

## the remaining of the solution is similar (to the previous solution)

# merge all the list elements into a data frame
short_df <- bind_rows(short_list) 

# add a column with the name of the source file
short_df$file_name <- rep(names(short_list), lapply(short_list, nrow))

# add a column for the db_server
short_df <- short_df %>%
     mutate (db_server = if_else( str_detect(file_name, 'Mongo'), 
               'MongoDB', 'MySQL'))





#########################################################################
###                           III. `sapply`                           ###
#########################################################################

### `sapply()` behaves similarly to lapply() except for the returned value 
###
### Essentially, sapply() calls lapply() on its input and then 
### applies the following algorithm:
###  - If the result is a list where every element is length 1, 
###       then a vector is returned
###  - If the result is a list where every element is a vector 
###       of the same length (> 1), a matrix is returned.
###  - If neither of the above simplifications can be performed 
###  then a list is returned


# With reference to the last two tasks, notice the result of...
lapply(short_list, nrow)
# ... relative to ...
sapply(short_list, nrow)


#########################################################################
##                            Task1:                                   ##
##  Extract/display in a separate data frame only the numeric columns  ##
##  of data frame `fuel_economy_2018`                                  ## 
#########################################################################

# before `select_if` was implemented...
result <- fuel_economy_2018 %>%
     select_if(is.numeric)

# ... one the most elegant solution was based on `sapply`
result <- fuel_economy_2018[, sapply(fuel_economy_2018, is.numeric)]


#########################################################################
###                            IV. `tapply`                           ###
#########################################################################

### tapply() applies a function over subsets of a vector. 
### It is primarily used when we have the following circumstances:
###
###  - The dataset must be broken up into groups
###  - A function must be applied to each group

### syntax of `tapply`
###            tapply(x, INDEX, FUN, ..., simplify = TRUE)

### - x is a vector
### - INDEX is a factor or a list of factors 
### - FUN is a function to be applied


#########################################################################
###                            Task1:                                 ###
###  Given the `fuel_economy_2018` data set...                        ### 
glimpse(fuel_economy_2018)
### Compute the mean of the following variable `'combined_l100km'`
### ... by each level of `n_of_cyl` variable
#########################################################################

## Of course we know how to do it with `dplyr`
fuel_economy_2018 %>%
     group_by(n_of_cyl) %>%
     summarise(mean_combined_l100km = mean(combined_l100km, na.rm = TRUE))

# the solution based on `taaply` is...
with (fuel_economy_2018, 
      tapply(combined_l100km, n_of_cyl, mean, na.rm = TRUE))

# Notice the difference in displaying the result


#########################################################################
###                            Task2:                                 ###
###  Given the `fuel_economy_2018` data set...                        ### 
glimpse(fuel_economy_2018)
### Compute the mean of the following variable...
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')
### ... by each level of `n_of_cyl` variable
#########################################################################


# we'll combine `apply` with `tapply`
apply(fuel_economy_2018[vars], 2, 
      function(x) tapply(x, fuel_economy_2018$n_of_cyl, mean, na.rm = TRUE))

# we can get something similar with `dplyr/tidyr`:
fuel_economy_2018 %>%
     select(!!vars, n_of_cyl) %>%
     gather(var, value, -n_of_cyl) %>%
     group_by(var, n_of_cyl) %>%
     summarise(mean = mean(value, na.rm = TRUE)) %>%
     spread(var, mean)






