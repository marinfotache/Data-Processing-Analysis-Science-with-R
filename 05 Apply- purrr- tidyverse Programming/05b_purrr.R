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
###            05b. Functional Programming with `purrr` package          ###   

### See also the presentation:
### xxxxxxxx
############################################################################
## last update: 19.11.2019


# needed packages
library(tidyverse)
library(readxl)
#library(purrr)
#library(stringr)


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
###  I.   `map_*`                                                     ###
###       I.a  `map` vs. `apply`                                      ###
###       I.b  `map_dfr` and `map_dfc`                                ###
###       I.c  `map_dbl`, `map_chr`, `map_lgl`                        ###
###       I.d  data frames and lists                                  ###
###  II.  `map2` (with some help from `keep`, `pluck`)                ###
###  III.  `walk` and `walk2`                                         ###
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
##        (Anonymized) FEAA students for 2014-2015 academic year 
## example taken from script `03b_tidy-verse.R`
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
## display the data frame structure
glimpse(studs)


#########################################################################
##             Chinook data base (imported from PostgreSQL)
## example taken from script `03b_tidy-verse.R`
load('chinook.RData')



#########################################################################
###      Create two versions of  descriptive statistics function      ###
#########################################################################

# first version returns a tibble in the `wide` format
descr_stats_wide <- function(x, na.omit=FALSE) {
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
     return(tibble(a__n=n, b___min=min, c__first_quartile=q_25, 
          d__median=med, e__third_quartile=q_75, f__max=max,  
          g__mean=mean, h__st_dev=st_dev, i__skew=skew, j__kurtosis=kurt))
}


# second version returns a tibble in the `long` format
descr_stats_long <- function(x, na.omit=FALSE) {
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
     return(
          bind_rows(
               tibble(statistic = "a__n", value = n),
               tibble(statistic = "b___min", value = min),
               tibble(statistic = "c__first_quartile", value = q_25),
               tibble(statistic = "d__median", value = med),
               tibble(statistic = "e__third_quartile", value = q_75),
               tibble(statistic = "f__max", value = max),
               tibble(statistic = "g__mean", value = mean),
               tibble(statistic = "h__st_dev", value = st_dev),
               tibble(statistic = "i__skew", value = skew),
               tibble(statistic = "j__kurtosis", value = kurt)
          ))
}


#########################################################################
###                           I.   `map_*`                            ###
#########################################################################

### purrr::map() is a function for applying a function to each 
### element of a list. 

###  Map functions in base R are the "applys": lapply(), sapply(), 
###  vapply(), etc. 


#########################################################################
###                       I.a  `map` vs. `apply`                      ###
#########################################################################

#########################################################################
###                     Task 1 (taken from script `05a`):             ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)

### ...create a data frame with the means of the following variables
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')

# first solution based on `map` function results in a data frame
# containing a list column
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%
     tibble() %>%   # `map` result is a list, so we need `tibble`
     set_names('mean') %>%
     mutate(variable = vars ) %>%
     transmute (variable, mean)

# ... the same for the second solution
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%
     tibble(variable = names(.), mean = .)

result
names(result)


# the third second solution intoroduces another notation (`~`)
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., ~ mean(.x, na.rm = TRUE)) %>%
     tibble(variable = names(.), mean = .)


# Comment:
# Notice the observation between...
View(result)
# ... and
print(result)


# column `mean` of of type `named list`
glimpse(result)


# in order to display the `mean` as a number, we'll need `unnest` function
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., ~ mean(.x, na.rm = TRUE)) %>%
     tibble(variable = names(.), mean = .) %>%
     unnest(mean)

glimpse(result)
print(result)

# to be fair, function `unnest` may be avoided:
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., ~ mean(.x, na.rm = TRUE)) %>%
     tibble(variable = names(.), mean = as.numeric(.))

glimpse(result)



#########################################################################
##                  Task 2 (taken from script `04a`):                 ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################

# solution based on `map`
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., sd, na.rm = TRUE) %>%
     tibble(variable = names(.), sd = as.numeric(.)) %>%
     transmute(variable, sd) %>%
     top_n(1, sd)

glimpse(result)


# the second notation
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., ~ sd (.x, na.rm = TRUE)) %>%
     tibble(variable = names(.), sd = as.numeric(.)) %>%
     transmute(variable, sd) %>%
     top_n(1, sd)

glimpse(result)



#########################################################################
##         Task 3: (taken from scripts `04a` and `05a`)               ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
## ... and the two versions of the UDF for descriptive statistics     ###
## created above                                                      ###

##
## 3.a Get, as a list, all the variables statistics
## 
## 3.b (NEW) Get, as a data frame, all the variables statistics
##   (with variables on rows, and each statistic as a separate column)
##   - this is a new requierement (compared to the task in script `05a`)
##


## Solutions:

## 3.a `result` will be a list - using `descr_stats_wide`

# first notation
result_wide1 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., descr_stats_wide)


# second notation
result_wide2 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., ~ descr_stats_wide(.x))
                

# third notation
result_wide3 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(~ descr_stats_wide(.x))


identical(result_wide1, result_wide2)
identical(result_wide1, result_wide3)


View(result_wide)
result_wide
glimpse(result_wide)



## 3.a - using `descr_stats_long` (`result` will be a list with a 
## different format) 
result_long <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., descr_stats_long)
                
View(result_long)
result_long



## 3.b Get, as a data frame, all the variables statistics
## 

## `descr_stats_wide` as argument of `map`
result_wide_df1 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map(., descr_stats_wide) %>%
     as.data.frame() %>%   # this will convert theh list into a data frame
     gather(variable_and_statistic, value) %>%
     separate(variable_and_statistic, into = c('variable', 'statistic'),
               sep = "\\.") %>%
     spread(statistic, value)

View(result_wide_df1)
result_wide_df1



# the second solution is simpler (it uses `map` and `unnest`)
result_wide_df2 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., descr_stats_wide) %>%
     tibble(variable = names(.), df = .) %>%
     unnest(df)

View(result_wide_df2)
result_wide_df2
glimpse(result_wide_df2)



## for `descr_stats_long`, `map` will be combined with `bind_rows`
result_long_df1 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map(., descr_stats_long) %>%
     bind_rows(.id = "variable")

View(result_long_df1)
result_long_df1


# the second solution is based on `unnest`
result_long_df2 <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map(., descr_stats_long) %>%
     tibble(variable = names(.), df = .) %>%
     unnest()
     
View(result_long_df2)
result_long_df




#########################################################################
##                  Task 4 (taken from script `04a`):                 ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##    Display the number of distinct values of all variables/column   ###
##    of type `character`                                             ###
#########################################################################

result <- fuel_economy_2018 %>% 
     select_if(is.character) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map(table) %>%   # on each column `table` function will be applied
     map(., length) %>%
     tibble(variable = names(.), n_of_distinct_values = as.numeric(.)) %>%
     select (-`.`)
     
table(fuel_economy_2018$Displ, fuel_economy_2018$Drive)



#########################################################################
###                     Task 5 (taken from scripts `02a`)             ###
###                    
### Import all text files whose names start with 
### `DragosDragosCogean__`
### and gather them (as they share a common stucture) into a single 
### data frame
getwd()

# get all the file names into a vector
file_names <- list.files(pattern = "DragosCogean__.+txt")

# solution based on `map`
result <- file_names %>%
     map(., readr::read_tsv) %>%
     bind_rows(.id = "file_number") %>%
     mutate (file_name = file_names[as.integer(file_number)]) %>%
     select (-file_number)  %>%
     mutate (db_server = if_else( str_detect(file_name, 'Mongo'), 
               'MongoDB', 'MySQL'))



#########################################################################
###            Using anonymous functions inside `map`                 ###
#########################################################################

#########################################################################
###                     Task 6 (taken from scripts `03b`)             ###
### Display the number of NA values for each column of a data frame
### Ex: data frame `customer` (`chinook` data base)

## Solution 1: 
temp1 <- map(customer, function(x) sum(is.na(x)))
temp1

## Solution 2: 
temp2 <- map(customer, ~ sum(is.na(.)))
temp2

identical(temp1, temp2)



#########################################################################
###                 I.b  `map_dfr` and `map_dfc`                      ###
#########################################################################

#########################################################################
###                     Task 1 (taken from section `I.a`):             ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)

### create a data frame with the mean of the following variables
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')


# in this case `map_df`, `map_dfr` and `map_dfc` yield the same
# result in the `wide` form

result1 <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map_df(., mean, na.rm = TRUE)

result2 <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map_dfr(., mean, na.rm = TRUE)

result3 <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map_dfc(., mean, na.rm = TRUE)

identical(result1, result2)
identical(result1, result3)


# if we want the result in the `wide` format, then `gather` is needed
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map_dfr(., mean, na.rm = TRUE) %>%
     gather(variable, mean)



#########################################################################
##                  Task 2 (taken from section `I.a`):                ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################


# solution based on `map_dfr` and `top_n`
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map_dfr(., sd, na.rm = TRUE) %>%
     gather(variable, sd) %>%
     top_n(1, sd)


# solution based on `map_dfr` and a second `select_if`
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map_dfr(., sd, na.rm = TRUE) %>%
     select_if(. == max(.))
     

# solutions based on `map_dfc` will be identical with `map_dfr`


#########################################################################
##                  Task 3 (taken from section `I.a`):                ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##    Display the number of distinct values of all variables/column   ###
##    of type `character`                                             ###
#########################################################################

# solution with `map_dfr` gets the result in the `wide` format...
result <- fuel_economy_2018 %>% 
     select_if(is.character) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfr(., n_distinct) 

# so `gather` is needed ...
result <- result %>% 
     gather(variable_name, n_of_distinct_values) 


# also in this case, solution with `map_dfc` gets the same result as `map_dfr`
result <- fuel_economy_2018 %>% 
     select_if(is.character) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfc(., n_distinct) %>%
     gather(variable_name, n_of_distinct_values) 



#########################################################################
##             Task 4 (task 3 from the previous section)              ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
## ... and the two versions of `descr_stats` function                 ###

##  Get, as a data frame, all the variables statistics

# solution with `map_dfr` applied on `descr_stats_wide` 
result_dfr_wide <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfr(., descr_stats_wide, .id = "variable") 

# solution with `map_dfr` applied on `descr_stats_long` 
result_dfr_long <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfr(., descr_stats_long, .id = "variable") 


# solution with `map_dfc` applied on `descr_stats_wide` extracts something like...
result_dfc_wide <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfc(., descr_stats_wide)

View(result_dfc_wide)

# so, it needs `gather`and also
# a tweak for including the variable name

result_dfc_wide <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfc(., descr_stats_wide) %>%
     gather(statistic, value) %>%
     transmute(variable = rep(
               fuel_economy_2018 %>% 
                    select_if(is.numeric) %>%
                    set_names(str_replace_all(names(.), '\\.| ', '_')) %>%
                    names(), each = 10),
               statistic, value     ) 


# also solution with `map_dfc` applied on `descr_stats_long` needs
# some processing
result_dfc_long <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfc(., descr_stats_long) 

View(result_dfc_long)

# the result ...
result_dfc_long <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map_dfc(., descr_stats_long) %>%
     gather(init_statistic, init_value) %>%
     mutate (
          statistic_ok = ifelse(is.na(as.numeric(init_value)), init_value, NA), 
          value_ok = ifelse(!is.na(as.numeric(init_value)), as.numeric(init_value), NA) 
          ) %>%
     transmute (wrong_var_name = init_statistic, 
                statistic = statistic_ok,
                value = lead(value_ok, 10))    %>% 
     filter (!is.na(value)) %>%
     transmute(variable = rep(
               fuel_economy_2018 %>% 
                    select_if(is.numeric) %>%
                    set_names(str_replace_all(names(.), '\\.| ', '_')) %>%
                    names(), each = 10),
               statistic, value     ) 
     


#########################################################################
###           Task 5: (taken from scripts `02a` and `05a`)            ###
###                    
### Import all text files whose names start with 
### `DragosDragosCogean__`
### and gather them (as they share a common stucture) into a single 
### data frame
getwd()

# get all the file names into a vector
file_names <- list.files(pattern = "DragosCogean__.+txt")

result <- file_names %>%
     map_dfr(., readr::read_tsv, .id = "file_number") %>%
     mutate (file_name = file_names[as.integer(file_number)]) %>%
     select (-file_number) %>%
     mutate (db_server = if_else( str_detect(file_name, 'Mongo'), 
               'MongoDB', 'MySQL'))

glimpse(result)     


#########################################################################
###                     Task 6 (taken from scripts `03b`)             ###
### Display, as a data frame, the number of NA values for each column 
### of a data frame
### Ex: data frame `customer` (`chinook` data base)


##
## Solution 1: 
temp1 <- map_dfr(customer, function(x) sum(is.na(x)))
temp1

## Solution 2: 
temp2 <- map_dfc(customer, ~ sum(is.na(.)))
temp2

identical(temp1, temp2)



#########################################################################
###            I.c  `map_dbl`, `map_chr`, `map_lgl`                   ###
#########################################################################

###    - map() makes a list. 
###    - map_lgl() makes a logical vector.
###    - map_int() makes an integer vector.
###    - map_dbl() makes a double vector.
###    - map_chr() makes a character vector.
###  Each function takes a vector as input, applies a function 
###  to each element, and then returns a new vector that’s the same 
###  length (and has the same names) as the input. 
###  The type of the vector is determined by the suffix of 
###       the map function.


#########################################################################
###                     Task 1 (taken from script `05a`):             ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)

### Create a data framme with the means of the following variable
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')


# first solution (taken from section `I.a`), with `map`, get the result as a list
result_map <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE)

# second solution, with `map_dbl`, get the result as a named vector
result_map_dbl <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map_dbl(., mean, na.rm = TRUE) 

class(result_map)
class(result_map_dbl)

result_map
result_map_dbl


# For getting the result as a data frame, we'll continue the solutions
# with `tibble` and `set_names`

result_map <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%
     tibble() %>%
     set_names('mean') %>%
     mutate(variable = vars )


result_map_dbl <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map_dbl(., mean, na.rm = TRUE) %>%
     tibble() %>%
     set_names('mean') %>%
     mutate(variable = vars )

# Here you can see that `result_map` is a data frame where 
#    column `mean` is a list...
result_map

# whereas `result_map_dbl` is more appropriate
result_map_dbl



#########################################################################
##                  Task 2 (taken from script `04a`):                 ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################


# solution based on `map_dbl` combined with `map_lgl` and `tibble`
# ok, it's a bit of a stretch here...
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map_dbl(., sd, na.rm = TRUE) %>%
     map_lgl(., function (x) if_else (x == max(.),  TRUE, FALSE)) %>%
     tibble(variable = names(.), is_max = .)     

result


#########################################################################
###                                Task 3                             ###
##   Given the students data set (see above) ...                       ##
glimpse(studs)
##    Extract, for each student, only the first surname 
##    (FIRST_NAME contains one or more surnames)
## Note:
##   The surnames could be separated either by:
## * "-", as in "MARIANA-SIMONA", "GEORGIAN-VLĂDUŢ", 
## * or by " ", as in "GEORGIANA ALEXANDRA"
#########################################################################

# solution based on `map_chr`
first_surname <- studs %>%
     select (STUD_ID, FIRST_NAME) %>%
     mutate(first_surname = map_chr(str_split(FIRST_NAME, '-| '),1))
     
glimpse(first_surname)



#########################################################################
###                    I.d  data frames and lists                     ###
#########################################################################


#########################################################################
###    Task 1 (from script `05a` and section `I.a` of this script):   ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)

### Create a data framme with the means of the following variables:
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')

## in section `I.a`, column `mean` is a list
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%
     tibble(variable = names(.), mean = .)
result

## simply adding am `unnest` function at the end of solution above...
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%
     tibble(variable = names(.), mean = .) %>%
     unnest()

## ... will get the mean values properly 
result


# there is also a solution that instead of `unnest` uses
#    a combination of `map_dbl` and `pluck`
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%
     tibble(variable = names(.), mean = .) %>%
     mutate(mean = map_dbl(.$mean, pluck(1)) )



#########################################################################
##         Task 2: (taken from scripts `04a` and `05a`)               ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
## ... and the two versions of the UDF for descriptive statistics     ###
## created above...                                                   ###

## Get, as a data frame, all the variables statistics
##   (with variables on rows, and each statistic as a separate column)
##

## Solution below combines `map`, `tibble` and `unnest`
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., descr_stats_long) %>%
     tibble() %>%
     unnest() %>%
     transmute (variable = rep(
               fuel_economy_2018 %>% 
                    select_if(is.numeric) %>%
                    set_names(str_replace_all(names(.), '\\.| ', '_')) %>%
                    names(), each = 10), 
               statistic, value)
                
result


#########################################################################
###                                Task 3                             ###
##   Given the students data set (see above) ...                       ##
glimpse(studs)

##    Create a data frame containing on each row one of a student's
##        surname; also include the surname number (ordinal position
##        if current surname)

## Note:
##   The surnames could be separated either by:
## * "-", as in "MARIANA-SIMONA", "GEORGIAN-VLĂDUŢ", 
## * or by " ", as in "GEORGIANA ALEXANDRA"
#########################################################################

## When examining the first name...
result <- studs %>%
     select (FIRST_NAME) 
#... we discover that this variable contains also the family name for 
#    the married people, with `CĂS. ` prefix

## So, we need to remove that part


# if we want to avoid `map` functions, `str_split` must be used in 
#    conjuction with `rowwise` ...
first_names_ok <- studs %>%
     mutate(FIRST_NAME = 
                 str_trim(str_replace_all(FIRST_NAME, ' - ', '-'), 
                          side = 'both')) %>%
     select (STUD_ID, FIRST_NAME) %>%
     rowwise() %>%
     mutate (first_name_ok = str_split(FIRST_NAME, ' căs\\.| CĂS\\.')[[1]][1])

#... but we do not want to avoid `map`, so `map` will be used along with `map_chr`
first_names_ok <- studs %>%
     mutate(FIRST_NAME = 
                 str_trim(str_replace_all(FIRST_NAME, ' - ', '-'), 
                          side = 'both')) %>%
     select (STUD_ID, FIRST_NAME) %>%
     mutate (first_name_list = map(.$FIRST_NAME, 
          function (x) unlist(str_split(x, ' căs\\.| CĂS\\.')) ) ) %>%
     mutate(first_name_ok = map_chr(.$first_name_list, 1))


# Next, we get the result using `unnest` and then `row_number`
result <- first_names_ok %>%
     mutate (
          first_name_ok = str_trim(first_name_ok, side = 'both'),
          first_name_list = map(.$first_name_ok, 
          function (x) unlist(str_split(x, '-| ')) ) ) %>%
     group_by(STUD_ID, FIRST_NAME, first_name_ok) %>%
     unnest() %>%
     mutate(surname_no = row_number()) %>%
     ungroup() %>%
     select (STUD_ID, FIRST_NAME, surname_no, surname = first_name_list)



#########################################################################
###                                Task 4                             ###
##   Given the students data set (see above) ...                       ##
glimpse(studs)

## For each student, get a separate column with a string containing
## only first two surnames (in case there are more - 
## see row 47 in `studs` as example)

# 
result <- studs %>%
     mutate(FIRST_NAME = 
                 str_trim(str_replace_all(FIRST_NAME, ' - ', '-'), 
                          side = 'both')) %>%
     select (STUD_ID, FIRST_NAME) %>%
     mutate (first_name_list = map(.$FIRST_NAME, 
          function (x) unlist(str_split(x, ' căs\\.| CĂS\\.')) ) ) %>%
     mutate(first_name_ok = map_chr(.$first_name_list, 1)) %>%
     mutate (
          first_name_ok = str_trim(first_name_ok, side = 'both'),
          first_name_list = map(.$first_name_ok, 
          function (x) unlist(str_split(x, '-| ')) ) ) %>%
     mutate(first_two_surnames = map_chr(.$first_name_list,   
                    ## `map_chr` here extracts first two surname
          function (x) paste(x[1], x[2])   ))                               

# examine line 47 in the result




#########################################################################
###            II. `map2` (with some help from `keep`, `pluck`)       ###
#########################################################################

#########################################################################
###                     Task 1 (taken from script `05a`):             ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)
### Create a data frame with the means of the following variables
###  (each row will be associated to a variable)
vars <- c('cty_l100km', 'hwy_l100km', 'combined_l100km', 'air_pollution',
          'greenhouse', 'combined_CO2')


# a solution based on `map2` function
result <- fuel_economy_2018 %>%
     select (!!!vars) %>%
     map(., mean, na.rm = TRUE) %>%    # this computes mean for each column/variable
     map2(., names(.), c) %>%   # `map_2` concatenates the mean with the variable name
     tibble(variable = map_chr(., 2),     # first `map_chr` extracts the second component
            mean = map_chr(., 1)) %>%     #    of each list element
     select (-`.`)                        # second `map_chr` extracts the first component
                                          #    of each list element


#########################################################################
##                  Task 2 (taken from script `04a`):                 ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################

# solution based on `map_dbl` combined with `map2`, `keep` and `pluck` 
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map_dbl(., sd, na.rm = TRUE) %>%
     map2(., max(.), c) %>%   # `map2` adds to each list element another
                              #  component containinf the max sd 
     keep(., function(x) x[1] == x[2]) %>%  # `keep` filters the list elements
     map(., pluck(1))    # `pluck extracts only the first component` of 
                         #  list's each element


result
     

#########################################################################
##                                 Task 3                             ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Display all the variables having the standard deviation greater   ###
##   than of variable `hwy_l100km`?                                   ###
#########################################################################

# solution based on `map` , `map2`, `keep` and `pluck` 
result <- fuel_economy_2018 %>% 
     select_if(is.numeric) %>%
     map(., sd, na.rm = TRUE) %>%
     map2(.x = ., .y = .$hwy_l100km, .f = c) %>%
     keep(., function(x) x[1] > x[2]) %>%  # `keep` filters the list elements
     map(., pluck(1))    # `pluck extracts only the first component` of 
                         #  list's each element

result




#########################################################################
###                 III.   `walk` and `walk2`                         ###
#########################################################################

#  Walk is an alternative to map that you use when you want 
#  to call a function for its side effects, rather than for 
#  its returned value. 
#  
#  Examples of usage:
#   - render output to the screen 
#   - save files to disk 
#


#########################################################################
###                                 Task 1                            ###
###  Given the `studs` data set (see beginning of this script) ...    ###
glimpse(studs)
### and an adapted function (from script `04e`) for saving a barplot...
f_barplot_stud <- function(column) {
     column <- enquo(column)
     # `dplyr::quo_name` changes the quosure into a string
     title_ <- paste("Value Frequency for Variable", quo_name(column))
     
     ggplot(studs, 
          aes(x = !!column)) +
          geom_bar() +
          ggtitle(title_) +
          theme(axis.text.x = element_text(angle = 45, # text angle on x-axis
               vjust = 1, # vertical justification (position near to bar center)
               hjust = 1 )) # horizontal justification (align towards the bar)
          
     # save the plot into the current working directory     
     ggsave(file = paste0(title_, ".pdf"))
}

## Task:
##   Display the barplot for all character variables with less than 20 values

# the solution uses `pluck`, `map` and a `walk` function which invokes `f_barplot_stud`
studs %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     select_if(function (col)  is.character(col) && n_distinct(col) < 20) %>%
     list(names(.)) %>%
     pluck(2) %>%
     map(., sym) %>%
     walk(., function (x) f_barplot_stud (!!x)  ) ## here is the `walk` function




#########################################################################
###                                 Task 2                            ###
###  Given the Excel file `northwind.xlsx`...                         ###
### Import all the file sheets as separate data frames

file_name <- "northwind.xlsx"

# solution is based on a `walk2` function which executes function `assign`
readxl::excel_sheets(file_name) %>%
     tibble() %>%
     set_names("sheet_name") %>%
     mutate(file_name = file_name, sheet_no = row_number()) %>%
     mutate (df = map2(.$file_name, .$sheet_no, readxl::read_excel)) %>%
     walk2(.x = .$sheet_name,
        .y = .$df,
        .f = assign, envir = .GlobalEnv)








