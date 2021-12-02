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
###                    05c. More on tidyverse programming                ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/05%20Apply-%20purrr-%20tidyverse%20Programming/05_apply__purrr__tidyverse_programming.pptx
############################################################################
## last update: 27.11.2021

# required packages
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


#########################################################################
###                                Agenda                             ###
#########################################################################
###  I. `select_*` functions                                          ###
###       I.a brief recap on `select`                                 ###
###       I.b `select_if`                                             ###
###       I.c `select_all`                                            ###
### II. `mutate_*` functions                                          ###
### III. `filter_*` functions ())  ###
### IV. `summarise_*` functions ())  ###
###  V.  `map_*` and models                                          ###

#########################################################################

## See also:
# https://suzan.rbind.io/2018/01/dplyr-tutorial-1/#selecting-columns-by-logical-expressions
# https://suzan.rbind.io/2018/01/dplyr-tutorial-1/#selecting-columns-by-logical-expressions
# https://towardsdatascience.com/functional-programming-in-r-with-purrr-469e597d0229



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
###                      I. `select_*` functions                      ###
#########################################################################
### For `select` basics - see script `03b`
###

#########################################################################
###                      I.a brief recap on `select`
#########################################################################

#########################################################################
###                                Task 1               ###
### Given the `fuel_economy_2018` data set...
glimpse(fuel_economy_2018)

### Extract in a separate date frame only the following columns
cols <- c('cty_l100km', 'hwy_l100km', 'combined_l100km')


## sol. 1 - without `dplyr` package
result <- fuel_economy_2018[c('cty_l100km', 'hwy_l100km', 'combined_l100km')]

## sol. 2 - also without `dplyr` package - using `cols` vector
result <- fuel_economy_2018[cols]

## sol.3 - `dplyr/tidyselect - atribute list
result <- fuel_economy_2018 %>%
     select (cty_l100km, hwy_l100km, combined_l100km)

## sol.4 - `dplyr/tidyselect - atribute sequence (it works because
##   the three attributes are `neightbours`)
result <- fuel_economy_2018 %>%
     select (cty_l100km:combined_l100km)

## sol.5 - `dplyr/tidyselect with partial column names (as these
## attributes al the only ending in `_l100km`)
result <- fuel_economy_2018 %>%
     select (ends_with("_l100km"))

## sol.6 - `dplyr/tidyselect with regular expression (as these
## attributes al the only ending in `_l100km`) - `matches`
result <- fuel_economy_2018 %>%
     select (matches("_l100km$"))

## sol.7 - `dplyr/tidyselect with tidyevaluation
result <- fuel_economy_2018 %>%
     select (!!cols)

## sol.8 - `dplyr/tidyselect with `one_of`
result <- fuel_economy_2018 %>%
     select (one_of(cols))


#########################################################################
###                      I.b `select_if`
#########################################################################
### The select_if function allows you to pass functions
###  which return logical statements
###
#########################################################################
###
### We already employed `select_if` in scripts `04b`, `04e`, `05a`


#########################################################################
##                  Task 1 - taken from script `05b`                ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################

# solution based on `map_dfr` and two `select_if`s
result <- fuel_economy_2018 %>%
     select_if(is.numeric) %>%
     map_dfr(., sd, na.rm = TRUE) %>%
     select_if(. == max(.))


#########################################################################
##                       Task 2 (taken from script `04b`:
##  Given the fuel efficiency data set (see above) ...                ###
##    - display columns whose mean exceeds 10
##    - display the first column whose mean exceeds 10


## display columns whose mean exceeds 10

# solution 1 - with anonymous function
fuel_economy_2018 %>%
     select_if (is.numeric) %>%
     select_if (function (x) mean(x, na.rm = TRUE) > 10) %>%
     map_dbl(., mean, na.rm = TRUE)

# solution 2 - with `~`
fuel_economy_2018 %>%
     select_if(is.numeric) %>%
     select_if( ~ mean(., na.rm=TRUE) > 10) %>%
     map_dbl(., mean, na.rm = TRUE)

# solution 2 - with `~` and compacted
fuel_economy_2018 %>%
     select_if(~ is.numeric(.) & mean(., na.rm=TRUE) > 10) %>%
     map_dbl(., mean, na.rm = TRUE)


##   display first column whose mean exceeds 10 - solution with `head`
fuel_economy_2018 %>%
     select_if (is.numeric) %>%
     select_if (~ mean(., na.rm=TRUE) > 10) %>%
     map_dbl(., mean, na.rm = TRUE) %>%
     head(1)

##   display first column whose mean exceeds 10 - solution with `select`
fuel_economy_2018 %>%
     select_if (is.numeric) %>%
     select_if (~ mean(., na.rm=TRUE) > 10) %>%
     select (1) %>%
     map_dbl(., mean, na.rm = TRUE)



#########################################################################
##                                 Task 3:                            ###
##  Given the fuel efficiency data set (see above) ...                ###
glimpse(fuel_economy_2018)

## Display only non-numeric columns

# notice `~` syntax
temp <- fuel_economy_2018 %>%
     select_if( ~!is.numeric(.))


#########################################################################
##                  Task 4 (taken from script `05b`):                  ##
##  Given the fuel efficiency data set (see above) ...                 ##
glimpse(fuel_economy_2018)
##    Display the variables/column of type `character` whose number    ##
##    of distinct values is smaller than 20                            ##
#########################################################################

# first solution with `select_if`
result <- fuel_economy_2018 %>%
     select_if(is.character) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfr(., n_distinct) %>%
     select_if(function (x) x < 10)


# second solution with `select_if`
result <- fuel_economy_2018 %>%
     select_if(~ is.character(.) & n_distinct(.) < 10) %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     map_dfr(., n_distinct)


#########################################################################
###                      I.c `select_all`
#########################################################################
### `select_all()` function allows changes to all columns, and takes
###       a function as an argument.


#########################################################################
##                              Task 1                                 ##
##  Given the students data set data set (see above) ...               ##
glimpse(studs)
##    Change the names of all variables in lowercase                   ##
#########################################################################

# first solution - with `set_names`
studs %>%
     set_names(tolower(names(.))) %>% ## fix the variable names
     glimpse()

# second solution - with `select_all`
studs %>%
     select_all(tolower)  %>%
     glimpse()



#########################################################################
##                              Task 2                                 ##
##  Given the fuel efficiency data set (see above) ...                 ##
glimpse(fuel_economy_2018)
##    Change the names of all variables replacing spaces and dots      ##
##    with underscores                                                 ##
#########################################################################

# first solution - with `set_names` (we've done it many times until now)
fuel_economy_2018 %>%
     set_names(str_replace_all(names(.), '\\.| ', '_')) %>% ## fix the variable names
     glimpse()

# second solution - with `select_all` (this is new)
fuel_economy_2018 %>%
     select_all(~ str_replace_all (., '\\.| ', '_')) %>%
     glimpse()



#########################################################################
###                 II. `mutate_*` functions                          ###
#########################################################################
### For `mutate` basics - see script `03b`
###


#########################################################################
###                      II.a brief recap on `mutate`                 ###
#########################################################################
### We have used `mutate` intensively in almost every script,
###  so we will only the problem of adding columns based on
###  functions (including user-defined functions)

#########################################################################
###                           Task 1
###  (see also task 3  (from script `05b`, section "I.d)              ###
##
##   Given the students data set                                       ##
glimpse(studs)

##  Create a data frame (`results`) with columns `STUD_ID` and
##   FAMILY_NAME and the students whose family name contains  at least
##   two words; add two columns containing the first and the second
##        words of the FAMILY_NAME
##
## Note:
##   The words in family name could be separated either by:
## * "-", as in "BÎRCĂ-SALCĂU"
## * or by " ", as in "DANDU POP"
#########################################################################


# the idea is to filter the students with at least two words in their
# family name, and then too split the family name...

# first solution is not ok... (see newly added columns)
result <- studs %>%
     select (STUD_ID, FAMILY_NAME) %>%
     filter (str_detect(FAMILY_NAME, ' |-')) %>%
     mutate (first_word = str_split(FAMILY_NAME, ' |-')  [[1]][1],
               second = str_split(FAMILY_NAME, ' |-')  [[1]][2])

# ... before mutate, a `rowwise` is needed
result <- studs %>%
     select (STUD_ID, FAMILY_NAME) %>%
     filter (str_detect(FAMILY_NAME, ' |-')) %>%
     rowwise() %>%
     mutate (first_word = str_split(FAMILY_NAME, ' |-')  [[1]][1],
               second = str_split(FAMILY_NAME, ' |-')  [[1]][2])



#... but we do not want to avoid `map`, so `map` will be used
#    along with `map_chr`
first_names_ok <- studs %>%
     mutate(FIRST_NAME = str_trim(str_replace_all(FIRST_NAME, ' - ', '-'),
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




###  IV.  `map_*` and models                                          ###

# https://towardsdatascience.com/functional-programming-in-r-with-purrr-469e597d0229

# https://www.youtube.com/watch?v=b0ozKTUho0A



