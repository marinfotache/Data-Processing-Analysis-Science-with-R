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
###                           4b. User-Defined Functions                 ###   
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/04%20Basic%20Programming/04_Programming_UDFs_eval_tidyeval.pptx
############################################################################
## last update: 22.11.2018

# packages
library(tidyverse)
library(skimr)
library(PerformanceAnalytics)
library(broom)

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
###  I. Anonymous (un-named) function                                 ###
###       I.a Simple anonymous function for a list                    ###
###       I.b `Filter`, `Find`, `Position` with anonymous function    ###
###  II. (Named) User-Defined Function                                ###
###       II.a Basic utility functions                                ###
###       II.b Search/get functions                                   ###
#########################################################################


#########################################################################
###                 I. Anonymous (un-named) function                  ###
#########################################################################

#########################################################################
###              I.a Simple anonymous function for a list             ###
#########################################################################


#########################################################################
## Task:
## Given a string with a list of authors separated by `;`
## write an anonymous function that returns the string as 
## a tibble, with each author on a separate row

# parameter of the function
string_authors <- 'Fotache, M.; Strimbei, C.; Cretu, L.'

# the function
(function (x) {
     y <- str_split(x,';') %>%
          set_names('author')     
     return (as_tibble(y))     
          }) (string_authors)



#########################################################################
###       I.b `Filter`, `Find`, `Position` with anonymous function    ###
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
     

# descriptive statistics
glimpse(fuel_economy_2018)
skimr::skim(fuel_economy_2018)


## Tasks: For `fuel_economy_2018` data set:
#    - display column means which exceed 10 
#    - display first column mean  of the data frame which exceeds 10
#    - display the position of the first column whose mean exceeds 10

# for selecting only `numeric` columns from a tibble, use `select_if` 
fuel_economy_2018 %>% 
     select_if (is.numeric)


#    - display columns whose means which exceed 10 
# `Filter`
Filter( function( x ) x > 10, 
        colMeans (fuel_economy_2018 %>% select_if (is.numeric), na.rm = TRUE ))


#    - display first column mean  that exceeds 10
# `Find`
Find( function( x ) x > 10, 
        colMeans (fuel_economy_2018 %>% select_if (is.numeric), na.rm = TRUE ))


#    - display the position (among the numerical columns) of the first 
#         column whose mean exceeds 10
# `Position`
Position( function( x ) x > 10, 
        colMeans (fuel_economy_2018 %>% select_if (is.numeric), na.rm = TRUE ))



##############################################################################
###                      II. (Named) User-Defined Function                 ###
##############################################################################

##############################################################################
###                      II.a Basic utility functions                      ###
##############################################################################

##############################################################################
###                 UDF for cleaning a string (in Romania)                 ###
library(stringr)
f_clean_string = function (the_string) {
	# remove punctuation characters (by replacing them with blanks)
	the_string <- str_replace_all(the_string, pattern = "[[:punct:]]", " ")
	# removing double (triple...) blanks
	the_string <- str_replace_all(the_string, pattern = "\\s+", " ")
	# uniformization (lowercase)
	the_string <- tolower(the_string)

	# transforming Romanian characters
	the_string <- str_replace_all(the_string, pattern = "ă", "a")
	the_string <- str_replace_all(the_string, pattern = "â", "a")
	the_string <- str_replace_all(the_string, pattern = "î", "i")
	the_string <- str_replace_all(the_string, pattern = "ş", "s")
	the_string <- str_replace_all(the_string, pattern = "ţ", "t")

	return (the_string)	
}          

# test the function
f_clean_string('Cocostârcii     se ; Descocostârcăresc')   


##############################################################################
###                 UDF for descriptive statistics                         ###
## for details about descriptive statistics - see script `09a...` 
# 
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

glimpse(fuel_economy_2018)
descr_stats(fuel_economy_2018$combined_CO2)


##############################################################################
###       UDF for displaying a liniar model as an equation                ###
### for this and other examples on linear regression, see scripts 11a and 11b
# lm() function requires a data frame;state.x77 dataset is contained in a matrix, 
#    so one must convert it:
states_ <- as.tibble(state.x77) %>%
     set_names(str_replace_all(names(.), '( |\\.)', '_'))
glimpse(states_)

# descriptive statistics about variables
skimr::skim(states_)

# this the liniar regression model
modelA <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states_)
summary(modelA)



# here is the function
equation_lm <- function(the_lm_model) {
     return (paste(
          the_lm_model$terms[[2]], the_lm_model$terms[[1]], 
               paste0(round(coefficients(the_lm_model)[1],5), "  +  ", 
                    paste(sprintf("%.5f * %s", 
                  coefficients(the_lm_model)[-1],  
                  names(coefficients(the_lm_model)[-1])), collapse=" + ")) ))
}     

# test the function
equation_lm(modelA)


# we will prefer using `broom` package (see scripts `11a` and `11b`)
equation_lm2 <- function(the_lm_model) {
     
     df <- broom::tidy(the_lm_model)
     return ( paste(
               paste(names(augment(the_lm_model)) [1], '~', round(df$estimate[1], 5)), 
                   paste( round(df$estimate[2:nrow(df)], 5), '*', df$term[2:nrow(df)],  
                         collapse = ' + '), 
                   sep = ' + '))
}          

# test the function
equation_lm2(modelA)


##############################################################################
###                 Create subdirectories (in a given directory)           ###
###   for student teams of a programme                                     ###

## The parameters are:
## `dest_directory` is the directory where subdirectories will be created
## `program` is the program abbreviationn (e.g. SAAS, FRM, SDBIS, SIA)
## `year_of_study`
## `n_of_teams` (for each team a subdirectory will be creates)

f_create_teams_directories <- function (dest_directory, program,
          year_of_study, n_of_teams) {
          setwd(dest_directory)
          
          for (i in 1:n_of_teams) {
               subDir <- paste0(program, year_of_study, 10+i)    
               ifelse(!dir.exists(file.path(dest_directory, subDir)), 
                    dir.create(file.path(dest_directory, subDir)), FALSE)
          }
	return ()	
}          

# test the function
f_create_teams_directories("/Users/marinfotache/Downloads/test", "SAAS", 1, 50)


##############################################################################
###                      II.b Search/get functions                         ###
##############################################################################

##############################################################################
###                           `   sales` data sets                         ###
load (file = 'sales.RData')

## ... taken the data frames `products` and `invoice_detailed`
glimpse(products)
glimpse(invoice_details)


## Tasks:
##   - Build a function for returning the vat percent for a given product
##   - Use this function in another function that returns the amount of 
##        a given invoice (without joining the data frames)


## function `g_get_vatpercent` reveives a `productid` and returns
## the value of its `vatpercent`
f_get_vatpercent <- function (productid_) {
     # `products` 
     products %>%
          filter (productid == productid_) %>%
          select (vatpercent) %>%
          pull()
}

# test the function
f_get_vatpercent(4)


##  function `f_invoice_amount` gets an `invoiceno` and 
##  returns its amount (without joining any data frames)
invoiceno_ = 1112
f_invoice_amount <- function (invoiceno_) {
     # `invoice_details` 
     invoice_details %>%
          filter (invoiceno == invoiceno_) %>%
          rowwise() %>%
          mutate (vat = f_get_vatpercent (productid)) %>%
          ungroup() %>%
          summarise (amount = sum(quantity * unitprice * (1 + vat))) %>%
          pull()
}
# test the function
f_invoice_amount(1111)
f_invoice_amount(9999)

invoiceno_ = 111999
f_invoice_amount <- function (invoiceno_) {
     # `invoice_details` 
     df <- invoice_details %>%
          filter (invoiceno == invoiceno_) 
     
     if (nrow(df) > 0) {
          df %>%
               rowwise() %>%
               mutate (vat = f_get_vatpercent (productid)) %>%
               ungroup() %>%
               summarise (amount = sum(quantity * unitprice * (1 + vat)))      %>%
               pull()
     } else {
          return (NA)
     }     
}
# test the function
f_invoice_amount(1111)
f_invoice_amount(111199)


# in order to invoke function `f_invoice_amount` with `invoices` data frame...

#... this does not work! 
temp <- invoices %>%
     mutate (amount = f_invoice_amount(invoiceno))

# we need `rowwise` so that the function will be executed on each row
# (see aslso `rowwise` inside the function `f_invoice_amount`)
temp <- invoices %>%
     rowwise() %>%
     mutate (amount = f_invoice_amount(invoiceno))





