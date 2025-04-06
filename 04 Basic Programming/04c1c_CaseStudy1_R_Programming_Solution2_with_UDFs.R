############################################################################
###                    (Simple) R Programming Case Study 1               ###
###                   Master Programmes Admission                   ###
### for problem description see:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/04%20Basic%20Programming/04c1a_CaseStudy1_R-Programming_1_Requirements.pdf                 
############################################################################
### last update: 2024-11-11
#library(readxl)
library(tidyverse)
# install.packages('rio', dependencies = TRUE)
library(rio)

############################################################################
###              4c1c  Solution 2 - with user defined function           ###
############################################################################

############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


load('master_admiss1.Rdata')
glimpse(master_progs)
glimpse(applicants)


############################################################################
## 1 order applicants by admission average points
applicants <- applicants %>%
     mutate (admin_avg_points = grades_avg * .6 + dissertation_avg * .4 ) %>%
     arrange(desc(admin_avg_points))

############################################################################
# 2 add a column in `master_progs` for keeping track of assigned applicants
master_progs <- master_progs %>%
     mutate (n_of_filled_positions = 0)



#######################################################################
## 3 Create a function for checking if a given 
##        programme has still available positions
abbrev_ <- 'xyz'
abbrev_ <- 'DM'


f_available <- function ( abbrev_ ) {
     
     test <- master_progs %>%
          filter (prog_abbreviation == abbrev_ & n_of_positions > n_of_filled_positions)

     if (nrow(test) > 0) 
          return (TRUE)
     else
          return (FALSE)
}     


# compile the function !

# now, test the function
f_available('DM')

f_available('xyz')


#######################################################################
# 4. set up the `results` tibble 
results <- tibble()

#######################################################################

# 5 main section: loop trough all applicants (in their average points 
# descending order)
master_progs$n_of_filled_positions <- 0

# i <- 1
for (i in 1:nrow(applicants)) {
     # store the current applicant's id
     crt_id <- applicants$applicant_id[i]

     options <- applicants  %>%
           filter(applicant_id == crt_id) %>%
           select (applicant_id, prog1_abbreviation:prog6_abbreviation) %>%
           pivot_longer(!applicant_id, names_to = "option_no", values_to = "value") %>%
           filter(!is.na(value))

     j <- 1
     # now loop through all applicant's options and try to assign it as 
     # early as possible 
     for (j in 1:nrow(options)) {
          
          # store the current option
          current_option <- options$value[j]
          
          # check if there is still available positions at the current option
          if (f_available(current_option)) { 
               
               # succes! applicant will be assigned to her/his current option
               results <- bind_rows(results, 
                    tibble(applicant_id = crt_id, 
                           prog_abbreviation_accepted = current_option))
               
               # increment `n_of_filled_positions` for the current option
               master_progs <- master_progs %>%
                    mutate (n_of_filled_positions = n_of_filled_positions +
                         if_else(prog_abbreviation == current_option, 1L, 0L)   
                                 )
               
               # exit from the options loop, otherwise the applicant will
               #      be accepted to her/his last option
               break()
          }     
     }     
}     

glimpse(master_progs)

# 6 check the number of filled positions for each programme
View(master_progs)

# 7 bring the results file in a more readable form...
results_ok2 <- applicants %>%
     left_join (results) %>%
     mutate(prog_abbreviation_accepted = if_else(is.na(prog_abbreviation_accepted), 
          'rejected', prog_abbreviation_accepted))

View(results_ok)


# 8 export results in excel

getwd()
rio::export(results_ok2, file = '04c1d_CaseStudy1_Results.xlsx', 
       format = 'xlsx')


