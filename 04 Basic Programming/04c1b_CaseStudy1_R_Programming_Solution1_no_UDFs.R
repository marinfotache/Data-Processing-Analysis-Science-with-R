############################################################################
###                    (Simple) R Programming Case Study 1               ###
###                       Master Programmes Admission                   ###
### for problem description see:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/04%20Basic%20Programming/04c1_CaseStudy1_R-Programming_1_Requirements.pdf                  
############################################################################
###

# last update: 2020-11-10
library(readxl)
library(tidyverse)


############################################################################
###            4c3a  Solution without any user defined function          ###
############################################################################

############################################################################
###            Download the necesary data sets for this script
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

#######################################################
###             New solution: 2020-11-10

## 1 order applicants by admission average points
applicants <- applicants %>%
     mutate (admin_avg_points = grades_avg * .6 + dissertation_avg * .4 ) %>%
     mutate (prog_abbreviation_accepted = '') %>%        
     arrange(desc(admin_avg_points))

# 2 add a column in `master_progs` for keeping track of assigned applicants
master_progs <- master_progs %>%
     mutate (n_of_filled_positions = 0)

glimpse(applicants)

# i <- 1
for (i in 1:nrow(applicants)) {
        
     # get as a data frame all the options for current applicant
     crt_options <- applicants[i,]  %>%
        select (applicant_id, prog1_abbreviation:prog6_abbreviation) %>%
        pivot_longer(!applicant_id, names_to = "attribute", values_to = "option") %>%
        filter(!is.na(option))
     
     # j <- 1
     for (j in 1:nrow(crt_options)) {
             crt_prog <- master_progs %>%
                     filter (prog_abbreviation == crt_options$option[j])
             
             if (crt_prog$n_of_positions[1] > crt_prog$n_of_filled_positions[1]) {
                     # there an available place
                     #applicants$prog_abbreviation_accepted[i] <- crt_prog$prog_abbreviation[1]
                     
                     applicants <- applicants %>%
                             mutate (prog_abbreviation_accepted = 
                                if_else(applicant_id == applicants$applicant_id[i], 
                                        crt_prog$prog_abbreviation[1], prog_abbreviation_accepted))
                     
                     
                     # increment number of filled positions
                     master_progs <- master_progs %>%
                             mutate (n_of_filled_positions = n_of_filled_positions +
                                ifelse (prog_abbreviation == crt_prog$prog_abbreviation[1], 1, 0))
                     break
             }
             
     }

}




#######################################################################
###                   Previous solution

## 1 order applicants by admission average points
applicants <- applicants %>%
     mutate (admin_avg_points = grades_avg * .6 + dissertation_avg * .4 ) %>%
     mutate (prog_abbreviation_accepted = '') %>%        
     arrange(desc(admin_avg_points))

# 2 add a column in `master_progs` for keeping track of assigned applicants
master_progs <- master_progs %>%
     mutate (n_of_filled_positions = 0)

# 3. set up the `results` tibble 
results <- tibble()

glimpse(applicants)

# 4 main section: loop trough all applicants (in their average points 
# descending order)
i <- 1
for (i in 1:nrow(applicants)) {
     # store the current applicant's id
     crt_id <- applicants$applicant_id[i]

     # build a tibble with current applicant's options
     options <- applicants %>%
          filter(applicant_id == crt_id) %>%
          gather(option_no, value, prog1_abbreviation:prog6_abbreviation) %>%
          filter(!is.na(value))
     
     # now loop thrrough all applicant's options and try to assign it as 
     # early as possible 
     j <- 1
     for (j in 1:nrow(options)) {
          # store the current option
          current_option <- options$value[j]
          
          # check if there is still available positions at the current option
          test <- master_progs %>%
               filter (prog_abbreviation == current_option & 
                            n_of_positions > n_of_filled_positions)
          
          if (nrow(test) > 0) {
               # succes! applicant will be assigned to her/his current option
               results <- bind_rows(results, 
                    tibble(applicant_id = crt_id, 
                           prog_abbreviation_accepted = current_option))
               
               # increment `n_of_filled_positions` for the current option
               master_progs <- master_progs %>%
                    mutate (n_of_filled_positions = n_of_filled_positions +
                         if_else(prog_abbreviation == current_option, 1L, 0L)                                 
                                 )
               
               # exit from the options loop, otherwise the applicant could
               # be accepted to  her last option
               break()
          }     
     }     
}     

# check the number of filled positions for each programme
View(master_progs)

# bring the results file in a more readable form...
results_ok <- applicants %>%
     left_join (results) %>%
     mutate(prog_abbreviation_accepted = if_else(is.na(prog_abbreviation_accepted), 
          'rejected', prog_abbreviation_accepted))

View(results_ok)


# export results in excel
rio::export(results_ok, file = '04c3a_CaseStudy1_Results.xlsx', 
       format = 'xlsx')

