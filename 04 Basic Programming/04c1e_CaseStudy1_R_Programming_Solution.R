#######################################################################
###             R Programming: (Simple) Case Study 1                ###
###                   Master Programmes Admission                   ###
#######################################################################
# last update: 2017-03-12
library(readxl)
library(tidyverse)

########################################################################
###            Dowload and unzip the archive for this script
########################################################################

# all the files needed o run this script are archived are available at:
# https://1drv.ms/u/s!AgPvmBEDzTOSgYRPvB8qlBVyCimurg
# Please download the archive in a local directory (such as 'RDataSets')
#  and unzip it 

#######################################################################
##                   Import data from Excel
#######################################################################
# take care of the working directory
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')
file_name <- 'students/05c2_Model1_R-Programming_2_Data.xlsx'

# import the first worksheet of the file (containing the programmes and
#    their available positions)
programmes <- read_excel(file_name, 1)

# remove empty rows
programmes <- programmes %>%
     filter(!is.na(Prog_Abbrev))

# import the applicats (the second worksheet)
applicants <- read_excel(file_name, 2)
# remove empty rows
applicants <- applicants %>%
     filter(!is.na(Applicant_Id))



#######################################################################
##        Create a function for checking if a given 
##        programme is full (no more available positions) or not 
#######################################################################
# just for testing the function on-the-fly, we initiate the parameter-variable
abbrev_ <- 'FRM'

f_prog_is_full <- function ( abbrev_ )
{
     # filter the data frame `programmes`
     p <- programmes %>%
          filter (Prog_Abbrev == abbrev_ & 
                       N_of_Positions > N_of_Filled_Positions)
     
     # if the result has no records, then the current programme is full
     if (nrow(p) == 0 )
          return (TRUE)
     else
          return (FALSE)
}     
# compile the function !

# now, test the function

f_prog_is_full('FRM')

f_prog_is_full('xyz')



#######################################################################
##                             Main section
#######################################################################

# empty `N_of_Filled_Positions`
programmes$N_of_Filled_Positions <- 0

# (re)create an empty data frame RESULTS { Applicant_Id, Prog_Abbrev_Accept}
results <- tibble (Applicant_Id = numeric(), 
                   Prog_Abbrev_Accept = character())

# order applicants by their average points
applicants <- applicants %>%
     arrange(desc(Points_Average))

# loop through applicants

# just for testing the next loop on-the-fly, we initiate the index variable `i`
i <- 1
for (i in 1:nrow(applicants))
{
     crt_Applicant_Id <- applicants[i,]$Applicant_Id
     
     # for the current applicant all the options are loaded
     crt_app_options <- applicants %>%
          filter (Applicant_Id == crt_Applicant_Id) %>%
          select (Applicant_Id, Option1_Abbrev:Option4_Abbrev) %>%
          gather(option, prog,  -Applicant_Id) %>%
          filter(!is.na(prog))
     
     # again just for testing the next loop on-the-fly, we initiate 
     #   the index variable `j`
     j <- 1
     # loop throug current applicant options
     for (j in 1:nrow(crt_app_options))
     {
          crt_option <- crt_app_options[j,]$prog
          # test if the current option (programme) is full
          if (f_prog_is_full(crt_option) == FALSE)
          {
               # the programme is not full yet, so assign it to the current appplicant...
               results <- bind_rows(results, 
                    tibble (Applicant_Id = crt_Applicant_Id, 
                            Prog_Abbrev_Accept = crt_option ))
               
               #... then increment the `N_of_Filled_Positions` for the current programme
               programmes$N_of_Filled_Positions <- ifelse(programmes$Prog_Abbrev ==
                    crt_option, programmes$N_of_Filled_Positions + 1, 
                    programmes$N_of_Filled_Positions)
               
               # exit the current loop (no interested in the remaining options of the current
               #    applicant, since she/he has just be assigned to a programme)
               break
          }
     }     
     
}     

# visualize the results
test <- applicants %>%
     inner_join(results) %>%
     arrange (desc(Points_Average))


