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
###                         1a. Introduction to R                        ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/01%20R_Introduction_Data%20Structures/01a_Introduction_to_R.pptx
############################################################################
## last update: 23.10.2023


############################################################################
###               Display basic information about R session              ###
############################################################################

# display directory where R is installed
.libPaths()

# display installed packages
library()

# another way to display packages which have been installed 
installed.packages() |>
     head(5)

# verify and update all installed packages
update.packages()

# basic parameters of the current session
sessionInfo()

# Get the current working directory
getwd()

# Set/change the current working directory - examples 
# on Mac OS
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

# on Windows (notice the `/` instead of `\`)
setwd("C:/Users/Marin F/Google Drive/R(Mac)-1 googledrive/DataSets")

# check the new current working directory
getwd() 

# display files in the current default directory
dir()
# or
list.files()

save.image('r_session_2023-10-23.RData')

###########################################################
###         Useful commands for managing objects        ###
###########################################################

# remove all objects in current workspace
rm(list=ls()) 

# restore (load) a previously saved workspace (not run, just for illustration)
load("r_session_2023-10-23.RData")

# List the objects in the current workspace
ls() 

# Remove (delete) one or more objects.
rm(a_complex_var, ts) 

ls() 

# list all objects whose name contains "a" letter
ls(pattern = 'a')

# list all objects whose name starts with "a" letter
ls(pattern = '^a')

# list all objects whose name starts with string "interval" 
ls(pattern = '^interval')

# list all objects whose name starts with string "interval." (. is a joker character)
ls(pattern = '^interval\\.')

# list all objects whose name starts with "a" or "t" letters
ls(pattern = '^[at]')
# or
ls(pattern = '^a|^t')

# list all objects whose name starts with "a" and, after "a",
#   there is at least one occcurence of "var" letter sequence
ls(pattern = '^a.+var')
# or
ls(pattern = '^a.+c+')
# ".+" stands for one or more occurences of any character

# list all objects whose name ends in "var" letter sequence
ls(pattern = "var$")

# save the current workspace (all variables) (not run, just for illustration)
#  in a single .RData file
save.image(file="session_2023-10-23.RData")

# if you want to save the workspaces if a file containing the current date: 
file_name <- paste('backup', as.Date(now()), sep = '_')
save.image(file=file_name)


# save the command launched in current session as an R script
#    (be careful, because there is a default limitation of number of saved commands)
savehistory(file = "work2023-10-23.R")
# after saving, the file can be opened as every R script


# remove all objects in current workspace
rm(list=ls()) 

# restore (load) a previously saved workspace (not run, just for illustration)
load("session_2023-10-23.RData")


############################################################################
###                           Installing packages                        ###

## install from the oficial site of R
install.packages("stringr")
# or
install.packages("stringr", repos='http://cran.us.r-project.org')
library(stringr)

install.packages("devtools")
require(devtools)


## install from another repository ()
## sometimes these versions are not posted on "cran...", but 
##    they are usually the most recent ones)

## One example
install.packages("RJSONIO", repos = "http://www.omegahat.org/R", type="source")


## Install an older version of the package from a web archive
install.packages("http://cran.r-project.org/src/contrib/Archive/RNetLogo/RNetLogo_0.9-6.tar.gz", repo=NULL, type="source")


## Another example (needs package "devtools")
# for Mac and Linux:
devtools::install_github("hadley/devtools")

# for Windows:
library(devtools)
build_github_devtools()

# install other two packages from "github"
install_github("rplos", "rOpenSci")
require(rplos)

# data.table
install.packages('data.table', type='source', repos='https://Rdatatable.github.io/data.table')



# After instalation, in every subsequent RStudio session that needs 
#   that package, command "library" (or require) is required (once per session) 
library(stringr)

# from time to time it is useful to update all the "oficially" installed
#   packages to their latest versions
update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')



############################################################################
###                           Learning R from R                          ###
### https://swirlstats.com/students.html
### https://github.com/swirldev/swirl_courses
install.packages("swirl")
library(swirl)
swirl()


sessionInfo()

