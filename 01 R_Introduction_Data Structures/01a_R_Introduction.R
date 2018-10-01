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
### xxxxxxxx
############################################################################
## last update: 01.10.2018


############################################################################
###               Display basic information about R session              ###
############################################################################

# display directory where R is installed
.libPaths()

# display installed packages
library()

# another way to display packages which have been installed 
installed.packages()

# verify and update all installed packages
update.packages()

# basic parameters of the current session
sessionInfo()

# Get the current working directory
getwd()

# Set/change the current working directory - examples 
# Mac OS
setwd("/Users/marinfotache/Google Drive/R(Mac)/DataSets")

# Windows
setwd("C:/Users/Marin F/Google Drive/R(Mac)/DataSets")

# check the new current working directory
getwd() 

# display files in the current default directory
dir()
# or
list.files()

# save.image('r_session_2018-10-01.RData')

###########################################################
###         Useful commands for managing objects        ###
###########################################################

# remove all objects in current workspace
rm(list=ls()) 

# restore (load) a previously saved workspace (not run, just for illustration)
load("r_session_2018-10-01.RData")

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
save.image(file="session_2018-10-01.RData")

# if you want to save 
as.Date(now())


# save the command launched in current session as an R script
# be careful, because there is a default limitation of number of saved commands
savehistory(file = "work2018-05-21.R")
# after saving, the file can be opened as every R script

# remove all objects in current workspace
rm(list=ls()) 

# restore (load) a previously saved workspace (not run, just for illustration)
load("work2018-05-21.RData")

load("work_2017-12-07.RData")


# restore the commands saved as history (not run, just for illustration)
loadhistory(file = "work2018-05-21.R")


????
     
####################################################
###   data frames used in further scripts


## data frame for enrollment at internationalized master programmes at FEAA
program = c(rep("ADL",7), rep("FRM",5), rep("SAAS", 7))
academicyear = c (2009, 2010, 2011, 2012, rep(c(2010, 2011, 2012), 2), 
     2011, 2012, 2009, 2010, 2011, 2012, 2010, 2011, 2012)
studyyear = c(rep("Anul I",4), rep("Anul II",3),rep("Anul I",3), rep("Anul II", 2),
                rep("Anul I",4), rep("Anul II", 3) )
nofstuds = c(19, 28, 49, 48, 17, 27, 49, 15, 20, 26, 15, 19, 9, 13, 0, 0, 9, 12, 0)

# the data frame
mpi <- data.frame(program, academicyear, studyyear, nofstuds, stringsAsFactors=TRUE)
mpi
str(mpi)
head(mpi)
tail(mpi)





# remove the vectors (we'll work only with the data frame)
rm(program, academicyear, studyyear, nofstuds)

# save the data frame
save(mpi, file="mpi.RData")


## leadership data frame (taken from Kabacoff (R in Action), 2011)
manager <- c(1, 2, 3, 4, 5)
date <- c("2010/10/24", "1995/10/28", "1985/10/1", "2000/12/10", "1966/1/9")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 59, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, 
        stringsAsFactors=FALSE)
leadership
save(leadership, file="leadership.RData")

???


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

