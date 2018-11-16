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
###             4a. Programming: Basic control structures                ###
### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 16.11.2018

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
###                   I. Basics of Control Structures                 ###
#########################################################################


#########################################################################
###                   I.a `if`, `ifelse`, `if_else`, `case_when`      ###
#########################################################################


## simple `if...else` structures
a <- 25
print(a)
(a <- 25)

if (a > 10) "TRUE" else "FALSE"
ifelse(a > 10,"TRUE", "FALSE")

# `dplyr` package provides an optimized version - `if_else`
if_else(a > 10,"TRUE", "FALSE")

a <- 4
if (a > 10) "TRUE" else "FALSE"

(result <- if_else(a < 10, "true", "false"))


##   writing as a `proper` code sequence
if (a < 10) {
     result <- "true"
} else {
     result <- "false"
}
print(result)


#########################################################################
##  `ifelse`, `if_else` - `dplyr` package` (and a source of troubles)  ##
##   
## (Anonymized) FEAA students for 2014-2015 academic year 
## example taken from script `03b_tidy-verse.R`
getwd()
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)
## display the data frame structure
glimpse(studs)

## display variabile `YEAR_OF_STUDY` values
studs$YEAR_OF_STUDY

##
## task: change the roman numbers (I, II, III) with the arabic equivalents (1, 2 or 3)
## 

# Sol.1 - with `ifelse` (base R)
studs <- mutate(studs, 
     YEAR_OF_STUDY = as.integer(ifelse(YEAR_OF_STUDY == 'I', 1, 
          ifelse(YEAR_OF_STUDY == 'II', 2, 
          ifelse(YEAR_OF_STUDY == 'III', 3, as.integer(YEAR_OF_STUDY))))))
table(studs$YEAR_OF_STUDY)
typeof(studs$YEAR_OF_STUDY)


# Sol.2 - with `if_else` (`dplyr` package) - DOES NOT WORK!
studs <- mutate(studs, 
     YEAR_OF_STUDY = as.integer(if_else(YEAR_OF_STUDY == 'I', 1, 
          if_else(YEAR_OF_STUDY == 'II', 2, 
          if_else(YEAR_OF_STUDY == 'III', 3, as.integer(YEAR_OF_STUDY))))))

# Sol.3 - with `if_else` (`dplyr` package) - forcing the numeric values
# `1`, `2`, and `3` to be integers (1L, 2L, 3L)
studs <- mutate(studs, 
     YEAR_OF_STUDY = as.integer(if_else(YEAR_OF_STUDY == 'I', 1L, 
          if_else(YEAR_OF_STUDY == 'II', 2L, 
          if_else(YEAR_OF_STUDY == 'III', 3L, as.integer(YEAR_OF_STUDY))))))
table(studs$YEAR_OF_STUDY)
typeof(studs$YEAR_OF_STUDY)


# Sol.4 - with `if_else` (`dplyr` package) - getting the `YEAR_OF_STUDY`
# as a real number (`double`) instead of `integer`
studs <- mutate(studs, 
     YEAR_OF_STUDY = as.numeric(if_else(YEAR_OF_STUDY == 'I', 1, 
          if_else(YEAR_OF_STUDY == 'II', 2, 
          if_else(YEAR_OF_STUDY == 'III', 3, as.numeric(YEAR_OF_STUDY))))))
table(studs$YEAR_OF_STUDY)
typeof(studs$YEAR_OF_STUDY)



#########################################################################
##                     case_when - `dplyr` package`                    ##
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
###                                I.b Loops                          ###
#########################################################################
###

#########################################################################
##  Task:                                                             ###
##  Given the fuel efficiency data set (see above) ...                ###
##  Which is the numeric variable with the largest                    ###
##       standard deviation?                                          ###
#########################################################################


#########################################################################
# Note: there are more elegant solutions (see script 05a and 05b),
#    but for now we will introduce the basic loop structures
glimpse(fuel_economy_2018)
names(fuel_economy_2018)


# sol. 1 - `repeat`/`break`
variable <- ""
max_sd <- 0
i <- 1
repeat {
     if (is.numeric(fuel_economy_2018[[i]])) {
          if (sd(fuel_economy_2018[[i]], na.rm = TRUE) > max_sd ) {
               variable = names(fuel_economy_2018) [i]
               max_sd <- sd(fuel_economy_2018[[i]], na.rm = TRUE)
          }
     }
     i <- i + 1
     if (i > ncol(fuel_economy_2018)) 
          break
}
print(variable)
print(max_sd)


# sol. 2 - `while`
variable <- ""
max_sd <- 0
i <- 1
while (i <= ncol(fuel_economy_2018)) {
     if (is.numeric(fuel_economy_2018[[i]])) {
          if (sd(fuel_economy_2018[[i]], na.rm = TRUE) > max_sd ) {
               variable = names(fuel_economy_2018) [i]
               max_sd <- sd(fuel_economy_2018[[i]], na.rm = TRUE)
          }
     }
     i <- i + 1
}
print(variable)
print(max_sd)


# sol. 3 - `for`
variable <- ""
max_sd <- 0
for (i in 1:ncol(fuel_economy_2018)) {
     if (is.numeric(fuel_economy_2018[[i]])) {
          if (sd(fuel_economy_2018[[i]], na.rm = TRUE) > max_sd ) {
               variable = names(fuel_economy_2018) [i]
               max_sd <- sd(fuel_economy_2018[[i]], na.rm = TRUE)
          }
     }
}
print(variable)
print(max_sd)


# sol. 3' - `for ... seq_along` 
variable <- ""
max_sd <- 0
for (i in seq_along(fuel_economy_2018)) {
     if (is.numeric(fuel_economy_2018[[i]])) {
          if (sd(fuel_economy_2018[[i]], na.rm = TRUE) > max_sd ) {
               variable = names(fuel_economy_2018) [i]
               max_sd <- sd(fuel_economy_2018[[i]], na.rm = TRUE)
          }
     }
}
print(variable)
print(max_sd)


# sol. 4 - another type of `for`
variable <- ""
max_sd <- 0
for (attribute in names(fuel_economy_2018)) {
     if (is.numeric(fuel_economy_2018[[attribute]])) {
          if (sd(fuel_economy_2018[[attribute]], na.rm = TRUE) > max_sd ) {
               variable = attribute
               max_sd <- sd(fuel_economy_2018[[attribute]], na.rm = TRUE)
          }
     }
}
print(variable)
print(max_sd)




#########################################################################
###  II. Other tasks and solutions requiring Basic Control Structures ###
#########################################################################


#########################################################################
##  II.1. Task:                                                        ##
##  Extract/display in a separate data frame only the numeric columns  ##
##  of data frame `fuel_economy_2018` (see above)                      ##
##     (for the moment, we pretend we don't know how to work with      ##
##     `apply` family, map, select_if, ...                             ##
#########################################################################


# Sol. 1
j <- 1
# initialize the result dataset
result <- tibble()

# loop through original data frame columns and check if they are
# numeric; if so, add to the current columns of `result`
for (j in 1:ncol(fuel_economy_2018)) {
     if (is.numeric(fuel_economy_2018[[j]])) {
          if (nrow(result) == 0) {
               result <- fuel_economy_2018[,j, drop = FALSE]
          } else {
               result <- bind_cols(result, fuel_economy_2018[,j, drop = FALSE])
          }
     }
}

# display result
glimpse(result)


## Sol. 2 - in the loop, only column names are "gathered" 
cols <- c()
for (j in 1:ncol(fuel_economy_2018)) {
     if (is.numeric(unlist(fuel_economy_2018[, j]))) {
          cols <- c(cols, names(fuel_economy_2018)[j])
     }
}

# now, get the result based on `cols`vector
result_2 <-  fuel_economy_2018[cols]
glimpse(result_2)



#########################################################################
##  II.2. Task:                                                        ##
##       Create sub-folders for each team at various courses           ##
#########################################################################
##            
##  Example:
## 1. Set `/Users/marinfotache/Google Drive/R(Mac)/DataSets/Students`                                                                 
##       as default working directory (change this with your current directory)
## 2. Check if in this directory there is a subdirectory called `Teams_FRM`
##       If not, created it
## 3. Set `Teams_FRM` as current working directory 
## 4. Given that there for FRM master programme there are 10 teams of
##       students, create a separate directory for each team called 
##       `FRM101`, `FRM102`, ... `FRM110`     
#########################################################################

####                           Solution

## 1. store the current directory in a variable (for latter restoring
##        the curent directory)
initial_wd <- getwd() 

### 2. Set `/Users/marinfotache/Google Drive/R(Mac)/DataSets/Students`                                                                 
###       as default working directory
main_dir <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/Students'
setwd(main_dir)
getwd()

### 3. Check if in this directory there is a subdirectory called `Teams_FRM`
###       If not, created it
sub_dir <- 'Teams_FRM'
if (!dir.exists( file.path(main_dir, sub_dir)))
     dir.create(file.path(main_dir, sub_dir))


### 4. Set `Teams_FRM` as current working directory 
new_wd <- paste(main_dir, sub_dir, sep = '/')
setwd(new_wd)
getwd()


### 5. Given that there for FRM master programme there are 10 teams of
###       students, create a separate directory for each team called 
###       `FRM101`, `FRM102`, ... `FRM110`     
for (i in 101:110)
{
     team_directory <- paste('FRM', i, sep='')
     if (!dir.exists(file.path(new_wd, team_directory)))
          dir.create(file.path(new_wd, team_directory))
}


### 6. Restore the initial working directory
setwd(initial_wd)



#########################################################################
##  II.3. Task:                                                        ##
##       Copy a given file into a set of destination directories       ##
#########################################################################
###            
###  Example:
###  Copy the file `northwind.RData` which is in 
###       `/Users/marinfotache/Google Drive/R(Mac)/DataSets/` directory
###       into ALL subdirectories of 
###  `/Users/marinfotache/Google Drive/R(Mac)/DataSets/students/Teams_FRM`
###       directory
#########################################################################

####                           Solution

# 1. store the current directory in a variable (for latter restoring
#        the curent directory)
initial_wd <- getwd() 

# 2. set the source file
source_file <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/northwind.RData'

# 3. get all the destionation subdirectories
base_dir <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/students/Teams_FRM'
setwd(base_dir)
destination_subdirs <- list.dirs(path = base_dir, 
                                 full.names = FALSE, recursive = FALSE)
print(destination_subdirs)


# 4. copy the source file in all the destionation subdirectories
for (crt_folder in destination_subdirs)
{
     full_path <- paste(base_dir, crt_folder, sep = '/')
     file.copy(from = source_file, to = full_path, 
               recursive = FALSE, copy.mode = TRUE, overwrite = TRUE)
}     

# 5. Restore the initial working directory
setwd(initial_wd)




#########################################################################
##  II.4. Task:                                                        ##
##       Copy a randomly chosen file (from a list of files in          ##  
##       a source directory) into a set of destination directories     ##
#########################################################################
##           
##  Example:
##  Directory `/Users/marinfotache/Google Drive/R(Mac)/DataSets/northwind_xlsx`
##       contains a set of `.xlsx` files (categories.xlsx, ...)  
##  Copy into ALL subdirectories of 
##       `/Users/marinfotache/Google Drive/R(Mac)/DataSets/students/Teams_FRM`
##       directory a randomly chosen file `.xlsx` file (the `.xlsx` file 
##       will be chosenn randomly for each destination directory
##       
#########################################################################

####                           Solution

# 1. store the current directory in a variable (for latter restoring
#        the curent directory)
initial_wd <- getwd() 

# 2. get the set all the source files
source_directory <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/northwind_xlsx'
source_files_set <- list.files( path = source_directory, 
          pattern = '.xlsx', full.names = TRUE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)

# 3. get all the destination subdirectories
base_dir <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/students/Teams_FRM'
setwd(base_dir)
destination_subdirs <- list.dirs(path = base_dir, full.names = TRUE, recursive = FALSE)
print(destination_subdirs)


# 4. copy a randomly chosen source file in each destionation subdirectory
for (crt_folder in destination_subdirs)
{
     source_file <- sample(source_files_set, 1)
     file.copy(from = source_file, to = crt_folder, 
               recursive = FALSE, copy.mode = TRUE, overwrite = TRUE)
}     

# 5. Restore the initial working directory
setwd(initial_wd)



#########################################################################
##  II.5. Task:                                                         ##
##   Given two source directories, `source_dir1` and `source_dir2`,    ##
##     each with a different number of files (of various types):       ##
##   Also given a set of destination directories (dest_dir1, ...).     ##
##   Copy, into each destination directory, a file from `source_dir1`  ##
##       and another one from the `source_dir2` using the following    ##
##       rule:                                                         ##
##       First file from `source_dir1` (`file_1__source_dir1`)         ##
##      will be copied into `dest_dir1`, second file from              ##
##       `source_dir1` (`file_2__source_dir1``) will be copied into    ##
##       `dest_dir2`, etc.                                             ##
##   When the number of destination directories exceeds                ##
##       the number of files in `source_dir1`,                         ##
##       the source files will restart with `file_1__source_dir1`      ##
##       and then will continue with file_2__source_dir1`, ...         ##
##   The same rule applies for the files in `source_dir2`              ##
########################################################################
###            
###  Example:
###  `source_dir1`: 
###    `/Users/marinfotache/Google Drive/R(Mac)/DataSets/northwind_xlsx`
###  `source_dir2`: 
###    `/Users/marinfotache/Google Drive/R(Mac)/DataSets/DragosCogean`
###   destination directories:   
###       ALL subdirectories of 
###  `/Users/marinfotache/Google Drive/R(Mac)/DataSets/students/Teams_FRM`
#########################################################################

####                           Solution

# 1. store the current directory in a variable (for latter restoring
#        the curent directory)
initial_wd <- getwd() 

# 2. get the set all the source files
source_dir1 <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/northwind_xlsx'
source_files_set1 <- list.files( path = source_dir1, 
          full.names = TRUE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)
print(source_files_set1)

source_dir2 <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/DragosCogean'
source_files_set2 <- list.files( path = source_dir2, 
          full.names = TRUE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE)
print(source_files_set2)


# 3. check if there is at least one file in each source directory
if (length(source_files_set1) == 0)
     print0(paste('Source directory `', source_dir1, '` contains no files'))

if (length(source_files_set2) == 0)
     print0(paste('Source directory `', source_dir2, '` contains no files'))


# 4. get all the destination subdirectories
base_dir <- '/Users/marinfotache/Google Drive/R(Mac)/DataSets/students/Teams_FRM'
setwd(base_dir)
destination_subdirs <- list.dirs(path = base_dir, full.names = TRUE, recursive = FALSE)
print(destination_subdirs)


# 5. set the indexes referring to the files in the source directories
index_source_1 <- 1
index_source_2 <- 1


# 6. copy, in each destionation subdirectory, two files according
# to the above rule
for (crt_folder in destination_subdirs)
{
     
     # get the source file in the first source directory
     source_file_1 <- source_files_set1 [index_source_1]
     # copy the file
     file.copy(from = source_file_1, to = crt_folder, 
               recursive = FALSE, copy.mode = TRUE, overwrite = TRUE)
     # increment the index
     index_source_1 <- index_source_1 + 1
     # check if the index exceeds the file set length
     if (index_source_1 > length(source_files_set1))
          index_source_1 <- 1
     
     
     # get the source file in second source directory
     source_file_2 <- source_files_set2 [index_source_2]
     # copy the file
     file.copy(from = source_file_2, to = crt_folder, 
               recursive = FALSE, copy.mode = TRUE, overwrite = TRUE)
     # increment the index
     index_source_2 <- index_source_2 + 1
     # check if the index exceeds the file set length
     if (index_source_2 > length(source_files_set2))
          index_source_2 <- 1
}     


# 7. Restore the initial working directory
setwd(initial_wd)






