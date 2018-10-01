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
###                           1d. Data Sets in R                        ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/01%20R_Introduction_Data%20Structures/01d_DataSets.pptx
############################################################################
## last update: 28.09.2018



#####################################################################
####                          Built-in datasets
#####################################################################
## Some datasets are available in base (core) R (e.g. faithful)
head(faithful)

###
### Most data sets are available in packages (e.g. ggplot2, vcd, ...)
###   as we already saw and also will see throughout the next scripts
###


##  In most cases, data sets are stored as data frames

## Example: the dataset "movies" from package "ggplot2"
# first, install the package
install.packages("ggplot2movies")
# load ggplot2 package
library(ggplot2movies)
# there are too many observations (rows), so 
#  display the structure of dataset 'movies' (information about the variables of dataset 'movie')
str(movies)

# or display first six rows (observations) in 'movies' dataset
head(movies)

# display last five observations
tail(movies, 5)


# display the mean of the length of the movies
mean.movies.length = mean(movies$length)
mean.movies.length

# display the mean of the movies buget, but prior remove NA values (Non-Available)
mean.movies.budget = mean(movies$budget)
mean.movies.budget
# the mean displayed is NA, as there are films for which the budget in non known 

# display the mean of the movies buget, but prior remove NA values (Non-Available)
mean.movies.budget = mean(movies$budget, na.rm = TRUE)
mean.movies.budget


#####################################################
### There are also exceptions, such as 
# Data set: "HairEyeColor" in "vcd" package
## which is stored as three-dimension table

# Taken from Friendly, M. - 
#    Working with categorical data with R and the vcd and vcdExtra packages
# downloaded in December 2013 from 
# http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf
# the form of tha data set: table
#install.packages("vcd")
library(vcd)
HairEyeColor
head(HairEyeColor)
str(HairEyeColor)
class(HairEyeColor)

table.4 <-  HairEyeColor
class(table.4)

# Variable names (factors) and their levels are given by function "dimnames"
dimnames(HairEyeColor)

# get the total number of observations
sum(HairEyeColor)
# get the number of dimensions of the table 
length(dimnames(HairEyeColor))
# get the table sizes 
sapply(dimnames(HairEyeColor), length)

HairEyeColor
# yelds the same result as
HairEyeColor[, ,]


#####################################################
### R has a special package called "datasets"
library(datasets)
# function "data" displays all the datasets in this package
data()

### in order to visualize all the data sets available in all packages:
data(package = .packages(all.available = TRUE))


# display the datasets available in package "ggplot2"
try(data(package = "ggplot2") )
# or
data(package = "ggplot2")$results

# a list (made in 2012) of all datasets in R is available at
# http://www.public.iastate.edu/~hofmann/data_in_r_sortable.html

