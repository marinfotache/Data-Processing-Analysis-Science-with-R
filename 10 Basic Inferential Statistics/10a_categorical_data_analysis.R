###############################################################################
### Document partially supported by research project: POC/398/1/1 nr. 124759 -
### „Research As A Service – Iasi (RaaS-IS)”
###############################################################################


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
###     10a. Categorical Data Analysis (Basic Inferential Statistics)    ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/10%20Basic%20Inferential%20Statistics/10_basic_inferential_statistics.pptx
############################################################################
## last update: 31.08.2021

### two R packages are particularly important for categorical data
###    analysis, "vcd" and "vcdExtra"
#install.packages("vcd", dependencies = T)
#install.packages("vcdExtra")
library(vcd)
library(vcdExtra)


#install.packages('ggmosaic')
library(ggmosaic)
library(scales)
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

# check if the current directory is ok
getwd()
############################################################################



###############################################################################
###                                Tutorials
###############################################################################

# Vassarstats - Chi Square Analyses
# https://www.youtube.com/watch?v=AdeCanzDw3E&feature=em-subs_digest


#https://www.youtube.com/watch?v=WXPBoFDqNVk&feature=em-subs_digest-vrecs

#    Chi-Square Test, Fishers Exact Test, and Cross Tabulations in R (R Tutorial 4.7)
# https://www.youtube.com/watch?v=POiHEJqmiC0&list=PLqzoL9-eJTNBDdKgJgJzaQcY6OXmsXAHU&index=31

#    Relative Risk, Odds Ratio and Risk Difference (aka Attributable Risk) in R (R Tutorial 4.8)
# https://www.youtube.com/watch?v=V_YNPQoAyCc&index=34&list=PLqzoL9-eJTNBDdKgJgJzaQcY6OXmsXAHU

# https://extension.usu.edu/evaluation/files/uploads/Start%20Your%20Engine/Study%20the%20Route/Analyze%20the%20Data/Interpreting_Chi_Square_Printouts.pdf

# https://onlinecourses.science.psu.edu/stat504/book/export/html/102

# http://www.statsdirect.com/help/default.htm#chi_square_tests/rc.htm

# https://www.youtube.com/watch?v=vtUXsmOIi9g


# giving up scientific notation (1.6e+07)
options(scipen = 999)



############################################################################
###              		 Categorical data analysis        	    	        ###
### Frequency/contingency tables, mosaic plots, independence tests, etc. ###
############################################################################

############################################################################
###  Before diving into the any statistical test, you have to 
###  explore data. 
###  See:
###  - in section `08 Data Visualization with -mostly- ggplot2` (https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/08%20Data%20Visualization%20with%20-mostly-%20ggplot2)
### the script `08b_categorical_variables_visualization.R` 
### 
###  - in section
###  `09 Exploratory Data Analysis` (https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/09%20Exploratory%20Data%20Analysis)
### the script `09a_descriptive_statistics.R` (mainly the sections `II.3` and `III.3`)

############################################################################

###  There are two types of categorical variables:
###		* nominal (qualitative, no order implied): sex/gender, 
###			marital status, country, etc.
###       * ordinal: academic cycle (undergraduate, master, Ph.D. student), 
###			Likert scale (1-5), etc.


###
### For nominal data analysis there are 3 common statistical procedures:
### a. The Chi-Square Test for Goodness of Fit assesses whether or not 
###     observed nominal data conforms to (or is statistically different 
###     from) some theoretical or expected distribution 
### b. The Chi-Square Test of Independence or Association tests whether 
###    or not two variables from the same sample are related 
### c. The Chi-Square Test of Homogeneity assesses whether or not two or 
###      more samples, drawn from different populations, are homogeneous 
###      (e.g., are similarly associated) on some characteristic of interest.
###
###  Note: Chi-Square is classified as a nonparametric statistical test
###
### The null hypotheses (H0) of the Chi-Square test are:
###  a. there is either “no differences between observed or 
###     expected frequencies" (Goodness of Fit) or
###  b. the variables or samples are not related, i.e., are independent" 
###     (Test for Independence) or 
###  c. the variables are homogeneous (Test of Homogeneity).
###
###   Chi-Square is not directed at any specific alternative hypothesis (Ha)
### 
### Degrees of freedom, alpha, and power must be specified or determined.

###  While a statistically significant Chi-Square establishes a 
###    relationship between two variables, it reveals little else. 
###  A statistically significant chi-square is not a measure 
###    of the strength of an association.

### Problems with the expected frequency cell size in Chi-Square test!
###   The greater the numerical difference between observed and 
###     expected frequencies within cells, the more likely is 
###     a statistically significant Chi-Square.
###   Cells with estimated frequencies (<5) may yield an inflated Chi-Square.
###   There is considerable debate concerning small cell expected frequencies 
###    A general advice:
### - for df = 1, all expected frequencies should be at least 5;
### - for df = 2, all expected frequencies should be at least 3; and
### - for df = 3, all but one expected frequency value should 
###            be equal or greater than 5.



#########################################################################
### 			I. Inference for a (Single) Proportion                 ###
#########################################################################
###
###  Theoretical example:
###  950  randomly selected people were asked the question: 
###     Are you pleased with Romanian politicians  (Yes or No)?
###  424 responded yes (you realize this is not a real survey :-))  

424 / 950


### H0: the proportion of yes respondents is equal (0.5) to 
###    the proportion of "no" respondents

# function "prop.test" tests the hypotheses of equality of proportion with 
#  a specified threshold and produce confidence intervals
prop.test(424, 950)
# or
prop.test(424, 950, p = .5, alternative = "two.sided", 
	conf.level = 0.95, correct = TRUE)
# for the above test, 
#   * H0: proportion is 0.5
#   * a 95% confidence interval (CI) for the proportion is calculated
#   * both the test and the CI incorporate a continuity correction


# change the parameters:
# * H0: proportion = 0.4 
# * Ha: the one-sided alternative  - proportion > 0.4 
# * a 99% (one-sided) CI for proportion
# * all without continuity correction
prop.test(424, 950, p = .4, alternative = "greater",
	conf.level = 0.95, correct=FALSE)


# change the parameters:
# * H0: proportion > 0.4 
# * Ha: the one-sided alternative proportion < 0.4 
# * a 95% (one-sided) CI for proportion
# * all with continuity correction
prop.test(424, 950, p = .4, alternative = "less",
	conf.level = 0.95, correct=TRUE)


# change the parameters:
# * H0:proportion < 0.4 
# * Ha: the one-sided alternative proportion > 0.4 
# * a 95% (one-sided) CI for proportion
# * all with continuity correction
prop.test(424, 950, p = .4, alternative = "greater",
     conf.level = 0.99, correct = TRUE)



#########################################################################
### 	               II. Tests of independence                         ###
#########################################################################
# You Tube tutorials

# Statistics 101: Introduction to the Chi-square Test
# https://www.youtube.com/watch?v=SvKv375sacA

# How to calculate Chi Square Test for Independence (two way)
# https://www.youtube.com/watch?v=xEiQn6sGM20

#
#http://www.r-bloggers.com/the-chi-squared-test-of-independence-an-example-in-both-r-and-sas/


##            Chi-square test of independence
# Function chisq.test() can be applied to a two-way table in order 
#  to produce a  chi-square test of independence of the row and 
#  column (categorical) variables.



#######################################################################
###	               II.1      "Arthritis" dataset" 
###  presentation, variables, visualization, tests of independence ###
#######################################################################
data(Arthritis, package = 'vcd')

# Data set "Arthritis" dataset" is included in the "vcd" package 
# (taken from the book "R in action" (by R. Kabacoff))
# Data - Kock & Edward (1988) - represent a double-blind clinical trial 
#  of new treatments for rheumatoid arthritis
# the form of tha data set: data frame
View(Arthritis)
# each observation describes one person

table(Arthritis$Treatment)


table(Arthritis$Improved)

Arthritis %>%
     group_by(Improved) %>%
     tally()


# There are two explanatory factors: "Treatment" and "Sex". 
# "Age" is a covariate, and "Improved" is the response (an 
#   ordered factor, with levels None < Some < Marked). 
# Excluding "Age", we would have a 2 x 2 x 3 contingency table for 
#   "Treatment", "Sex" and "Improved".

# "Treatment" (whose possible values are Placebo and Treated), 
#   "Sex" (Male, Female), and "Improved" (None, Some, Marked) 
#     are all categorical factors.
str(Arthritis)
# order the factor "Improved" 
Arthritis$Improved <- ordered(Arthritis$Improved, 
	levels=c("None", "Some", "Marked"))


### visualize information about attribute `Improved` - sol. 1
table(Arthritis$Improved)

ggplot(Arthritis, 
     aes(x = factor(Improved), fill = "yellow2")) + 
	geom_bar(width = 1, color="white") +
	geom_text(stat="count", color="black", hjust=.5, vjust=1.2, size=6,
		aes(y=..count.., label=..count..)) + 
	ggtitle("Result of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Result") +
	guides(fill=FALSE)  # no legend



### visualize information about attribute `Improved` - sol. 2
dat <- data.frame(table(Arthritis$Improved))
dat
ggplot(dat, aes(x = Var1, fill = Var1)) +
  	geom_bar(stat="identity",
  		aes(y=Freq), position="dodge") +
  	geom_text(aes(x=Var1, y=Freq,  
  		label=Var1, hjust=ifelse(sign(Freq)>0, 1.1, 0)),
  		size = 5.5,
          position = position_dodge(width=1), show.legend = F) +
  	scale_y_continuous(labels = waiver()) + 
     coord_flip() +
     	ggtitle("Result of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Result") +
	guides(fill=FALSE)  # no legend

# we do not need text on axis y
ggplot(dat, aes(x = Var1, fill = Var1)) +
  	geom_bar(stat="identity",
  		aes(y=Freq), position="dodge") +
  	geom_text(aes(x=Var1, y=Freq,  
  		label=Var1, hjust=ifelse(Freq>20, 1.1, 0)),
  		size = 5.5,
          position = position_dodge(width=1), show.legend = F) +
  	scale_y_continuous(labels = waiver()) + 
     coord_flip() +
     	ggtitle("Result of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
     theme(axis.text.y = element_blank(),    # here we remove the text in axis Y
		text=element_text(size=12)) +      
   	ylab("Number of cases") + xlab("Result") +
	guides(fill=FALSE)  # no legend
    
     
# visualize information about attribute `Treatment`
table(Arthritis$Treatment)

ggplot(Arthritis, aes(x = factor(Treatment), fill = "yellow2")) + 
	geom_bar(width = 1, color="white") +
	geom_text(stat="count", color="black", hjust=.5, vjust=1.2, size=6,
		aes(y=..count.., label=..count..)) + 
	ggtitle("Patients of the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Treatement Groups") +
	guides(fill=FALSE)  # no legend


# visualize information about attribute `Sex`
table(Arthritis$Sex)

ggplot(Arthritis, aes(x = factor(Sex), fill = "yellow2")) + 
	geom_bar(width = 1, color="white") +
	geom_text(stat="count", color="black", hjust=.5, vjust=1.2, size=6,
		aes(y=..count.., label=..count..)) + 
	ggtitle("Sex of the Patients included in the Treatment", 
	        subtitle = "(`Arthritis` dataset is included in the `vcd` package)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
   	ylab("Number of cases") + xlab("Treatment Groups") +
	guides(fill=FALSE)  # no legend


##   visualize information about attribute `Age` (numeric)
Arthritis$Age 

# histogram
ggplot(Arthritis %>% filter (!is.na(Age)),
     aes(Age)) + 
     geom_histogram(binwidth = 5,colour = "black", fill = "red") +
	ggtitle("Distribution of Patients Age ") +
	theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     theme(axis.text.y=element_text(size=12)) +
     theme(axis.text.x=element_text(size=10, angle = 25)) +
	xlab("Patient's Age") + ylab("Number of cases") +
     scale_x_continuous(breaks = seq(0,100,5)) 
     
# density curve
ggplot(Arthritis %>% filter (!is.na(Age)),
     aes(Age)) + 
     geom_density(fill = 'blue', alpha = 0.2) +
	ggtitle("Distribution of Patients Age ") +
	theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     theme(axis.text.y=element_text(size=12)) +
     theme(axis.text.x=element_text(size=10, angle = 45)) +
	xlab("Patient's Age") + ylab("Number of cases") +
     scale_x_continuous(breaks = seq(0,100,5)) 


#######################################################################
# Research Question
#   Is the treatment effective in fighting with the disease?
#######################################################################

###   Two-Way Tables
# For two-way tables, the "table()" function can be used in format: 
#   mytable <- table(A, B)
#   where A is the row variable, and B is the column variable
#
## Arthritis 
(twt1 <- table(Arthritis$Treatment, Arthritis$Improved))
class(twt1)

# Alternatively, there is "xtabs" function: 
#  mytable <- xtabs(~ A + B, data=mydata)
#     where mydata is a matrix or data frame.
twt2 = xtabs( ~ Treatment + Improved, data=Arthritis)
twt2
class(twt2)


##     Two-way table using function "CrossTable" from package "gmodels" 
#install.packages("gmodels")
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

###  Three-way contingency table
thwt1 <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
thwt1


# Visualize association between thow nominal variables with a spinogram
vcd::spine(twt1, main="Spinogram Example - \nTreatment Results for Rheumatoid Arthritis")
# ..or
vcd::spine(Treatment ~ Improved, data = Arthritis)



# Visualize association between two nominal variables with a mosaic plot
mosaicplot(twt1, color = seq(1:ncol(twt1)), 
	main="Mosaic Graph - Treatment Results \nfor Rheumatoid Arthritis")


# suggested version with ggplot and ggmosaic
# library(ggmosaic)
ggplot(data = Arthritis) +
     geom_mosaic(aes(weight = 1, x = product(Improved, Treatment), 
                   fill=factor(Improved)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=0, hjust= 0.5, size = 12)) + 
     labs(x="Treatment", title='Treatment vs. Result') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     guides(fill=guide_legend(title = "Treatment Result", reverse = TRUE))



#######################################################################
###      Three tests of independence for nominal variables 

# H0: Variables `Treatment` and `Improved` are independent
mytable <- xtabs(~ Treatment + Improved, data = Arthritis)
# The p-values are the probability of obtaining the sampled 
#  results assuming independence of the row 
#  and column variables in the population. 
# When p-values < alpha (usually 0.05)  the hypothesis that treatment type and outcome are independent
#    is rejected
chisq.test(mytable)
# p-value = 0.001 so we reject null hypothesis ! 
#    Variables Treatment and Improved seem to be related 

qchisq(.95, df=2)


# H0: Improved and Sex are independent
mytable <- xtabs(~ Improved + Sex, data = Arthritis)
chisq.test(mytable)
qchisq(.95, df=2)

# p-value = 0.09 so we DO NOT reject null hypothesis ! 
#   There is no enough (statistical) evidence to reject 
# the claim that variables "Improved" and "Sex" are independent.
#  In other words, is seems plausible that variables "Improved" and 
#  "Sex" are independent

# The warning message is produced because one of the six cells in 
#   the table (male-some improvement) has an expected value less than five,
#   which may invalidate the chi-square approximation.


##  fisher.test() function for computing Fisher’s exact 
##  test of independence
# Fisher’s exact test evaluates the null hypothesis of independence 
#  of rows and columns in a contingency table with fixed marginals.
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
# H0: Treatment and Improved are independent
fisher.test(mytable)
#  p-value = 0.001 so H0 is rejected

# H0: Improved and Sex are independent
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)

fisher.test(mytable)
# p-value = 0.1094 so H0 is not rejected 


##
## Cochran–Mantel–Haenszel chi-square test  
##   the null hypothesis (H0): two nominal variables are conditionally 
##      independent in each stratum of a third variable.
# 
# H0: two nominal variables are conditionally independent, 
#   A and B in each stratum defined by variable C, assuming that 
#   there is no three-way interaction.
# mantelhaen.test(X)
# X is a 3 dimensional contingency table, where the last 
#  dimension refers to the strata.


# Arthritis data set
# H0: Variables `Treatment` and `Improved` are independent 
#    within each level `Sex`.
#  The test assumes that there’s no three-way (`Treatment` x `Improved` x `Sex`)
#   interaction.
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)
# p-value = 0.0006647, so H0 is rejected !



#######################################################################
###	      II.2    General Social Survey (Agresti 2002)
#######################################################################

## Example (Agresti 2002): results for the 1991 General Social Survey, 
##     with respondents classified by `sex` and `party` identification.
## Taken from
## Michael Friendly - Working with categorical data with R and the vcd 
##   and vcdExtra packages, York University, Toronto, 
## Available at: http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf
## (Downloaded in December 2013)
## 
###  Another data structure for categorical variables: frequency form 
###  Frequency form is a data frame containing one or more factors, 
###    and a frequency variable, often called Freq or count
### Useful functions: "expand.grid()" for the factors and "c()" 
###     to list the counts in a vector.

# create the data frame
GSS <- data.frame( expand.grid( sex=c("female", "male"), 
	party=c("dem", "indep", "rep")), count=c(279,165,73,47,225,191))
GSS

## Here data are already pre-aggregated (e.g. each observation does not 
##  refer to an individual, but to a group)

# visualize the data
ggplot(GSS, aes(x = sex, fill = sex)) +
  	geom_bar(stat="identity", aes(y=count), position="dodge") +
     	ggtitle("Party Identification", 
	        subtitle = "General Social Survey (Agresti, 2002)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
     theme(text=element_text(size=12)) +      
   	ylab("Number of cases") + xlab("Genre") +
     facet_wrap(~ party) +
	guides(fill=FALSE) +  # no legend 
     scale_y_continuous(breaks = seq(0,300,50)) 


## Research question: 
## Are females more "attracted" than males by a specific party?


# mosaic plot
ggplot(data = GSS) +
     geom_mosaic(aes(weight = count, x = product(sex, party), # notice `weight = count`
                   fill=factor(sex)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=0, hjust= 0.5, size = 12)) + 
     labs(x="Party", title='Party Identification, by Genre') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     guides(fill=guide_legend(title = "Genre", reverse = TRUE))


#######################################################################
###      Tests of independence 
# H0: Variables `sex` and `party` are independent
(GSStab <- xtabs(count ~ sex + party, data=GSS))
chisq.test(GSStab)
# X-squared = 7, df = 2, p-value = 0.03
# p-value < 0.05 so we reject null hypothesis ! 
#    Variables `sex` and `party` seem to be related 

fisher.test(GSStab)
# p-value = 0.03




#######################################################################
###	                    II.3   Job Satistifaction
#######################################################################

# Table created from a matrix
# taken from Friendly, M. - Working with categorical data with R and 
#    the vcd and vcdExtra packages
# downloaded in December 2013 from 
# http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf
# Friendly took it from Agresti (2002, Table 2.8, p. 57) 
## 	Job Satisfaction -  a 4 x 4 table 
# enter the matrix values...
JobSat <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4)
# ...then the dimensions, income levels (rows) and
#       satisfaction levels (columns)
dimnames(JobSat) = list(
	income=c("< 15k", "15-25k", "25-40k", "> 40k"),
	satisfaction=c("VeryD", "LittleD", "ModerateS", "VeryS"))
JobSat
str(JobSat)
class(JobSat)

# convert matrix into a table
JobSat <- as.table(JobSat)
str(JobSat)
class(JobSat)

dfJobSat <- as.data.frame(JobSat)
str(dfJobSat)

# mosaic plot
ggplot(data = dfJobSat) +
     geom_mosaic(aes(weight = Freq, x = product(satisfaction, income), 
                   fill=factor(satisfaction)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=0, hjust= 0.5, size = 12)) + 
     labs(x="Income", title='Job Satisfaction, by Income') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     guides(fill=guide_legend(title = "Satisfaction Level", reverse = TRUE))


#######################################################################
###      Tests of independence 
# H0: Variables `satisfaction` and `income` are independent
(JStab <- xtabs(Freq ~ satisfaction + income, data=dfJobSat))
chisq.test(JStab)
# X-squared = 6, df = 9, p-value = 0.7
# p-value < 0.7 so we FAIL to reject the null hypothesis ! 
#    Variables `sex` and `party` seem to be unrelated 

fisher.test(JStab)
# p-value = 0.8




#######################################################################
###	                    II.4   Hair - Eye - Color
#######################################################################
###  Another data structure for categorial variables: table form -  
### matrix, array or table object, whose elements are the frequencies 
### in an n-way (n dimensional) table.
# Data set: "HairEyeColor" in "vcd" package
# Taken from Friendly, M. - Working with categorical data with R and the vcd and vcdExtra packages
# downloaded in December 2013 from 
# http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf
# the form of this data set: table
HairEyeColor
head(HairEyeColor)
class(HairEyeColor)
str(HairEyeColor)

# Variable names (factors) and their levels are found 
#  with function "dimnames"
dimnames(HairEyeColor)

# some basic informations about the table
# get the total number of observations
sum(HairEyeColor)
# get the number of dimensions of the table 
length(dimnames(HairEyeColor))
# get the table "dimensions" sizes 
sapply(dimnames(HairEyeColor), length)

HairEyeColor
# yields the same result as
HairEyeColor[, ,]

# re-order eye color (by indexing) from dark to light
str(HairEyeColor)
HairEyeColor <- HairEyeColor[, c(1,3,4,2), ]
str(HairEyeColor)

# convert the list into a data frame
HairEyeColor_df <- data.frame(unlist(HairEyeColor))
HairEyeColor_df
str(HairEyeColor_df)
names(HairEyeColor_df)

###  One-Way (One-Dimension) Tables	 		    
## When the data source is a 3-way (or larger) tables the "structable" 
###  function from "vcd" package is useful

# Get the number of respondents for each eye colour
structable( ~ Eye, HairEyeColor)
# ...or
HairEyeColor_df %>%
     group_by(Eye) %>%
     dplyr::summarise(frequency = sum(Freq))

# Get the number of respondents for each hair colour
structable( ~ Hair, HairEyeColor)
# Get the number of respondents for each sex
structable( ~ Sex, HairEyeColor)

## transforming a 3-way table into a 2-way table -
##   with "structable" function ("vcd" package)
HairEyeColor

# Get the frequency of eye colour vs. hair colour
structable( Hair ~ Eye, HairEyeColor)
# the plot


###  Three-way contingency table
# For 3-way and larger tables the "structable" function in "vcd" package 
# variables assigned to the rows and columns of 
#  a two-way display can be specifed by a model formula.
# HairEyeColor data set
HairEyeColor
ftable(HairEyeColor)
structable(HairEyeColor)

# specify col ~ row variables (model formula)
structable(Hair+Sex ~ Eye, HairEyeColor) 


# mosaic plot
ggplot(data = HairEyeColor_df) +
     geom_mosaic(aes(weight = Freq, x = product(Eye, Hair), 
                   fill=factor(Eye)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=45, hjust= 1, size = 12)) + 
     labs(x="Hair", title='Eye Colour vs. Hair Colour, by Genre') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     guides(fill=guide_legend(title = "Eye Colour", reverse = TRUE)) +
     facet_wrap(~ Sex)


# mosaic plot
ggplot(data = HairEyeColor_df) +
     geom_mosaic(aes(weight = Freq, x = product(Eye, Sex), 
                   fill=factor(Eye)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=45, hjust= 1, size = 12)) + 
     labs(x="Genre", title='Eye Colour vs. Genre, by Hair Colour') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     theme(strip.text = element_text(size = 11, face = "bold")) +
     guides(fill=guide_legend(title = "Eye Colour", reverse = TRUE)) +
     facet_wrap(~ Hair, labeller = "label_both")    # we change the label of the panel 



#######################################################################
###      Tests of independence 

# H0: Variables `Hair` and `Sex` are independent
mytable <- xtabs(Freq ~ Hair + Sex, data = HairEyeColor_df)
chisq.test(mytable)

# ... or
mytable <- structable( ~ Hair + Sex, HairEyeColor)
chisq.test(mytable)

# The p-values are the probability of obtaining the sampled 
#  results assuming independence of the row and column variables 
#  in the population. 
# When p-values < alpha (usually 0.05)  the hypothesis that treatment type 
#    and outcome are independent is rejected

# X-squared = 7.994, df = 3, p-value = 0.0461 so we narrowly reject the null hypothesis ! 
#    Variables `Hair` and `Sex` seem to be related 

# Fisher’s exact test evaluates the null hypothesis of independence 
#  of rows and columns in a contingency table with fixed marginals.
# H0: Variables `Hair` and `Sex` are independent
fisher.test(mytable)
#  p-value = 0.0449 so H0 is rejected


##
## Cochran–Mantel–Haenszel chi-square test  
##   the null hypothesis (H0): two nominal variables are conditionally 
##      independent in each stratum of a third variable.
# 
# H0: two nominal variables are conditionally independent, 
#   A and B in each stratum defined by variable C, assuming that 
#   there is no three-way interaction.
# mantelhaen.test(X)
# X is a 3 dimensional contingency table, where the last 
#  dimension refers to the strata.

# H0: Variables `Hair` and `Eye` are independent within each level `Sex`.
#  The test assumes that there’s no three-way (`Hair` x `Eye` x `Sex`)
#   interaction.
mytable <- xtabs(~ Hair + Eye + Sex, data = HairEyeColor_df)
mantelhaen.test(mytable)
# p-value = 1, so we fail to reject H0 (Hair and Eye seem independent
# for both Females and Males)






###########################################################################
####                 III.    Measures of association                   ####

# The significance tests evaluate whether or not sufficient evidence exists 
#  to reject a null hypothesis of independence between variables. 
# When rejecting the null hypothesis, it is interesting to measure the 
#  association in order to estimate the strength of the relationships  

#  assocstats() function in the vcd package can be used to calculate 
#    the phi coefficient, contingency coefficient, 
#     and Cramer’s V for a two-way table.

###     Measures of association for a two-way table (nominal variables)
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

#       Interpretation of the Phi coefficient (similar to correlations):
# -1.0 to -0.7 strong negative association.
# -0.7 to -0.3 weak negative association.
# -0.3 to +0.3 little or no association.
# +0.3 to +0.7 weak positive association.
# +0.7 to +1.0 strong positive association.


##### trying 

# Spearman’s Rank Order correlation coefficient assesses the 
# degree of relationship between two rank-ordered variables (nonparametric). 
cor(faithful$eruptions, faithful$waiting, method="spearman")


### 				Test the correlation coefficient for significance

##   adapt on some ordinal variables on Arthritis data set
## transform ordinal variables into numeric one 
arth2 <- Arthritis
arth2
table(arth2$Treatment)
arth2$Treatment.t <- 0
arth2[arth2$Treatment == "Placebo",]$Treatment.t <- 0
arth2[arth2$Treatment == "Treated",]$Treatment.t <- 1

table(arth2$Improved)
arth2$Improved.t <- 0
arth2[arth2$Improved == "None",]$Improved.t <- 0
arth2[arth2$Improved == "Some",]$Improved.t <- 1
arth2[arth2$Improved == "Marked",]$Improved.t <- 2


###  Compute  Kendall’s Tau for Improved.t vs. Age
cor.test(arth2$Improved.t, arth2$Age, method="kendall", alternative="two.side")

# test the correlation with pgirmess::cormat
library(pgirmess)
# H0: there is no relationship 
cormat(arth2[c("Age", "Improved.t")], method="kendall", sep=TRUE)


###  Compute  Kendall’s Tau for Improved.t vs. Treatment.t
cor.test(arth2$Improved.t, arth2$Treatment.t, method="kendall", 
     alternative="two.side")

# test the correlation with pgirmess::cormat
library(pgirmess)
# H0: there is no relationship 
cormat(arth2[c("Treatment.t", "Improved.t")], method="kendall", sep=TRUE)



###############################################################################
####   IV. Case study:  Admission of students into postgraduate courses    ####
####         University of California, Berkeley, 1973                      ####
###############################################################################

# Data set: "UCBAdmissions" in "vcd" package
# Taken from Friendly, M. - Working with categorical data with R and the vcd and vcdExtra packages
# downloaded on December 2013 from 
# http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf
UCBAdmissions
str(UCBAdmissions)
class(UCBAdmissions)
UCBAdmissions[[1]]

UCBAdmissions_df <- as.data.frame(UCBAdmissions)

# summary of the admission process - 1
ftable(UCBAdmissions)

# summary of the admission process - 2
structable(UCBAdmissions)

# Number of applicants for each department
structable( ~ Dept, UCBAdmissions)

# Numbers of admitted and rejected applicants 
structable( ~ Admit, UCBAdmissions)

# Result of admission by department (mosaic plot `Admit` vs 'Dept`)
st1.UCBA <- structable( Admit ~ Dept, UCBAdmissions)
mosaicplot(st1.UCBA, color = seq(1:ncol(st1.UCBA)),
     main="Result of admission by department")
#  ...or
ggplot(data = UCBAdmissions_df) +
     geom_mosaic(aes(weight = Freq, x = product(Admit, Dept), 
                   fill=factor(Admit)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=45, hjust= 1, size = 12)) + 
     labs(x="Admitted", title='Admitted, by Departments') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     theme(strip.text = element_text(size = 11, face = "bold")) +
     guides(fill=guide_legend(title = "Admission Result", reverse = TRUE))


# Result of admission by gender 
st2.UCBA <- structable( Gender ~ Admit, UCBAdmissions)
st2.UCBA

# mosaic plot `Admit` vs 'Gender` - overall
# 
mosaicplot(st2.UCBA, color = seq(1:ncol(st2.UCBA)), 
           main="Result of admission by gender")
# ... or
ggplot(data = UCBAdmissions_df) +
     geom_mosaic(aes(weight = Freq, x = product(Admit, Gender), 
                   fill=factor(Admit)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=45, hjust= 1, size = 12)) + 
     labs(x="Genre", title='Admitted, by Gender') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     theme(strip.text = element_text(size = 11, face = "bold")) +
     guides(fill=guide_legend(title = "Admission Result", reverse = TRUE))



# mosaic plot `Admit` vs 'Gender` - by `Dept`
ggplot(data = UCBAdmissions_df) +
     geom_mosaic(aes(weight = Freq, x = product(Admit, Gender), 
                   fill=factor(Admit)), na.rm=TRUE) +    
     theme(axis.text.x=element_text(angle=45, hjust= 1, vjust = 1, size = 12)) + 
     labs(x="Genre", title='Admitted vs. Gender, by Departments') + 
     theme (plot.title = element_text (colour="black", size=17, hjust = 0.5))+
     theme(strip.text = element_text(size = 11, face = "bold")) +
     guides(fill=guide_legend(title = "Admission Results", reverse = TRUE)) +
     facet_wrap(~ Dept, scales = 'free', labeller = "label_both")  


##
## Research question: 
##   UC Berkeley was (at that time) accused of sexual discrimination !
##        Was that true ?


## Apply the Chi-Square Test of Independence or Association for proving that 
##   variables "Admit" and "Gender" are related or not

### The null hypothesis (H0) :the variables "Admit" and "Gender" are independent
chisq.test(st2.UCBA)
# X-squared = 91.609, df = 1, p-value < 0.00000000000000022

# The p-values are the probability of obtaining the sampled results 
#  assuming independence of the row and column variables in 
#   the population (the probability of obtaining the results when H0 is true)

# As the p-value < alpha (0.05)  the hypothesis that "Admit" and "Gender" 
#  are independent is rejected
# Good point for the feminists!


## Double check with Fisher's exact test
# H0 :the variables "Admit" and "Gender" are independent
m.st2.UCBA <- as.matrix(st2.UCBA)
fisher.test(m.st2.UCBA)
#  p-value < 0.001, so H0 is rejected


### But let's deepen the analysis


# Is there any relationship between admission result (Admit) and Department (Dept) ?
# Result of admission by department
st1.UCBA <- structable( Admit ~ Dept, UCBAdmissions)

# H0 :the variables "Admit" and "Dept" are independent
chisq.test(st1.UCBA)
# As the p-value < alpha (0.05)  the hypothesis that 
#    "Admit" and "Dept" are independent
#    is rejected. The result of  admission varies among 
#    departments (there are departments which reject 
#    more applicants than others)

# Fisher's exact test does not work in this case !
m.st1.UCBA <- as.matrix(st1.UCBA)
fisher.test(m.st1.UCBA)


### Final defense for UC Berkeley: 
## Cochran–Mantel–Haenszel chi-square test  
##   the null hypothesis (H0): two nominal variables are conditionally 
##      independent in each stratum of a third variable.
# 
# H0: two nominal variables are conditionally independent, 
#   A and B in each stratum defined by variable C, assuming that 
#   there is no three-way interaction.

# mantelhaen.test(X)
# X is a 3 dimensional contingency table, where the last 
#  dimension refers to the strata.

## UC Berkeley Student Admissions
# UCBAdmissions is a  2 x 2 x 6 table, with Dept as the stratifying variable.
UCBAdmissions
# H0: The Result of the admission ("Admit") is independent of "Sex" (of
#    the candidate/applicant) within each level "Department".
mantelhaen.test(UCBAdmissions)       
#  p-value = 0.2323, so for UCBA departments there is no enough evidence
#  to support the claim of gender discrimination in the admission process




##########################################################################
###                         V. Other Case Studies                      ###
##########################################################################

##
# Data set: "DaytonSurvey" in "vcdExtra" package
# Taken from Friendly, M. - Working with categorical data 
#   with R and the vcd and vcdExtra packages
# downloaded on December 2013 from 
# http://cran.us.r-project.org/web/packages/vcdExtra/vignettes/vcd-tutorial.pdf
#
# Table with the frequencies of reported use ("ever used?") of alcohol, 
#    cigarettes and  marijuana in a sample of high school seniors, 
#     also classied by sex and race.
DaytonSurvey
str(DaytonSurvey)
head(DaytonSurvey)
class(DaytonSurvey)


## Data frame "DaytonSurvey": the sex and race structure of the respondents 
DaytonSurvey
twt4 = xtabs(Freq ~ sex + race, data=DaytonSurvey)
# the graphics
spine(twt4, main = "Sex & Race \n in Dayton Survey")
mosaic(twt4, main = "Sex & Race \n in Dayton Survey", 
	shade=TRUE, legend=TRUE )
# or
mosaicplot(twt4, main = "Sex & Race \n in Dayton Survey", 
	color = seq(1:ncol(twt4)))





#########################################################################
###                                
###                      To do (during lab classes):
###                      
#########################################################################

###  
###  Given the `FEAA studs` data set:
#######################################################################
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')
file <- "anonymized_students_FEAA_2014.xlsx"
studs <- read_excel(file, sheet = 1, col_names = TRUE, skip = 0)

###  
###  1. Is there any association between the level of study (undergraduate,
###            master, ph.d.) and the financial support?
###             
###  2. Is there any association between the year of study and the financial support
###       for the undegraduate students?
###
###  3. Is there any association between the year of study and the financial support
###       for the master students?
###       
###  4. Is there any association between the programme and the financial support
###       for the undegraduate students?
###
###  5. Is there any association between the programme and the financial support
###       for the master students?
###
#########################################################################




