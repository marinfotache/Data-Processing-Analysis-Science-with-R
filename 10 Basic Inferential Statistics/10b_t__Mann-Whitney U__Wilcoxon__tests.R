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
###       10b.Testing Group Differences (Basic Inferential Statistics)   ###
###    t-test, non-parametric tests for group differences, power tests   ###
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/10%20Basic%20Inferential%20Statistics/10_basic_inferential_statistics.pptx
############################################################################
## last update: 31.08.2021

library(vcd)
library(tidyverse)
library(readxl)
#install.packages('ggmosaic')
library(ggmosaic)
library(scales)

############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')
############################################################################



###############################################################################
###  Main objective:                                                        ###
###  Compares if two (sub)populations differ statistically significant      ###
###  in terms of a numeric variable (populations)                           ###
###  (but also we can compare a population parameter with a given/fixed     ###
###  value - see bellow)                                                    ###
###############################################################################
##             t-test assumptions:
# - Each of the two populations being compared should follow a 
#    normal distribution (this can be tested using a normality test)
#    or it can be assessed graphically using a normal quantile plot.
# -  If using Student's original definition of the t-test, the two 
#    populations being compared should have the same variance 
#    (testable using F-test, Levene's test, Bartlett's test, 
#    or the Brown–Forsythe test; or assessable graphically using a Q–Q plot). 
# - If the sample sizes in the two groups being compared are equal, 
#    Student's original t-test is highly robust to the presence of unequal 
#    variances.
# - Welch's t-test is insensitive to equality of the variances regardless 
#    of whether the sample sizes are similar.
# - The data used to carry out the test should be sampled independently
#    from the two populations being compared. 


#########################################################################
### 	    I. Visualizing numeric variables with histograms,            ###
### 	           density plots and boxplots                            ###
#########################################################################


#########################################################################
### 	                    I.1 Arthritis data set                 ###
# Data set "Arthritis" dataset" is included in the "vcd" package 
# (taken from the book "R in action" (by R. Kabacoff))
# Data - Kock & Edward (1988) - represent a double-blind clinical trial 
#  of new treatments for rheumatoid arthritis
# the form of tha data set: data frame
head(Arthritis)
names(Arthritis)
# each observation describes one person

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


#   Display overall distribution for variable `Age` with histogram 
#   (1) - Overall
ggplot(Arthritis, aes(x = Age)) + 
	geom_histogram(binwidth = 5, colour="black", fill="yellow") + 
     xlab("Age (Years)") + ylab("Frequency") +
     ggtitle("Age Distribution", 
	        subtitle = "(`Arthritis` data set)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
     theme(axis.text.x=element_text(size=10, angle = 0)) +
 	guides(fill=FALSE) +
     scale_x_continuous(breaks = seq(20,80, 5)) 


#   Display overall distribution for variable `Age` with histogram 
#   (2) separate histogram for each gender
ggplot(Arthritis, aes(x = Age)) + 
	geom_histogram(binwidth = 5, colour="black", fill="yellow") + 
     xlab("Age (Years)") + ylab("Frequency") +
     ggtitle("Age Distribution, by Gender", 
	        subtitle = "(`Arthritis` data set)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
     theme(axis.text.x=element_text(size=9, angle = 45)) +
 	guides(fill=FALSE) +
     scale_x_continuous(breaks = seq(20,80, 5)) +
     scale_y_continuous(breaks = seq(0,15, 1)) +
     facet_grid( ~ Sex)


#   Display overall distribution for variable `Age` with histogram 
#   (3) separate histogram for each treatment group
ggplot(Arthritis, aes(x = Age)) + 
	geom_histogram(binwidth = 5, colour="black", fill="yellow") + 
     xlab("Age (Years)") + ylab("Frequency") +
     ggtitle("Age Distribution, by Treatment Group", 
	        subtitle = "(`Arthritis` data set)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
     theme(axis.text.x=element_text(size=9, angle = 90)) +
 	guides(fill=FALSE) +
     scale_x_continuous(breaks = seq(20,80, 5)) +
     scale_y_continuous(breaks = seq(0,15, 1)) +
     facet_grid( ~ Treatment) +
     theme(strip.text.x = element_text(size = 14))  # Here we increase the font
                                                    #  size for the panel titles

#   Display overall distribution for variable `Age` with histogram 
#   (4) separate histogram for each treatment result
ggplot(Arthritis, aes(x = Age)) + 
	geom_histogram(binwidth = 5, colour="black", fill="yellow") + 
     xlab("Age (Years)") + ylab("Frequency") +
     ggtitle("Age Distribution, by Treatment Result", 
	        subtitle = "(`Arthritis` data set)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
     theme(axis.text.x=element_text(size=9, angle = 90)) +
 	guides(fill=FALSE) +
     scale_x_continuous(breaks = seq(20,80, 5)) +
     scale_y_continuous(breaks = seq(0,15, 1)) +
     facet_grid( ~ Improved) +
     theme(strip.text.x = element_text(size = 14))  # Here we increase the font
                                                    #  size for the panel titles


# Transform nominal variable `Improved` into a numeric variable - `Result`
Arthritis_new <- Arthritis %>%
     mutate (Result = ifelse(Improved == 'None', 0, 
               ifelse(Improved == 'Some', 1, 
                      ifelse(Improved == 'Marked', 2, NA))))

#   Display overall distribution for variable `Result` with histogram 
ggplot(Arthritis_new, aes(x = Result)) + 
	geom_histogram(binwidth = 1, colour="black", fill="yellow") + 
     xlab("Result") + ylab("Frequency") +
     ggtitle("Distribution of Variable `Result`", 
	        subtitle = "(`Arthritis` data set)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=12)) +	
     theme(axis.text.x=element_text(size=10, angle = 0)) +
 	guides(fill=FALSE) 



#########################################################################
### 	          I.2 `tips` data set (package `reshape`)                ###

# Information about each tip a waiter received over a period of a 
# few months working in one restaurant. 
# Variables:
#    - tip in dollars,
#    - bill in dollars,
#    - sex of the bill payer,
#    - whether there were smokers in the party,
#    - day of the week,
#    - time of day,
#    - size of the party.

data(tips, package="reshape2")
str(tips)
head(tips)

#   Display overall distribution for variable `tip` with histogram 
ggplot(tips, aes(x = tip)) + 
	geom_histogram(binwidth = 1, colour="black", fill="yellow") + 
     ggtitle("Overall Distribution of Tip Amount", 
	        subtitle = "(`tips` data set in package `reshape`)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 0)) +
   	xlab("Tip (in USD)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(0,20, 1)) 


#   Display overall distribution for variable `tip` with a density plot 
ggplot(tips, aes(x = tip)) + 
	geom_density(colour="black", fill="yellow", alpha = 0.3 )+ 
     ggtitle("Overall Distribution of Tip Amount", 
	        subtitle = "(`tips` data set in package `reshape`)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 0)) +
   	xlab("Tip (in USD)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(0,20, 1)) 


#   Display overall distribution for variable `tip` for each (payer) gender 
#   with a density plot 
ggplot(tips, aes(x = tip)) + 
	geom_density(colour="black", fill="yellow", alpha = 0.3 )+ 
     ggtitle("Tip Amount, by Payer Gender", 
	        subtitle = "(`tips` data set in package `reshape`)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 0)) +
     theme(strip.text.x = element_text(size = 14)) +
   	xlab("Tip (in USD)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(0,20, 1)) +
     facet_grid( ~ sex)


#   Display overall distribution for variable `tip` for each day of the week 
#   with a density plot 
ggplot(tips, aes(x = tip)) + 
	geom_density(colour="black", fill="yellow", alpha = 0.3 )+ 
     ggtitle("Tip Amouny, by Day of the Week", 
	        subtitle = "(`tips` data set in package `reshape`)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 0)) +
     theme(strip.text.x = element_text(size = 14)) +
   	xlab("Tip (in USD)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(0,20, 1)) +
     facet_grid( ~ day)

#   Display overall distribution for variable `tip` for each time (of the day) 
#   with a density plot 
ggplot(tips, aes(x = tip)) + 
	geom_density(colour="black", fill="yellow", alpha = 0.3 )+ 
     ggtitle("Tip Amouny, by Time", 
	        subtitle = "(`tips` data set in package `reshape`)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 0)) +
     theme(strip.text.x = element_text(size = 14)) +
   	xlab("Tip (in USD)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(0,20, 1)) +
     facet_grid( ~ time)



#########################################################################
### 	                    I.3 Dragos Cogean's data set                 ###
# Dragos Cogean's data set (performance comparison MongoDB vs MySQL)
# Aggregate all the data sets in DragosCogean directory into two
#  data frames, `all_inserts` and `all_reads`

all_inserts <- bind_rows(
     read_tsv("DragosCogean__InsertMongo_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta / 1000, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MongoDB'), 
     read_tsv("DragosCogean__InsertMySQL_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta / 1000, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MySQL'))

glimpse(all_inserts)

#    The outcome variables is `latency`
all_inserts$latency  

    
#   Display overall distribution for variable `latency` with histogram 
#   superimposed on a density plot (1)  
ggplot(all_inserts, aes(x = latency)) + 
	geom_histogram(aes(y = ..density..), colour="black", fill="yellow") + 
	geom_density(alpha=0.3, fill="green") +
     ggtitle("Overall Distribution of `latency` for INSERT", 
	        subtitle = "(Single-Node Data Server)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 90)) +
   	xlab("Latency (seconds)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(-2000,2000, 250)) 


#   Display overall distribution for variable `latency` for each data server 
#   with a density plot 
#   (2 - separate panels for each data server)  
ggplot(all_inserts, aes(x = latency)) + 
	geom_density(alpha=0.3, fill="green") +
     ggtitle("Distribution of `latency` for INSERT", 
	        subtitle = "(Single-Node Data Server)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=9)) +	
     theme(axis.text.x=element_text(size=7, angle = 90)) +
   	xlab("Latency (seconds)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(-2000,2000, 250)) +
     facet_grid( ~ db_server)
     

#   Display overall distribution for variable `latency` for each data server 
#   with a density plot 
#   (3 - separate panels for each data server
#   and  with removal of the results where latency is more than +/- 1250)
#   In addition, the title of each panel will be greater (more visible)
ggplot(all_inserts %>% filter (latency >= -1250 & latency <= 1250)  , 
     aes(latency)) + 
	geom_density(alpha=0.3, fill="green") +
     ggtitle("Distribution of `latency` for INSERT", 
	        subtitle = "(Single-Node Data Server)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=9)) +	
     theme(axis.text.x=element_text(size=8, angle = 45)) +
     theme(strip.text.x = element_text(size = 14)) +
   	xlab("Latency (seconds)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(-1000,1000, 250)) +
     facet_grid( ~ db_server)
     

#   Display distribution for variable `latency` for each data server, by
#   subtypes of INSERT (`test_type` variable) 
#   with a density plot 
#   (4 - separate panels for each `test_type` and `db_server`
#   and  with removal of the results where latency is more than +/- 1250)
ggplot(all_inserts %>% filter (latency >= -1250 & latency <= 1250)  , 
     aes(latency)) + 
	geom_density(alpha=0.3, fill="green") +
     ggtitle("Distribution of `latency` for INSERT", 
	        subtitle = "(Single-Node Data Server)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=9)) +	
     theme(axis.text.x=element_text(size=8, angle = 45)) +
     theme(strip.text.x = element_text(size = 14)) +
   	xlab("Latency (seconds)") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(-1000,1000, 250)) +
     facet_grid( ~ test_type + db_server )


#   Display distribution for variable `latency` for each data server with boxplot 
#   (with removal of the results where latency is more than +/- 1250)
ggplot(all_inserts %>% filter (latency >= -1250 & latency <= 1250)  , 
     aes(y=latency, x=factor(db_server))) + 
     geom_boxplot() +
     ggtitle("Distribution of `latency` for INSERT", 
	        subtitle = "(Single-Node Data Server)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=9)) +	
     theme(axis.text.x=element_text(size=11, angle = 0)) +
     theme(strip.text.x = element_text(size = 11)) +
     xlab("Data Server") + ylab("Latency (seconds)") +
	guides(fill=FALSE) 


#   Display distribution for variable `latency` by data server with boxplot 
#   for each value of `test_type`
#   (with removal of the results where latency is more than +/- 1250)
ggplot(all_inserts %>% filter (latency >= -1250 & latency <= 1250)  , 
     aes(y=latency, x=factor(db_server))) + 
     geom_boxplot() +
     ggtitle("Distribution of `latency` for INSERT", 
	        subtitle = "by categories of INSERT operation" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=11)) +	
     theme(axis.text.x=element_text(size=12, angle = 0)) +
     theme(strip.text.x = element_text(size = 14)) +
     xlab("Data Server") + ylab("Latency (seconds)") +
	guides(fill=FALSE)  +
     facet_wrap(~ test_type) +
     theme(strip.text.x = element_text(size = 12))


#####################################################################
###
all_reads <- bind_rows(
     read_tsv("DragosCogean__ReadMongo_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta / 1000, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MongoDB'), 
     read_tsv("DragosCogean__ReadMySQL_ALL.txt") %>%
          transmute (test_type = testtype, oper_type = optype, 
            millisecond = sec_x_1000, latency = latenta / 1000, 
            target_opers = `target ops`, 
            ops__record_cound = `operations/recordcount`, threads) %>%
          mutate (db_server = 'MySQL'))


# to be continued during lectures or at (your) home


#########################################################################
### 	                    I.4 Ionut Hrubaru's data set                 ###

load('Hrubaru_2016-02.RData')


# to be continued during lectures or at (your) home




#########################################################################
### 	       II.  Testing the Normality of the Distribution            ###
#########################################################################

#########################################################################
### 	               II.1 Graphically - with qqplot                    ###
# taken from 
# http://www.sthda.com/english/wiki/ggplot2-qq-plot-quantile-quantile-graph-quick-start-guide-r-software-and-data-visualization


# Arthritis data set - variable `Age`
ggplot(Arthritis_new, aes(sample=Age))+
     stat_qq() 

# Arthritis data set - variable `Result`
ggplot(Arthritis_new, aes(sample=Result))+
     stat_qq() 


# test visually the normality for variable `tip` in 
# `tips` data set
ggplot(tips, aes(sample=tip)) +
     stat_qq() 

# test visually the normality for variable `latency` in 
# Dragos Cogean's data set (INSERT operations)
ggplot(all_inserts, aes(sample=latency))+
     stat_qq() 

# test visually the normality for variable `duration` in 
# Ionut Hrubaru's data set
ggplot(results_ih_2016, aes(sample=duration))+
     stat_qq() 



#########################################################################
### 	           II.2  Shapiro-Wilk Test for Normality                 ###

## 	test the normality of variable `Age` in Arthritis data set                
# H0: the distribution of `Age` is normal
# Ha: variable `Age` values do NOT follow a normal distribution
shapiro.test(Arthritis_new$Age)
# W = 0.91913, p-value = 5.813e-05
# As the p-value is smaller than 0.05,  H0 is rejected.
# Age distribution is far from normal


## 	test the normality of variable `Result` in Arthritis data set                
# H0: the distribution of `Result` is normal
# Ha: variable `Result` values do NOT follow a normal distribution
shapiro.test(Arthritis_new$Result)
# W = 0.7216, p-value = 2.517e-11
# As the p-value is smaller than 0.05,  H0 is rejected.
# Age distribution is (extremely) far from normal


## 	test the normality of variable `latency`
## in Dragos Cogean's data set (INSERT operations)

# H0: the distribution of `latency` is normal
# Ha: variable `latency` values do NOT follow a normal distribution
shapiro.test(all_inserts$latency)
# Error!!!
#         Error in shapiro.test(all_inserts$latency) : 
#              sample size must be between 3 and 5000
# Solution: repeat the test with a sample of values
shapiro.test(sample(all_inserts$latency, 5000))
# W = 0.48844, p-value < 2.2e-16
# As the p-value is much smaller than 0.05,  H0 is rejected.
# `latency` distribution is far from normal


## 	test the normality of variable `duration`
## in Ionut Hrubaru's data set

# H0: the distribution of `duration` is normal
# Ha: variable `duration` values do NOT follow a normal distribution
shapiro.test(results_ih_2016$duration)
# W = 0.8838, p-value < 2.2e-16
# As the p-value is much smaller than 0.05,  H0 is rejected.
# `duration` distribution is far from normal



################################################################
# Apply the test by groups in just one step
# 
tapply(Arthritis_new$Age, Arthritis_new$Sex, shapiro.test)

sample__all_inserts <- all_inserts %>%
     sample_n(5000)
tapply(sample__all_inserts$latency, sample__all_inserts$db_server, shapiro.test)

tapply(results_ih_2016$duration, results_ih_2016$dbserver, shapiro.test)

tapply(results_ih_2016$duration, 
     factor(results_ih_2016$scale), 
       shapiro.test)


#########################################################################
### 	     II.3  Kolmogorov-Smirnov Test for Normality                 ###
# It is similar to the previous test.
# H0: the distribution is normal
# 
# The test in applied to continuous variables, with no duplicate values;
#   each time there will be duplicate (tied) values, 
#    a warning message will be displayed

## for Kolmogorov-Smirnov Test of normal distribution
##   there are some other functions in different packages

# Kolmogorov-Smirnov Test with package "pgirmess"
#install.packages("pgirmess")
library(pgirmess)

# The result of ...
ks.gof(Arthritis_new$Age)

#... is quite different from 
shapiro.test(Arthritis_new$Age)


#########################################################################
###     II.4  Other Normality Tests available in package "nortest"    ###
# install.packages("nortest")
library(nortest) 

###     Anderson-Darling test of normality (ad.test)
ad.test(Arthritis_new$Age)


### 					Cramer-von Mises test for normality
cvm.test(Arthritis_new$Age) 


###				 Lilliefors (Kolmogorov-Smirnov) test for normality 
lillie.test(Arthritis_new$Age)


###			 Shapiro-Francia test for normality
sf.test(Arthritis_new$Age)




#########################################################################
### 	              III.  Tests for Equality of Variance               ###
#########################################################################
## Traditional t-test require not only normality of the distribution, 
#    but also, when two populations are compared, that variances
#     to be is similar 

# Ansari-Bradley test checks the equality of variances even if 
#    the distribution is not normal

#  H0: the variances of the two populations are similar 
#  Ha: the variances of the two populations are NOT similar 


# Examples:
## 	Arthritis data set                
# 
#  H0: the variances of Age the two men and women are similar 
#  H0: the variances of Age the two men and women are NOT similar 
ansari.test(Age ~ Sex, Arthritis_new)
# AB = 1291, p-value = 0.6629
# There is not enough evidence to reject H0 (in other words, 
#    the variance of Age for Women does not differ significantly from
#    the variance of Age for Men)


# `tips` data set 
# is the variance of tip different between women and 
#   men "payers"  ?
#  H0: the variances of tip is similar for women and men
ansari.test(tip ~ sex, data=tips)
# p-value = 0.376; H0 cannot be rejected, 
#    so the variance of tip seem similar for men and women



#########################################################################
### 	                    IV.  Single Sample t-test                    ###
#########################################################################


#########################################################################
### 	               IV.1  Normal human body temperature               ###

### taken from 
####   http://ww2.coastal.edu/kingw/statistics/R-tutorials/singlesample.html

# Normal human body temperature
# when taken orally: 37 degrees  Celsius, but can vary between 
#   36.5 şi 37.2 degrees Celsius. 
# when taken under arm, normal values are  0.2-0.3 degrees Celsius smaller. 

# We hypothesize that the mean of human body temperature is 37 degrees Celsius 

# The data are from a random sample (supposedly) of 130 cases 
# We already downloaded the data in the file normtemp_fahr.txt
# normtemp_fahr = read.table("other/normtemp_fahr.txt")
# 
# normtemp_fahr = read.table("other/normtemp_fahr.txt")
file <- "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/normtemp.txt"
normtemp <- read.table(file, header=F, col.names=c("temp","sex","hr"))
head(normtemp)

# actually the temperatures are in the first column of the data set
# now we convert the Fahrenheit scale to Celsius
normtemp$tempC <- (normtemp$temp - 32) / 1.8
normtemp$tempC



# The t-test assumes a random sample of independent values drawn 
#   from a normal parent distribution. 
# Check the normality assumption visually...with qqline
qqnorm(normtemp$tempC) ;  qqline(normtemp$tempC)   
# ...or
ggplot(normtemp, aes(sample=tempC)) +
     stat_qq() 

# ... and kernel density function
ggplot(normtemp, aes(x = tempC)) + 
	geom_density(colour="black", fill="yellow", alpha = 0.3 )+ 
     ggtitle("Overall Distribution of Tip Amount", 
	        subtitle = "(`tips` data set in package `reshape`)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=10)) +	
     theme(axis.text.x=element_text(size=9, angle = 0)) +
   	xlab("Tip (in USD)") +
	guides(fill=FALSE)   # no legend 
     

# Now check the normality assumption with the Shapiro-Wilk test 
# H0: the distribution of temperatures is normally distributed
# Ha: the distribution of temperatures  is NOT normal 
shapiro.test(normtemp$tempC)
# p-value = 0.2332, so H0 is NOT rejected (the distribution 
#   seems to be close to normal)

###
# in this case, t-test is applied for is one sample (recorded body temperatures); 
#   this is a two-tail test
# 	H0: average temperature is 37 degrees
# 	Ha: average temperature is different than 37 degrees
t.test(normtemp$tempC, mu=37, alternative="two.sided")	
# p-value = 2.411e-07, so H0 is rejected! the human body average 
#   temperature seems not to be 37 degree C

## (t-statistic is the ratio of the observed coefficient to the
##  standard error of that coefficient)
##

# second t-test: one sample one-tail test	
# 	H0: average temperature is greater or equal to 37 degrees
# 	Ha: average temperature is less than 37 degrees	
t.test(normtemp$tempC, mu=37, alternative="less")	
# p-value = 1.205e-07, so H0 is rejected!
	
# third t-test: one sample one-tail test	
# 	H0: average temperature is less or equal to 37 degrees
# 	Ha: average temperature is greater than 37 degrees	
t.test(normtemp$tempC, mu=37, alternative="greater")	
# p-value = 1, so H0 is NOT rejected; average temperature seems less than 37
	
# fourth t-test: one sample two-tail test	
# 	H0: average temperature is 36.8 degrees
# 	Ha: average temperature is different than 36.8 degrees
t.test(normtemp$tempC, mu=36.8, alternative="two.sided")	
# p-value = 0.8861, so H0 is NOT rejected



#########################################################################
### 	                    IV.2  `tips` data set                        ###
# example taken from "R for Everyone"

## Is the average tip equal to 2.50 $ ?
# 	H0: average tip is equal to 2.50 $ 
# 	Ha: average tip is not equal to 2.50 $
t.test(tips$tip,  mu=2.5, alternative="two.sided")

# buit a T distribution
tip.t.distribution <- rt(3000, df=nrow(tips)-1)
# t-test
tip.t.test <- t.test(tips$tip,  mu=2.5, alternative="two.sided")

# the graphic
library(ggplot2)
ggplot(data.frame (x=tip.t.distribution)) +
	geom_density( aes (x=x), fill="grey", color="grey" ) +
	geom_vline( xintercept = tip.t.test$statistic ) +
	geom_vline( xintercept = mean(tip.t.distribution) + 
	c (-2, 2) * sd(tip.t.distribution), 
	linetype = 2) 



#########################################################################
### 	           V. t-test for Two Independent Samples                 ###
#########################################################################

#########################################################################
### 	                      V.1  Drugs & Memory                        ###
### taken from http://ww2.coastal.edu/kingw/statistics/R-tutorials/independent-t.html
# Looking for a possible relationship between marijuana smoking 
#   and a deficit in performance on a task measuring short term memory -
#   the digit span task from the Wechsler Adult Intelligence Scale. 
# Two groups of ten subjects were tested. One group, the "nonsmokers,"
#   claimed not to smoke marijuana. 
# A second group, the "smokers," claimed to smoke marijuana regularly. 
nonsmokers = c(18,22,21,17,20,17,23,20,22,21)
smokers = c(16,20,14,21,20,18,13,15,17,21)	

# build the data frame
smoke_mem <- tibble (smoking_status = c( rep("non-smoker", 10), 
	rep("smoker", 10)), score=c(nonsmokers, smokers))


# Examination of the two distributions in separate panels 
ggplot(smoke_mem , aes(score)) + 
	geom_density(alpha=0.3, fill="green") +
     ggtitle("Short Memory Performance Score, \nby Smoking Status", 
	        subtitle = "(http://ww2.coastal.edu/kingw/statistics/R-tutorials)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=9)) +	
     theme(axis.text.x=element_text(size=8, angle = 45)) +
     theme(strip.text.x = element_text(size = 14)) +
   	xlab("Score") +
	guides(fill=FALSE) +  # no legend +
     scale_x_continuous(breaks = seq(0,30, 1)) +
     facet_grid( ~ smoking_status )


# Examination of the two distributions super-imposed 
ggplot(smoke_mem , aes(score)) + 
	geom_density(aes(group=smoking_status, fill = smoking_status), alpha=0.3) +
     ggtitle("Short Memory Performance Score, \nby Smoking Status", 
	        subtitle = "(http://ww2.coastal.edu/kingw/statistics/R-tutorials)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=9)) +	
     theme(axis.text.x=element_text(size=8, angle = 45)) +
   	xlab("Score") +
     scale_x_continuous(breaks = seq(0,30, 1)) 


# Examine the data graphically with side-by-side boxplots
ggplot(smoke_mem, aes(y=score, x=factor(smoking_status))) + 
     geom_boxplot() +
     ggtitle("Short Memory Performance Score, \nby Smoking Status", 
	        subtitle = "(http://ww2.coastal.edu/kingw/statistics/R-tutorials)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))+
	theme(text=element_text(size=11)) +	
     theme(axis.text.x=element_text(size=12, angle = 0)) +
     theme(strip.text.x = element_text(size = 14)) +
     xlab("Smoking Status") + ylab("Short-Memory Performance Score") +
     scale_y_continuous(breaks = seq(0,30, 1)) 


# The t-test assumes a random sample of independent values has been extracted from a 
#	normal parent distribution. 
# Check the normality assumption... 
qqnorm(nonsmokers); 	qqline(nonsmokers)  
qqnorm(smokers);         qqline(smokers)  

ggplot(smoke_mem, aes(sample=score)) +
     stat_qq(aes(group=smoking_status, color = smoking_status)) 
	
# Normality tests:
# 
# H0: the distribution of nonsmokers performance is normally distributed
# Ha: the distribution of nonsmokers performance  does NOT follow 
#    a normal distribution
shapiro.test(nonsmokers)
# p-value = 0.295, so H0 is NOT rejected (the distribution seems 
#   to be close to normal)

# H0: the distribution of smokers performance is normally distributed
# Ha: the distribution of smokers performance  does NOT follow a 
#   normal distribution
shapiro.test(smokers)
# p-value = 0.3521, so H0 is NOT rejected (the distribution seems 
#  to be close to normal)

# Two Sample Two Tail t-test
# H0: average performance of nonsmokers is equal to the
#    average performance of smokers
# Ha: average performance of nonsmokers is not equal the 
#    average performance of smokers
t.test(nonsmokers, smokers, alternative="two.side")	
# t = 2.2573, df = 16.376, p-value = 0.03798, so H0 is rejected

# Two Sample One Tail t-test
# H0: average performance of nonsmokers is less or equal to the
#    average performance of smokers
# Ha: average performance of nonsmokers is greater than the 
#    average performance of smokers
t.test(nonsmokers, smokers, alternative="greater")	
# p-value = 0.01899, so H0 is rejected


	

#########################################################################
### 	                      V.2  `tips` data set                       ###
###
# example taken from "R for Everyone"

###
### RQ1: Do women give significantly different amounts of tip than men ?
## The t-test
# 	H0: average tip paid by women is equal to the average tip paid by men  
# 	Ha: average tip paid by women is NOT equal to the average tip paid by men  

# check if we can apply the t-test

#   as previoulsy seen, the variance of tip is equal for men and women
ansari.test(tip ~ sex, data=tips)
# p-value = 0.376, so the variance is similar

# Check the normality of the distribtuion:
# H0: the distribution of tip for a given gender is close to normal
tapply(tips$tip, tips$sex, shapiro.test)

# in the case of female payers, the p-value = 0.005448, the amount of tip is 
#   not normally distributed
# in the case of male payers, the p-value = 3.708e-10, so tip  
#  is not normally distributed

## Conclusion: the results of the t-test in this case are not reliable, 
## but we will do it


#                Two Sample Two Tail t-test
# H0: average tip for female payers is equal to the tip provided by males
# Ha: average tip for female payers is significantly different than
#    the tips provided by males
t.test(tip ~ sex, data = tips)
# t = -1.4895, df = 215.71, p-value = 0.1378
# p-value > 0.05, so we fail to reject H0
# Men and Women seem similarly generous in giving tip (in the restaurant)


###
### RQ2: Do smokers give significantly different amounts of tip than non-smokers ?
## The t-test
# 	H0: average tip paid by smokers is equal to the average tip paid by non-smokers  
# 	Ha: average tip paid by smokers is NOT equal to the average tip paid by non-smokers  

# check if we can apply the t-test

#   as previoulsy seen, the variance of tip is equal for men and women
ansari.test(tip ~ smoker, data=tips)
# p-value = 0.99, so the variance is similar

# Check the normality of the distribution:
# H0: the distribution of tip for a given smoking status is close to normal
tapply(tips$tip, tips$smoker, shapiro.test)

# in both cases (smokers and non-smokers), the amount of tip is 
#   not normally distributed

## Conclusion: the results of the t-test in not reliable, 
## but we will do it


# Two Sample Two Tail t-test
# 	H0: average tip paid by smokers is equal to the average tip paid by non-smokers  
# 	Ha: average tip paid by smokers is NOT equal to the average tip paid by non-smokers  
t.test(tip ~ smoker, data = tips)
# t = -0.091844, df = 192.26, p-value = 0.9269
# p-value > 0.05, so we fail to reject H0
# Non-Smokers and Smokers seem similarly generous in giving tip 



#########################################################################
### 	                    V.3 Dragos Cogean's data set                 ###
###
### RQ1: Does MongoDB perform better than MySQL in the case of 
###  INSERT operations?
###  
## The t-test
# 	H0: average latency for inserts is similar in the case of MongoDB 
# 	     to the average latency for inserts in the case of MySQL   
# 	Ha: average latency for insert operations differs significantly
# 	     in the case of two data servers 

# check if we can apply the t-test

#   as previoulsy seen, the variance of tip is equal for men and women
ansari.test(latency ~ db_server, data = all_inserts)
# AB = 19542000, p-value < 2.2e-16, so the variance is NOT similar

# Check the normality of the distribtuion:
# H0: the distribution of latency for a given data server is close to normal
sample__all_inserts <- all_inserts %>%
     sample_n(5000)
tapply(sample__all_inserts$latency, sample__all_inserts$db_server, shapiro.test)
# in both cases (MongoDB and MySQL) the p-value < 2.2e-16, so latency  
#  is not normally distributed

## Conclusion: the results of the t-test in this case are not reliable, 
## but we will do it

# Two Sample Two Tail t-test
# 	H0: average latency for inserts is similar in the case of MongoDB 
# 	     to the average latency for inserts in the case of MySQL   
# 	Ha: average latency for insert operations differs significantly
# 	     in the case of two data servers 
t.test(latency ~ db_server, data = all_inserts)
# t = 2.9759, df = 10076, p-value = 0.002928
# p-value < 0.05, so there is enough evidence to reject H0
# The two data servers have significantly different performances for 
#    INSERT operations


###
# Two Sample One Tail t-test (1)
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     less than or equal to the average latency for inserts 
# 	     in the case of MySQL   
# 	Ha: average latency for inserts is, in the case of MongoDB, 
# 	     greater than the average latency for inserts 
# 	     in the case of MySQL   
t.test(latency ~ db_server, data = all_inserts,  alternative="greater" )
# t = 2.9759, df = 10076, p-value = 0.001464
# H0 is rejected: MongoDB seems to perform poorer (latency is a 
#    'negative' metric, indicating a delay) than MySQL in the case of
#    MySQL
#    
# Important notice:
# Who is the first data server and who is the second in 
# `t.test(latency ~ db_server, data = all_inserts,  alternative="greater" )`
# MongoDB precedes (alphabetically) MySQL, so MongoDB is the first

###
# Two Sample One Tail t-test (2)
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     EQUAL TO OR GREATER THAN the average latency for inserts 
# 	     in the case of MySQL   
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     LESS THAN the average latency for inserts 
# 	     in the case of MySQL   
t.test(latency ~ db_server, data = all_inserts,  alternative="less" )
# t = 2.9759, df = 10076, p-value = 0.9985
# H0 is not rejected: MongoDB seems to perform poorer (latency is a 
#    'negative' metric, indicating a delay) than MySQL in the case of
#    MySQL



#########################################################################
### 	               VI. Dependent (paired) t-test                     ###
#########################################################################
# https://www.r-bloggers.com/paired-students-t-test/
# https://www.youtube.com/watch?v=yD6aU0fY2lo
# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
# 
# The dependent t-test (also called the paired t-test or 
# paired-samples t-test) compares the means of two related groups 
# to determine whether there is a statistically significant difference 
# between these means.
# 
# Assumptions of paired t-test:
#    - #1: Dependent variable should be measured on a continuous scale 
#         (i.e., it is measured at the interval or ratio level). 
#    - #2: Independent variable should consist of two categorical, 
#         "related groups" or "matched pairs". 
#         "Related groups" indicates that the same subjects are present 
#         in both groups. The reason that it is possible to have 
#         the same subjects in each group is because each subject 
#         has been measured on two occasions on the same dependent variable. 
#    - #3: There should be no significant outliers in the differences 
#         between the two related groups. 
#    - #4: The distribution of the differences in the dependent variable 
#         between the two related groups should be approximately 
#         normally distributed. 
#

#########################################################################
### 	                   VI.1 Ionut Hrubaru's data set                 ###

str(results_ih_2016)

# het the paired result
paired_results <- results_ih_2016 %>%
     filter (dbserver == 'Hive') %>%
     transmute (queryId, duration_Hive = duration) %>%
     inner_join(
          results_ih_2016 %>%
               filter (dbserver == 'PostgreSQL') %>%
               transmute (queryId, duration_Pg = duration)
     )
head(paired_results)


# RQ:
# Is Hive (Hadoop) performing better than PostgreSQL for a single-node
# architecture?

## Normality assumption check:
shapiro.test(paired_results$duration_Hive)
shapiro.test(paired_results$duration_Pg)
shapiro.test((paired_results$duration_Hive - paired_results$duration_Pg))

# assumption normality is not checked, so the paired t-test result
# is not reliable !!!

# 	Nevertheless, we'll run the test:
# 	
# Two Tail t-test for Paired Data
# H0: the mean query duration for Hive is equal to the mean for PostgreSQL
# Ha: the mean query duration for Hive is different than the mean for PostgreSQL
with(paired_results, t.test(duration_Hive, duration_Pg, paired=TRUE))
# or...
t.test(paired_results$duration_Hive, 
       paired_results$duration_Pg, 
       paired=TRUE)
# data:  paired_results$duration_Hive and paired_results$duration_Pg
# t = 118.35, df = 1999, p-value < 2.2e-16
# H0 is rejected: Hives perform differently than Pg in a single-node 
# architecture


# One Tail t-test for Paired Data
# H0: the mean query duration for Hive is less than or equal to 
# the mean for PostgreSQL
# # Ha: the mean query duration for Hive is greater than 
# the mean for PostgreSQL
with(paired_results, t.test(duration_Hive, duration_Pg, paired=TRUE, 
          alternative = 'greater'))
# or...
t.test(paired_results$duration_Hive, paired_results$duration_Pg, 
       paired=TRUE, alternative = 'greater')
# data:  paired_results$duration_Hive and paired_results$duration_Pg
# t = 118.35, df = 1999, p-value < 2.2e-16
# H0 is rejected: Hive performs worse (the query duration is greater)
#  than PostgreSQL in a single-node architecture



#########################################################################
###     VII. Nonparametric tests of differences between two           ### 
###                 independent groups                                ###
#########################################################################

# If the two groups are independent, the Wilcoxon rank sum test 
#   (Mann–Whitney U test) assesses whether the observations are 
#  sampled from the same probability distribution 
#  wilcox.test(y ~ x, data)
#  wilcox.test(y1, y2)

# Mann Whitney U aka Wilcoxon Rank-Sum Test in R (R Tutorial 4.3)
# https://www.youtube.com/watch?v=KroKhtCD9eE&index=27&list=PLqzoL9-eJTNBDdKgJgJzaQcY6OXmsXAHU

# Mann-Whitney U-Test
# https://www.youtube.com/watch?v=nRAAAp1Bgnw

#########################################################################
### 	                    VII.1  Drugs & Memory                        ###
# As the outcome variable was normally distributed, in section V.1 - we 
# applied the t-test:
# Two Sample Two Tail t-test
# H0: average performance of nonsmokers equal to the
#    average performance of smokers
# Ha: average performance of nonsmokers is not equal the 
#    average performance of smokers
t.test(nonsmokers, smokers, alternative="two.side")	
# t = 2.2573, df = 16.376, p-value = 0.03798, so H0 is rejected

# Even t-test result is reliable, we'll apply also the 
#  Wilcoxon rank sum test (Mann–Whitney U test),
#  just for comparison
wilcox.test(score ~ smoking_status , data = smoke_mem)
# W = 76.5, p-value = 0.04715, so H0 is rejected

### 
# Two Sample One Tail t-test
# H0: average performance of nonsmokers is less or equal to the
#    average performance of smokers
# Ha: average performance of nonsmokers is greater than the 
#    average performance of smokers
t.test(nonsmokers, smokers, alternative="greater")	
# p-value = 0.01899, so H0 is rejected

# results are similar with 
wilcox.test(score ~ smoking_status , data = smoke_mem, alternative="greater")
# W = 76.5, p-value = 0.02358

	

#########################################################################
### 	                     VII.2  `tips` data set                       ###
###
# example taken from "R for Everyone"

###
### RQ1: Do women give significantly different amounts of tip than men ?
## The t-test
# 	H0: average tip paid by women is equal to the average tip paid by men  
# 	Ha: average tip paid by women is NOT equal to the average tip paid by men  

# Two Sample Two Tail t-test
# H0: average tip for female payers is equal to the tip provided by males
# Ha: average tip for female payers is significantly different than
#    the tips provided by males
t.test(tip ~ sex, data = tips)
# t = -1.4895, df = 215.71, p-value = 0.1378
# p-value > 0.05, so we faill to reject H0
# Men and Women seem similarly generous in giving tip (in the restaurant)

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
wilcox.test(tip ~ sex , data = tips)
# W = 6369.5, p-value = 0.3834
# p-value > 0.05, so we faill to reject H0
# Men and Women seem similarly generous in giving tip (in the restaurant)



###
### RQ2: Do smokers give significantly different amounts of tip than non-smokers ?
## The t-test
# 	H0: average tip paid by smokers is equal to the average tip paid by non-smokers  
# 	Ha: average tip paid by smokers is NOT equal to the average tip paid by non-smokers  

# Two Sample Two Tail t-test
# 	H0: average tip paid by smokers is equal to the average tip paid by non-smokers  
# 	Ha: average tip paid by smokers is NOT equal to the average tip paid by non-smokers  
t.test(tip ~ smoker, data = tips)
# t = -0.091844, df = 192.26, p-value = 0.9269
# p-value > 0.05, so we faill to reject H0
# Non-Smokers and Smokers seem similarly generous in giving tip 

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
wilcox.test(tip ~ smoker , data = tips)
# W = 6880, p-value = 0.7919
# p-value > 0.05, so we faill to reject H0



#########################################################################
### 	                    VII.3 Dragos Cogean's data set                 ###
###
### RQ1: Does MongoDB perform better than MySQL in the case of 
###  INSERT operations?
###  
## We applied the t-test, even the outcome variable (Latency) distribution
## was not normal
## # Two Sample Two Tail t-test
# 	H0: average latency for inserts is similar in the case of MongoDB 
# 	     to the average latency for inserts in the case of MySQL   
# 	Ha: average latency for insert operations differs significantly
# 	     in the case of two data servers 

t.test(latency ~ db_server, data = all_inserts)
# t = 2.9759, df = 10076, p-value = 0.002928
# p-value < 0.05, so there is enough evidence to reject H0
# The two data servers have significantly different performances for 
#    INSERT operations

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
wilcox.test(latency ~ db_server, data = all_inserts)
#    W = 18662000, p-value = 0.7596 !!!!!
# p-value = 0.76, so there is NO enough evidence to reject H0
# The two data servers have NO significantly different performances for 
#    INSERT operations


###
# Two Sample One Tail t-test (1)
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     less than or equal to the average latency for inserts 
# 	     in the case of MySQL   
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     greater than the average latency for inserts 
# 	     in the case of MySQL   
t.test(latency ~ db_server, data = all_inserts,  alternative="greater" )
# t = 2.9759, df = 10076, p-value = 0.001464
# H0 is rejected: MongoDB seems to perform poorer (latency is a 
#    'negative' metric, indicating a delay) than MySQL in the case of
#    MySQL
#    

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
wilcox.test(latency ~ db_server, data = all_inserts,  alternative="greater" )
# W = 18662000, p-value = 0.3798, H0 CANNOT BE REJECTED

# Important notice:
# Who is the first data server and who is the second in 
# `wilcox.test(latency ~ db_server, data = all_inserts,  alternative="greater" )`
# MongoDB precedes (alphabetically) MySQL, so MongoDB is the first


###
# Two Sample One Tail t-test (2)
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     EQUAL TO OR GREATER THAN the average latency for inserts 
# 	     in the case of MySQL   
# 	H0: average latency for inserts is, in the case of MongoDB, 
# 	     LESS THAN the average latency for inserts 
# 	     in the case of MySQL   
t.test(latency ~ db_server, data = all_inserts,  alternative="less" )
# t = 2.9759, df = 10076, p-value = 0.9985
# H0 is not rejected: MongoDB seems to perform poorer (latency is a 
#    'negative' metric, indicating a delay) than MySQL in the case of
#    MySQL

wilcox.test(latency ~ db_server, data = all_inserts,  alternative="less" )
# W = 18662000, p-value = 0.6202
# in this particular case result is similar to the t-test



#########################################################################
###      VIII. Nonparametric tests of differences between two         ### 
###                   dependent (paired) groups                       ###
#########################################################################
#  Wilcoxon signed rank test (Mann-Whitney U test) is also 
#     appropriate for paired groups  

#########################################################################
### 	                 VIII.1 Ionut Hrubaru's data set                 ###
load('Hrubaru_2016-02.RData')
glimpse(results_ih_2016) 

# het the paired result
paired_results <- results_ih_2016 %>%
     filter (dbserver == 'Hive') %>%
     transmute (queryId, duration_Hive = duration) %>%
     inner_join(
          results_ih_2016 %>%
               filter (dbserver == 'PostgreSQL') %>%
               transmute (queryId, duration_Pg = duration)
     )

# RQ:
# Is Hive (Hadoop) performing better than PostgreSQL for a single-node
# architecture?

# Two Tail t-test for Paired Data (not reliable, since `duration` in not 
#    normally distributed)
# H0: the mean query duration for Hive is equal to the mean for PostgreSQL
# Ha: the mean query duration for Hive is different than the mean for PostgreSQL
with(paired_results, t.test(duration_Hive, duration_Pg, paired=TRUE))
# data:  paired_results$duration_Hive and paired_results$duration_Pg
# t = 118.35, df = 1999, p-value < 2.2e-16
# H0 is rejected: Hives perform differently than Pg in a single-node 
# architecture

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
with(paired_results, wilcox.test(duration_Hive, duration_Pg, paired=TRUE))
# V = 1999000, p-value < 2.2e-16
# H0 is rejected: Hives perform differently than Pg in a single-node 
# architecture
# So Mann–Whitney U test yields the same result as the t-test for paired data


# One Tail t-test for Paired Data
# H0: the mean query duration for Hive is less than or equal to 
# the mean for PostgreSQL
# # Ha: the mean query duration for Hive is greater than 
# the mean for PostgreSQL
with(paired_results, t.test(duration_Hive, duration_Pg, paired=TRUE, 
          alternative = 'greater'))
# data:  paired_results$duration_Hive and paired_results$duration_Pg
# t = 118.35, df = 1999, p-value < 2.2e-16
# H0 is rejected: Hive performs worse (the query duration is greater)
#  than PostgreSQL in a single-node architecture

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
with(paired_results, wilcox.test(duration_Hive, duration_Pg, 
          paired=TRUE, alternative = 'greater' ))
# V = 1999000, p-value < 2.2e-16
# H0 is rejected: Hive performs worse (the query duration is greater)
#  than PostgreSQL in a single-node architecture
# So Mann–Whitney U test yields the same result as the t-test for paired data



#########################################################################
###    IX. Nonparametric tests of differences for ordinal data        ###
#########################################################################
#  Wilcoxon signed rank test (Mann-Whitney U test) is also appropriate 
#   		for non continous variables (ordinal variables) !


#########################################################################
###                      IX.1. `Arthritis_new` data set               ###

# Outcome variable `Arthritis_new$Result` is ordinal

# RQ:
# Does the level of Result (0, 1, or 2) depend on the treatment group?

# Two Tail Wilcoxon sign (Mann-Whitney U) test
# H0: the median of `Result` for treated patients does not differ from
#    the median of `Result` for placebo patients
# Ha: the median of `Results` is significantly different between treated
#    and placebo groups
wilcox.test(Result ~ Treatment, Arthritis_new)
# W = 517.5, p-value = 0.0003666
# H0 is rejected!


# One Tail Wilcoxon sign (Mann-Whitney U) test (1)
# H0: the median of `Result` for placebo patients is less than or
#    equal to the median of `Result` for treated patients
# Ha: the median of `Results` is significantly greater for the placebo
#    group than the treated group
wilcox.test(Result ~ Treatment, Arthritis_new, alternative="greater")
# W = 517.5, p-value = 0.9998
# H0 cannot be rejected


# One Tail Wilcoxon sign (Mann-Whitney U) test (2)
# H0: the median of `Result` for placebo patients is greater than or
#    equal to the median of `Result` for treated patients
# Ha: the median of `Results` is significantly greater for the treatment
#    group than the placebo group
wilcox.test(Result ~ Treatment, Arthritis_new, alternative="less")
# W = 517.5, p-value = 0.0001833
# H0 is rejected!




#########################################################################
###         X. Compare more than two independent groups               ###
#########################################################################
# When normality/linearity assumptions are met, use ANOVA 
# Otherwise, use non-parametric tests (Kruskal–Wallis test, 
#    Friedman test, or some functions in `pgirmess` package )

# When groups are independent, Kruskal–Wallis test is advisable
# kruskal.test(y ~ A, data) where y is a numeric outcome variable 
#     and A is a grouping variable with two or more levels 


#########################################################################
### 	                     X.1  `tips` data set                       ###
###
summary(tips)
table(tips$day)

###
### RQ3: Does the amount of the tip vary significantly through the 
###  days of the week ?

# Even the `tip` is not normally distributed, we'll apply ANOVA

# 	H0: average tip does not vary significatly among the days of the week 
# 	Ha: average tip varies significatly among the days of the week   
anova1 <- aov(tip ~ day, tips)
summary(anova1)

#              Df Sum Sq Mean Sq F value Pr(>F)
# day           3    9.5   3.175   1.672  0.174
# Residuals   240  455.7   1.899 

# p-value = 0.174, HO cannot be rejected. 
# Consequently, there are no significant variations of tip
#    among the days of the week


##
## Multiple Comparisons
## You can get Tukey HSD tests using the function below. 
## By default, it calculates post hoc comparisons on each factor in the model. 

# Tukey Honestly Significant Differences (95% family-wise confidence level)
TukeyHSD(anova1) # where fit comes from aov()

#$day
#                diff        lwr       upr     p adj
#Sat-Fri   0.25836661 -0.6443694 1.1611026 0.8806455
#Sun-Fri   0.52039474 -0.3939763 1.4347658 0.4558054
#Thur-Fri  0.03671477 -0.8980753 0.9715049 0.9996235
#Sun-Sat   0.26202813 -0.2976929 0.8217492 0.6203822
#Thur-Sat -0.22165184 -0.8141430 0.3708394 0.7678581
#Thur-Sun -0.48367997 -1.0937520 0.1263921 0.1724212

# No significant difference between any pair of days


###  (Non-parametric) Kruskal–Wallis test
kruskal.test(tip ~ day, tips) 
# Kruskal-Wallis chi-squared = 8.5656, df = 3, p-value = 0.03566
# Result of the Kruskal–Wallis test contradict ANOVA !!!!
# There seems to be slight differences of tip among days of the week


### pgirmess package provides the nonparametric multiple comparisons 
# install.packages("pgirmess")
library(pgirmess)

# compare the tip for every pair of days of the week
kruskalmc(tip ~ day, tips)  
#Comparisons
#           obs.dif critical.dif difference
#Fri-Sat   4.535995     47.15427      FALSE
#Fri-Sun  28.407895     47.76202      FALSE
#Fri-Thur  4.111205     48.82861      FALSE
#Sat-Sun  23.871900     29.23694      FALSE
#Sat-Thur  8.647201     30.94868      FALSE
#Sun-Thur 32.519100     31.86701       TRUE

# The only slightly significant difference appears to be
# between Sunday and Thursday


#########################################################################
### 	                 VIII.1 Ionut Hrubaru's data set                 ###
load('Hrubaru_2016-02.RData')
glimpse(results_ih_2016) 

# het the paired result
paired_results <- results_ih_2016 %>%
     filter (dbserver == 'Hive') %>%
     transmute (queryId, duration_Hive = duration) %>%
     inner_join(
          results_ih_2016 %>%
               filter (dbserver == 'PostgreSQL') %>%
               transmute (queryId, duration_Pg = duration)
     )

# RQ:
# Is Hive (Hadoop) performing better than PostgreSQL for a single-node
# architecture?

# Two Tail t-test for Paired Data (not reliable, since `duration` in not 
#    normally distributed)
# H0: the mean query duration for Hive is equal to the mean for PostgreSQL
# Ha: the mean query duration for Hive is different than the mean for PostgreSQL
with(paired_results, t.test(duration_Hive, duration_Pg, paired=TRUE))
# data:  paired_results$duration_Hive and paired_results$duration_Pg
# t = 118.35, df = 1999, p-value < 2.2e-16
# H0 is rejected: Hives perform differently than Pg in a single-node 
# architecture

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
with(paired_results, wilcox.test(duration_Hive, duration_Pg, paired=TRUE))
# V = 1999000, p-value < 2.2e-16
# H0 is rejected: Hives perform differently than Pg in a single-node 
# architecture
# So Mann–Whitney U test yields the same result as the t-test for paired data


# One Tail t-test for Paired Data
# H0: the mean query duration for Hive is less than or equal to 
# the mean for PostgreSQL
# # H0: the mean query duration for Hive is greater than 
# the mean for PostgreSQL
with(paired_results, t.test(duration_Hive, duration_Pg, paired=TRUE, 
          alternative = 'greater'))
# data:  paired_results$duration_Hive and paired_results$duration_Pg
# t = 118.35, df = 1999, p-value < 2.2e-16
# H0 is rejected: Hive performs worse (the query duration is greater)
#  than PostgreSQL in a single-node architecture

# Now, with the Wilcoxon rank sum test (Mann–Whitney U test)
with(paired_results, wilcox.test(duration_Hive, duration_Pg, 
          paired=TRUE, alternative = 'greater' ))
# V = 1999000, p-value < 2.2e-16
# H0 is rejected: Hive performs worse (the query duration is greater)
#  than PostgreSQL in a single-node architecture
# So Mann–Whitney U test yields the same result as the t-test for paired data



#########################################################################
###   XI. Compare more than two dependent groups (repeated measures)  ###
#########################################################################
#
# https://www.sheffield.ac.uk/polopoly_fs/1.714578!/file/stcp-marquier-FriedmanR.pdf
# https://www.youtube.com/watch?v=DPfFNVpt4po



#########################################################################
### 	                 XI.1 Ionut Hrubaru's data set                 ###
### we'll apply the test, even there are just two dependent groups
load('Hrubaru_2016-02.RData')
glimpse(results_ih_2016) 

# het the paired result
paired_results <- results_ih_2016 %>%
     filter (dbserver == 'Hive') %>%
     transmute (queryId, duration_Hive = duration) %>%
     inner_join(
          results_ih_2016 %>%
               filter (dbserver == 'PostgreSQL') %>%
               transmute (queryId, duration_Pg = duration)
     )

# RQ:
# Is the query performance significantly different among various 
# data servers ? (in our case, just two, but could be more)

# H0: distributions of the query duration 
# are similar (there is no difference among all database servers)
# Ha: distributions of the query duration being compared 
# are different (differences among all database servers are statistically 
# significant)
friedman.test(as.matrix(paired_results[,2:3]))
# Friedman chi-squared = 1996, df = 1, p-value < 2.2e-16
# H0 is rejected! The differences are significant

## Pairwise Comparisons
# To find out which pairs are different, you will need to install 
# the package ‘PMCMR’ and load the library after that:
# install.packages('PMCMR')
library(PMCMR)
# install.packages('PMCMRplus')
library(PMCMRplus)
# You will then need to conduct the Nemenyi Post-hoc tests 
# to compare all the pairs. They will be presented as a table.
PMCMR::posthoc.friedman.nemenyi.test(as.matrix(paired_results[,2:3]))



#########################################################################
### 	         XI.2 Alexandru Tica's data set (Exadata)                 ###
### we'll apply the test, even there are just two dependent groups

#?????????
#load(file = 'Tica2018-03.RData')
#glimpse(df) 

# RQ:
# Is the query performance significantly different among the six 
# Exadata scenarious ? 

# H0: distributions of the results are similar among scenarious
# (there is no difference among all scanarios)
# Ha: distributions of the results differ significantly 
# among scenarious (there are significant differences among scanarios)

friedman.test(as.matrix(df[,2:7]))
# Friedman chi-squared = 1473.2, df = 5, p-value < 2.2e-16
# H0 is rejected! The differences are significant

## Pairwise Comparisons
# install.packages('PMCMRplus')
library(PMCMRplus)
# You will then need to conduct the Nemenyi Post-hoc tests 
# to compare all the pairs. They will be presented as a table.
posthoc.friedman.nemenyi.test(as.matrix(df[,2:7]))




