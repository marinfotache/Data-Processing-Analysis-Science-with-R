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
###        10c. The R package on inferential statistics: `ggstatplot`    ###
### See: https://indrajeetpatil.github.io/ggstatsplot/
############################################################################
## last update: 2022-12-04

# install.packages("ggstatsplot")
library(ggstatsplot)
library(tidyverse)
library(scales)

# giving up scientific notation (1.6e+07)
options(scipen = 999)


#library(vcd)
#library(vcdExtra)
#install.packages('ggmosaic')
#library(ggmosaic)
# library(readxl)


############################################################################
###            Download the necessary data sets for this script
############################################################################
# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you downloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')



############################################################################
###              A. Categorical data analysis with `ggstatsplot`         ###
############################################################################

#########################################################################
### 	               A.I. Tests of independence                        ###
#########################################################################

#######################################################################
###	               A.I.1      "Arthritis" dataset" 
#######################################################################
data(Arthritis, package = 'vcd')

# for data set details, research questions and test results, 
#         see script 10a, section II.1

#######################################################################
# Research Question
#   Is the treatment effective in fighting with the disease?
#######################################################################


#######################################################################
###    H0: Variables `Treatment` and `Improved` are independent

#  ... results from script 10a:
mytable <- xtabs(~ Improved + Treatment, data = Arthritis)
chisq.test(mytable)
# p-value = 0.001 so we reject null hypothesis ! 
#    Variables Treatment and Improved seem to be related 

# Now, the same test with `ggstatsplot`:
ggbarstats(
  data = Arthritis,
  Improved,
  Treatment
)

# ...or:
ggpiestats(
  data = Arthritis,
  Improved,
  Treatment
)


#######################################################################
###       H0: Variables `Improved` and `Sex` are independent
mytable <- xtabs(~ Improved + Sex, data = Arthritis)
chisq.test(mytable)

# p-value = 0.09 so we DO NOT reject null hypothesis ! 
#   There is no enough (statistical) evidence to reject 
# the claim that variables "Improved" and "Sex" are independent.
#  In other words, is seems plausible that variables "Improved" and 
#  "Sex" are independent

ggbarstats(
  data = Arthritis,
  Improved,
  Sex
)


#######################################################################
###	               A.I.2      "Heart disease" dataset" 
#######################################################################
# for data set details, research questions and test results, 
#         see script 09c, section II.1
heart <- read_csv('Heart.csv') |>
     select (-`...1`) %>%
     mutate(
          Sex = recode (Sex, `0` = "Female", `1` = "Male"),
          Fbs = recode (Fbs, `0` = "No", `1` = "Yes"),
          RestECG = factor (RestECG, levels = c(0, 1, 2)),
          ExAng = recode (ExAng, `0` = "No", `1` = "Yes"),
          Slope = factor (Slope, levels = c(1, 2, 3))
          ) %>%
     mutate_if(is.character, as.factor)
glimpse(heart)

# Descriptive statistics
heart %>%
     skimr::skim()


#######################################################################
###             RQ1: Is the heart disease related to the sex?
#######################################################################

###  H0: Variables `AHD` and `Sex` are independent
ggbarstats(
  data = heart,
  AHD,
  Sex
)

mytable <- xtabs(~ AHD + Sex, data = heart)
chisq.test(mytable) 


#######################################################################
###             RQ2: Is the heart disease related to the chest pain?
#######################################################################

###  H0: Variables `AHD` and `ChestPain` are independent
ggbarstats(
  data = heart,
  AHD,
  ChestPain
)



############################################################################
###                 B. Numeric data analysis with `ggstatsplot`          ###
############################################################################

#########################################################################
### 	                      B.I. Correlation tests                     ###
#########################################################################

#####################################################################
### 	                  B.I.1 Insurance dataset                    ###
#####################################################################
# for data set details, research questions and test results, 
#         see script 09c, section I.2
insurance <- readr::read_csv('insurance.csv')
glimpse(insurance)

# descriptive statistics
insurance %>%
     skimr::skim()


#######################################################################
###       RQ1: Is variable `charges` associated with age?

##  
##  H0: variable `charges` and `age` are not correlated
##
##
# parametric correlation coefficient
ggscatterstats(
  data = insurance,
  x = age,
  y = charges
)

# non-parametric correlation coefficient
ggscatterstats(
  data = insurance,
  x = age,
  y = charges,
  type = "np"
)


#######################################################################
###     RQ2: Is variable `charges` associated with body mass index?

##  
##  H0: variable `charges` and `bmi` are not correlated
##
##
# parametric correlation coefficient
ggscatterstats(
  data = insurance,
  x = bmi,
  y = charges
)

# non-parametric correlation coefficient
ggscatterstats(
  data = insurance,
  x = bmi,
  y = charges,
  type = "np"
)



############################################################################
###             C. Testing group differences with `ggstatsplot`          ###
############################################################################

#########################################################################
### 	                  C.I. Two independent groups                    ###
#########################################################################

# Heart Diseases data set 
#######################################################################
###             RQ3: Is the heart disease related to the age?
#######################################################################
glimpse(heart)

# check the normality of `Age` distribution
shapiro.test(heart$Age)
# non-normal

#just the text
wilcox.test(heart$Age ~ heart$AHD) 

# details about test results provided by `report` package
wilcox.test(heart$Age ~ heart$AHD) |>
     report::report()

# now, the Man-Whitney test with `ggstatsplot`
ggbetweenstats(
  data = heart,
  x = AHD,
  y = Age,
  plot.type = "boxviolin",
  type = "np"
)


#######################################################################
###      RQ4: Is the heart disease related to the Cholesterol level?
#######################################################################
glimpse(heart)

# check the normality of `Chol` distribution
shapiro.test(heart$Chol)
# non-normal

#just the text
wilcox.test(heart$Chol ~ heart$AHD) 

# details about test results provided by `report` package
wilcox.test(heart$Chol ~ heart$AHD) |>
     report::report()

# now, the Man-Whitney test with `ggstatsplot`
ggbetweenstats(
  data = heart,
  x = AHD,
  y = Chol,
  plot.type = "boxviolin",
  type = "np"
)


#########################################################################
### 	          C.I.1 `tips` data set (package `reshape`)              ###
# for data set details, research questions and test results, 
#         see script 10b, section I.2
#########################################################################
data(tips, package="reshape2")
glimpse(tips)


#########################################################################
### RQ1: Do women give significantly different amounts of tip than men ?


## 	check test the normality of variable `tip`

# H0: the distribution of `tip` is normal
# Ha: variable `tip` values do NOT follow a normal distribution
shapiro.test(tips$tip)
# W = 0.89781, p-value = 0.0000000000082
# As the p-value is much smaller than 0.05,  H0 is rejected.
# `tip` distribution is far from normal


# Ansari-Bradley test checks the equality of variances even if 
#    the distribution is not normal
# is the variance of tip different between women and  men "payers"  ?
#  H0: the variances of tip is similar for women and men
ansari.test(tip ~ sex, data=tips)
# p-value = 0.376; H0 cannot be rejected, 
#    so the variance of tip seem similar for men and women

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


# now, the same t-test, but this time with `ggstatsplot`
ggbetweenstats(
  data = tips,
  x = sex,
  y = tip,
  plot.type = "boxviolin",
  type = "parametric"
)


# actually, the assumptions of the t-test are not met, so the Mann–Whitney test
# is approppriate in this case

wilcox.test(tip ~ sex , data = tips)
# W = 6369.5, p-value = 0.3834
# p-value > 0.05, so we faill to reject H0
# Men and Women seem similarly generous in giving tip (in the restaurant)


# now, the same test, but this time with `ggstatsplot`:
ggbetweenstats(
  data = tips,
  x = sex,
  y = tip,
  plot.type = "boxviolin",
  type = "np"
)



#########################################################################
### RQ2: Do smokers give significantly different amounts of tip 
###    than non-smokers ?

# Check the normality of the distribution:
# H0: the distribution of tip for a given smoking status is close to normal
tapply(tips$tip, tips$smoker, shapiro.test)

# in both cases (smokers and non-smokers), the amount of tip is 
#   not normally distributed

#   as previoulsy seen, the variance of tip is equal for smokers and not smokers
ansari.test(tip ~ smoker, data=tips)
# p-value = 0.99, so the variance is similar

# The Wilcoxon rank sum test (Mann–Whitney U test)
wilcox.test(tip ~ smoker , data = tips)
# W = 6880, p-value = 0.7919
# p-value > 0.05, so we faill to reject H0

# now, the same test, but this time with `ggstatsplot`:
ggbetweenstats(
  data = tips,
  x = smoker,
  y = tip,
  plot.type = "boxviolin",
  type = "np"
)



#########################################################################
### 	                     C.II. Two paired groups                     ###
#########################################################################

#########################################################################
### 	                 C.II.1 Ionut Hrubaru's data set                 ###
#########################################################################
### for other details on this data set, see script 10b
### 
load('Hrubaru_2016-02.RData')
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


#########################################################################
# RQ:
# Is Hive (Hadoop) performing better than PostgreSQL for a single-node
# architecture?

## Normality assumption check:
shapiro.test(paired_results$duration_Hive)
shapiro.test(paired_results$duration_Pg)
shapiro.test((paired_results$duration_Hive - paired_results$duration_Pg))

# assumption normality is not checked, so the paired t-test result
# is not reliable !!!

# Instead of the t-test for paired date, wel'll use the Wilcoxon rank sum test (Mann–Whitney U test)
with(paired_results, wilcox.test(duration_Hive, duration_Pg, paired=TRUE))
# V = 1999000, p-value < 0.0001
# H0 is rejected: Hives perform differently than Pg in a single-node 
# architecture


# now, the same test, but this time with `ggstatsplot`:
ggwithinstats(
  data = results_ih_2016,
  x = dbserver,
  y = duration,
  type = "np"
)


#########################################################################
### 	           C.III. More than two independent groups               ###
#########################################################################
# When normality/linearity assumptions are met, use ANOVA 
# Otherwise, use non-parametric tests (Kruskal–Wallis test, 
#    Friedman test, or some functions in `pgirmess` package )

# When groups are independent, Kruskal–Wallis test is advisable
# kruskal.test(y ~ A, data) where y is a numeric outcome variable 
#     and A is a grouping variable with two or more levels 



#########################################################################
### 	                     C.III.1  `tips` data set                    ###
#########################################################################

#########################################################################
### RQ3: Does the amount of the tip vary significantly through the 
###  days of the week ?

# Even the `tip` is not normally distributed, we'll apply ANOVA

# 	H0: average tip does not vary significatly among the days of the week 
# 	Ha: average tip varies significatly among the days of the week   
anova1 <- aov(tip ~ day, tips)
summary(anova1)

# p-value = 0.174, HO cannot be rejected. 
# Consequently, there are no significant variations of tip
#    among the days of the week


##
## Multiple Comparisons
## You can get Tukey HSD tests using the function below. 
## By default, it calculates post hoc comparisons on each factor in the model. 

# Tukey Honestly Significant Differences (95% family-wise confidence level)
TukeyHSD(anova1) # where fit comes from aov()


# now, the same test, but this time with `ggstatsplot`:
ggbetweenstats(
  data = tips,
  x = day,
  y = tip,
)


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

# The only slightly significant difference appears to be
# between Sunday and Thursday

# now, the same test, but this time with `ggstatsplot`:
ggbetweenstats(
  data = tips,
  x = day,
  y = tip,
  type = 'np'
)





