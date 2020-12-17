#######################################################################
###                    Al.I. Cuza University of Iași                ###
###       Faculty of Economics and Business Administration          ###
###   Department of Accounting, Information Systems and Statistics  ###
#######################################################################
###
#######################################################################
###        Data Processing/Analysis/Science with R                  ###
#######################################################################
###
#######################################################################
###     11a2 Multiple (Ordinary Least Square) Linear Regression      ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/11%20Scoring%20(Regression)%20and%20Classification/11_scoring%20and%20clasisification.pptx
#######################################################################
## last update: 19.11.2019

library(tidyverse) 
library(skimr)
library(corrplot)
library(broom)
library(readxl)
# install.packages("corrgram")
library(corrgram)

library(car)
library(QuantPsyc)


#######################################################################
###           Download the necesary data sets for this script       ###
#######################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

#######################################################################
options(scipen=999, digits=4)


#######################################################################
###                  Agenda of the previous script                  ###
#######################################################################
###       I. What is Linear Regression?                             ###
###       II. Simple Linear Regression. Package `broom`             ###
###       III. Polynomial regression                                ###
#######################################################################
###		

#######################################################################
###                  Agenda of the current script                   ###
#######################################################################
###       IV. Multiple linear regression      		             ###
###       V. Multiple Regression with Categorical Predictors        ###
###       VI. Multiple linear regression with interactions          ###
#######################################################################


#######################################################################
###                  IV. Multiple linear regression      		   ###
#######################################################################

#######################################################################
##  Recap:   Statistical assumptions of the linear regression:
#        Yi = b0 + b1 * Xi1  +  b2 * Xi2 + ... 
#  1. Normality: for fixed values of the predictors (Xs), 
#     the outcome variable (response) is normally distributed 
#     (predictors variables may have a non-normal distribution )
#  2. Independence: the Yi values are independent of each other.
#  3. Linearity: the dependent variable (response) is linearly related 
#     to the independent variables (predictors)
#  4. Homoscedasticity: the variance of the dependent variable Y doesn’t vary 
#      with the levels of the independent variables Xj. 

## we'll see later how to check for each assumption

#######################################################################
## 
#  The basic check of the fit for every model is
#     deviation = sum ((observed - model)^2)
#  	

#  better the fit means smaller deviation

#  A basic strategy for predicting the outcome is using the mean
# If the model is the mean
# SST (total sum of squares) = sum of squared differences between observed values and 
#   the values predicted by the mean
#  SST represents how good the mean is as a model of the observed data

# If the model is the line of best fit
# SSR (residual sum of squares) = sum of squared differences between the observed values 
#   and the values predicted by the regression line (the model) 
#  SSR represents the degree of inaccuracy when the best model is fitted to the data

# SST and SSE can be used for calculating how much better the regression line (the
#  line of best fit) is than just using the mean as a model
# SSM (model sum of squares) = SST - SSR
# SSM represents the improvement in prediction resulting from using the regression
#   model rather than the mean

# R^2 (R squared) = SSM / SST
#  R^2 represents the amount of variance in the outcome explained by the model (SSM)
#    relative to how much variation there is in the data

# Model can be also assessed through F-test.
# Every statistical test (like F-test) is generally the amount of systematic variance
#  divided by the amount of unsystematic variance (the model compared against the 
#    error in the model)
#  F = MSM / MSR
#  where 
#	MSM is the mean squares for the model
#	MSR is the mean residual mean squares

# Why use MSM and MSR instead of SSM and SSR ?
#   Because SSM and SSR depend on the number of differences (observations)

# MSM = SSM / degrees of freedom (degrees of freedom = number of variables in the model)
# MSR = SSR / degrees of freedom (degrees of freedom = number of observations minus
#	number of paramenters being estimated - number of "b"s (b0, b1, ...))
#######################################################################




#######################################################################
###                                IV.1 States (USA)
#  Example taken from Kabacoff's `R in Action` (Manning), 2013, 2015 
# state.x77 dataset in the base package 
# Explore the relationship between a state’s murder rate and 
#    other characteristics of the state, including population, 
#    illiteracy rate, average income, 
#   and frost levels (mean number of days below freezing).

# lm() function requires a data frame;state.x77 dataset is contained in a matrix, 
#    so one must convert it:
states_ <- as_tibble(state.x77) %>%
     set_names(str_replace_all(names(.), '( |\\.)', '_'))
glimpse(states_)

# descriptive statistics about variables
skimr::skim(states_)

# examine bivariate relationships
cor(states_)

corrplot::corrplot(cor(states_), method = "circle", type='upper')
corrplot::corrplot(cor(states_, method = "spearman"), 
     method = "ellipse", type = "upper")

corrplot::corrplot(cor(states_, method = "spearman"), method = "number", type = "upper")


# a multiple regression model for `states_` data set
states_lm2 <- lm(Murder ~ Population + Illiteracy + Income + 
                        Frost, data=states_)

summary(states_lm2)

glance(states_lm2)
tidy(states_lm2, conf.int = TRUE)

# the regression coefficients indicate the increase in the dependent variable 
#  for a unit change  in a predictor variable, holding all other predictor 
#  variables constant.

# standardized versions of beta values are easier to interpret because they
#    are not dependent on the units of measurement of the variables
# standardized betas show the number of standard deviations by which the 
#    outcome will change as a result of one standard seviation change
#    in the predictor
#  lm.beta() function in QuantPsyc package
QuantPsyc::lm.beta(states_lm2)
# standardized beta for Illiteracy is 0.684, so as the value of Illiteracy will
#   increase by one standard deviation, Murder value will increase by 0.684 
#   standard deviations

glance(states_lm2)
# Multiple R-squared:  0.567, so, taken together, the predictor variables account for
#       57 percent of the variance in murder rates across states



#######################################################################
###                                IV.2 Education (USA)
##	 example taken from http://www.princeton.edu/~otorres/Regression101R.pdf
##     and http://dss.princeton.edu/training/Regression101.pdf

##   Are SAT scores higher in states that spend more money on education controlling 
##     by other factors?
# the data set have to be importes into R from Stata 
education <- foreign::read.dta("states.dta")
glimpse(education)
# descriptive statistics about the numeric variables
skimr::skim(education)

# Outcome (Y) variable is csat (SAT scores) 
# Predictor (X) variables are:
# - per pupil expenditures primary & secondary (expense)
# - % HS graduates taking SAT (percent)
# - Median household income (income)
# - % adults with HS diploma (high)
# - % adults with college degree (college)
# - Region (region)

education_lm2 <- lm(csat ~ expense + percent + income , data=education)
glance(education_lm2)
tidy(education_lm2, conf.int = TRUE)



#######################################################################
###                 IV.3 Prestige Data Set - Fox 2016, pp.133-138
###       
# Data Set: Prestige of Canadian Occupations
# Source: Canada (1971) Census of Canada. Vol. 3, Part 6. Statistics Canada 
#  [pp. 19-1–19-21].
#  Personal communication from B. Blishen, W. Carroll, and C. Moore, Departments of
#    Sociology, York University and University of Victoria.
# Variables:
#    - education: Average education of occupational incumbents, years, in 1971.
#    - income: Average income of incumbents, dollars, in 1971.
#    - women: Percentage of incumbents who are women.
#    - prestige: Pineo-Porter prestige score for occupation, from a social survey 
#         conducted in the mid-1960s.
#    - census: Canadian Census occupational code.
#    - type: Type of occupation: bc, Blue Collar; wc, White Collar; prof, 
#         Professional, Managerial, and Technical.

#library(car) 
data(Prestige, package = 'carData')
# general description or the data set
help(Prestige)
# structure of the data frame
glimpse(Prestige)

# descriptive statistics
skimr::skim(Prestige)

# linear regression with two predictors
prestige_lm1 <- lm(prestige ~ income + education, data=Prestige)
glance(prestige_lm1)
tidy(prestige_lm1, conf.int = TRUE)

# linear regression with three predictors (also as `income` is skewed,
#   the predictor will use `log2`)
prestige_lm2 <- lm(prestige ~ education + log2(income) + women, data=Prestige)
glance(prestige_lm2)
tidy(prestige_lm2, conf.int = TRUE)



#######################################################################
###            IV.4 Regressing Grades on Homework & Parent Education
# 
# Example taken from Timothy Keith - Multiple Regression and Beyond, 
#    Routhledge, 2015
# Chapter 2
# Data is available at:
# http://tzkeith.com/data-files/

# The data set contains three variables:
#  1. 8th grade student's overall Grade-point average (GPA) in all subjects 
#    (in a standard 100 point scale)
#  2. Levels of Education of the students' parents, in years of schooling
#      (for the parent with the higher education level)
#  3. Average number of hours spent on Homeworks per week
grade__educ_home <- readxl::read_excel('chap 2, hw grades.xls', sheet = 1, 
     col_names = TRUE, skip = 0)
glimpse(grade__educ_home)

# descriptive statistics about the numeric variables
skimr::skim(grade__educ_home)


##  correlation plots
corrplot::corrplot(cor(grade__educ_home), method = "circle")

corrplot::corrplot(cor(grade__educ_home, method = "spearman"), 
     method = "ellipse", type = "upper")

corrplot::corrplot(cor(grade__educ_home, method = "spearman"), 
     order="hclust", method = "ellipse", type = "upper")

corrplot::corrplot(cor(grade__educ_home, method = "spearman"), order="hclust", 
     method = "number", type = "upper")


# Regress `GPA`` on `Parents' Education`` and `Average 
#    number of hours spent on Homeworks per week`

gpa_lm1 <- lm (GRADES ~ ., data =  grade__educ_home, na.action = na.exclude)
glance(gpa_lm1)
tidy(gpa_lm1, conf.int = TRUE)

# interpret the results!


#######################################################################
### 	          IV.5 `tips` data set (package `reshape`)                ###

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
glimpse(tips)
skimr::skim(tips)

tips_lm1 <- lm(tip ~ total_bill + size, data=tips, na.action = na.exclude)
summary(tips_lm1)

glance(tips_lm1)
tidy(tips_lm1, conf.int = TRUE)




#######################################################################
###       V. Multiple Regression with Categorical Predictors        ###
#######################################################################

#######################################################################
###                 V.1 Births (2006) data set
###  Births

births2006 <- readr::read_tsv('births2006_package_nutshell.txt') %>%
     na.omit()
glimpse(births2006)

# descriptive statistics on  two variables of interest
births2006 %>%
     skimr::skim()

##
## New models built incrementally
 
# first categorical predictor - `BabySex`
table(births2006$BabySex)
births2006_lm3 <- lm(BabyWeight ~ Gestation_Weeks + 
     Mother_WeightGain_Kg +
     BabyScore + BabySex, data=births2006)

summary(births2006_lm3)
glance(births2006_lm3)
tidy(births2006_lm3, conf.int = TRUE)

# another categorical predictor - `DeliveryMethod`
table(births2006$DeliveryMethod)
births2006_lm4 <- lm(BabyWeight ~ Gestation_Weeks + Mother_WeightGain_Kg +
     BabyScore + BabySex + DeliveryMethod, data=births2006)
glance(births2006_lm4)
tidy(births2006_lm4, conf.int = TRUE)


# another categorical predictor - `BirthDay_WeekDay`
births2006_lm5 <- lm(BabyWeight ~ Gestation_Weeks + Mother_WeightGain_Kg +
     BabyScore + BabySex + DeliveryMethod + factor(BirthDay_WeekDay), data=births2006)
glance(births2006_lm5)
tidy(births2006_lm5, conf.int = TRUE)


### discussions ...


#######################################################################
###                 V.2 Prestige Data Set - Fox 2016, pp.133-138
### (see description above)      
# Variables:
#    - education: Average education of occupational incumbents, years, in 1971.
#    - income: Average income of incumbents, dollars, in 1971.
#    - women: Percentage of incumbents who are women.
#    - prestige: Pineo-Porter prestige score for occupation, from a social survey 
#         conducted in the mid-1960s.
#    - census: Canadian Census occupational code.
#    - type: Type of occupation: bc, Blue Collar; wc, White Collar; prof, 
#         Professional, Managerial, and Technical.

data(Prestige, package = 'carData')
prestige <- Prestige %>%
     as.tibble() %>%
     na.omit()
glimpse(prestige)
skimr::skim(prestige)


# correlation plot among numeric variables 
corrplot::corrplot.mixed(corr=cor(prestige %>% select_if(is.numeric) , 
     method = "spearman"), upper = 'ellipse', tl.pos='lt')

corrgram::corrgram(prestige %>% select_if(is.numeric),
                   lower.panel=panel.conf, upper.panel=panel.pts)

corrgram::corrgram(prestige %>% select_if(is.numeric),
     lower.panel=panel.pie, upper.panel=panel.pts,
     diag.panel=panel.density)

corrgram::corrgram(prestige %>% select_if(is.numeric),
     lower.panel=panel.conf, upper.panel=panel.pts,
     diag.panel=panel.density)



# a. Linear model without factors (predictors are only of type numeric)
prestige_lm3 <- lm(prestige ~ income + education, data=prestige)
glance(prestige_lm3)
tidy(prestige_lm3, conf.int = TRUE)


# For each level of `type` (type of occupation), show the relation between
#    each of the predictors and the outcome
ggplot(prestige, aes(x=income, y=prestige)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm) +   # Add linear regression line (with 95% confidence region)
     facet_grid(~ type)

ggplot(prestige, aes(x=education, y=prestige)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm) +   # Add linear regression line (with 95% confidence region)
    facet_grid(~ type)

prestige %>%
     group_by(type) %>%
     summarise(mean_of_prestige = mean(prestige)) %>%
     spread(type, mean_of_prestige)


# b. Linear model where predictor is only the factor 
# (compute the average prestige for each 
#    factor level - without any other regressor
prestige_lm4 <- lm(prestige ~ type, data=prestige)
glance(prestige_lm4)
tidy(prestige_lm4, conf.int = TRUE)


# c. Linear model two predictors of type factor (compute 
# the average prestige for each factor level - without any other regressor)
prestige_lm5 <- lm(prestige ~ type + census, data=prestige)
glance(prestige_lm5)
tidy(prestige_lm5, conf.int = TRUE)


# d. 
prestige_lm6 <- lm(prestige ~ income + education + type, data=prestige)
glance(prestige_lm6)
tidy(prestige_lm6, conf.int = TRUE)


# e. a model with `census` predictor treated as factor
prestige$census <- as.factor(prestige$census)
prestige_lm7 <- lm(prestige ~ income + education + type +
          census, data=prestige)
glance(prestige_lm7)
tidy(prestige_lm7, conf.int = TRUE)



#######################################################################
###                      V.3 Education (USA)
### see above for the data set description and import 

glimpse(education)
# descriptive statistics about the numeric variables
skimr::skim(education)

# A new model with a categorical predictor
education_lm3 <- lm(csat ~ expense + percent + income + high + 
     college + region , data=education)
glance(education_lm3)
tidy(education_lm3, conf.int = TRUE)


# Comparing two models
summary(education_lm2)
summary(education_lm3)

# as R^2 increases (both `education_lm2` and `education_lm3` have p-value: < 2.2e-16), 
#  `education_lm3` is a better model



#######################################################################
### 	          V.4 `tips` data set (package `reshape`)                ###

# the previous linear model (without categorical predictors)
tips_lm1 <- lm(tip ~ total_bill + size, data=tips, na.action = na.exclude)
glance(tips_lm1)
tidy(tips_lm1, conf.int = TRUE)

# the new linear model (with three categorical predictors)
tips_lm2 <- lm(tip ~ total_bill + size + sex + day + time, 
     data=tips, na.action = na.exclude)

summary(tips_lm2)
glance(tips_lm2)
tidy(tips_lm2, conf.int = TRUE)

table(tips$time)

# Nothing was gained, on the contrary (see R^2) !!!


#######################################################################
### 	                   V.5 `insurance` data set                    ###
### data available on 
### https://github.com/stedy/Machine-Learning-with-R-datasets

## Variables
## `age`: age of primary beneficiary
## `sex`: insurance contractor gender, female, male
## `bmi`: Body mass index, providing an understanding of body, 
##   weights that are relatively high or low relative to height, 
##   objective index of body weight (kg / m ^ 2) using the ratio 
##   of height to weight, ideally 18.5 to 24.9
## `children`: Number of children covered by health insurance / Number of dependents
## `smoker`: Smoking
## `region`: the beneficiary's residential area in the US, northeast, southeast, 
##        southwest, northwest.
## `charges`: Individual medical costs billed by health insurance
insurance <- readr::read_csv('insurance.csv')

# descriptive statistics
insurance %>%
     skimr::skim()

# examine bivariate relationships
insurance %>%
     select_if(is.numeric) %>%
     cor()

# library('corrplot')
corrplot::corrplot(cor(insurance %>%
     select_if(is.numeric) , 
             method = "spearman"), method = "number", type = "upper")

library(corrgram)
corrgram::corrgram(insurance %>% select_if(is.numeric),
     lower.panel=panel.pie, upper.panel=panel.pts,
     diag.panel=panel.density)

corrgram::corrgram(insurance %>% select_if(is.numeric),
     lower.panel=panel.conf, upper.panel=panel.pts,
     diag.panel=panel.density)


insurance_lm1 <- lm(charges ~ ., data = insurance)
summary(insurance_lm1)



#######################################################################
###       VI.  Multiple linear regression with interactions         ###
#######################################################################


### see the case study

