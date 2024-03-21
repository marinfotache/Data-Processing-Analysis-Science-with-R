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
###      11a1 Simple (Ordinary Least Square) Linear Regression       ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/11%20Scoring%20(Regression)%20and%20Classification/11_scoring%20and%20clasisification.pptx
#######################################################################
## last update: 2024-03-21

library(tidyverse) 
#library(readxl)
# 
# `skimr` for descriptive statistics
library(skimr)

# 	"car" for regression diagnostics
# install.packages("car")
library(car)
#	"QuantPsyc" for standardized regression coefficients
# install.packages("QuantPsyc")
library(QuantPsyc)

# "pairsp" function from "pgirmess" package
# install.packages("pgirmess")
library(pgirmess)

# package `broom` for "tidying" the regression models
# install.packages('broom')
library(broom)

library(foreign)

# this package helps interpreting the model results
library(report)

#######################################################################
###           Download the necessary data sets for this script       ###
#######################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

#######################################################################
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)


#######################################################################
###                                Agenda                           ###
#######################################################################
###       I. What is Linear Regression?                             ###
###       II. Simple Linear Regression. Package `broom`             ###
###       III. Polynomial regression                                ###
#######################################################################
###		


###################################################################################
###                     I. Introduction to Regression                           ###
###################################################################################


###################################################################################
##                  Statistical assumptions of the linear regression:
#        Yi = b0 + b1 * Xi1  +  b2 * Xi2 + ... 
#  1. Normality: for fixed values of the predictors (Xs), 
#     the outcome variable (response) is normally distributed 
#     (predictors variables may have a non-normal distribution )
#  2. Independence: the Yi values are independent of each other.
#  3. Linearity: the dependent variable (response) is linearly related 
#     to the independent variables (predictors)
#  4. Homoscedasticity: the variance of the dependent variable Y doesn’t vary with the
#     levels of the independent variables Xj. 

## we'll see later how to check for each assumption

###################################################################################
## 
#  The basic check of the fit for every model is
#     deviation = sum ((observed - model)^2)
#  	

#  better fit means smaller deviation

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
###################################################################################


###################################################################################
###            II. Simple  Linear Regression. Package `broom`                   ###
###################################################################################


###################################################################################
###                 II.1 Faithful (a geyser in Yellowstone Park)
#  Example taken from
#  http://www.r-tutor.com/elementary-statistics/simple-linear-regression/significance-test-linear-regression
#  A R built-in data frame is called faithful contains a collection of observations 
#   of the Old Faithful geyser in the USA Yellowstone National Park. 
head(faithful)
# variabile "eruptions"  is the duration of the geyser eruptions
# "waiting" is the length of waiting period until the next eruption

# descriptive statistics about the two variables of "faithful" data set
summary(faithful)

# with `skimr` package (see script `09a...`)
faithful %>%
     skimr::skim()

# draw the scatterplots 

library(GGally) # for correlation plots
# Solution with `ggpairs` function from package `GGally`
GGally::ggpairs(faithful, title = "Scatter Plot for eruptions ~ waiting")


# or "scatterplot" function
car::scatterplot( eruptions ~ waiting, data = faithful, pch=19,
	spread=FALSE, main="Scatter Plot for eruptions ~ waiting")

###
# now, for the liniar model (lm)
# newModel = lm (outcome ~ predictors, data = dataFrame, na.action = action)
#   na.action = na.fail (any missing value will cancel the computations)
#	or
#   na.action = na.omit 

# the regresssion equation is: 
#       eruptions = b0 + b1 * waiting + error

# H0: the coefficient "b1" of predictor "waiting" is 0 
#    (there is NOT a significant relationship between the variables in the linear 
#     regression model of the data set faithful).
eruption_lm1 <- lm(eruptions ~ waiting, data=faithful)
summary(eruption_lm1)

# the unstandardized regression coefficient (b1) estimated value = 0.076; since it is 
#    different from zero and p-value < 2.2e-16, there’s an expected 
#    increase of 0.076 seconds in eruption time ("eruptions") for 
#    every one second increase of "waiting";
#  

# getting the model interpretation with `report` package
eruption_lm1 |>
     report::report()


# sometimes STANDARDIZED regression coefficients ("beta"s) are more useful
# - unstandardized regression coefficients is interpreted as the change in the 
#    outcome variable (Y) for each unit change in the predictor (X)
# - standardized regression coefficient is interpreted as the change, measured
#    in standard deviations, in the outcome (Y) for each stardard deviation 
#    increase of X

# standardized regression coefficients computed directy
eruption_scaled_lm1 <- lm(scale(eruptions) ~ scale(waiting), data=faithful)
summary(eruption_scaled_lm1)

# standardized regression coefficients computed by variaous packages ("lm.beta", "MBESS", ...)
# Ex: "QuantPsyc" contains function "lm.beta" that provides the standardized 
#    regression coefficients
library(QuantPsyc)
QuantPsyc::lm.beta(eruption_lm1)
# for every increase of "waiting" with its standard deviation, eruption time is
#   expected to increase with 0.9 of its standard deviation
sd(faithful$waiting)
sd(faithful$eruption)
# for every increase of "waiting" with its standard deviation (13.59 seconds), 
# eruption time is expected to increase with 0.9 of its standard deviation (1.14 seconds)



# the multiple R-squared (0.8115) indicates that the model accounts for 
#    81% of the variance in "eruptions"
# the residual standard error (0.4965) can be thought of as the average error in
#    predicting eruptions (duration) using this model
# The F statistic tests whether the predictor variables taken together, 
#   predict the response variable above chance levels

# Rule of thumb (Charles Wheelan): The coefficient b1 (0.075628) is likely 
#   to be statistically significant when it is at least twice the size of
#   standard error (0.002219)

# confidence interval of the predictors unstandardized coefficients
coefficients(eruption_lm1)
confint(eruption_lm1)

# confidence interval of the predictors standardized coefficients
coefficients(eruption_scaled_lm1)
confint(eruption_scaled_lm1)


# compare actual values (of y) to fitted values (predicted values of y)
faithful$eruption
fitted(eruption_lm1)
# y values - predicted y values = residuals
residuals(eruption_lm1)

# draw a scatter plot with regression line for "eruptions" predicted from "waiting"
ggplot(faithful, aes( x = waiting, y = eruptions)) + 
     geom_point() + 
	geom_smooth( method = "lm") + 
     labs(x="waiting time", y="eruption duration") 

## Obs:
# Note, that R^2 does NOT tell whether:
#    * the independent variables are a true cause of the changes in the dependent variable
#    * the correct regression type was used
#    * the most appropriate independent variable has been chosen
#    * the model might be improved by using transforming the independent variable


## with package `broom`, information about models can be yielded in a 
##   `tidy` format

# three verbs, `glance`, `tidy`, and `augment`

# overall information about the model...
broom::glance(eruption_lm1)

# information about predictors
broom::tidy(eruption_lm1, conf.int = TRUE)

# information about observations
broom::augment(eruption_lm1)


## get a density plot the residuals of the model 
ggplot(broom::augment(eruption_lm1), 
       aes( x = .resid)) + 
     geom_density(color = "white", fill = 'red', alpha = .5) + 
	ggtitle("Density Plot of the Residuals for the Model", 
	        subtitle = "lm(eruptions ~ waiting, data=faithful)" ) +
  	xlab("residuals") +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="14", hjust = 0.5))


# draw a scatter plot with regression line for "eruptions" predicted from "waiting"
ggplot(broom::augment(eruption_lm1), 
          aes( x = .fitted, y = eruptions)) + 
     geom_point() + 
	geom_smooth( method = "lm") + 
     labs(x=".fitted (estimated)", y="eruption duration (observed)") +
     ggtitle("Fitted vs. Estimated", 
	     subtitle = "lm(eruptions ~ waiting, data=faithful)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="14", hjust = 0.5))



###################################################################################
###                                II.2 Women
#  Example taken from R. Kabacoff - R in Action (Manning), 2013, 2015
# The dataset women in the base installation provides the height and weight for 
#   a set of 15 women ages 30 to 39. 
##	We want to predict weight from height
head(women)

# descriptive statistics about the two variables of the data set
skimr::skim(women)

GGally::ggpairs(women %>% dplyr::select (height, weight), title = "Women Age 30-39")

# scatterplot of the relationship between "weight" and "height"
car::scatterplot(weight ~ height, data=women, spread=FALSE, pch=19,
	main="Women Age 30-39", xlab="Height (inches)", ylab="Weight (lbs.)")

# the linear model: weight = beta0 + beta1 * height
weight_lm1 <- lm(weight ~ height, data=women)
summary(weight_lm1)
# the regression coefficient (3.45) is significantly different from zero at p < 0.001
#   and indicates that there’s an expected increase of 3.45 pounds of weight
#     for every 1 inch increase in height. 
# the multiple R-squared (0.991) indicates that the model accounts for 99.1 percent 
#   of the variance in weights.
# the residual standard error (1.53 lbs.) can be thought of as the average error in
#   predicting weight from height using this model
# The F statistic tests whether the predictor variables taken together, 
#   predict the response variable above chance levels;

# The prediction equation would be:
#   weight.hat = -87.52 + 3.45 * height 

# Rule of thumb (Charles Wheelan): The coefficient b1 (3.45) is likely 
#   to be statistically significant when it is at least twice the size of
#   standard error (0.09): it is!

# confidence interval of the parameters
tidy(weight_lm1, conf.int = TRUE)

# compare actual/observed values of y (`weight`) to fitted values (`.fitted`)
augment (weight_lm1)

# scatter plot with regression line for "weight" predicted from "height"
ggplot(women, aes( x = height, y = weight)) + geom_point() + 
	geom_smooth( method = "lm") + labs(x="Height (in inches)", 
	    y="Weight (in pounds)") 


# superimposed density curves of observed and fitted values for the outcome
ggplot(augment (weight_lm1) %>% 
            mutate (row_num = row_number()) %>%
            transmute (row_num, observed = weight, fitted = .fitted) %>%
            gather (type_of_y, value, -row_num), 
          aes(x = value, fill = type_of_y, col = type_of_y)) +  
     geom_density(alpha = .5) +
  	xlab("") +
     ggtitle("Fitted vs. Observed", 
	     subtitle = "lm(weight ~ height, data=women)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="14", hjust = 0.5))




###################################################################################
###                                II.3 Education (USA)
##	 Example taken from http://www.princeton.edu/~otorres/Regression101R.pdf
##     and http://dss.princeton.edu/training/Regression101.pdf
##   Are SAT scores higher in states that spend more money on education ?
#  the data set have to be imported into R from Stata (the file was previously downloaded)

states <- foreign::read.dta("states.dta")
glimpse(states)
# descriptive statistics about two variables of the data set
skimr::skim(states)


# outcome (Y) variable is "csat" (SAT scores) 
# predictor (X) variable is "expense" (Per pupil expenditures primary & secondary)
car::scatterplot(states$csat~ states$expense , data=states, spread=FALSE, pch=19,
	main="Scores at SAT test vs. expenses on education", 
	xlab="Expenses (per child)", ylab="SAT scores")

# the linear model:  csat = b0 + b1 * expense
states_lm1 <- lm(csat ~ expense , data=states)
glance(states_lm1)
tidy(states_lm1, conf.int = TRUE)
tidy(states_lm1, conf.int = TRUE, conf.level = .90)

# getting the model interpretation with `report` package
states_lm1 |>
     report::report()


# scatter plot with regression line for "csat" scores predicted from "expense"
ggplot(states, aes( x = expense, y = csat)) +
     geom_point() + 
	geom_smooth( method = "lm") + labs(x="Expenses (per child)", y="SAT test score") +
     ggtitle("Scores at SAT test vs. expenses on education", 
	     subtitle = "lm(csat ~ expense , data=states)" ) +
	theme (plot.title = element_text (colour="black", size="16", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="14", hjust = 0.5))
     


###################################################################################
###                                II.4 Births (2006)
###  Births
births2006 <- readr::read_tsv('births2006_package_nutshell.txt')
births2006 <- na.omit(births2006)
glimpse(births2006)

# descriptive statistics on  two variables of interest
births2006 %>%
     dplyr::select (BabyWeight, Mother_WeightGain_Kg) %>%
     skimr::skim()

###
# Predict "BabyWeight" from "Mother_WeightGain_Kg"
#  
ggplot(births2006, 
       aes( x = Mother_WeightGain_Kg, y = BabyWeight)) + 
     geom_point() + 
	geom_smooth( method = "lm") + 
     labs(x="Mother Weight Gain (Kg)", y="Baby Weight") +
     ggtitle("Predict `BabyWeight` from `Mother_WeightGain_Kg`", 
	     subtitle = "lm(BabyWeight ~ Mother_WeightGain_Kg, data=births2006)" ) +
	theme (plot.title = element_text (colour="black", size="14", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))
     
# the liniar model
births2006_lm1 <- lm(BabyWeight ~ Mother_WeightGain_Kg, data=births2006)
glance(births2006_lm1)
tidy(births2006_lm1, conf.int = TRUE, conf.level = .05)

# getting the model interpretation with `report` package
births2006_lm1 |>
     report::report()


###
# Predict "BabyWeight" from "Gestation_Weeks"
#  
ggplot(births2006, 
       aes( x = Gestation_Weeks, y = BabyWeight)) + 
     geom_point() + 
	geom_smooth( method = "lm") + 
     labs(x="Gestation Weeks", y="Baby Weight") +
     ggtitle("Predict `BabyWeight` from `Gestation_Weeks`", 
	     subtitle = "lm(BabyWeight ~ Gestation_Weeks, data=births2006)" ) +
	theme (plot.title = element_text (colour="black", size="14", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))
     

# well, a number of gestation weeks larger than 45 is quite strange, so 
#    we'll remove those observations
ggplot(births2006 %>%
            filter (Gestation_Weeks < 45),
       aes( x = Gestation_Weeks, y = BabyWeight)) + 
     geom_point() + 
	geom_smooth( method = "lm") + 
     labs(x="Gestation Weeks", y="Baby Weight") +
     ggtitle("Predict `BabyWeight` from `Gestation_Weeks`", 
	     subtitle = "lm(BabyWeight ~ Gestation_Weeks)" ) +
	theme (plot.title = element_text (colour="black", size="14", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))

# the liniar model
births2006_lm2 <- lm(BabyWeight ~ Gestation_Weeks, data = births2006 %>%
            filter (Gestation_Weeks < 45))
glance(births2006_lm2)

# confidence interval of the predictors
tidy(births2006_lm2, conf.int = TRUE)



###################################################################################
###                           III. Polynomial regression                        ###
###################################################################################
# Polynomial regression predicts a response variable from a predictor variable, 
#   where the form of the relationship is an n-th degree polynomial.


###################################################################################
###                                 III.1 Women
#  Example taken from Kabacoff's R in Action (Manning), 2013 - women data set 

# height^2 adds a height-squared term to the prediction equation. 
# the I function treats the contents within the parentheses as an R regular expression

# the new liniear model is: weight = b0 + b1 * height + b2 * heigth ^ 2 
weight_lm2 <- lm(weight ~ height + I(height^2), data=women)
glance(weight_lm2)
tidy(weight_lm2, conf.int = TRUE)

ggplot(women,
       aes( x = height, y = weight)) + 
     geom_point() + 
	geom_smooth( method = "lm", formula = y ~ x + I(x^2)) + 
     labs(x="Height (in inches)", y="Weight (in lbs)") +
     ggtitle("Simple Polynomial Regression", 
	     subtitle = "lm(weight ~ height + I(height^2), data=women)" ) +
	theme (plot.title = element_text (colour="black", size="14", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))

# getting the model interpretation with `report` package
weight_lm2 |>
     report::report()


# now (following Kabacoff's example) going further: 
#    weight = b0  +  b1 * height  +  b2 * heigth^2  +  b3 * height^3
weight_lm3 <- lm(weight ~ height + I(height^2) +I(height^3), data=women)
glance(weight_lm3)
tidy(weight_lm3, conf.int = TRUE)

ggplot(women,aes( x = height, y = weight)) + 
     geom_point() + 
	geom_smooth( method = "lm", formula = y ~ x + I(x^2) + I(x^3)) + 
     labs(x="Height (in inches)", y="Weight (in lbs)") +
     ggtitle("Simple Polynomial Regression", 
	     subtitle = "lm(weight ~ height + I(height^2) +I(height^3), data=women)" ) +
	theme (plot.title = element_text (colour="black", size="14", hjust = 0.5))+
     theme (plot.subtitle = element_text (colour="black", size="12", hjust = 0.5))


