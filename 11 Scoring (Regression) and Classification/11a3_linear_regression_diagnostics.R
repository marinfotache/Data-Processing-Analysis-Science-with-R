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
###                 11a3 Regression Diagnostics                      ###
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/11%20Scoring%20(Regression)%20and%20Classification/11_scoring%20and%20clasisification.pptx
#######################################################################
## last update: 19.11.2019

library(tidyverse) 

#######################################################################
###           Download the necesary data sets for this script       ###
#######################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')

# check if the current directory is ok
getwd()
#######################################################################
# giving up scientific notation (1.6e+07)
#options("scipen"=30, "digits"=14)

#######################################################################
###                  Agenda of the previous scripts                 ###
#######################################################################
###       I. What is Linear Regression?                             ###
###       II. Simple Linear Regression. Package `broom`             ###
###       III. Polynomial regression                                ###
#######################################################################
###       IV. Multiple linear regression      		             ###
###       V. Multiple Regression with Categorical Predictors        ###
###       VI. Multiple linear regression with interactions          ###
#######################################################################


#######################################################################
###                  Agenda of the current script                   ###
#######################################################################
###        VII. Graphic approach for regression diagnostics         ###
###        VIII. Unusual and influential observations               ###
###        IX. Enhanced approaches to regression diagnostics        ###
#######################################################################

# some needed packages
library(tidyverse)
library(car)
library(QuantPsyc)



###################################################################################
##  Recap:   Statistical assumptions of the linear regression:
#        Yi = b0 + b1 * Xi1  +  b2 * Xi2 + ... 
#  1. Normality: for fixed values of the predictors (Xs), 
#     the outcome variable (response) is normally distributed 
#     (predictors variables may have a non-normal distribution )
#  2. Independence: the Yi values are independent of each other.
#  3. Linearity: the dependent variable (response) is linearly related 
#     to the independent variables (predictors)
#  4. Homoscedasticity: the variance of the dependent variable Y doesn’t vary with the
#     levels of the independent variables Xj. 



#######################################################################################
###        VII. Graphic approach for regression diagnostics        ###
#######################################################################################

#######################################################################################
###                 VII.1 Women (see a previous script (11a) for description)
#  Example taken from R. Kabacoff - R in Action (Manning), 2013, 2015
weight.lm1 = lm(weight ~ height, data=women)
# combine the four plots produced by the plot() function into one large 2x2 graph.
par(mfrow=c(2,2))
# draw the four plots
plot(weight.lm1)
par(mfrow=c(1,1))

##
#     Normality 

# If the dependent variable is normally distributed for a fixed set of predictor values, 
#    then the residual values should be normally distributed with a mean of 0. 
#    The Normal Q-Q plot (upper right) is a probability plot of the standardized
#     residuals against the values that would be expected under normality. If
#     the normality assumption is met, the points on this graph should fall on the
#     straight 45-degree line. 
# Standardized residuals = residuals / their estimated standard deviation
# Standardized residuals are useful for comparing residuals from different models

##
#     Independence
# There are no information about independece in the plots

##
#     Linearity
# If the dependent variable is linearly related to the independent variables, 
#   there should be no systematic relationship between the residuals and 
#   the predicted (fitted) values. 
#  Residuals versus Fitted graph (upper left)

##
#     Homoscedasticity
# When the constant variance assumption is met, the points in the Scale-Location graph 
#   (bottom left) should be a random band around a horizontal line

##
# Residual versus Leverage graph (bottom right) identifies:
#   * outliers (observations having large positive or negative residual)
#   * high-leverage points (an outlier in the predictor space)
#   * influential observations (observations that have a disproportionate impact on
#        the determination of the model parameters (Cook’s distance)


weight.lm2 <- lm(weight ~ height + I(height^2), data=women)
par(mfrow=c(2,2))
plot(weight.lm2)
# removing the outlier/influential observation
weight.lm3 <- lm(weight~ height + I(height^2), data=women[-c(13,15),])
par(mfrow=c(2,2))
plot(weight.lm1)


#######################################################################################
###                    VIII. Unusual and influential observations                  ###
#######################################################################################
# 		          
#  Influential observations are observations that have a disproportionate 
#   impact on the values of the model parameters

###
###   Outliers (observations that aren’t predicted well by the model)
#   Some general rules for standardized residuals (Andy Field):
#    (1) standardized residuals less than - 3.29 or greater 
#          than + 3.29 are cause for concern
#    (2) if more than 1% of sample cases have standardized residuals
#          less than - 2.58 or greater than + 2.58, there is evidence that
#		 the level of error in the model is unacceptable
#    (3) if more than 5% of cases have standardized residuals less than 
#       - 1.96 or greater than + 1.96, there is also an evidence that 
#       the level of error in the model is unacceptable

###
###    Influential cases (observations)
#  If we delete some observations, will regression coefficients will
#   significantly differ?
#
#	Adjusted predicted value (Yapv) for a case is computed when the case is
#      excluded from the analysis. A new model is calculated without
#      a particular case and then the new model is used to predict the 
#      value of the outcome variable for the excluded case. If the case
#      does not exert a large influence over the model then the adjusted
#      predicted value is similar to the predictd value then the case is
#      included.
#    DFFit = the difference between the adjusted predicted value (Yapv) and the
#      original predicted value (Y^)
#    Studentized residual = diference between the adjusted predicted value (Yapv)
#      and the original observed value (Y) divided by the standard error
#      		StudentizedResidual =  (Yapv - Y) / SE
#    StudRezid is called studentized because it follows a Student's t distribution  
#    Studentized residuals assess the influence of a case on the ability of 
#      a model to predict that case, but they do not provide any information about
#      how a case influences the model as a whole.
#    Cook's distance is a measure of the overall influence of a case on the model.
#    If Cook's distante > 1, there may be a cause for concern
#    Hat values (or leverage) measure the influence of the observed value of the 
#      outcome variable (Y) over the predicted values
#    Average leverage = (k+1)/n, where k is the number of predictors in the model,
#      and n is the number of participants
#    A 0 value for the leverage indicates the case has no influence at all
#    Value 1 for the leverage suggests the case has complete influence over prediction.
#    Recommendations for investigating cases (Field, 2012):
#		* with values > 2 * Average leverage
#     or
#		* with values > 2 * Average leverage

#	DFBeta = the difference between a parameter estimated using all cases and 
#     estimated when one case is excluded.
#	  DFBeta is calculated for every case and for each of the parameters in the model	
#    DFFit = difference between the predicted value for a case when the model is
#      calculated including that case and when the model is calculated excluding that 
#      case.
#    DFFit = 0 when the case not influential
#
#	Advice (Stevens, cited in Field, 2012):
#    If a point is a significant outlier on Y, but its Cook's distance is < 1,
#       there is no real need to delete that point since it does not have a
#       large effect on the regression analysis. However, this type of points
#       must be studied to understand why they do not fit the model.


###################################################################################
###                                VIII.1 States (USA)
#  Example taken from Kabacoff's R in Action (Manning), 2013, 2015 
states_ <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", 
	"Income", "Frost")])
head(states_)
states.mrlm1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states_)
summary(states.mrlm1)



#  Outliers
# residuals
resid(states.mrlm1)
# standardized residuals
rstandard(states.mrlm1)
# studentized residuals
rstudent(states.mrlm1)


#  Influential cases
# Cook's distance
cooks.distance(states.mrlm1)
# DFBeta
dfbeta(states.mrlm1)
# DFFit
dffits(states.mrlm1)
# hat values (leverage)
hatvalues(states.mrlm1)
# covariance ratio
covratio(states.mrlm1)


# outlierTest() function (car package) reports the Bonferroni adjusted p-value for 
# the largest absolute studentized residual
library(car)
outlierTest(states.mrlm1)


#######
#  High leverage points (outliers with regard to the other predictors)
# An observation with a hat value greater than 2 or 3 times the average hat value 
#   should be examined.
hat.plot <- function(mlm1) {
  p <- length(coefficients(mlm1))
  n <- length(fitted(mlm1))
  plot(hatvalues(mlm1), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(mlm1), names(hatvalues(mlm1)))
}
hat.plot(states.mrlm1)



###################################################################################
###                 IX. Enhanced approaches to regression diagnostics           ###
### Kabacoff's R in Action (Manning), 2015 
###################################################################################
### States (USA)
states.mrlm1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states_)
summary(states.mrlm1)

# package "car" 
library(car)

###################################################################################
##     Normality 
# qqPlot() function  plots the studentized residuals (also called studentized 
# deleted residuals or jackknifed residuals) against a t distribution 
# with n-p-1 degrees of freedom, where n is the sample size and p is 
# the number of regression parameters (including the intercept).
par(mfrow=c(1,1))

# probability plot 
# id.method="identify" makes the plot interactive; after the graph is drawn, 
# mouse clicks on points within the graph will label them with values 
# specified in the labels option of the function.
# simulate=TRUE produced  a 95 percent confidence envelope (using a 
#   parametric bootstrap)
qqPlot(states.mrlm1, labels=row.names(states_), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

# examine Nevada
states_["Nevada",]
fitted(states.mrlm1)["Nevada"]
residuals(states.mrlm1)["Nevada"]
rstudent(states.mrlm1)["Nevada"]

# also for checking normality
# "residplot" function generates a histogram of the studentized residuals 
#   and superimposes a normal curve, kernel density curve, and rug plot
residplot <- function(states.mrlm1, nbreaks=10) {
  z <- rstudent(states.mrlm1)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(states.mrlm1)

###################################################################################
#     Independence of errors
# For any two observations the residual terms shoul be uncorrelated (independent)
# Durbin-Watson test can vary between 0 and 4.
#   * a value of 2 suggests that the residuals are uncorrelated
#   * a value > 2 indicates a negative correlation
#   * a value < 2 indicates a positive correlation

# H0: Errors are not independent
# Ha: Errors are independent
     
#  Durbin Watson test for Autocorrelated Errors
durbinWatsonTest(states.mrlm1)


###################################################################################
#     Linearity
# Look for evidence of nonlinearity in the relationship between the dependent variable 
#    and the independent variables by using component plus residual plots (also known as 
#    partial residual plots). 
# crPlots() function in the car package
library(car)
crPlots(states.mrlm1)
# Nonlinearity in any of the plots suggests nonliniearity of the model

###################################################################################
#     Homoscedasticity (constant error variance)
#  ncvTest() function (car package)  produces a score test of the hypothesis 
#    of constant error variance against the alternative that the error variance 
#    changes with the level of the fitted values. 
#  A significant result (p<0.05) suggests heteroscedasticity (nonconstant error variance).
library(car)
ncvTest(states.mrlm1)
# p = 0.1863156: the assumtion that the variance is constant is met

# spreadLevelPlot() function creates a scatter plot of the absolute standardized
#  residuals versus the fitted values, and superimposes a line of best fit. 
spreadLevelPlot(states.mrlm1)
# when the assuption is violated the line is nonhorizontal


###################################################################################
#                   Best way !
# Global validation of linear model assumption

# "gvlma" function performs a global validation of linear model assumptions as well 
# as separate evaluations of skewness, kurtosis, and heteroscedasticity.
#install.packages("gvlma")
library(gvlma)


##
## Diagnostics of multiple regression models described in previous script

# "states"
states_ <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", 
	"Income", "Frost")])
states.mrlm1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states_)
gvm.states.mrlm1 <- gvlma(states.mrlm1)
summary(gvm.states.mrlm1)


### Education (USA)
library(foreign)
education <- foreign::read.dta("states.dta")
education.mrlm1 <- lm(csat ~ expense + percent + income , data=education)
education.mrlm2 <- lm(csat ~ expense + percent + income + high + college + region , data=education)
gvm.education.mrlm1 <- gvlma(education.mrlm1)
summary(gvm.education.mrlm1)
gvm.education.mrlm2 <- gvlma(education.mrlm2)
summary(gvm.education.mrlm2)


### Prestige
library(car)
prestige.mrlm1 <-lm(prestige ~ education + log2(income) + women, data=Prestige)
gvm.prestige.mrlm1 <- gvlma(prestige.mrlm1)
summary(gvm.prestige.mrlm1)



###################################################################################
#     Multicollinearity
# Can be detected using a statistic called the variance inflation factor (VIF). 
#  VIF indicates whether a predictor has a strong linear relationship with
#   other predictors.
#  vif() function in the car package
#  sqlrt(vif) > 2 indicates a multicollinearity problem
library(car)
vif(states.mrlm1)
sqrt(vif(states.mrlm1))


