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
###     11a4 Case study
### Regression Models for Randomized/Blocked Data            ###
##  Single node simple queries results (select... from... where...),
##    SQL vs. Hive                  ###

### See also the presentation:
### xxxxxxxxxx
#######################################################################
## last update: 30.10.2018

library(tidyverse) 
library(lubridate)
library(stringr)

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
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)

#######################################################################
###                                Agenda                           ###
#######################################################################
###                     I. Introduction to Regression                           ###
###                       II. Simple  Linear Regression                         ###
###                           III. Polynomial regression                        ###




##############################################################################
###          04e. Regression Models for Randomized/Blocked Data            ###
##                  Single node simple queries results ###
###            (select... from... where...)  SQL vs. Hive                  ###
##############################################################################



#########################################################################
### 	                    I.4 Ionut Hrubaru's data set                 ###

load('Hrubaru_2016-02.RData')
results_ih_2016 <- results_details.ok


##############################################################################
###                         Load and clean data                            ###
##############################################################################
setwd('/Users/marinfotache/Dropbox/Hruby/Basic queries performance in SQL, NoSQL and Hadoop 2015/R/A - Single node')
#load(file='results.RData')
#load(file='details.RData')
load(file='result_details.RData')
results_details.ok <- results_details
# divide avg_number of rows to 1000000
results_details.ok$avg_nof_rows <- results_details.ok$avg_nof_rows / 1000000
names(results_details.ok)
save(results_details.ok, file = 'results_details.ok.RData')



##############################################################################
###       Separating queries executed in Hive and PostgreSQL               ###
###  in order to avoid "independence" assumption violations
##############################################################################

row_to_be_selected <- c()
for (i in unique(results_details.ok$queryId))
     row_to_be_selected <- c(row_to_be_selected, 
         paste(i, 
          sample(c('Hive', 'PostgreSQL'), 1) , sep='-') )

#row_to_be_selected

results_details.ok_rand <- results_details.ok[row_to_be_selected,]


##############################################################################
####                     New Model A (model A randomized)
#### duration = b0 + b1 * nof_attributes_select + b2 * nof_attributes_where +
#### b3 * nof_joins
####
####   duration ~ nof_attributes_select + nof_attributes_where + nof_joins
##############################################################################


### data
data.A.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
     'nof_attributes_where', 'nof_joins', 'dbserver')]
head(rownames(data.A.rand))

### correlation plot (nonparametric)
#citation('corrplot')
library(corrplot)
corrplot.mixed(corr=cor(data.A.rand[-5], method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')


### Scatterplot Matrix
#citation('car')
library(car)
scatterplotMatrix(data.A.rand[-5], spread=FALSE, smoother.args=list(lty=2),
     main="Scatter Plot Matrix for Variables of the Randomized Model A ")

### Linear model A
lm.A.rand <- lm(duration ~ nof_attributes_select + nof_attributes_where + nof_joins, 
     data=data.A.rand)
summary(lm.A.rand)
confint(lm.A.rand)


##############################################################################
####                     New model B (Model B randomized)
#### duration = b0 + b1 * nof_attributes_select + b2 * nof_attributes_where +
#### b3 * nof_joins + b4 * avg_nof_rows
###
####   duration ~ nof_attributes_select + nof_attributes_where + nof_joins +
####                avg_nof_rows 
##############################################################################
names(results_details.ok_rand)

### data
data.B.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
     'nof_attributes_where', 'nof_joins', 'avg_nof_rows', 'dbserver')]
head(rownames(data.B.rand))

### correlation plot (nonparametric)
library(corrplot)
corrplot.mixed(corr=cor(data.B.rand, method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')

# 
#library(gpairs)
#gpairs(data.B)

### Scatterplot Matrix
library(car)
scatterplotMatrix(data.B.rand, spread=FALSE, smoother.args=list(lty=2),
     main="Scatter Plot Matrix for Variables of the Randomized Model B Randomized")

### Linear model B
lm.B.rand <- lm(duration ~ nof_attributes_select + nof_attributes_where + 
          nof_joins + avg_nof_rows, data=data.B.rand)
summary(lm.B.rand)
confint(lm.B.rand)



## Comparing nested models using the anova function
# H0: Model lm.B does not improve the prediction of the outcome
anova(lm.B.rand, lm.A.rand)


# Comparing models with the Akaike Information 
#  Models with smaller AIC are preferred
AIC(lm.B.rand, lm.A.rand)



##############################################################################
####                     New model C (Model C randomized)                 ####
#### duration = b0 + b1 * nof_attributes_select + b2 * nof_attributes_where +
####    b3 * nof_joins + b4 * avg_nof_rows + b5 * is_hive
###
####   duration ~ nof_attributes_select + nof_attributes_where + nof_joins +
####                avg_nof_rows + is_hive
##############################################################################

names(results_details.ok)

### data
data.C.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
     'nof_attributes_where', 'nof_joins', 'avg_nof_rows', 'is_hive', 'dbserver')]
head(rownames(data.C.rand))

### correlation plot (nonparametric)
library(corrplot)
corrplot.mixed(corr=cor(data.C.rand, method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')

# 
#library(gpairs)
#gpairs(data.C)

### Scatterplot Matrix
library(car)
scatterplotMatrix(data.C.rand, spread=FALSE, smoother.args=list(lty=2),
     main="Scatter Plot Matrix for Variables of the Randomized Model C Randomized")

### Linear model C
lm.C.rand <- lm(duration ~ nof_attributes_select + nof_attributes_where + 
          nof_joins + avg_nof_rows + is_hive, data=data.C.rand)
summary(lm.C.rand)
confint(lm.C.rand)

lm.C.rand2 <- lm(duration ~ nof_attributes_select + nof_attributes_where + 
          nof_joins + avg_nof_rows + dbserver, data=data.C.rand)
summary(lm.C.rand2)
confint(lm.C.rand2)


## Comparing nested models using the anova function
# H0: Model lm.C does not improve the prediction of the outcome
anova(lm.B.rand, lm.C.rand)


# Comparing models with the Akaike Information 
#  Models with smaller AIC are preferred
AIC(lm.B.rand, lm.C.rand)



##############################################################################
####                     New model D (Model D randomized)                 ####
#### duration = b0 + b1 * nof_attributes_select + b2 * nof_joins +
####      b3 * avg_nof_rows + b4 * is_hive +
####      b5 * length_char_attributes_where + 
####      b6 * length_varchar_attributes_where +  b7 * nof_ors +
####      b8 * nof_in_s + b9 * nof_all_in_values 
###
####   duration ~ nof_attributes_select +  nof_joins +
####      avg_nof_rows + is_hive + length_char_attributes_where +
####      length_varchar_attributes_where + nof_ors + nof_in_s + 
####      nof_all_in_values
##############################################################################
names(results_details.ok_rand)

### data
data.D.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
      'nof_joins', 'avg_nof_rows', 'is_hive', 'dbserver',
     'length_char_attributes_where', 'length_varchar_attributes_where', 
     'nof_ors', 'nof_in_s', 'nof_all_in_values' )]
head(rownames(data.D.rand))

### correlation plot (nonparametric)
library(corrplot)
corrplot.mixed(corr=cor(data.D.rand, method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')

# 
#library(gpairs)
#gpairs(data.D.rand)

### Scatterplot Matrix
library(car)
scatterplotMatrix(data.D.rand, spread=FALSE, smoother.args=list(lty=2),
     main="Scatter Plot Matrix for Variables of the Randomized Model D Randomized")

### Linear model D
lm.D.rand <- lm(duration ~ nof_attributes_select + nof_joins + avg_nof_rows + 
          is_hive + length_char_attributes_where +
          length_varchar_attributes_where + nof_ors + nof_in_s + 
          nof_all_in_values, data=data.D.rand)
summary(lm.D.rand)
confint(lm.D.rand)

lm.D.rand2 <- lm(duration ~ nof_attributes_select + nof_joins + avg_nof_rows + 
          dbserver + length_char_attributes_where +
          length_varchar_attributes_where + nof_ors + nof_in_s + 
          nof_all_in_values, data=data.D.rand)
summary(lm.D.rand2)
confint(lm.D.rand2)


## Comparing nested models using the anova function
# H0: Model lm.D.rand does not improve the prediction of the outcome (comparing
#    to model C)
anova(lm.D.rand, lm.C.rand)


# Comparing models with the Akaike Information 
#  Models with smaller AIC are preferred
AIC(lm.D.rand, lm.C.rand)



##############################################################################
####                     New model E (Model E randomized)                 ####
####
#### duration = b0 + b1 * nof_attributes_select + b2 * is_hive +
####      b3 * length_char_attributes_where + 
####      b4 * length_varchar_attributes_where +  b5 * nof_ors +
####      b6 * nof_joins * avg_nof_rows 
###
##############################################################################

data.E.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
      'nof_joins', 'avg_nof_rows', 'is_hive', 'dbserver',
     'length_char_attributes_where', 'length_varchar_attributes_where', 
     'nof_ors', 'nof_in_s', 'nof_all_in_values' )]
head(rownames(data.E.rand))

data.E.rand$length_string_attrib_where <- data.E.rand$length_char_attributes_where +
     data.E.rand$length_varchar_attributes_where


### Linear model E (using interactions)
lm.E.rand <- lm(duration ~ nof_attributes_select  + is_hive + 
          length_char_attributes_where + length_varchar_attributes_where +
          nof_ors +
          nof_joins:avg_nof_rows ,
     data=data.E.rand)
summary(lm.E.rand)
confint(lm.E.rand)

lm.E.rand2 <- lm(duration ~ nof_attributes_select  + dbserver + 
          length_char_attributes_where + length_varchar_attributes_where +
          nof_ors +
          nof_joins:avg_nof_rows ,
     data=data.E.rand)
summary(lm.E.rand2)
confint(lm.E.rand2)


lm.F.rand <- lm(duration ~ nof_attributes_select  + is_hive + 
          length_string_attrib_where + nof_ors +
          nof_joins:avg_nof_rows ,
     data=data.E.rand)
summary(lm.F.rand)
confint(lm.F.rand)


data.G.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
      'nof_joins', 'avg_nof_rows', 'is_hive',
     'length_char_attributes_where', 'length_varchar_attributes_where', 
     'nof_ors', 'nof_ands', 'nof_in_s', 'nof_all_in_values' )]

lm.G.rand <- lm(duration ~ nof_attributes_select  + is_hive + 
           nof_ors + nof_ands +
          nof_joins:avg_nof_rows ,
     data=data.G.rand)
summary(lm.G.rand)
confint(lm.G.rand)



##############################################################################
####                     New model H (Model H randomized)                 ####
#### 
#### duration = b0 + b1 * nof_attributes_select + b2 * is_hive +
####      b3 * sqrt(nof_ors) + b4 * nof_joins * avg_nof_rows +
####           + b4 * is_hive * avg_nof_rows 
###
##############################################################################

data.H.rand <- results_details.ok_rand[, c('duration', 'nof_attributes_select', 
      'nof_joins', 'avg_nof_rows', 'is_hive', 'nof_ors', 'dbserver', 
     'nof_attributes_where')]

### correlation plot (nonparametric)
library(corrplot)
corrplot.mixed(corr=cor(data.H.rand, method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')

lm.H.rand <- lm(duration ~ nof_attributes_select  + is_hive +
           sqrt(nof_ors) + is_hive:avg_nof_rows +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand)
confint(lm.H.rand)

lm.H.rand2 <- lm(duration ~ nof_attributes_select  + dbserver +
           sqrt(nof_ors) + dbserver:avg_nof_rows +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand2)
confint(lm.H.rand2)

lm.H.rand3 <- lm(duration ~ nof_attributes_select + 
          dbserver + 
           sqrt(nof_ors) + dbserver:avg_nof_rows + dbserver:nof_joins +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand3)
confint(lm.H.rand3)

# no variables appearing in the interactions
lm.H.rand4 <- lm(duration ~ nof_attributes_select + 
           sqrt(nof_ors) + dbserver:avg_nof_rows + dbserver:nof_joins +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand4)
confint(lm.H.rand4)

lm.H.rand5 <- lm(duration ~ nof_attributes_select + 
           sqrt(nof_ors) + dbserver + avg_nof_rows + nof_joins +
                dbserver:avg_nof_rows + dbserver:nof_joins +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand5)
confint(lm.H.rand5)




summary(lm.A.rand)
summary(lm.B.rand)
summary(lm.C.rand2)
summary(lm.D.rand2)
summary(lm.E.rand2)
summary(lm.H.rand2)
summary(lm.H.rand3)

summary(lm.H.rand5)



# (u + v + w)^3

lm.I.rand <- lm(duration ~ nof_attributes_select  + sqrt(nof_ors) +
          (dbserver + nof_joins + avg_nof_rows) ^3
     , data=data.H.rand )
summary(lm.I.rand)
confint(lm.I.rand)

lm.I.rand <- lm(duration ~ nof_attributes_select  + 
           sqrt(nof_ors) + (dbserver) +
               dbserver:avg_nof_rows + nof_joins:avg_nof_rows  +
          dbserver:nof_joins
     ,data=data.H.rand )
summary(lm.I.rand)


data.G <- results_details.ok[, c('duration', 'nof_attributes_select', 
      'nof_joins', 'avg_nof_rows', 'is_hive',
     'length_char_attributes_where', 'length_varchar_attributes_where', 
     'nof_ors', 'nof_ands', 'nof_in_s', 'nof_all_in_values' )]

lm.J.rand <- lm(duration ~ (.)^3, data = data.G)
summary(lm.J.rand)

###
# visualize interactions

lm.X.rand <- lm(duration ~ (.)^2, data = data.G)
summary(lm.X.rand)


#install.packages('effects')
#citation('effects')
library(effects)
--
plot(effect("is_hive:avg_nof_rows", lm.H.rand, xlevels=list(avg_nof_rows=
          c(0.5, 1, 1.5, 2, 2.5, 3))), multiline=TRUE, rug=FALSE,
      main = "Interaction effect \nis_hive * avg_nof_rows \nin Model H Randomized")
--
plot(effect("avg_nof_rows:nof_joins", lm.H.rand, xlevels=list(nof_joins = 1:7)),
     multiline=TRUE, rug=FALSE,
           main = "Interaction effect \nnof_joins * avg_nof_rows \nin Model H Randomized")

#plot(effect("avg_nof_rows:nof_joins", lm.H.rand, xlevels=list(avg_nof_rows=
#          c(0.5, 1, 1.5, 2, 2.5, 3))),
#     multiline=TRUE, rug=FALSE)


# https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
#install.packages('interplot')
#library(interplot)
#interplot(m = lm.H.rand, var1 = "is_hive", var2 = "avg_nof_rows")
#interplot(m = lm.H.rand, var1 = "nof_joins", var2 = "avg_nof_rows")

# Comparing models with the Akaike Information 
#  Models with smaller AIC are preferred
AIC(lm.H.rand, lm.D.rand)




##############################################################################
####                     Randomized Models (A-D, H)  Diagnostics
##############################################################################

# There are three main categories of potential problems (Faraway 2015, 73):
#    I. Errors
#         Ia.	Are independent
#         Ib.	Have equal variance
#         Ic.	Are normally distributed
#    II. Model linearity
#    III.	Unsual observations
#         IIIa.	Outliers
#         IIIb.	High leverage values
#         IIIc.	Influential observations
# We'all also add
#    IV. Collinearity


##############################################################################
###                      Ia Error independence 
# Is best assessed knowing how the data was collected. 

### Durbin–Watson test detects serially correlated errors (useful especially when dealing to time series). 
#  The null hypothesis of this test is H0: the outcome variables values are not autocorrelated, 
#  that is, errors are are independent (Kabacoff 2015, 190). 

library(car)
durbinWatsonTest(lm.A.rand)
durbinWatsonTest(lm.B.rand)
durbinWatsonTest(lm.C.rand)
durbinWatsonTest(lm.D.rand)
durbinWatsonTest(lm.H.rand)

durbinWatsonTest(lm.H.rand2)
durbinWatsonTest(lm.H.rand3)

durbinWatsonTest(lm.H.rand5)


# Only model manifests autocorrelation (p-value is < 0.05).
# All other models are GOOD!!!

# install.packages('lmtest')
# test also with package "lmtest"
#citation('lmtest')
library(lmtest)
dwtest(lm.A.rand)
dwtest(lm.B.rand)
dwtest(lm.C.rand)
dwtest(lm.D.rand)
dwtest(lm.H.rand)
dwtest(lm.H.rand2)
dwtest(lm.H.rand3)
dwtest(lm.H.rand4)



##############################################################################
###                      Ib. Error homoscedasticity 

# Error variance (homoscedasticity) can be assessed with two functions 
# of car package, ncvTest and spreadLevelPlot.

# ncvTest() function produces a score test of the null hypothesis 
#   H0: error variance is constant -homoscedasticity), 
#   alternate hypothesis (Ha) : error variance is nonconstant – heteroscedastocity 
# (Kabacoff 2015, 191). 

ncvTest(lm.A.rand) 
ncvTest(lm.B.rand) 
ncvTest(lm.C.rand) 
ncvTest(lm.D.rand) 
ncvTest(lm.H.rand) 
ncvTest(lm.H.rand2) 
ncvTest(lm.H.rand3) 
ncvTest(lm.H.rand5) 



# The spreadLevelPlot() function creates a scatter plot of the absolute  
#  standardized residuals versus the fitted values and superimposes a line 
#  of best fit (Kabacoff 2015, 192). 
#  The points form a random horizontal band around a horizontal line of best fit.  
#  If homoscedasticity assumption is violated, the line is nonhorizontal. 


### bptest Breusch-Pagan Test with package "lmtest"
library(lmtest)
bptest(lm.A.rand) 
bptest(lm.B.rand) 
bptest(lm.C.rand) 
bptest(lm.D.rand) 
bptest(lm.H.rand) 

### The spreadLevelPlot() function creates a scatter plot of the absolute 
##   standardized residuals versus the fitted values and superimposes 
# a line of best fit. 
par(mfrow=c(3,2))
spreadLevelPlot(lm.A.rand, main="Spread-Level Plot for Model A Randomized")
spreadLevelPlot(lm.B.rand, main="Spread-Level Plot for Model B Randomized")
spreadLevelPlot(lm.C.rand, main="Spread-Level Plot for Model C Randomized")
spreadLevelPlot(lm.D.rand, main="Spread-Level Plot for Model D Randomized")
spreadLevelPlot(lm.H.rand, main="Spread-Level Plot for Model H Randomized")
# spreadLevelPlot(lm.H.rand2, main="Spread-Level Plot for Model H Randomized")
#spreadLevelPlot(lm.H.rand3, main="Spread-Level Plot for Model H Randomized")
spreadLevelPlot(lm.H.rand5, main="Spread-Level Plot for Model H Randomized")

par(mfrow=c(1,1))


# The points form a random horizontal band around a horizontal line of best fit. 
# If you’d violated the assumption, you’d expect to see a nonhorizontal line.

# In our models, erors were not independent. Faraway 2015, p.113 suggests using
#  Generalized Least Squares instead of Ordinary Least Squares
# see next section


##############################################################################
###                      Ic. Errors are normally distributed 

# Error normality is the least critical (Faraway 2015, ) of all requirements, 
# especially when the number of observations is large. 
# Function qqPlot() plots the studentized residuals (also called 
#   studentized deleted residuals or jackknifed residuals) against 
#  a t distribution with n – p – 1 degrees of freedom, 
#   where n is the sample size and p is the number of regression 
# parameters, including the intercept (Kabacoff 2015).


par(mfrow=c(3,2))
qqPlot(lm.A.rand, labels=row.names(data.A), id.method="identify",
     simulate=TRUE, main="Studentized Residuals Q-Q Plot for Linear Model A Randomized")
qqPlot(lm.B.rand, labels=row.names(data.B), id.method="identify",
     simulate=TRUE, main="Studentized Residuals Q-Q Plot for Linear Model B Randomized")
qqPlot(lm.C.rand, labels=row.names(data.C), id.method="identify",
     simulate=TRUE, main="Studentized Residuals Q-Q Plot for Linear Model C Randomized")
qqPlot(lm.D.rand, labels=row.names(data.D), id.method="identify",
     simulate=TRUE, main="Studentized Residuals Q-Q Plot for Linear Model D Randomized")
qqPlot(lm.H.rand, labels=row.names(data.H), id.method="identify",
     simulate=TRUE, main="Studentized Residuals Q-Q Plot for Linear Model H Randomized")
par(mfrow=c(1,1))


shapiro.test(residuals(lm.A.rand))
shapiro.test(residuals(lm.B.rand))
shapiro.test(residuals(lm.C.rand))
shapiro.test(residuals(lm.D.rand))
shapiro.test(residuals(lm.H.rand))
shapiro.test(residuals(lm.H3))


# When error normality is not fulfilled, Faraway (2015, 123) suggests
#         using robust regression
# see next section



##############################################################################
###                           II. Model linearity
#  Model linearity. Linearity or nonlinearity in the relationship between 
# the outcome variable and the predictor variables can be identified using  
#  component plus residual plots (Kabacoff 2015, 190-191). 
# Nonlinearity in any of the plots suggests the model is not properly built, 
# and one must add curvilinear (components such as polynomial terms), 
# transform one or more variables or abandon linear regression in favor of
#  some other regression variant.

par(mfrow=c(3,2))
plot(lm.A.rand, which=1, main = "Model A Randomized")
plot(lm.B.rand, which=1, main = "Model B Randomized")
plot(lm.C.rand, which=1, main = "Model C Randomized")
plot(lm.D.rand, which=1, main = "Model D Randomized")
plot(lm.H.rand, which=1, main = "Model H Randomized")
plot(lm.H.rand2, which=1, main = "Model H2 Randomized")
plot(lm.H.rand3, which=1, main = "Model H3 Randomized")
plot(lm.H.rand4, which=1, main = "Model H3 Randomized")



par(mfrow=c(1,1))


library(car)
# Component + Residual Plots
par(mfrow=c(3,2))
crPlots(lm.A.rand, main = "Component + Residual Plots for Model A Randomized")
library(effects)
plot(lm.A.rand, partial.residuals=FALSE)
crPlots(lm.B.rand, main = "Component + Residual Plots for Model B Randomized")
crPlots(lm.C.rand, main = "Component + Residual Plots for Model C Randomized")
crPlots(lm.D.rand, main = "Component + Residual Plots for Model D Randomized")

library(effects)
plot(Effect(c("nof_joins", "avg_nof_rows"),lm.H.rand, partial.residuals=TRUE),
  span=1)

plot(Effect(c("avg_nof_rows", "nof_joins"),lm.H.rand, partial.residuals=TRUE),
  span=7)

plot(Effect(c("avg_nof_rows", "is_hive"),lm.H.rand, partial.residuals=TRUE),
  span=2)



#install.packages('visreg')
library(visreg)
par(mfrow=c(3,2))
visreg(lm.A.rand)

visreg(lm.H.rand, "avg_nof_rows", by = "nof_joins")
visreg(lm.H.rand, "avg_nof_rows", by="nof_joins", overlay=TRUE)
visreg(lm.H.rand, "avg_nof_rows", by="nof_joins", partial=FALSE)

visreg(lm.H.rand, "avg_nof_rows", by="nof_joins", overlay=TRUE, partial=TRUE)
visreg(lm.H.rand, "avg_nof_rows", by="nof_joins", type = "contrast")
visreg2d(lm.H.rand, "avg_nof_rows", "nof_joins", plot.type = "image")
visreg2d(lm.H.rand, "avg_nof_rows", "nof_joins", plot.type = "persp")
visreg2d(lm.H.rand5, "avg_nof_rows", "nof_joins", plot.type = "persp")

par(mfrow=c(1,1))


##############################################################################
###                 IIIa.	Unsual observations: Outliers
###
###
# Outliers are observations that aren’t predicted well by the model. 
# Rule of thumb is that standardized residuals that are larger than 2 or 
# less than –2 are worth attention (Kabacoff 2015, 194.). 
# Function outlierTest(lm.A.rand) in package car reports the Bonferroni adjusted 
#    p-value for the largest absolute standardized residual

library(car)
outlierTest(lm.A.rand)
outlierTest(lm.B.rand)
outlierTest(lm.C.rand)
outlierTest(lm.D.rand)
outlierTest(lm.H.rand)


# examine outliers for model H
out.H <- outlierTest(lm.H.rand)
names(out.H[[1]])

# get the query id's of outliers
qIds <- as.numeric(substr(names(out.H[[1]]), 1, 4))

if (!exists('queries_gen_info') | !exists('queries_text') ) 
     load(file = 'queries.rda')
row.names(queries_text) <- queries_text$queryId

# display the queries text for outliers
outliers.query.texts <- queries_text[qIds,]
outliers.query.texts




##############################################################################
###            IIIb.	Unsual observations: High leverage values
# High-leverage points. 
# Observations with high leverage have an unusual combination of predictor values. 
# Observations with high leverage are identified through the hat statistic. 
# The average hat value is p / n, where p is the number of parameters 
#  estimated in the model (including the intercept) and n is the sample size. 
# An observation with a hat value greater than 2 or 3 times the 
#  average hat value should be examined. 

hat.plot <- function(fit, model_) {
     p <- length(coefficients(fit))
     n <- length(fitted(fit))
     plot(hatvalues(fit), main=paste("Index Plot of Hat Values for Model", 
          model_, sep=' '), ylim=c(0, 4 * p / n) )
     abline(h = 2 * p / n, lty=2, lwd = 1.5, col='green')
     abline(h = 3 * p / n, lty=2, lwd = 2, col='red')

     #abline(h=c(2,3, 4) * p / n, col="red", lty=2, lwd = 2)
     #identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

par(mfrow=c(3,2))
hat.plot(lm.A.rand, 'A')
hat.plot(lm.B.rand, 'B')
hat.plot(lm.C.rand, 'C')
hat.plot(lm.D.rand, 'D')
hat.plot(lm.H.rand, 'H')
par(mfrow=c(1,1))

# Horizontal lines are drawn at 2 and 3 times the average hat value. 
#  The locator function places the graph in interactive mode

# High-leverage observations may or may not be influential observations. 
# That will depend on whether they’re also outliers


##############################################################################
###            IIIc.	Unsual observations: Influential observations
# Influential observations have a disproportionate impact on the values of 
# the model parameters. 
# An observation is said to be influential if removing the observation 
# substantially changes the estimate of the regression coefficients.  
# Influence can be thought of as the product of leverage and outlierness.

# Kabacoff  (2015, 196) presents two methods for identifying influential
# observations: 
#         - Cook’s distance (or D statistic) and 
#          - added variable plots.
# Cook’s D values greater than 4 / (n – k – 1), where n is the sample size 
#   and k is the number of predictor variables, indicate influential 
# observations.

# Cook's D Plot
# identify D values > 4/(n-k-1) 

cook.d.plot <- function(fit, data, model_) {
     cutoff <- 4/((nrow(data)-length(fit$coefficients)-2)) 
     plot(fit, which=4, cook.levels=cutoff,
     main = paste("Model", model_),
          sub=NULL)
     abline(h=cutoff, lty=3, col="red")
}

par(mfrow=c(3,2))
cook.d.plot(lm.A.rand, data.A.rand, 'A Randomized')
cook.d.plot(lm.B.rand, data.B.rand, 'B Randomized')
cook.d.plot(lm.C.rand, data.C.rand, 'C Randomized')
cook.d.plot(lm.D.rand, data.D.rand, 'D Randomized')
cook.d.plot(lm.H.rand, data.H.rand, 'H Randomized')
cook.d.plot(lm.H.rand2, data.H.rand, 'H2 Randomized')
cook.d.plot(lm.H.rand3, data.H.rand, 'H3 Randomized')

par(mfrow=c(1,1))


summary(influence.measures(lm.A.rand))
summary(influence.measures(lm.B.rand))
summary(influence.measures(lm.C.rand))
summary(influence.measures(lm.D.rand))
summary(influence.measures(lm.H.rand))

nrow(summary(influence.measures(lm.A.rand)))
nrow(summary(influence.measures(lm.B.rand)))
nrow(summary(influence.measures(lm.C.rand)))
nrow(summary(influence.measures(lm.D.rand)))
nrow(summary(influence.measures(lm.H.rand)))


# According to Kabacoff [27, p.197] Cook’s D plots can help identify influential
#  observations, but they don’t provide information about how these 
#  observations affect the model. 
# Added-variable plots are built for each predictor Xk. 
# Values on y axis are the residuals from regressing outcome variable on the 
#  other k-1 predictors. 
# On x axis values are residuals of regressing the predictor Xk on the 
#  other k-1 predictors. 
# The straight line in each plot is the actual regression coefficient for that predictor variable. 
# Impact of influential observations can be assessed be imagining how the line 
# would change if the point representing that observation was deleted.
library(car)
#avPlots(lm.A.rand, ask=FALSE, id.method="identify")
avPlots(lm.H.rand, ask=FALSE)

avPlots(lm.H.rand, ask=FALSE, main="Added-Variable Plots for Model H Randomized")


##############################################################################
###                           III.	Influence plots
#  You can combine the information from outlier, leverage, 
#  and influence plots into one highly informative plot 
#    using the influencePlot() function from the car package:
library(car)
#influencePlot(lm.A.rand, id.method="identify", main="Influence Plot", 
#              sub="Circle size is proportial to Cook's Distance" )

par(mfrow=c(1,2))
influencePlot(lm.A.rand, main="Influence Plot for \nModel A Randomized", 
              sub="Circle size is proportial to Cook's Distance" )
influencePlot(lm.H.rand, main="Influence Plot for \nModel H Randomized", 
              sub="Circle size is proportial to Cook's Distance" )
par(mfrow=c(1, 1))



influenceIndexPlot(model = lm.A.rand)

influenceIndexPlot(model = lm.H.rand)

inverseResponsePlot(model = lm.H.rand, id.n = 5)
inverseResponsePlot(model = lm.H.rand)

residualPlots(model = lm.H.rand, id.n = 5)

library(effects)
## Plot effect of each term
allEffects.lm.full <- allEffects(lm.H.rand)
plot(allEffects.lm.full)

allEffects.lm.full <- allEffects(lm.H3)
plot(allEffects.lm.full)


##############################################################################
###                      IV	Multicollinearity
###
# Apart from the three main categories of potential problems discussed above, 
# multicollinearity is important in interpreting multiple regression results. 
# Multicollinearity is detected using a statistic called variance 
#    inflation factor (VIF).
# For any predictor variable, the square root of the VIF indicates 
#   the degree to which the confidence interval for that 
#  variable’s regression parameter is expanded relative to a model 
#  with uncorrelated predictors (hence the name). 
# As a general rule, SQRT(vif) > 2 indicates a multicollinearity problem. 
vif(lm.A.rand) 
sqrt(vif(lm.A.rand)) > 2 # problem?

vif(lm.B.rand) 
sqrt(vif(lm.B.rand)) > 2 # problem?

vif(lm.C.rand) 
sqrt(vif(lm.C.rand)) > 2 # problem?

vif(lm.D.rand) 
sqrt(vif(lm.D.rand)) > 2 # problem?

vif(lm.H.rand) 
sqrt(vif(lm.H.rand)) > 2 # problem?

vif(lm.H.rand2) 
sqrt(vif(lm.H.rand2)) > 2 # problem?

vif(lm.H.rand3) 
sqrt(vif(lm.H.rand3)) > 2 # problem?

vif(lm.H.rand4) 
sqrt(vif(lm.H.rand4)) > 2 # problem?





##############################################################################
###           Corrective Measures, Alternate Methods/Tools                 ###
##############################################################################
###


##############################################################################
###                           Bootstrapping
# Parametric statistical methods, like linear regression, test hypotheses 
#  (such as all coefficients or one specific coefficies are/is equal to zero) 
#  and estimate confidence intervals for population parameters by assuming that 
#  the analyzed data is sampled from a normal distribution or some other 
#  well-known distributions. 
# Statistical approaches based on randomization and resampling are 
# suitable for many cases considered indesirable by classical 
# (parametric statistics) [Kabacoff, p.279]:
#    •	data is sampled from unknown distribution
#    •	data is sampled from mixed distribution
#    •	sample size is small
#    •	outliers are a problem
#    •	tests based on theoretical distributions are too complex.
#
# Bootstrapping is such a technique, generating an empirical distribution of
# a test statistic or a set of test statistics by repeated random sampling 
# with replacements from the original sample [Kabacoff, p.291]. 
# We’ll use bootstrapping to confirm or infirm regression models R-squared,
# and also confidence intervals for unstandardized regression coeffcients. 

## bootstrapping a single statistic (R-squared)
#citation('boot')
library(boot)
rsq <- function(formula, data, indices) {
         d <- data[indices,]
         fit <- lm(formula, data=d)
         return(summary(fit)$r.square)
} 
rsq.boot.model.H <- boot(data=data.H.rand, statistic=rsq, 
                R=1000, formula=duration ~ nof_attributes_select  + is_hive +
           sqrt(nof_ors) + nof_joins:avg_nof_rows + 
          is_hive:avg_nof_rows)
print(rsq.boot.model.H)
plot(rsq.boot.model.H)
boot.ci(rsq.boot.model.H, type="perc")
#boot.ci(rsq.boot.model.H, type="bca")
summary(lm.H.rand)

## bootstrapping several statistics (regression coefficients)
library(boot)
bs <- function(formula, data, indices) {                
        d <- data[indices,]                                    
        fit <- lm(formula, data=d)                                 
        return(coef(fit))                                    
}
coefs.boot.h.rand <- boot(data=data.H.rand, statistic=bs,             
                R=1000, formula=duration ~ nof_attributes_select  + is_hive +
           sqrt(nof_ors) + is_hive:avg_nof_rows +
          nof_joins:avg_nof_rows) 
print(coefs.boot.h.rand)
plot(coefs.boot.h.rand, index=2)
boot.ci(coefs.boot.h, type="perc", index=2)

summary(lm.H.rand)
confint(lm.H.rand)
boot.ci(coefs.boot.h.rand, type="perc", index= 2)
boot.ci(coefs.boot.h.rand, type="perc", index= 3)
boot.ci(coefs.boot.h.rand, type="perc", index= 4)
boot.ci(coefs.boot.h.rand, type="perc", index= 5)


# compare coefficients of the model with the bootstrapping results
names(summary(lm.H)$coefficients[,1])[2]
summary(lm.H)$coefficients [2,]
confint(lm.H)[2,]
boot.ci(coefs.boot.h, type="perc", index= 2)

print("----------------")
names(summary(lm.H)$coefficients[,1])[3]
summary(lm.H)$coefficients [3,]
confint(lm.H)[3,]
boot.ci(coefs.boot.h, type="perc", index= 3)

print("----------------")
names(summary(lm.H)$coefficients[,1])[4]
summary(lm.H)$coefficients [4,]
confint(lm.H)[4,]
boot.ci(coefs.boot.h, type="perc", index= 4)

print("----------------")
names(summary(lm.H)$coefficients[,1])[5]
summary(lm.H)$coefficients [5,]
confint(lm.H)[5,]
boot.ci(coefs.boot.h, type="perc", index= 5)

print("----------------")
names(summary(lm.H)$coefficients[,1])[6]
summary(lm.H)$coefficients [6,]
confint(lm.H)[6,]
boot.ci(coefs.boot.h, type="perc", index= 6)




##############################################################################
###                         Weighted Least Squares
###
# When linear regression models incorporate no error dependence, but error
#  heteroscedasticity, Faraway (2015, p.113) suggest using weight least squares 

###
### lm.H.rand
lm.H.rand <- lm(duration ~ nof_attributes_select  + is_hive +
           sqrt(nof_ors) + is_hive:avg_nof_rows +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand)
confint(lm.H.rand)
plot(lm.H.rand)

library(nlme)
lm.H.rand.wls1 <- gls(duration ~ nof_attributes_select  + is_hive +
           sqrt(nof_ors) + is_hive:avg_nof_rows + nof_joins:avg_nof_rows,
     weights = varIdent(form = ~ 1|is_hive), 
     data=data.H.rand)
print(lm.H.rand.wls1)
summary(lm.H.rand.wls1)
(intervals(lm.H.rand.wls1, which="var-cov"))

# variance function
ms.lm.H.rand.wls1 <- lm.H.rand.wls1$modelStruct
vs.lm.H.rand.wls1 <- ms.lm.H.rand.wls1$varStruct
summary(vs.lm.H.rand.wls)
coef(vs.lm.H.rand.wls1)
summary(lm.H.rand.wls1)$sigma

lm.H.rand.wls2 <- gls(duration ~ nof_attributes_select  + is_hive +
           sqrt(nof_ors) + is_hive:avg_nof_rows + nof_joins:avg_nof_rows,
     weights = varIdent(form = ~ is_hive | avg_nof_rows), 
     data=data.H.rand)
print(lm.H.rand.wls2)
summary(lm.H.rand.wls2)

# variance function
ms.lm.H.rand.wls2 <- lm.H.rand.wls2$modelStruct
vs.lm.H.rand.wls2 <- ms.lm.H.rand.wls2$varStruct
summary(vs.lm.H.rand.wls2)


AIC(lm.H.rand.wls1, lm.H.rand)
AIC(lm.H.rand.wls2, lm.H.rand.wls1)


###
### lm.H.rand2

lm.H.rand2 <- lm(duration ~ nof_attributes_select  + dbserver +
           sqrt(nof_ors) + dbserver:avg_nof_rows +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand2)
confint(lm.H.rand2)
plot(lm.H.rand2)

summary(lm.H.rand)
confint(lm.H.rand)

library(nlme)
lm.H.rand2.wls1 <- gls(duration ~ nof_attributes_select  + dbserver +
           sqrt(nof_ors) + dbserver:avg_nof_rows + nof_joins:avg_nof_rows,
     weights = varIdent(form = ~ 1|dbserver), 
     data=data.H.rand)
print(lm.H.rand2.wls1)
summary(lm.H.rand2.wls1)
(intervals(lm.H.rand2.wls1, which="var-cov"))

# variance function
ms.lm.H.rand2.wls1 <- lm.H.rand2.wls1$modelStruct
vs.lm.H.rand2.wls1 <- ms.lm.H.rand2.wls1$varStruct
summary(vs.lm.H.rand2.wls1)
coef(vs.lm.H.rand2.wls1)
summary(lm.H.rand2.wls1)$sigma


lm.H.rand2.wls2 <- gls(duration ~ nof_attributes_select  + dbserver +
           sqrt(nof_ors) + dbserver:avg_nof_rows + nof_joins:avg_nof_rows,
     weights = varIdent(form = ~ dbserver | avg_nof_rows), 
     data=data.H.rand)
print(lm.H.rand2.wls2)
summary(lm.H.rand2.wls2)

# variance function
ms.lm.H.rand2.wls2 <- lm.H.rand2.wls2$modelStruct
vs.lm.H.rand2.wls2 <- ms.lm.H.rand2.wls2$varStruct
summary(vs.lm.H.rand2.wls2)


AIC(lm.H.rand.wls2, lm.H.rand.wls)
AIC(lm.H.rand.wls2, lm.H.rand.wls)

anova(lm.H.rand.wls2, lm.H.rand.wls)

summary(lm.H.rand)
summary(lm.H.rand.wls)
summary(lm.H.rand.wls2)


###
### lm.H.rand3

lm.H.rand3 <- lm(duration ~ nof_attributes_select + dbserver + 
          sqrt(nof_ors) + dbserver:avg_nof_rows + dbserver:nof_joins + 
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand3)
confint(lm.H.rand3)
plot(lm.H.rand3)

summary(lm.H.rand)
summary(lm.H.rand2)
summary(lm.H.rand3)

confint(lm.H.rand)
confint(lm.H.rand2)
confint(lm.H.rand3)

library(nlme)
lm.H.rand3.wls1 <- gls(duration ~ nof_attributes_select  + dbserver +
           sqrt(nof_ors) + dbserver:avg_nof_rows + dbserver:nof_joins +
          nof_joins:avg_nof_rows, data=data.H.rand,
     weights = varIdent(form = ~ 1|dbserver))
print(lm.H.rand3.wls1)
summary(lm.H.rand3.wls1)
(intervals(lm.H.rand3.wls1, which="var-cov"))

# variance function
ms.lm.H.rand3.wls1 <- lm.H.rand3.wls1$modelStruct
vs.lm.H.rand3.wls1 <- ms.lm.H.rand3.wls1$varStruct
summary(vs.lm.H.rand3.wls1)
coef(vs.lm.H.rand3.wls1)
summary(lm.H.rand3.wls1)$sigma


lm.H.rand3.wls2 <- gls(duration ~ nof_attributes_select  + dbserver +
           sqrt(nof_ors) + dbserver:avg_nof_rows + dbserver:nof_joins + 
          nof_joins:avg_nof_rows,data=data.H.rand, 
     weights = varIdent(form = ~ dbserver | avg_nof_rows))
print(lm.H.rand3.wls2)
summary(lm.H.rand3.wls2)

# variance function
ms.lm.H.rand3.wls2 <- lm.H.rand3.wls2$modelStruct
vs.lm.H.rand3.wls2 <- ms.lm.H.rand3.wls2$varStruct
summary(vs.lm.H.rand3.wls2)

AIC(lm.H.rand.wls1, lm.H.rand)
AIC(lm.H.rand.wls2, lm.H.rand.wls1)

AIC(lm.H.rand2.wls1, lm.H.rand2)
AIC(lm.H.rand2.wls1, lm.H.rand.wls1)
AIC(lm.H.rand2.wls2, lm.H.rand2.wls1)

summary(lm.H.rand)
summary(lm.H.rand.wls1)
summary(lm.H.rand.wls2)

summary(lm.H.rand2)
summary(lm.H.rand2.wls1)
summary(lm.H.rand2.wls2)

summary(lm.H.rand3)
summary(lm.H.rand3.wls1)
summary(lm.H.rand3.wls2)




???????????
# no variables appearing in the interactions
lm.H.rand4 <- lm(duration ~ nof_attributes_select + 
           sqrt(nof_ors) + dbserver:avg_nof_rows + dbserver:nof_joins +
          nof_joins:avg_nof_rows ,data=data.H.rand )
summary(lm.H.rand4)
confint(lm.H.rand4)


##############################################################################
###                         Robust Regression
###
# When models present large numbers of outliers and high leverage points, 
#   robust regression is recommended. 
# Robust regression is done by iterated re-weighted least squares (IRLS)

#  Faraway [25] presents two types of robust regression:
#    •	M-Estimation, related to weighted least squares
#    •	Least Trimmed Square.


##  M-Estimation has two variants, 
#    * Huber regression
#    * LAD (Least Absolute Deviation) regression


# Huber method 
#citation('MASS')
library(MASS)
rlm.H.huber <- rlm(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + nof_joins:avg_nof_rows
     , data=data.H)
summary(lm.H)
summary(rlm.H.huber)



# LAD (Least Absolute Deviation) regression
# package 'quantreg'  - suggestion taken from 
# http://www.alastairsanderson.com/R/tutorials/robust-regression-in-R/
# install.packages('quantreg')
# also suggested by Faraway (2015, 126)

# This models the median of y as a function of x, rather than modelling 
# the mean of y as a function of x, in the case of least squares regression.
#citation('quantreg')
library(quantreg)

rlm.H.lad <- rq(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + nof_joins:avg_nof_rows
     , data=data.H)
summary(rlm.H.lad)
summary(lm.H)


# Bisquare (also a variant of  M-Estimation)
rlm.H.bisquare <- rlm(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + 
          nof_joins:avg_nof_rows, data=data.H, psi = psi.bisquare)
summary(rlm.H.bisquare)


???????????
##        Least Trimmed Squares 
# Faraway 2015, p.126
library(MASS)
rlm.H.lts <- ltsreg(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + 
          nof_joins:avg_nof_rows, data=data.H)

rlm.H.lts <- ltsreg(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive*avg_nof_rows + 
          nof_joins*avg_nof_rows, data=data.H)

coef(rlm.H.lts)
summary(lm.H)

rlm.H.lms <- lmsreg(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + 
          nof_joins:avg_nof_rows, data=data.H)
coef(rlm.H.lms)
summary(lm.H)

????

#install.packages('rrcov')
library(rrcov)
rlm.H.lts <- ltsreg(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + 
          nof_joins:avg_nof_rows, data=data.H)
summary(rlm.H.lts)
coefficients(rlm.H.lts)

rlm.H.lts$coefficients





##############################################################################
###                 Non-parametric, rank-based Regression
### Kloke, McKean 2015, pp.83-

#install.packages('Rfit')
library(Rfit)

rlm.H.np1 <- rfit(duration ~ nof_attributes_select  + is_hive + 
          sqrt(nof_ors) + is_hive:avg_nof_rows + nof_joins:avg_nof_rows
     , data=data.H)
summary(rlm.H.np1)
summary(lm.H)

rlm.H.np2 <- rfit(duration ~ nof_attributes_select  + dbserver + 
          sqrt(nof_ors) + dbserver:avg_nof_rows + nof_joins:avg_nof_rows
     , data=data.H.rand)
summary(rlm.H.np2)
summary(lm.H)



###  Not finished!!!
##############################################################################
###                                Transformations
###
#  When models don’t meet the normality, linearity, or homoscedasticity 
#   assumptions, transforming one or more variables can often improve 
#   or correct the situation. 
# Transformations typically involve replacing a variable Y with Y ^ λ.


### Transformation of the outcome

# Box-Cox Transformation to Normality
library(MASS)
boxcox(lm.H, plotit = T)
boxcox(lm.H, plotit = T, lambda = seq(0, 1.4, by=0.005))

#???
logtrans(lm.H, plotit=T, alpha = seq(0, 2, by=0.01))

## ???
summary(powerTransform(data.H$duration))


# lambda is not consistent among methods !
#   that is supposedly because of outliers


### Transforming the Predictors


# Additive models
library(mgcv)
lm.H.add <- gam(duration ~ s(nof_attributes_select)  + 
           s(sqrt(nof_ors)) + s(is_hive:avg_nof_rows) +
          s(nof_joins:avg_nof_rows)
     ,data=data.H )

#########
James et al. 282
library(splines)
#install.packages('gam')
library(gam)

ns(data.H$nof_attributes_select, 5)

lm.I.gam <- lm (duration ~ s(nof_attributes_select, 10)  + 
           s(nof_ors,10) + is_hive + s(avg_nof_rows, 10) + nof_joins
     ,data=data.H)
summary(lm.I.gam)

plot.gam(lm.I.gam, se=TRUE, col='blue')


lm.J.gam <- lm (duration ~ s(nof_attributes_select, 5)  + 
           s(sqrt(nof_ors),5) + is_hive + s(is_hive:avg_nof_rows, 5) + 
          s(is_hive:avg_nof_rows, 5) + 
     ,data=data.H)
summary(lm.J.gam)

plot.gam(lm.J.gam, se=TRUE, col='blue')


data.K <- data.H
data.K$is_hive__avg_nof_rows <- data.K$is_hive * data.K$avg_nof_rows
data.K$nof_joins__avg_nof_rows <- data.K$nof_joins * data.K$avg_nof_rows


lm.K.gam <- lm (duration ~ s(nof_attributes_select, 5)  + 
           s(sqrt(nof_ors),5) + s(is_hive__avg_nof_rows, 10) +
          s(nof_joins__avg_nof_rows, 10)
     ,data=data.K)
summary(lm.K.gam)

plot.gam(lm.K.gam, se=TRUE, col='blue')


lm.L.add <- gam(duration ~ s(nof_attributes_select,5)  + 
           s(sqrt(nof_ors),5) + s(is_hive*avg_nof_rows, 5) +
          s(nof_joins*avg_nof_rows,5)
     ,data=data.H )
summary(lm.L.add)

plot.gam(lm.L.add, se=TRUE, col='blue')


lm.M.add <- gam(duration ~ s(nof_attributes_select,5)  + 
           s(nof_ors,5) + is_hive + s(avg_nof_rows, 5) + nof_joins
     ,data=data.H )
summary(lm.M.add)

plot.gam(lm.L.add, se=TRUE, col='blue')



# Box-Tidwell Transformations to Linearity
boxTidwell(duration ~ nof_attributes_select + nof_joins + avg_nof_rows + 
          is_hive + length_char_attributes_where +
          length_varchar_attributes_where + nof_ors + nof_in_s + 
          nof_all_in_values, data=data.D)

# The spreadLevelPlot() function creates a scatter plot of the absolute 
#   standardized residuals versus the fitted values and superimposes 
# a line of best fit. 
spreadLevelPlot(lm.D)




##############################################################################
####           Transformed  fourth regression model (model D.transf)
####   sqrt(duration) ~ nof_attributes_select +  nof_joins +
####      avg_nof_rows + is_hive + length_char_attributes_where +
####      length_varchar_attributes_where + nof_ors + nof_in_s + 
####      nof_all_in_values
##############################################################################
names(results_details)

### data
data.D.transf <- results_details[, c('duration', 'nof_attributes_select', 
      'nof_joins', 'avg_nof_rows', 'is_hive',
     'length_char_attributes_where', 'length_varchar_attributes_where', 
     'nof_ors', 'nof_in_s', 'nof_all_in_values' )]
rownames(data.D.transf)


data.D.transf$duration.transf <- sqrt(data.D.transf$duration)


data.D.transf$duration <- NULL

### correlation plot (nonparametric)
library(corrplot)
corrplot.mixed(corr=cor(data.D.transf, method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')

#install.packages('gpairs')
#library(gpairs)
#gpairs(data.D)

### Scatterplot Matrix
library(car)
scatterplotMatrix(data.D.transf, spread=FALSE, smoother.args=list(lty=2),
     main="Scatter Plot Matrix for Variables of the Transformed Regression Model D")

### Linear model D transformed
lm.D.transf <- lm(duration.transf ~ nof_attributes_select + nof_joins + avg_nof_rows + 
          is_hive + length_char_attributes_where +
          length_varchar_attributes_where + nof_ors + nof_in_s + 
          nof_all_in_values, data=data.D.transf)
summary(lm.D.transf)
confint(lm.D.transf)

summary(lm.A)
summary(lm.B)
summary(lm.C)
summary(lm.D)

###   we can verify the model assumptions with 'gvlma' package
library(gvlma)
gvlma(lm.D.transf)

### Assessing normality
library(car)
qqPlot(lm.D.transf, labels=row.names(data.D), id.method="identify",
     simulate=TRUE, main="Q-Q Plot for Linear Model D transformed")

# residplot function
residplot <- function(fit, nbreaks=10) {
               x <- rstudent(fit)
               h<-hist(x, breaks=nbreaks, freq=FALSE, 
                    xlab="Studentized Residual", 
                    main="Distribution of Errors") 
               xfit<-seq(min(x),max(x),length=40) 
               lines(xfit, dnorm(xfit),col="blue", lwd=2)
               lines(density(x)$x, density(x)$y, col="red", lwd=2, lty=2)
               legend("topright", legend = c( "Normal Curve", "Density Curve"), 
               lty=1:2, col=c("blue","red"), cex=.7)
 }
residplot(lm.D.transf)


###  Independence of Errors
#  Durbin–Watson test detects such serially correlated errors. 
# H0: the outcome variables values are not autocorrelated (that is, errors are
#    are indenpedent)
durbinWatsonTest(lm.D.transf)
# The significant p-value (p=0) suggests autocorrelation and, 
# conversely, violtation of assumptions of independence of errors. 


###  Linearity
library(car)
# Component + Residual Plots
crPlots(lm.D.transf)

# Nonlinearity in any of these plots suggests that you may not have 
#   adequately modeled the functional form of that predictor
#  in the regression. 
# If so, you may need to add curvilinear components such as polynomial
#  terms, transform one or more variables (for example, use log(X) instead of X),
#   or abandon linear regression in favor of some other regression variant.


###  Assessing homoscedasticity

# The ncvTest() function produces a score test of the hypothesis of constant
# error variance against the alternative that the error variance changes 
# with the level of the fitted values. 
#  A significant result suggests heteroscedasticity (nonconstant error variance).
ncvTest(lm.D.transf)

# The spreadLevelPlot() function creates a scatter plot of the absolute 
#   standardized residuals versus the fitted values and superimposes 
# a line of best fit. 
spreadLevelPlot(lm.D.transf)

# The points form a random horizontal band around a horizontal line of best fit. 
# If you’d violated the assumption, you’d expect to see a nonhorizontal line.


### Evaluating multi-collinearity

# Multicollinearity can be detected using a statistic called 
#  the variance inflation factor (VIF). 
# For any predictor variable, the square root of the VIF indicates 
#   the degree to which the confidence interval for that 
#  variable’s regression parameter is expanded relative to a model 
#  with uncorrelated predictors (hence the name). 
# As a general rule, SQRT(vif) indicates a multicollinearity problem. 
vif(lm.D.transf) 
sqrt(vif(lm.D.transf)) > 2 # problem?


###       Assessing outliers

# Outliers are observations that aren’t predicted well by the model

# A rough rule of thumb is that standardized residuals that are 
#   larger than 2 or less than –2 are worth attention
outlierTest(lm.D.transf)

# If it isn’t significant (see p-value), there are no outliers 
#  in the dataset. If it’s significant, you must delete it and
#   rerun the test to see if others are present.


### High-leverage points

# Observations that have high leverage are outliers with regard to 
#  the other predictors. In other words, they have an unusual combination 
#   of predictor values. 
#  The response  value isn’t involved in determining leverage.

# Observations with high leverage are identified through the hat statistic. 
# For a given dataset, the average hat value is p/n, where p is the 
# number of parameters estimated in the model (including the intercept) 
#  and n is the sample size. 
# Roughly speaking, an observation with a hat value greater 
#   than 2 or 3 times the average hat value should be examined. 

hat.plot <- function(fit) {
     p <- length(coefficients(fit))
     n <- length(fitted(fit))
     plot(hatvalues(fit), main="Index Plot of Hat Values")
     abline(h=c(2,3)*p/n, col="red", lty=2)
     identify(1:n, hatvalues(fit), names(hatvalues(fit)))
     }
hat.plot(lm.D.transf)

# Horizontal lines are drawn at 2 and 3 times the average hat value. 
#  The locator function places the graph in interactive mode

# High-leverage observations may or may not be influential observations. 
# That will depend on whether they’re also outliers


### Influential observations
# Influential observations have a disproportionate impact on the values 
# of the model parameters

# There are two methods for identifying influential observations: 
#  - Cook’s distance (or D statistic) and 
#  - added variable plots. 
#  Roughly speaking, Cook’s D values greater than 4/(n – k – 1), 
#    where n is the sample size and k is the number of predictor variables,
#    indicate influential observations

# Cook's D Plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(lm.D.transf)-length(lm.D.transf$coefficients)-2)
plot(lm.D.transf, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
# Kabacoff tends to find a cutoff of 1 more generally useful than
#   4/(n – k – 1)


# Cook’s D plots can help identify influential observations, but they 
#  don’t provide information about how these observations affect the model. 
# Added-variable plots can help in this regard. For one response variable 
#  and k predictor variables, you’d create k added-variable plots as follows.

# Added variable plots
library(car)
avPlots(lm.D.transf, ask=FALSE, id.method="identify")
# The straight line in each plot is the actual regression coefficient 
# for that predictor variable. 
#  You can see the impact of influential observations by imagining 
# how the line would change if the point representing that observation was deleted


#  You can combine the information from outlier, leverage, 
#  and influence plots into one highly informative plot 
#    using the influencePlot() function from the car package:
library(car)
influencePlot(lm.D.transf, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )







########################
###  Corrective measures

#  When models don’t meet the normality, linearity, or homoscedasticity 
#   assumptions, transforming one or more variables can often improve 
#   or correct the situation. 
# Transformations typically involve replacing a variable Y with Y ^ λ.


# Box-Cox Transformation to Normality
summary(powerTransform(data.D.transf$duration.transf))


# Box-Tidwell Transformations to Linearity
boxTidwell(duration ~ nof_attributes_select + nof_joins + avg_nof_rows + 
          is_hive + length_char_attributes_where +
          length_varchar_attributes_where + nof_ors + nof_in_s + 
          nof_all_in_values, data=data.D)

# The spreadLevelPlot() function creates a scatter plot of the absolute 
#   standardized residuals versus the fitted values and superimposes 
# a line of best fit. 
spreadLevelPlot(lm.D.transf)







##############################################################################
###                                ANOVA models                           ###
##############################################################################

details$scale_ <- factor(details$scale)

aov.1 <- aov(duration ~ scale_, data = details)
anova(aov.1)

# install.packages('multcomp')
library(multcomp)
glht(aov.1)

aov.1 <- aov(duration ~ -1 + scale_, data = details)
anova(aov.1)
glht(aov.1)

# plot confidence intervals
plot(glht(aov.1), las=0)


### Bayesian ANOVA (Chapman & McDonnell Feit, 2015, pp.150-156)
# install.packages('BayesFactor')
library(BayesFactor)

# compare two models
aov.bayes.1 <- lmBF(duration ~ scale_, data = details)
aov.bayes.2 <- lmBF(duration ~ scale_ + nof_joins + nof_attributes_where, data = details)
# ratio of the models
aov.bayes.1 / aov.bayes.2

aov.bayes.1.chain <- posterior(aov.bayes.1, 1, iterations = 1000)
aov.bayes.1.chain

plot(aov.bayes.1.chain)

aov.bayes.2.chain <- posterior(aov.bayes.2, 1, iterations = 1000)
aov.bayes.2.chain
plot(aov.bayes.2.chain)


summary(aov.bayes.1.chain)
summary(aov.bayes.2.chain)


head(aov.bayes.1.chain)

head(aov.bayes.2.chain)


##############################################################################
###                           Regression models                           ###
##############################################################################
names(results_details)


# Comparing nested models using the anova function
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit2, fit1)

# Comparing models with the Akaike Information Criterion
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
AIC(fit1,fit2)

# Backward stepwise selection
library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
stepAIC(fit, direction="backward")

# All subsets regression
#install.packages('leaps')
library(leaps)
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                   Frost, data=states, nbest=4)
plot(leaps, scale="adjr2", 
     main="Best model subsets based on Adjusted R2")

library(car)
subsets(leaps, statistic="cp", legend=TRUE, # legend is placed interactively
         main="Cp Plot for All Subsets Regression") 
abline(1,1,lty=2,col="red")

# Function for k-fold cross-validated R-square
shrinkage <- function(fit,k=10){
require(bootstrap)
# define functions 
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 

# matrix of predictors
x <- fit$model[,2:ncol(fit$model)]
# vector of predicted values
y <- fit$model[,1]

results <- crossval(x,y,theta.fit,theta.predict,ngroup=k)
r2 <- cor(y, fit$fitted.values)**2 # raw R2 
r2cv <- cor(y,results$cv.fit)**2 # cross-validated R2
cat("Original R-square =", r2, "\n")
cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
cat("Change =", r2-r2cv, "\n")
}

# using it
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)
fit2 <- lm(Murder~Population+Illiteracy,data=states)
shrinkage(fit2)

#  Calculating standardized regression coefficients
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfit)

########################################################################
# The relweights function determines the relative importance of each   # 
# independent variable to the dependent variable in an OLS regression. # 
# The code is adapted from an SPSS program generously provided by      # 
# Dr. Johnson.                                                         #
#                                                                      #
# See Johnson (2000, Multivariate Behavioral Research, 35, 1-19) for   #
# an explanation of how the relative weights are derived.              #                                           
######################################################################## 
relweights <- function(fit,...){                              
  R <- cor(fit$model)      # correlation matrix with criterion in 1st column
  nvar <- ncol(R)          # number of variables
  rxx <- R[2:nvar, 2:nvar] # correlations among predictors
  rxy <- R[2:nvar, 1]      # correlations between predictors and criterion
  svd <- eigen(rxx)        # singular value decomposition
  evec <- svd$vectors      # eigenvectors                     
  ev <- svd$values         # eigenvalues
  delta <- diag(sqrt(ev))  # diag matrix with sqrts of eigenvalues
  
  # correlations between original predictors and new orthogonal variables
  lambda <- evec %*% delta %*% t(evec)        
  lambdasq <- lambda ^ 2   # square of the correlations
  
  # regression coefficients of Y on orthogonal variables
  beta <- solve(lambda) %*% rxy           
  rsquare <- colSums(beta ^ 2)       # calculate R-square            
  rawwgt <- lambdasq %*% beta ^ 2    # raw relative weights
  import <- (rawwgt / rsquare) * 100 # rescale to % of R-square
  lbls <- names(fit$model[2:nvar])   # predictor labels
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  
  # plot results
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables", 
          sub=paste("R-Square = ", round(rsquare, digits=3)),
          ...)  
return(import)
}

# using it
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="lightgrey")


################






# install.packages('coefplot')
library(coefplot)
coefplot(lm.1, intercept=FALSE, outerCI=1.96, lwdOuter=1.5)



#####
# standardizing the predictors
data.1.std <- data.1
data.1.std[, 2:3] <- scale(data.1.std[, 2:3])

lm.1.std <- lm(duration ~ nof_attributes_where + nof_joins, 
     data=data.1.std)
summary(lm.1.std)
confint(lm.1.std)

summary(lm.1)
confint(lm.1)


# check model fit
par(mfrow=c(2,2))
plot(lm.1.std)
par(mfrow=c(1,1))

#library(coefplot)
coefplot(lm.1.std, intercept=FALSE, outerCI=1.96, lwdOuter=1.5)

library(robustbase)
robbase.lm.1.std <- lmRob(duration ~ nof_attributes_where + nof_joins, 
     data=data.1.std)
summary(robbase.lm.1.std)

# compare with unscaled
summary(robbase.lm.1)


##############################################################################
####                     Second regression model
####       duration ~ nof_attributes_where + nof_joins + avg_nof_rows

data.2 <- details[, c('duration',  'nof_attributes_where', 'nof_joins', 
     'avg_nof_rows')]
#install.packages('gpairs')
library(gpairs)
gpairs(data.2)

# correlation plot (nonparametric)
library(corrplot)
corrplot.mixed(corr=cor(data.2, method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')

# the model
lm.2 <- lm(duration ~ nof_attributes_where + nof_joins + avg_nof_rows , 
     data=data.2)
summary(lm.2)
confint(lm.2)

# check model fit
par(mfrow=c(2,2))
plot(lm.2)
par(mfrow=c(1,1))

library(coefplot)
coefplot(lm.2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5)

# verify the model assumptions with 'gvlma' package
library(gvlma)
gvlma(lm.2)



###
# standardizing the predictors
data.2.std <- data.2
data.2.std[, 2:4] <- scale(data.2.std[, 2:4])

lm.2.std <- lm(duration ~ nof_attributes_where + nof_joins + avg_nof_rows , 
     data=data.2.std)
summary(lm.2.std)
confint(lm.2.std)

# check model fit
par(mfrow=c(2,2))
plot(lm.2.std)
par(mfrow=c(1,1))

library(robustbase)
robbase.lm.2.std <- lmRob(duration ~ nof_attributes_where + nof_joins + avg_nof_rows, 
     data=data.2.std)
summary(robbase.lm.2.std)

# compare with unscaled
summary(robbase.lm.2)



# compare linear model 1 with linear model 2
anova(lm.1, lm.2)

anova(robbase.lm.1, robbase.lm.2)





##############################################################################
###                           query duration                               ###

ggplot(data=details) +
	geom_histogram(aes(x=duration), fill="grey50")

# a density kernel for duration
ggplot(details, aes(x = duration)) + geom_density()
# filled curve
ggplot(details, aes(x = duration)) + 
	geom_density( fill= "yellow")

# histogram and density plot superimposed
ggplot(details, aes(x = duration)) + 
	geom_histogram(aes(y = ..density..), colour="black", 
		fill="yellow") + 
	geom_density(alpha=0.3, fill="green") 


# the distribution is not normal, so we try a Box-Cox transformation
library(car)
powerTransform(details$duration)

lambda <- coef(powerTransform(details$duration))
transformed.duration <- bcPower(details$duration, lambda)
hist(transformed.duration)






