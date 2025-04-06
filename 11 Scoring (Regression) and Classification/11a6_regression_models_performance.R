##################################################################
#########        Regression Models Performance        ############
######### Train/Test Sets, k-fold Cross-Validation
##################################################################

####  last update: 14.11.2017

########################################################################
###            Dowload and unzip the archive for this script
########################################################################

# all the files needed o run this script are archived are available at:
# https://1drv.ms/u/s!AgPvmBEDzTOSgrkyn71qk7GtS6IgqA
# Please download the archive in a local directory (such as 'RDataSets')
#  and unzip it;
#  Make this local directory the default working directory (see below) 


###############################################################################
###                 Set/change the current working directory 
###############################################################################
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


# some needed packages
# 	"car" for regression diagnostics
# install.packages("car")
library(car)
# 	"boot" for bootstrapping
# install.packages("boot")
library(boot)
#	"QuantPsyc" for standardized regression coefficients
# install.packages("QuantPsyc")
library(QuantPsyc)

library(tidyverse)

options(scipen=10)
library (stringr)

## install.packages('devtools')
#library(devtools)
#devtools::install_github("dgrtwo/broom")
library (broom)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)


###################################################################################
###                      Case study: `States` (USA)
#  Example taken from Kabacoff's R in Action (Manning), 2013, 2015 
# state.x77 dataset in the base package 
# Explore the relationship between a state’s murder rate and other characteristics of
#   the state, including population, illiteracy rate, average income, 
#   and frost levels (mean number of days below freezing).
###################################################################################
# lm() function requires a data frame;state.x77 dataset is contained in a matrix, 
#    so one must convert it:
states <- as.data.frame(state.x77)
states$State <- row.names(states)
names(states) <- str_replace_all(names(states), ' |\\.', '_')
head(states)


################################################################
###             I. Train/Test Sets Split                     ### 
################################################################

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(states$Murder, SplitRatio = 0.66)
training_set = subset(states, split == TRUE)
test_set = subset(states, split == FALSE)

################################################################

states_lm <- lm(Murder ~ Population + Life_Exp + Frost + Area, 
                 data=training_set %>% select (-State))
summary(states_lm)


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Murder ~ Population + Life_Exp + Frost + Area,
               data = training_set %>% select (-State))


# Predicting the Test set results
y_pred_train <- predict(regressor, newdata = training_set)
y_pred_test = predict(regressor, newdata = test_set)

training_set$Murder_Predicted <- y_pred_train
test_set$Murder_Predicted <- y_pred_test


df_comparison <- bind_rows(
     training_set %>%
          transmute(set_name = 'train', outcome_type = 'Real',
                    Outcome = Murder), 
     training_set %>%
          transmute(set_name = 'train', outcome_type = 'Predicted', 
                    Outcome = Murder_Predicted),
     test_set %>%
          transmute(set_name = 'test', outcome_type = 'Real',
                    Outcome = Murder), 
     test_set %>%
          transmute(set_name = 'test', outcome_type = 'Predicted', 
                    Outcome = Murder_Predicted)
     )


#
ggplot (data = df_comparison, aes (Outcome)) +
     geom_density() +
     facet_wrap(~ set_name + outcome_type)


#
ggplot (data = df_comparison, aes (x = Outcome, color = set_name)) +
     geom_density(alpha = 0.2) +
     facet_wrap(~ set_name + outcome_type)




################################################################
###         II. Model Validation  using                      ###    
###             the k-fold cross-validation technique        ###
################################################################

# Cross-validation does not not require explicit
# partion of data into training and testing data sets
states_lm <- lm(Murder ~ Population + Life_Exp + Frost + Area, 
                 data=states %>% select (-State))


################################################################
###       k-fold cross-validation with package `DAAG` 
### function `cv.lm` 
# install.packages('DAAG')
library(DAAG)

formula_ <- formula(Murder ~ Population + Life_Exp + Frost + Area)
### m is the number of folds
cv10_states <- cv.lm(data = states %>% select (-State), 
             form.lm = formula_, m=10)

# object `cv10_states` is a data frame
View(cv10_states)
# `Predicted` is the predicted value of the Murder rate (the output variable) 
#         on the entire data set
# `cvpred` is the average predicted value for each case (using k-fold)

#  test set mse is accessibile through
attributes(cv10_states)$ms


CVlm(data=states %>% select (-State), 
     form.lm=formula(Murder ~ Population + Life_Exp + Frost + Area),
           plotit="Observed")

CVlm(data=states %>% select (-State), 
     form.lm=formula(Murder ~ Population + Life_Exp + Frost + Area),
           plotit="Residual")


cvResults <- CVlm(data=states %>% select (-State), 
     form.lm=formula(Murder ~ Population + Life_Exp + Frost + Area),
           plotit="Residual")

### Test set MSE is...
attr(cvResults, 'ms')  # => 251.2783 mean squared error



################################################################
###    k-fold cross-validation with packages `modelr` (part  ###
###    of the `tidyverse` package) and broom                 ###  
library(modelr)

# see
# https://rpubs.com/dgrtwo/cv-modelr
# https://github.com/tidyverse/modelr

# k-fold cross-validation (k=10)
cv10_states_modelr <- crossv_kfold(states %>% select (-State), 10)
View(cv10_states_modelr)

states_models_cv <- states %>%
  select(-State) %>%
  crossv_kfold(k = 10) %>%
  mutate(model = map(train, ~ lm(Murder ~ Population + Life_Exp + Frost + Area, 
                              data = .)))

predictions_states_models <- states_models_cv %>%
  unnest(map2(model, test, ~ augment(.x, newdata = .y)))

View(predictions_states_models)


#  Calculate the cross validated MSE of our model, 
#  and compare it to simply predicting the mean:
predictions_states_models %>%
  summarize(MSE = mean((Murder - .fitted) ^ 2),
            MSEIntercept = mean((Murder - mean(Murder))^2))



predictions_states_models %>%
  mutate(residual = .fitted - Murder) %>%
  gather(Pred, Val, c(Population:Life_Exp, HS_Grad:Area)) %>%
  ggplot(aes(Val, residual)) +
  geom_point() +
  facet_wrap(~ Pred)



## Model quality metrics
attr(cv10_states_modelr)

# RMSE
rmse(states_lm, states %>% select (-State))

# R^2
rsquare(states_lm, states %>% select (-State))

# R^2
adj.rsquare(states_lm, states %>% select (-State))


#> [1] 2.949163
rsquare(mod, mtcars)
#> [1] 0.7528328
mae(mod, mtcars)
#> [1] 2.340642
qae(mod, mtcars)
#>        5%       25%       50%       75%       95% 
#> 0.1784985 1.0005640 2.0946199 3.2696108 6.1794815


