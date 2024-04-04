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
###     13.a.1 The simplest (and more recent) way to build and compare   ###
###         scoring models with `tidymodels`  (train-test, no tuning)    ### 
############################################################################

############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 2024-03-26

#install.packages('ranger')
library(ranger)
library(tidyverse)
library(tidymodels)
options(scipen = 999)

############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')



#####################################################################
### 	   Insurance data set (for EDA, see script `09_c_eda_...`)   ###
#####################################################################
## Variables
## `age`: age of primary beneficiary
## `sex`: insurance contractor gender, female, male
## `bmi`: Body mass index, providing an understanding of body, 
##   weights that are relatively high or low relative to height, 
##   objective index of body weight (kg / m ^ 2) using the ratio 
##   of height to weight, ideally 18.5 to 24.9
## `children`: Number of children covered by health insurance / Number 
##   of dependents
## `smoker`: Smoking
## `region`: the beneficiary's residential area in the US, northeast, southeast, 
##        southwest, northwest.
## `charges`: Individual medical costs billed by health insurance

insurance <- readr::read_csv('insurance.csv')

lm1 <- lm(charges ~ ., data = insurance)
summary(lm1)

# are there any missing values ?
any(is.na(insurance))

table(insurance$region)



##########################################################################
###                             Main split of the data                 ###
set.seed(1234)
splits   <- initial_split(insurance, prop = 0.75)
train_tbl <- training(splits)
test_tbl  <- testing(splits)



##########################################################################
###                        The recipe for data preparation             ###
### not all steps (in the following recipe) are really necessary 
### in this case, but in many other situations they are really useful
the_recipe <- recipe(charges ~ ., data = train_tbl) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
    step_impute_knn(all_predictors(), neighbors = 3) %>%   # ... when having missing values
    step_zv(all_predictors()) # this removes predictors with zero variance


#########################################################################
###                           Models Specification
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

rf_spec <- rand_forest() %>%
     set_engine("ranger") %>%
     set_mode("regression")


#########################################################################
###                   Assemble the workflows and fit the models

### the liniar regression model
set.seed(1234)
lm_fit <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lm_spec) %>%
    fit(data = train_tbl)

# examine the resulted tibble
lm_fit

lm_fit_summary <- broom::glance(lm_fit)
lm_fit_coefficients <- broom::tidy (lm_fit)
# model predictions on the training set
lm_fit_predictions <- broom::augment (lm_fit, new_data = train_tbl)
# ..or
lm_fit_predictions2 <- bind_cols(train_tbl,
     predict (lm_fit, new_data = train_tbl))

identical(lm_fit_predictions, lm_fit_predictions2)



### the random forest regression model
set.seed(1234)
rf_fit <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_spec) %>%
    fit(data = train_tbl)

# examine the resulted tibble
rf_fit

# no model summary and coefficients for random forests models
# ...

# rf model predictions
rf_fit_predictions <- broom::augment(rf_fit, new_data = train_tbl)



#########################################################################
###                        Explore the results 

broom::augment (lm_fit, new_data = train_tbl) %>%
     ggplot( aes (x = charges, y = `.pred`)) +
     geom_abline(lty = 2) + 
     geom_point(alpha = 0.5) +
     labs(y = "Predicted values of Charges", x = "Charges (observed values)") +
     ggtitle('Predicted vs. Real Values - the linear model')

broom::augment(rf_fit, new_data = train_tbl) %>%
     ggplot( aes (x = charges, y = `.pred`)) +
     geom_abline(lty = 2) + 
     geom_point(alpha = 0.5) +
     labs(y = "Predicted values of Charges", x = "Charges (observed values)") +
     ggtitle('Predicted vs. Real Values - the random forest model')



#########################################################################
###     The moment of truth: model performance on the TEST data set

scoring_metrics <- metric_set(rmse, rsq, mae, ccc)

set.seed(1234)
test__lm <- broom::augment(lm_fit, new_data = test_tbl)
scoring_metrics(test__lm, truth = charges, estimate = .pred)

set.seed(1234)
test__rf <- broom::augment(rf_fit, new_data = test_tbl)
scoring_metrics(test__rf, truth = charges, estimate = .pred)



#########################################################################
###                        Variable importance 
#########################################################################

### Examine the regression coefficients
lm_fit
lm_fit_coefficients <- broom::tidy (lm_fit)
lm_fit_coefficients
# ...or...
lm_fit_coefficients2 <- broom::tidy(extract_fit_parsnip(lm_fit))
identical(lm_fit_coefficients, lm_fit_coefficients2)


### Variable importance  for the Random Forest modsl
library(vip)

set.seed(1234)
rf_imp_spec <- rf_spec %>%
    set_engine("ranger", importance = "permutation")

workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_imp_spec) %>%
    fit(train_tbl) %>%
    extract_fit_parsnip() %>%
    vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))

