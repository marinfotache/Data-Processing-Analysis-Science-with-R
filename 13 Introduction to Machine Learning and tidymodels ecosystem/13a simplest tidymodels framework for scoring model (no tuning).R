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
###           13.a. The simplest (and more recent) way to build          ###
###                 and compare scoring models with `tidymodels`         ### 
############################################################################

############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 08.12.2021

#install.packages('ranger')
library(ranger)
library(tidyverse)
library(broom)
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
### 	   Insurance data set (for EDA, see script `09_c_eda_...`)  ###
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

# are there any missing values ?
any(is.na(insurance))

table(insurance$region)


##########################################################################
###                             Main split of the data                 ###
set.seed(1234)
splits   <- initial_split(insurance, prop = 0.75)
train_tbl <- training(splits)
test_tbl  <- testing(splits)


## cross-validation folds
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 5)
cv_train



##########################################################################
###                        The recipe for data preparation             ###
### not all steps (in the following recipe) are really necessary 
### in this case, but in many other situations they are really useful
the_recipe <- recipe(charges ~ ., data = train_tbl) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
    step_knnimpute(all_predictors(), neighbors = 3) %>%   # ... when having missing values
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

set.seed(1234)
lm_resamples <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lm_spec) %>%
    fit_resamples(resamples = cv_train, 
                  control = control_resamples(save_pred = TRUE))


# examine the resulted tibble
View(lm_resamples)


set.seed(1234)
rf_resamples <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_spec) %>%
    fit_resamples(resamples = cv_train, 
                  control = control_resamples(save_pred = TRUE))


# examine the resulted tibble
View(rf_resamples)


#########################################################################
###                        Explore the results 

# performance metrics (mean) across folds for each grid line
lm_resamples %>% collect_metrics()

#  get the metrics for each resample
detailed_metrics_lm <- lm_resamples %>% collect_metrics(summarize = FALSE)
View(detailed_metrics_lm)  


# performance metrics (mean) across folds for each grid line
rf_resamples %>% collect_metrics()

#  get the metrics for each resample
detailed_metrics_rf <- rf_resamples %>% collect_metrics(summarize = FALSE)
View(detailed_metrics_rf)  






#########################################################################
###     The moment of truth: model performance on the test data set

### Function last_fit() fits the finalized workflow one last time 
### to the training data and evaluates one last time on the testing data.

set.seed(1234)
test__lm <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lm_spec) %>%
    last_fit(splits) 

test__lm %>% collect_metrics() 


set.seed(1234)
test__rf <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_spec) %>%
    last_fit(splits) 

test__rf %>% collect_metrics() 




#########################################################################
###                 Variable importance (for Random Forest)
#########################################################################

### Examine the regression coefficients

set.seed(1234)
lm_fit <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lm_spec) %>%
    fit(data = train_tbl)
lm_fit

coeffs <- tidy(extract_fit_parsnip(lm_fit))
coeffs


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

