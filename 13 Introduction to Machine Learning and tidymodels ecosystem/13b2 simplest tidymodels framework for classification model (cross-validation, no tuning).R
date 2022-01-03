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
###    13.b.2 The simplest (and more recent) way to build  and compare   ###
### classification models with `tidymodels` (cross-validation, no tuning)### 
############################################################################
## last update: 02.01.2022

library(ranger)
library(tidyverse)
library(broom)
library(tidymodels)
options(scipen = 999)

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you downloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


#####################################################################
###                      Heart disease data set                   ###
#####################################################################
## for the description and EDA - see script `09c...``
## for logistic regression models fit - see scripts `11b...` 
## for validation based on simple train-test split - 
##             see scripts `13b1a...` and `13b1b...` 
load('ml_datasets.RData')
rm(states)

glimpse(heart)

# check missing values 
any(is.na(heart))
# there are!!!

table(heart$AHD)

139 / (164 + 139)



##########################################################################
###                             Main split of the data                 ###
set.seed(1234)
splits   <- initial_split(heart, prop = 0.75, strata = AHD)
train_tbl <- training(splits)
test_tbl  <- testing(splits)


## cross-validation folds
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 5, strata = AHD)
cv_train



##########################################################################
###                        The recipe for data preparation             ###
### not all steps (in the following recipe) are really necessary 
### in this case, but in many other situations they are really useful
the_recipe <- recipe(AHD ~ ., data = train_tbl) %>%
    step_knnimpute(all_predictors(), neighbors = 3) %>%   # .missing values impoutation
    step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
    step_zv(all_predictors()) # this removes predictors with zero variance


#########################################################################
###                           Models Specification
lr_spec <- logistic_reg(mode = "classification") %>%
          set_engine("glm") 

rf_spec <- rand_forest() %>%
     set_engine("ranger") %>%
     set_mode("classification")


#########################################################################
###                   Assemble the workflows and fit the models

set.seed(1234)
lr_resamples <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lr_spec) %>%
    fit_resamples(resamples = cv_train, 
                  control = control_resamples(save_pred = TRUE))


# examine the resulted tibble
View(lr_resamples)


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
lr_resamples %>% collect_metrics()

#  get the metrics for each resample
detailed_metrics_lr <- lr_resamples %>% collect_metrics(summarize = FALSE)
View(detailed_metrics_lr)  


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
test__lr <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lr_spec) %>%
    last_fit(splits) 

test__lr %>% collect_metrics() 



set.seed(1234)
test__rf <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_spec) %>%
    last_fit(splits) 

test__rf %>% collect_metrics() 



### ROC Curves
### 
test__lr %>%
    unnest(`.predictions`) %>%
    roc_curve(truth = AHD, estimate = .pred_No) %>%
    autoplot()

test__rf %>%
    unnest(`.predictions`) %>%
    roc_curve(truth = AHD, estimate = .pred_No) %>%
    autoplot()



#########################################################################
###                 Variable importance (for Random Forest)
#########################################################################


### Examine the regression coefficients

set.seed(1234)
lr_fit <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(lr_spec) %>%
    fit(data = train_tbl)
lr_fit

coeffs <- tidy(extract_fit_parsnip(lr_fit))
coeffs

lr_fit %>%
    tidy() %>%
    mutate(term = fct_reorder(term, estimate)) %>%
    ggplot(aes(estimate, term, fill = estimate > 0)) +
    geom_col() +
    theme(legend.position = "none")


coeffs_exp <- tidy(extract_fit_parsnip(lr_fit), exponentiate = TRUE)
coeffs_exp


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

