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
###       15.b. Building and Tuning Tree-Based Classification Models     ###
###                           with `tidymodels`                          ###  
############################################################################
## last update: 2025-01-13

options(java.parameters = "-Xmx12g")
options(scipen = 999)
library(tidyverse)
library(tidymodels)
#install.packages('themis')  # for class imbalancies
library(themis)
library(ranger)
library(xgboost)

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
###                      Heart disease data set                   ###
#####################################################################
## for the description and EDA - see script `09c...``
load('ml_datasets.RData')
rm(states)

glimpse(heart)

# check missing values 
any(is.na(heart))
# there are!!!



##########################################################################
###                             Main split of the data                 ###
set.seed(1234)
splits   <- initial_split(heart, prop = 0.75, strata = ahd)
train_tbl <- training(splits)
test_tbl  <- testing(splits)


## cross-validation folds
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1, strata = ahd)
cv_train



##########################################################################
###                        The recipe for data preparation             ###
the_recipe <- recipe(ahd ~ ., data = train_tbl) |>
    step_impute_knn(all_predictors(), neighbors = 3) |>   # missing values imputation
    step_dummy(all_nominal(), -all_outcomes()) |> # dummification of the predictors
    step_zv(all_predictors()) # this removes predictors with zero variance




#########################################################################
###                           Models Specification

## RF
rf_spec <- parsnip::rand_forest(
     mtry = tune(), 
     trees = 500,
     min_n = tune()     
          ) |>
     set_engine("ranger", importance = "impurity") |>
     set_mode("classification")
rf_spec


### XGBoost
xgb_spec <- parsnip::boost_tree(
    trees = 700, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                         ## step size
    ) |> 
    set_engine("xgboost") |> 
    set_mode("classification")
xgb_spec


#########################################################################
###                   Grid specification (for model tuning)
#########################################################################

##############################
## for Random Forest models
set.seed(1234)
rf_grid <- dials::grid_random(
    finalize(mtry(), train_tbl %>% select (-ahd)),
    min_n(),  
    size = 20)  # the number should be larger, but nodel fitting would take longer

rf_grid


##############################
##  for XGBoost models
set.seed(1234)
xgb_grid <- dials::grid_random(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_tbl %>% select (-ahd)),
    learn_rate(),
    size = 60   # the number should be larger, but it would take longer
)
xgb_grid



#########################################################################
###                 Assembling the models with workflows
#########################################################################

## Two separate series of Workflows for Random Forest Models and XGB Boost Models

# Random forest
wf_rf <- workflow() |>
    add_recipe(the_recipe) |>
    add_model(rf_spec) 


# XGBoost
wf_xgb <- workflow() |>
    add_recipe(the_recipe) |>
    add_model(xgb_spec) 



#########################################################################
###        Fit the models for all cross-validation folds 
#########################################################################

require(doParallel)
cores <- parallel::detectCores(logical = FALSE)
doParallel::registerDoParallel(cores = cores)


# Random Forest model, `original` data set
set.seed(1234)
rf_resamples <- wf_rf |> 
    tune_grid(
        resamples = cv_train,
        grid = rf_grid,
        control = control_grid(save_pred = TRUE)
              )
rf_resamples
# temp <- rf_resamples$.notes[[1]][1]
# temp$.notes[1]


# XGBoost model, `original` data set
set.seed(1234)
xgb_resamples <- wf_xgb |> 
    tune_grid(
        resamples = cv_train,
        grid = xgb_grid,
        control = control_grid(save_pred = TRUE)
              )
xgb_resamples
# temp <- xgb_resamples$.notes[[1]][1]
# temp$.notes[1]



#########################################################################
###                           Model evaluation
#########################################################################

# folder metrics
rf_resamples |> 
    collect_metrics()

# roc curves for RF on the training data set
rf_resamples |>
     collect_predictions() |>
     group_by(id) %>%
     roc_curve(ahd, .pred_No) |>
     ggplot(aes(1 - specificity, sensitivity, color = id)) +
     geom_abline(lty = 2, color = "gray80", linewidth = 1) +
     geom_path(show.legend = FALSE, alpha = 0.5, linewidth = 1) +
     coord_equal()


# roc curves for both RF and XGB on the training data set
df_auc <- bind_rows(
     rf_resamples |>
          collect_predictions() |>
          group_by(id) %>%
          roc_curve(ahd, .pred_No)  |>
          mutate (model = 'random forest'),
     xgb_resamples |>
          collect_predictions() |>
          group_by(id) |>
          roc_curve(ahd, .pred_No)  |>
          mutate (model = 'xgboost')) |>
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1) +
  geom_path(show.legend = FALSE, alpha = 0.5, linewidth = 1) +
  coord_equal() +
  facet_wrap(~ model)

df_auc



#########################################################################
###         Choose the best combination of hyper-parameter values
#########################################################################

best_rf <- rf_resamples |>
    select_best(metric = "roc_auc")
best_rf


best_xgb <- xgb_resamples |>
    select_best(metric = "roc_auc")


#########################################################################
###                   Finalize the models
#########################################################################
#  Finalize means take the tuned parameters and  fit the model 
#   on ALL the training data. 
#########################################################################

rf_model_wflow_fit <- finalize_workflow(wf_rf, best_rf)

xgb_model_wflow_fit <- finalize_workflow(wf_xgb, best_xgb)



#########################################################################
###                        Variable importance
#########################################################################
library(vip)


rf_model_wflow_fit |> 
    fit(data = train_tbl) |>
    extract_fit_parsnip() |> 
    vip::vip() 


xgb_model_wflow_fit |> 
    fit(data = train_tbl) |>
    extract_fit_parsnip() |> 
    vip::vip()



#########################################################################
###               Model performance on the test set
#########################################################################

test__rf <- rf_model_wflow_fit |> last_fit(splits) 
test__rf |> collect_metrics() 

test__xgb <- xgb_model_wflow_fit |> last_fit(splits) 
test__xgb |> collect_metrics() 




