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
###       14.f. Building and tuning tree-based classification models     ###
###                           with `tidymodels`                          ###  
############################################################################

options(java.parameters = "-Xmx12g")
options(scipen = 999)
library(tidyverse)
library(tidymodels)
#install.packages('themis')
library(themis)
library(ranger)
library(xgboost)
#install.packages('kernlab')
#library(kernlab)

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')



#####################################################################
###  1. Data import and preparation  (taken from script `01...`)  ###
#####################################################################
heart_init <- read_csv('Heart.csv') %>%
    mutate (AHD = if_else(AHD == 'Yes', 'No', 'Yes'))
glimpse(heart_init)

# prepare the data set, by removing the first column 
# and recoding the factors (also convertinf the character
# variables into factor)
heart_with_missing <- heart_init %>%
     select (-X1) %>%
     mutate(
          Sex = recode (Sex, `0` = "Female", `1` = "Male"),
          Fbs = recode (Fbs, `0` = "No", `1` = "Yes"),
          RestECG = factor (RestECG, levels = c(0, 1, 2)),
          ExAng = recode (ExAng, `0` = "No", `1` = "Yes"),
          Slope = factor (Slope, levels = c(1, 2, 3))
          ) %>%
     mutate_if(is.character, as.factor)

glimpse(heart_init)
glimpse(heart_with_missing)

rm(heart_init)

# Descriptive statistics
heart_with_missing %>%
     skimr::skim()



#####################################################################
###                  2. Missing value imputation             ###
#####################################################################

heart <- recipe( ~ ., data = heart_with_missing) %>%
    step_knnimpute(all_nominal(), all_numeric()) %>%
    prep() %>%
    bake(new_data = NULL) 
    
any(is.na(heart))

# glimpse
glimpse(heart)



#########################################################################
###           Train/test split and prepare cross-validation
#########################################################################

####################################
### for the original data set...
set.seed(seed = 1234) 
train_test_split <- rsample::initial_split(
        data = heart,     
        prop = 0.75,
      strata = 'AHD'
    ) 
train_test_split


train_tbl <- train_test_split %>% rsample::training() 
test_tbl  <- train_test_split %>% rsample::testing()

# cross-validation folder
set.seed(1234)
cv_train <- train_tbl %>%  
    rsample::vfold_cv(v = 5, repeats = 1)



#########################################################################
###        The recipe (used for both random forest and xgboost)
#########################################################################

the_recipe <-  recipes::recipe(AHD ~ ., data = train_tbl) %>%
     step_dummy(all_nominal(), -AHD) 


#########################################################################
###               Model specification (common for both data sets)
#########################################################################

## RF
rf_spec <- parsnip::rand_forest(
     mtry = tune(), 
     trees = 500,
     min_n = tune()     
          ) %>%
     set_engine("ranger", importance = "impurity") %>%
     set_mode("classification")
rf_spec


### XGBoost
xgb_spec <- parsnip::boost_tree(
    trees = 700, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                         ## step size
    ) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
xgb_spec



#########################################################################
###                   Grid specification (for model tuning)
#########################################################################

##############################
## for Random Forest models
set.seed(1234)
rf_grid <- dials::grid_random(
     finalize(mtry(), bake (the_recipe %>% prep(), train_tbl) %>% select (-AHD)),
     min_n(),  
     size = 10) # the number should be larger, but it would take longer
rf_grid



##############################
##  for XGBoost models
set.seed(1234)
xgb_grid <- dials::grid_random(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(), 
    finalize(mtry(), bake (the_recipe %>% prep(), train_tbl) %>% select (-AHD)),
    learn_rate(),
    size = 10 ) # the number should be larger, but it would take longer
xgb_grid



#########################################################################
###                 Assembling the models with workflows
#########################################################################

## Two separate series of Workflows for Random Forest Models and XGB Boost Models


# Random forest
wf_rf <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_spec) 


# XGBoost
wf_xgb <- workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(xgb_spec) 



#########################################################################
###        Fit the models for all cross-validation folds 
#########################################################################

require(doParallel)
cores <- parallel::detectCores(logical = FALSE)
doParallel::registerDoParallel(cores = cores)


# Random Forest model, `original` data set
set.seed(1234)
rf_resamples <- wf_rf %>% 
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
xgb_resamples <- wf_xgb %>% 
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
rf_resamples %>% 
    collect_metrics()

# roc curves
rf_resamples %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(AHD, .pred_No) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1) +
  geom_path(show.legend = FALSE, alpha = 0.5, size = 1) +
  coord_equal()


df_auc <- bind_rows(
    rf_resamples %>%
        collect_predictions() %>%
        group_by(id) %>%
        roc_curve(AHD, .pred_No)  %>%
        mutate (model = 'random forest'),
    xgb_resamples %>%
        collect_predictions() %>%
        group_by(id) %>%
        roc_curve(AHD, .pred_No)  %>%
        mutate (model = 'xgboost')
  ) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1) +
  geom_path(show.legend = FALSE, alpha = 0.5, size = 1) +
  coord_equal() +
  facet_wrap(~ model)

df_auc



#########################################################################
###         Choose the best combination of hyper-parameter values
#########################################################################

best_rf <- rf_resamples %>%
    select_best("roc_auc")
#best_rf


best_xgb <- xgb_resamples %>%
    select_best("roc_auc")


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


rf_model_wflow_fit %>% 
    fit(data = train_tbl) %>%
    pull_workflow_fit() %>% 
    vip::vip() 


xgb_model_wflow_fit %>% 
    fit(data = train_tbl) %>%
    pull_workflow_fit() %>% 
    vip::vip()



#########################################################################
###               Model performance on the test set
#########################################################################

test__rf <- rf_model_wflow_fit %>% last_fit(train_test_split) 
test__rf %>% collect_metrics() 

test__xgb <- xgb_model_wflow_fit %>% last_fit(train_test_split) 
test__xgb %>% collect_metrics() 




