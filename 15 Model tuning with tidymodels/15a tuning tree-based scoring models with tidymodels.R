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
###           15.a. Building and Tuning Tree-Based Scoring Models        ###
###                           with `tidymodels`                          ###  
############################################################################
## last update: 2024-04-04
# install.packages('ranger')
library(ranger)   # for Random Forest models
library(xgboost)  # for XGBoost models
library(tidyverse)
#library(broom)
library(tidymodels)
library(vip)
# install.packages('DALEXtra')
library(DALEXtra)
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
### 	                        Insurance data set                  ###
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


##########################################################################
###                             Main split of the data           
###
set.seed(1234)
splits   <- initial_split(insurance, prop = 0.75)
train_tbl <- training(splits)
test_tbl  <- testing(splits)


## cross-validation folds
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1)
cv_train



##########################################################################
###                        The recipe for data preparation             ###
### not all steps (in the following recipe) are really necessary 
### in this case, but in many other situations they are really useful
the_recipe <- recipe(charges ~ ., data = train_tbl) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
    step_impute_knn(all_predictors(), neighbors = 3) %>%   # ... when having missing values
    step_zv(all_predictors()) # this removes predictors with zero variance



#########################################################################
###                           Model Specification

## Random Forest models
rf_spec <- rand_forest(
     mtry = tune(),    # first hyper-parameter to be tuned
     trees = 500,
     min_n = tune()     # second hyper-parameter to be tuned
          ) %>%
     set_engine("ranger") %>%
     set_mode("regression")

rf_spec


### XGBoost models
xgb_spec <- boost_tree(
    trees = 700, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                          ## step size
    ) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")

xgb_spec


#########################################################################
###                           Assemble the workflows

wf_rf <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(the_recipe)

wf_xgb <- workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(the_recipe)



#########################################################################
###                      Grids for hyper-parameter tuning

set.seed(1234)
rf_grid <- dials::grid_random(
    finalize(mtry(), train_tbl %>% select (-charges)),
    min_n(),  
    size = 20)  # the number should be larger, but it would take longer
rf_grid


set.seed(1234)
xgb_grid <- dials::grid_random(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_tbl %>% select (-charges)),
    learn_rate(),
    size = 50   # the number should be larger, but it would take longer
)
xgb_grid



#########################################################################
###   Fit the models for all k-fold folders and hyper-parameters grid
#install.packages('doParallel')
doParallel::registerDoParallel()


set.seed(1234)
rf_resamples <- wf_rf %>% 
    tune_grid(
      resamples = cv_train,
      grid = rf_grid
              )
rf_resamples


set.seed(1234)
xgb_resamples <- wf_xgb %>% 
    tune_grid(
      resamples = cv_train,
      grid = xgb_grid
    )
xgb_resamples

# In case of errors, one can extract additional information as follows ...
#temp <- xgb_resamples$.notes[[1]][1]
#temp$.notes[1]



#########################################################################
### Explore the results and choose the best hyper-parameter combination


# performance metrics (mean) across folds for each grid line
temp <- rf_resamples %>% collect_metrics()
View(temp)
autoplot(rf_resamples)

rf_resamples %>% 
    collect_metrics() %>%
    filter(`.metric` == 'rsq') %>%
    summarise(avg_rsq = mean(mean))


xgb_resamples %>% collect_metrics()
autoplot(xgb_resamples)

xgb_resamples %>% 
    collect_metrics() %>%
    filter(`.metric` == 'rsq') %>%
    summarise(avg_rsq = mean(mean))



# choose the best hyper-parameter combination

# this syntax does not work anymore...
#best_rf <- rf_resamples %>% select_best("rmse")
# ... being superseded by:
best_rf <- rf_resamples %>% select_best(metric = "rmse")
best_rf


best_xgb <- xgb_resamples %>% select_best(metric = "rmse")
best_xgb



#########################################################################
###        Finalize the workflows with the best performing parameters

final_wf_rf <- wf_rf %>% 
     finalize_workflow(best_rf)

final_wf_xgb <- wf_xgb %>% 
    finalize_workflow(best_xgb)


## fit the final models on the entire train data set

set.seed(1234)
final_rf_train <- final_wf_rf %>%
    fit(data = train_tbl) 
final_rf_train

set.seed(1234)
final_xgb_train <- final_wf_xgb %>%
    fit(data = train_tbl) 
final_xgb_train



#########################################################################
###             Model performance on the test data set

### Function last_fit() fits the finalized workflow one last time 
### to the training data and evaluates one last time on the testing data.

set.seed(1234)
test__rf <- final_wf_rf %>% last_fit(splits) 
test__rf %>% collect_metrics() 

set.seed(1234)
test__xgb <- final_wf_xgb %>% last_fit(splits) 
test__xgb %>% collect_metrics() 



#########################################################################
###                        Variable importance
#########################################################################
#library(vip)

set.seed(1234)
rf_imp_spec <- rf_spec %>%
    finalize_model(best_rf) %>%
    set_engine("ranger", importance = "permutation")

workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_imp_spec) %>%
    fit(train_tbl) %>%
    extract_fit_parsnip() %>%
    vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))



set.seed(1234)
xgb_imp_spec <- xgb_spec %>%
    finalize_model(best_xgb) %>%
    set_engine("xgboost", importance = "permutation")

workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(xgb_imp_spec) %>%
    fit(train_tbl) %>%
    extract_fit_parsnip() %>%
    vip(aesthetics = list(alpha = 0.8, fill = "red"))


# ..or
final_xgb_train %>% 
    extract_fit_parsnip() %>% 
    vip()



#########################################################################
###              Interpretable (Explainable) Models
#########################################################################

# Creating a pre-processed dataframe of the train dataset
imp_data <- the_recipe |>
     prep() |>
     bake(new_data = NULL)


# Final model with the best parameters
set.seed(1234)
df_spec_final_rf <- rf_spec |>
     finalize_model(best_rf) |>
     set_engine("ranger", importance = "permutation")

#building the explainer-object
explainer_rf <- DALEXtra::explain_tidymodels(
     df_spec_final_rf |> 
          fit(charges ~ ., data = imp_data),
          data = imp_data %>% select(-charges), 
          y = train_tbl$charges,
     verbose = FALSE
     )

# Calculates the variable-importance measure
set.seed(1234)
vip_df <- model_parts( 
     explainer = explainer_rf, 
     loss_function = loss_root_mean_square, 
     B = 100, #the number of permutations
     type = "difference",
     label =""
  )

# Plotting ranking of the importance of explanatory variables
plot(vip_df) +
     ggtitle("Mean variable-importance over 100 permutations", "\nRandom Forests")+
     theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size=13),
        axis.text = element_text(size=12)) 


# Partial dependence profiles for smokers
set.seed(1234)
pdp_smoker <- model_profile(explainer_rf, variables = "smoker_yes")
 
as_tibble(pdp_smoker$agr_profiles) |>
     ggplot(aes(`_x_`, `_yhat_`)) +
     geom_line(data = as_tibble(pdp_smoker$cp_profiles),
            aes(x = smoker_yes, group = `_ids_`),
            size = 0.5, alpha = 0.05, color = "gray50")+
     geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)+
     labs(title= "Charges vs. Smoker_yes")+
     theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_line(color="grey"))


# Partial dependence profiles for bmi
set.seed(1234)
pdp_bmi <- model_profile(explainer_rf, variables = "bmi")
 
as_tibble(pdp_bmi$agr_profiles) |>
     ggplot(aes(`_x_`, `_yhat_`)) +
     geom_line(data = as_tibble(pdp_bmi$cp_profiles),
            aes(x = bmi, group = `_ids_`),
            size = 0.5, alpha = 0.05, color = "gray50")+
     geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.8)+
     labs(title= "Charges vs. bmi")+
     theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_line(color="grey"))


# Partial dependence profiles for age
set.seed(1234)
pdp_age <- model_profile(explainer_rf, variables = "age")
 
as_tibble(pdp_age$agr_profiles) |>
     ggplot(aes(`_x_`, `_yhat_`)) +
     geom_line(data = as_tibble(pdp_age$cp_profiles),
            aes(x = age, group = `_ids_`),
            size = 0.5, alpha = 0.05, color = "gray50")+
     geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.8)+
     labs(title= "Charges vs. age")+
     theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.x = element_line(color="grey"))
