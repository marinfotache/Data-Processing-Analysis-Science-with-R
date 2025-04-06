options(scipen = 999)
library(tidyverse)
library(tidymodels)
library(ranger)
library(xgboost)

#1 load data, transform target column to factor
heart_failure <- readr::read_csv('heart_failure.csv') %>%
  mutate(HeartDisease = factor(HeartDisease))
any(is.na(heart_failure))
table(heart_failure$HeartDisease)

#2 create train test split
set.seed(1234)
splits   <- initial_split(heart_failure, prop = 0.75, strata = HeartDisease)
train_tbl <- training(splits)
test_tbl  <- testing(splits)

#3 create folds based on train data
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1, strata = HeartDisease)
cv_train

#4 create the recipe
the_recipe <- recipe(HeartDisease ~ ., data = train_tbl) %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%   # missing values imputation
  step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
  step_zv(all_predictors()) # this removes predictors with zero variance

#5 create rf spec with mtry and min_n to be tuned
rf_spec <- parsnip::rand_forest(
  mtry = tune(), 
  trees = 500,
  min_n = tune()     
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")
rf_spec


#6 create xgb spec with tree_depth, min_n, loss_reduction, sample_size, mtrt and learn_rate to be tuned
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


#7 create rf param grid
set.seed(1234)
rf_grid <- dials::grid_random(
  finalize(mtry(), train_tbl %>% select (-HeartDisease)),
  min_n(),  
  size = 20)  # the number should be larger, but nodel fitting would take longer

rf_grid

#8 create xgb param grid
set.seed(1234)
xgb_grid <- dials::grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_tbl %>% select (-HeartDisease)),
  learn_rate(),
  size = 60   # the number should be larger, but it would take longer
)
xgb_grid


#9 create rf workflow
wf_rf <- workflow() %>%
  add_recipe(the_recipe) %>%
  add_model(rf_spec) 


#10 create xgb workflow
wf_xgb <- workflow() %>%
  add_recipe(the_recipe) %>%
  add_model(xgb_spec) 

#11 fit rf folds
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


#12 fit xgb folds
set.seed(1234)
xgb_resamples <- wf_xgb %>% 
  tune_grid(
    resamples = cv_train,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )
xgb_resamples

#13 display rf metrics
rf_resamples %>% 
  collect_metrics()

#14 display xgb metrics

#15 plot the roc auc for both models
df_auc <- bind_rows(
  rf_resamples %>%
    collect_predictions() %>%
    group_by(id) %>%
    roc_curve(HeartDisease, .pred_0)  %>%
    mutate (model = 'random forest'),
  xgb_resamples %>%
    collect_predictions() %>%
    group_by(id) %>%
    roc_curve(HeartDisease, .pred_0)  %>%
    mutate (model = 'xgboost')
) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", linewidth = 1) +
  geom_path(show.legend = FALSE, alpha = 0.5, linewidth = 1) +
  coord_equal() +
  facet_wrap(~ model)

df_auc

#16 select best rf and xgb models
best_rf <- rf_resamples %>%
  select_best("roc_auc")
best_xgb <- xgb_resamples %>%
  select_best("roc_auc")

#17 finalize both workflows
rf_model_wflow_fit <- finalize_workflow(wf_rf, best_rf)
xgb_model_wflow_fit <- finalize_workflow(wf_xgb, best_xgb)


library(vip)
#18 plot rf feature importances
rf_model_wflow_fit %>% 
  fit(data = train_tbl) %>%
  extract_fit_parsnip() %>% 
  vip::vip() 

#19 plot xgb feature importances
xgb_model_wflow_fit %>% 
  fit(data = train_tbl) %>%
  extract_fit_parsnip() %>% 
  vip::vip()

#20 collect metrics on test data for rf
test__rf <- rf_model_wflow_fit %>% last_fit(splits) 
test__rf %>% collect_metrics() 

#21 collect metrics on test data for xgb
test__xgb <- xgb_model_wflow_fit %>% last_fit(splits) 
test__xgb %>% collect_metrics() 

