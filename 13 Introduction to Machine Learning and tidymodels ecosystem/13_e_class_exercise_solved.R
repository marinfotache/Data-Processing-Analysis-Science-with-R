library(ranger)
library(tidyverse)
library(tidymodels)
options(scipen = 999)

setwd('/Users/nicolairomanov/Documents/projects/Data-Processing-Analysis-Science-with-R/DataSets')

#1 load data
car_prices <- readr::read_csv('CarPrice_Assignment.csv')
any(is.na(car_prices))

#2 create train test split
set.seed(1234)
splits   <- initial_split(car_prices, prop = 0.75)
train_tbl <- training(splits)
test_tbl  <- testing(splits)

#3 create the recipe
the_recipe <- recipe(price ~ ., data = train_tbl) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
  step_impute_knn(all_predictors(), neighbors = 3) %>%   # ... when having missing values
  step_zv(all_predictors()) # this removes predictors with zero variance

#4 create linear regression specs
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

#5 create rf specs
rf_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

#6 fit lr model
set.seed(1234)
lm_fit <- workflow() %>%
  add_recipe(the_recipe) %>%
  add_model(lm_spec) %>%
  fit(data = train_tbl)

#7 make lr predictions on train data
#lm_fit_summary <- broom::glance(lm_fit)
#lm_fit_coefficients <- broom::tidy (lm_fit)
lm_fit_predictions <- broom::augment (lm_fit, new_data = train_tbl)

#8 fit rf model
set.seed(1234)
rf_fit <- workflow() %>%
  add_recipe(the_recipe) %>%
  add_model(rf_spec) %>%
  fit(data = train_tbl)

#9 make rf predictions or train data
rf_fit_predictions <- broom::augment(rf_fit, new_data = train_tbl)

#10 plot true and pred values for lr model
broom::augment (lm_fit, new_data = train_tbl) %>%
  ggplot( aes (x = price, y = `.pred`)) +
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) +
  labs(y = "Predicted values of Price", x = "Price (observed values)") +
  ggtitle('Predicted vs. Real Values - the linear model')

#11 plot true and pred values for rf model
broom::augment(rf_fit, new_data = train_tbl) %>%
  ggplot( aes (x = price, y = `.pred`)) +
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) +
  labs(y = "Predicted values of Price", x = "Prices (observed values)") +
  ggtitle('Predicted vs. Real Values - the random forest model')

#12 define scoring metrics - rmse, rsq, mae, ccc
scoring_metrics <- metric_set(rmse, rsq, mae, ccc)

#13 make predictions on test data for lr model
set.seed(1234)
test__lm <- broom::augment(lm_fit, new_data = test_tbl)
scoring_metrics(test__lm, truth = price, estimate = .pred)

#14 make predictions on test data for rf model
set.seed(1234)
test__rf <- broom::augment(rf_fit, new_data = test_tbl)
scoring_metrics(test__rf, truth = price, estimate = .pred)

#15 display coeeficients of lr model 
lm_fit
lm_fit_coefficients <- broom::tidy (lm_fit)
lm_fit_coefficients

library(vip)

#16 display variable importances for rf model
set.seed(1234)
rf_imp_spec <- rf_spec %>%
  set_engine("ranger", importance = "permutation")

workflow() %>%
  add_recipe(the_recipe) %>%
  add_model(rf_imp_spec) %>%
  fit(train_tbl) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))

