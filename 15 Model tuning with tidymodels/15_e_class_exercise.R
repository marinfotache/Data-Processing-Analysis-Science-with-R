options(scipen = 999)
library(tidyverse)
library(tidymodels)
library(ranger)
library(xgboost)

#1 load heart failure data, transform target column to factor

#2 create train test split

#3 create folds based on train data

#4 create the recipe

#5 create rf spec with mtry and min_n to be tuned

#6 create xgb spec with tree_depth, min_n, loss_reduction, sample_size, mtrt and learn_rate to be tuned

#7 create rf param grid

#8 create xgb param grid

#9 create rf workflow

#10 create xgb workflow

#11 fit rf folds

#12 fit xgb folds

#13 display rf metrics

#14 display xgb metrics

#15 plot the roc auc for both models

#16 select best rf and xgb models

#17 finalize both workflows

#18 plot rf feature importances

#19 plot xgb feature importances

#20 collect metrics on test data for rf

#21 collect metrics on test data for xgb

