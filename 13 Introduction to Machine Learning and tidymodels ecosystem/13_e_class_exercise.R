library(ranger)
library(tidyverse)
library(tidymodels)
options(scipen = 999)

setwd('/Users/nicolairomanov/Documents/projects/Data-Processing-Analysis-Science-with-R/DataSets')

#1 load car prices data

#2 create train test split


#3 create the recipe
# (3 steps - data dummyfication, missing data imputation, drop zero variance columns)


#4 create linear regression specs


#5 create rf specs


#6 create the workflow and fit lr model


#7 make lr predictions on train data


#8 create the workflow and fit rf model


#9 make rf predictions or train data


#10 plot true and pred values for lr model


#11 plot true and pred values for rf model


#12 define scoring metrics - rmse, rsq, mae, ccc


#13 make predictions on test data for lr model


#14 make predictions on test data for rf model


#15 display coeeficients of lr model 


#16 display variable importances for rf model

