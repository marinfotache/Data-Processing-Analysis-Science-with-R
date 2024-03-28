#######################################################################
###                    Al.I. Cuza University of Iași                ###
###       Faculty of Economics and Business Administration          ###
###   Department of Accounting, Information Systems and Statistics  ###
#######################################################################
###
#######################################################################
###        Data Processing/Analysis/Science with R                  ###
#######################################################################
###
#######################################################################
###              11b1 An example of Logistic Regression             ###
#######################################################################
## last update: 2024-03-28

library(tidyverse) 
library(janitor)
library(tidymodels)
library(skimr)

# package `broom` for "tidying" the regression models
# install.packages('broom')
library(broom)

# this package helps interpreting the model results
library(report)

#######################################################################
###           Download the necessary data sets for this script       ###
#######################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

#######################################################################
# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)



#####################################################################
###                      1 Heart disease                       ###
#####################################################################
###       sources:
### https://archive.ics.uci.edu/ml/datasets/heart+Disease
###       description: 
### https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/heart-disease.names 
### 
# ### -- Attribute documentation:
#    `Age`: age in years
#    `Sex`: sex (1 = male; 0 = female)
#    `ChestPain`: chest pain type
#              -- Value 1: typical angina
#              -- Value 2: atypical angina
#              -- Value 3: non-anginal pain
#              -- Value 4: asymptomatic
#    `RestBP`: resting blood pressure (in mm Hg on admission to the 
#         hospital)
#    `Chol`: serum cholestoral in mg/dl
#    `Fbs`: (fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)
#    `RestECG`: resting electrocardiographic results. 
#              0: normal, 
#              1: having ST-T wave abnormality (T wave inversions and/or ST, 
#                   elevation or depression of > 0.05 mV) 
#              2: showing probable or definite left ventricular hypertrophy 
#                   by Estes' criteria
#    `MaxHR`: maximum heart rate achieved
#    `ExAng`: exercise induced angina (1 = yes; 0 = no)
#    `Oldpeak`: ST depression induced by exercise relative to rest
#    `Slope`: the slope of the peak exercise ST segment 
#              1: upsloping, 
#              2: flat, 
#              3: downsloping
#    `Ca`: number of major vessels (0-3) colored by flourosopy 
#    `Thal`:    
#              3 = normal; 
#              6 = fixed defect; 
#              7 = reversable defect 
#    `AHD`: diagnosis of heart disease (angiographic disease status). 
#              0: < 50 
#              1: > 50 
#         (in any major vessel: attributes 59 through 68 are vessels)              

# EDA for this data set is also available at:
# https://rileyking.netlify.com/post/heart-disease-prediction-from-patient-data-in-r/

heart <- read_csv('Heart.csv')  |>
     tibble() |>
     janitor::clean_names() |>
     dplyr::select(-x1) |>
     mutate(
          sex = case_when(
               sex == 0 ~ "Female",
               sex == 1 ~ "Male",
               is.na(sex) ~ 'unknown',
               .default = 'other'),
          fbs = case_when(
               fbs == 0 ~ "No",
               fbs == 1 ~ "Yes",
               .default = 'ERROR!'),
          ex_ang = case_when(
               ex_ang == 0 ~ "No",
               ex_ang == 1 ~ "Yes",
               .default = 'ERROR!'),
          slope = factor (slope, levels = c(1, 2, 3))
          ) |>
     mutate_if(is.character, as.factor) |>
     na.omit()   ###### remove rows with NA values

glimpse(heart)

anyNA (heart$ahd)

# Descriptive statistics
heart %>%
     skimr::skim()

##
## For Exploratory Data Analysis on this dataset, see script 09c....


## Fit the model 
log_reg__heart <- glm(ahd ~ ., data = heart, family = 'binomial')

## Info about the model
summary(log_reg__heart)

# Details provided by `report`
report::report(log_reg__heart)


## General info on the model
glance(log_reg__heart)

## Model coefficients

# raw coefficients
tidy(log_reg__heart)

# exponentiated coefficients
tidy(log_reg__heart, exponentiate = TRUE)

# exponentiated coefficients and CI
tidy(log_reg__heart, exponentiate = TRUE, conf.int = TRUE)


## Model performance

# model predictions on the data set 

# `predict`, by default we get the predictions as probabilities 
preds_prob <- log_reg__heart |>
     predict(heart, type = "response")


# get a data frame with predictions as both probabilities and classes

heart_with_preds <- bind_cols(
     heart,
     pred_prob = predict(log_reg__heart, newdata = heart, type = "response")
     ) |>
     mutate(pred_class = case_when(
          preds_prob >= 0.5 ~ 'Yes',
          preds_prob < 0.5 ~ 'No',
          .default = NA)) |>
     mutate(pred_class = factor(pred_class)) |>
     mutate(preds_prob_no = 1 - pred_prob)

glimpse(heart_with_preds)
     

#########################################################################
###                        Model performance

# Confusion matrix: 
conf_mat(heart_with_preds, truth = ahd, estimate = pred_class)

# Accuracy
accuracy(heart_with_preds, truth = ahd, estimate = pred_class)

# Matthews correlation coefficient:
mcc(heart_with_preds, truth = ahd, estimate = pred_class)

# F1 metric:
f_meas(heart_with_preds, truth = ahd, estimate = pred_class)


# ROC curve 

lr_roc_curve <- roc_curve(heart_with_preds, ahd, preds_prob_no)
autoplot(lr_roc_curve)

lr_roc_auc <- roc_auc(heart_with_preds, ahd, preds_prob_no)
lr_roc_auc

