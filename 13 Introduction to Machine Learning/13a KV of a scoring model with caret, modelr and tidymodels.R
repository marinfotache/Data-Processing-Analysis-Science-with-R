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
###          13a. Introduction to Machine Learning with `caret`,         ### 
###      `modelr` and `tidymodels` packages                              ### 
###            Cross-validation of a scoring (regression) model          ###   
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 24.12.2018


# needed packages
library(tidyverse)
#install.packages('skimr')
library(skimr)
library(corrplot)
library(corrgram)
library(caret)
#library(readxl)
library(modelr)
#install.packages('tidymodels')
library(tidymodels)
#library(Metrics)
# install.packages('pryr')
#library(pryr)
options(scipen = 6)


############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/admin/SkyDrive/DataSets')
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
### O. Import and prepare the data sets                               ###
###  I. Estimate performance of a linear regression model with        ###
###       cross-validation  using `caret` package                     ###
###  II. Estimate performance of a linear regression model with       ###
###       cross-validation  using `modelr` package                    ###
###  III. Estimate performance of a linear regression model with      ###
###       cross-validation  using `tidymodels` packages               ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################


#########################################################################
###            0.a. `States` (USA) data set - see script `11b`        ###
###                      (for scoring/regression)                     ###
#  Example taken from Kabacoff's R in Action (Manning), 2013, 2015 
# state.x77 dataset in the base package 
# Explore the relationship between a state’s murder rate and 
#    other characteristics of the state, including population, 
#    illiteracy rate, average income, and frost levels (mean number 
#    of days below freezing).
#########################################################################

# lm() function requires a data frame;state.x77 dataset is contained in a matrix, 
#    so one must convert it:
states <- as_tibble(state.x77, rownames = 'State')
names(states) <- str_replace_all(names(states), ' |\\.', '_')
head(states)
class(states)


# descriptive statistics
states %>%
     skimr::skim()

# examine bivariate relationships
cor(states %>% select (-State))

corrplot::corrplot(cor(states %>% select (-State), 
             method = "spearman"), method = "number", type = "upper")

corrgram::corrgram(states %>% select (-State) %>% select_if(is.numeric),
                   lower.panel=panel.conf, upper.panel=panel.pts)

corrgram::corrgram(states %>% select (-State) %>% select_if(is.numeric),
     lower.panel=panel.pie, upper.panel=panel.pts,
     diag.panel=panel.density)

corrgram::corrgram(states %>% select (-State) %>% select_if(is.numeric),
     lower.panel=panel.conf, upper.panel=panel.pts,
     diag.panel=panel.density)



#######################################################################
### 	                0.b.  insurance` data set                      ###
###                    (for scoring/regression)                     ###
### data available on 
### https://github.com/stedy/Machine-Learning-with-R-datasets

## Variables
## `age`: age of primary beneficiary
## `sex`: insurance contractor gender, female, male
## `bmi`: Body mass index, providing an understanding of body, 
##   weights that are relatively high or low relative to height, 
##   objective index of body weight (kg / m ^ 2) using the ratio 
##   of height to weight, ideally 18.5 to 24.9
## `children`: Number of children covered by health insurance / Number of dependents
## `smoker`: Smoking
## `region`: the beneficiary's residential area in the US, northeast, southeast, 
##        southwest, northwest.
## `charges`: Individual medical costs billed by health insurance
insurance <- readr::read_csv('insurance.csv')

# descriptive statistics
insurance %>%
     skimr::skim()

# examine bivariate relationships
insurance %>%
     select_if(is.numeric) %>%
     cor()

corrplot::corrplot(cor(insurance %>%
     select_if(is.numeric) , 
             method = "spearman"), method = "number", type = "upper")

corrgram::corrgram(insurance %>% select_if(is.numeric),
     lower.panel=panel.pie, upper.panel=panel.pts,
     diag.panel=panel.density)

corrgram::corrgram(insurance %>% select_if(is.numeric),
     lower.panel=panel.conf, upper.panel=panel.pts,
     diag.panel=panel.density)

insurance_lm1 <- lm(charges ~ ., data = insurance)
summary(insurance_lm1)



#########################################################################
###     I. Estimate Performance of a Linear Regression Model with     ###
###                 Cross-Validation  Using `caret` Package           ###
#########################################################################

#########################################################################
###                      I.a  `States` (USA) data set                ###
#########################################################################

#   Baseline (from script 11b):
#   `All-in Model` - the model built with all variables               
#        (except `State`) and all observations                         
states_lm1 <- lm(Murder ~ ., data = states %>% select (-State))
summary(states_lm1)

## Set up the resampling approach - 5-fold cross validation

## Set seed for reproducibility
set.seed(123)

## Split the data into train/test using an index of row numbers:
index__states <- caret::createDataPartition(y = states$Murder, p = .7, list = FALSE)
main_train__states <- states[index__states, ] %>%
     select (-State)
main_test__states <- states[-index__states, ] %>%
     select (-State)

# Set up the resampling, here repeated CV
tr_states1 <- caret::trainControl(method = "cv", number = 5)


#########################################################################
##           First model (`lm_states1_caret`) with all predictors

# train the model using 5-fold CV
lm_states1_caret <- caret::train(Murder ~ ., data = main_train__states, 
                    method = "lm", trControl = tr_states1, trace = FALSE)

# extract general information about the model
lm_states1_caret
lm_states1_caret$results

# extract predictor coefficients in the final model
lm_states1_caret$finalModel

# compare the RMSE, R2 and MAE across folders
lm_states1_caret$resample

# folders average RMSE on the training set
mean(lm_states1_caret$resample$RMSE)


# Find out which variables contribute to predictive accuracy in the training
#    subset:
caret::varImp(lm_states1_caret)
plot(caret::varImp(lm_states1_caret))


#########################################################################
##   Second model (`lm_states2_caret`) got by stepwise regression 
##   with `caret`
## Here we run automated stepwise regression and look at 
##   the resampling results. 
lm_states2_caret <- caret::train(Murder ~ ., data = main_train__states , 
                    method = "lmStepAIC", trControl = tr_states1, trace = FALSE)

lm_states2_caret$results

lm_states2_caret$finalModel


#########################################################################
##   Third model - just for comparison in section `II.a` 
lm_states3_caret <- caret::train(Murder ~ Frost + HS_Grad + Illiteracy + 
                                      Life_Exp + Population, 
                    data = main_train__states, 
                    method = "lm", trControl = tr_states1, trace = FALSE)


#########################################################################
##                     Compare the trained models

# Inspect the results table with resamples(). 
mods <- caret::resamples(list(lm_states1_caret = lm_states1_caret, 
          lm_states2_caret = lm_states2_caret))
summary(mods)

# t-test for assessing if two models are significally different
caret::compare_models(lm_states1_caret, lm_states2_caret)



#########################################################################
##           Assess the performance of the models on the test set

# get the values of outcome (`Murder`) predicted by the models 
#    for the testing data set
predicted__lm_states1_caret__main_test <- predict(lm_states1_caret, main_test__states)
predicted__lm_states2_caret__main_test <- predict(lm_states2_caret, main_test__states)
predicted__lm_states3_caret__main_test <- predict(lm_states3_caret, main_test__states)


# RMSE 
caret::RMSE (predicted__lm_states1_caret__main_test, main_test__states$Murder)
caret::RMSE (predicted__lm_states2_caret__main_test, main_test__states$Murder)
caret::RMSE (predicted__lm_states3_caret__main_test, main_test__states$Murder)


# R2
caret::R2 (predicted__lm_states1_caret__main_test, main_test__states$Murder)
caret::R2 (predicted__lm_states2_caret__main_test, main_test__states$Murder)
caret::R2 (predicted__lm_states3_caret__main_test, main_test__states$Murder)


# the same but with `dplyr` - here onl for `lm_states1_caret`
tibble(predicted = predicted__lm_states1_caret__main_test, 
       observed = main_test__states$Murder, 
       model = 1) %>%
     group_by(model) %>%
     summarise(mean_rmse_test = caret::RMSE (predicted, observed))
     


#########################################################################
###                     I.b. `insurance` data set                    ###
#########################################################################

# descriptive statistics
insurance %>%
     skimr::skim()

#   Baseline:
#   `All-in Model` - the model built with all variables               
#        and all observations                         
insurance_lm1 <- lm(charges ~ ., data = insurance)
summary(insurance_lm1)


## Set seed for reproducibility
set.seed(123)

## Split the data into train/test using an index of row numbers:
index__insurance <- caret::createDataPartition(y = insurance$charges, p = .8, list = FALSE)
main_train__insurance <- insurance[index__insurance, ]
main_test__insurance <- insurance[-index__insurance, ]

# Set up the resampling, here repeated CV
tr_insurance1 <- trainControl(method = "cv", number = 10)


#########################################################################
##                model `lm_insurance1_caret` with all predictors

# train the model using 10-fold CV
lm_insurance1_caret <- train(charges ~ ., data = main_train__insurance, 
                    method = "lm", trControl = tr_insurance1, trace = FALSE)
lm_insurance1_caret

lm_insurance1_caret$results

lm_insurance1_caret$finalModel

lm_insurance1_caret$resample

mean(lm_insurance1_caret$resample$RMSE)

# Find out which variables contribute to predictive accuracy:
varImp(lm_insurance1_caret)
plot(varImp(lm_insurance1_caret))



#########################################################################
###     II. Estimate Performance of a Linear Regression Model with    ###
###               Cross-Validation  using `modelr` package            ###
#########################################################################

#########################################################################
###                         II.a `States` data set                   ###
#########################################################################

## we already split the data in section `I.a`
main_train__states 
main_test__states 

## From the `main_train__states` data set, we'll built
## a dataframe that contains a number of random splits of 
##   original data into a subdataset for training a model, 
##   and a subdataset to test a model inside the `main_train__states`. 
##  this task is performed by `crossv_mc()` function from 
##        the `modelr` package.
## Afterwards, for comparability with other techniques,
##   we'll measure model performance on `main_test__states` dataset
  
set.seed(123)  # Run to replicate this post

## 1. split the `main_train__states` into 5 folders
##   we'll rename `train` and `test` columns into `analysis` and `assessment`
##   for sharing the vocabulary with `tidymodels`
df_split_states_modelr <- main_train__states %>%
     modelr::crossv_kfold (k = 5) %>%
     transmute (analysis = train, assessment = test, .id) %>%
     print()

glimpse(df_split_states_modelr)

str(df_split_states_modelr[1,])


## 2. add a column containing the models for each fold
models_and_folds_states1_modelr <- df_split_states_modelr %>% 
     mutate(model = purrr::map(analysis, ~ lm(Murder ~ ., data = .)))
## The result is a new model column containing fitted 
##   regression models based on each of the `analysis` data 

glimpse(models_and_folds_states1_modelr)
models_and_folds_states1_modelr

# extract information about the model fitted in the 
#    training data of the first fold:
models_and_folds_states1_modelr$model[[1]] %>% summary()

# ... and the model of the second fold
models_and_folds_states1_modelr$model[[2]] %>% summary()

##   Note:
## In `models_and_folds_states1_modelr` data frame, 
##   column `assessment` refers to test set in each k-fold.
## Do not confound with `main_test__states` which is used
##   for comparisons with other modeling techniques/packages


##  3. Predicting the outcome variables values in each training 
##   folder's assessment data, i.e. test data inside each k-fold 
##   (within `main_train__states` data set)
##   General format:
##   `folds %>% 
##        mutate(predicted = map2(model, test, <FUNCTION_TO_PREDICT_TEST_DATA> ))`
## `map2(model, test, ...)` iterates through each model and 
##   set of test data in parallel. 


## `augment` from `broom` package can get the predicted values inside the folders
models_and_folds_states1_modelr <- models_and_folds_states1_modelr %>% 
     mutate(predicted_within_folds = purrr::map2(model, assessment, 
                                          ~ broom::augment(.x, newdata = .y)))

## To extract the relevant information from these predicted results, 
## we’ll unnest the data frames:
predicted_within_folds_states1_modelr <- models_and_folds_states1_modelr %>%
  tidyr::unnest(predicted_within_folds)
###  We now have a tibble of the test data for each fold (.id = fold number) 
###  and the corresponding .fitted, or predicted values for the 
###  outcome variable (`Murder`)  in each case.

# Plot the residuals
predicted_within_folds_states1_modelr %>% 
     mutate(residual = .fitted - Murder) %>%
     ggplot(aes(Murder, residual)) +
          geom_hline(yintercept = 0) +
          geom_point() +
          stat_smooth(method = "loess") +
          theme_minimal()
## It looks like our models seems to overestimate `Murder` between 2 and 6 and 
##        to underestimate it above 12.
## But there are clearly fewer data points, making prediction difficult.

## we compute some metrics with `modelr` for model performance
##   on the assessment date (i.e. the test data inside each fold)
models_and_folds_states1_modelr %>% 
     transmute (
          analysis_rmse_modelr = purrr::map2_dbl(model, analysis, modelr::rmse),
          assessment_rmse_modelr = purrr::map2_dbl(model, assessment, modelr::rmse),
          analysis_rsq_modelr = purrr::map2_dbl(model, analysis, modelr::rsquare),
          assessment_rsq_modelr = purrr::map2_dbl(model, assessment, modelr::rsquare)             
             ) %>%
     print()

# for fold `5`, the R2 on the folder test set is negative!!!


#########################################################################
###   Assess the model against the `main_test__states` data subset

models_and_folds_states1_modelr <- models_and_folds_states1_modelr %>%
     mutate (main_test__states = list(main_test__states)) %>%
     mutate(predicted_main_test = purrr::map2(model, main_test__states, 
                    ~ broom::augment(.x, newdata = .y)))

###  Plot the residuals for the `main_test__states` data subset:
models_and_folds_states1_modelr %>%
     tidyr::unnest(predicted_main_test) %>%
     mutate(residual = .fitted - Murder) %>%
     ggplot(aes(Murder, residual)) +
          geom_hline(yintercept = 0) +
          geom_point() +
          stat_smooth(method = "loess") +
          theme_minimal()

## metrics with  `modelr`
metrics_states1_modelr <- models_and_folds_states1_modelr %>% 
     mutate (
          analysis_rmse_modelr = purrr::map2_dbl(model, analysis, modelr::rmse),
          assessment_rmse_modelr = purrr::map2_dbl(model, assessment, modelr::rmse),
          test_rmse_modelr = purrr::map2_dbl(model, main_test__states, modelr::rmse),
          analysis_rsq_modelr = purrr::map2_dbl(model, analysis, modelr::rsquare),
          assessment_rsq_modelr = purrr::map2_dbl(model, assessment, modelr::rsquare),
          test_rsq_modelr = purrr::map2_dbl(model, main_test__states, modelr::rsquare)             
             ) %>%
    summarise(
          mean_rmse_assessment_modelr = mean(assessment_rmse_modelr),
               sd_rmse_assessment_modelr = sd(assessment_rmse_modelr),
          mean_rmse_test_modelr = mean(test_rmse_modelr),
               sd_rmse_test_modelr = sd(test_rmse_modelr),
          mean_r2_assessment_modelr = mean(assessment_rsq_modelr),
               sd_r2_assessment_modelr = sd(assessment_rsq_modelr),
          mean_r2_test_modelr = mean(test_rsq_modelr),
               sd_r2_test_modelr = sd(test_rsq_modelr)
          ) %>%
     gather(statistic, value)   %>%
     arrange(statistic)



#########################################################################
###                       II.b. `insurance` data set                  ###
#########################################################################

## we already split the data in section `I.a`
main_train__insurance 
main_test__insurance 

set.seed(123)  # Run to replicate this post

models_and_folds_insurance1_modelr <- main_train__insurance %>%
     modelr::crossv_kfold (k = 10) %>%      # split the `main_train__insurance` into 10 folds
     transmute (analysis = train, assessment = test, .id) %>%   # rename fold's `train` and `test` subsets
     mutate (main_test__insurance = list(main_test__insurance)) %>%  # add a column with the main test test
     mutate(model = purrr::map(analysis, ~ lm(charges ~ ., data = .))) %>% # add a fold model column 
     mutate(predicted_within_folds = purrr::map2(model, assessment,     # predict on fold's `assessment` data
                                          ~ broom::augment(.x, newdata = .y))) %>%
     mutate(predicted_main_test = purrr::map2(model, main_test__insurance, # predict on main test set
                    ~ broom::augment(.x, newdata = .y)))

## for plotting the residuals, we'll `unnest` `predicted_within_folds`

#  Plot the residuals for the each fold `assessment` subset:
models_and_folds_insurance1_modelr %>%
     tidyr::unnest(predicted_within_folds) %>%
     mutate(residual = .fitted - charges) %>%
     ggplot(aes(charges, residual)) +
          geom_hline(yintercept = 0) +
          geom_point() +
          stat_smooth(method = "loess") +
          theme_minimal()

#  Plot the residuals for the `main_test__insurance` data subset:
models_and_folds_insurance1_modelr %>%
     tidyr::unnest(predicted_main_test) %>%
     mutate(residual = .fitted - charges) %>%
     ggplot(aes(charges, residual)) +
          geom_hline(yintercept = 0) +
          geom_point() +
          stat_smooth(method = "loess") +
          theme_minimal()


## metrics with  `modelr`
metrics_insurance1_modelr <- models_and_folds_insurance1_modelr %>% 
     mutate (
          analysis_rmse_modelr = purrr::map2_dbl(model, analysis, modelr::rmse),
          assessment_rmse_modelr = purrr::map2_dbl(model, assessment, modelr::rmse),
          test_rmse_modelr = purrr::map2_dbl(model, main_test__insurance, modelr::rmse),
          analysis_rsq_modelr = purrr::map2_dbl(model, analysis, modelr::rsquare),
          assessment_rsq_modelr = purrr::map2_dbl(model, assessment, modelr::rsquare),
          test_rsq_modelr = purrr::map2_dbl(model, main_test__insurance, modelr::rsquare)             
             ) %>%
    summarise(
          mean_rmse_assessment_modelr = mean(assessment_rmse_modelr),
               sd_rmse_assessment_modelr = sd(assessment_rmse_modelr),
          mean_rmse_test_modelr = mean(test_rmse_modelr),
               sd_rmse_test_modelr = sd(test_rmse_modelr),
          mean_r2_assessment_modelr = mean(assessment_rsq_modelr),
               sd_r2_assessment_modelr = sd(assessment_rsq_modelr),
          mean_r2_test_modelr = mean(test_rsq_modelr),
               sd_r2_test_modelr = sd(test_rsq_modelr)
          ) %>%
     gather(statistic, value)   %>%
     arrange(statistic)




#########################################################################
###  III. Estimate Performance of a Linear Regression Model with      ###
###       Cross-Validation Using `tidymodels` Packages                ###
#########################################################################

###                      `tidymodels` ecosystem
##
## - for data import/prep/exploration/cleaning: `tidyverse`

## - for(re)sampling (e.g. 10-fold CV): `rsample`
##   In R Studio session, launch:     
##        `browseVignettes("rsample")`

## - for pre-processing (scale, center, impute, etc.): `recipes`

## - for model fitting: `parsnip`

## - for model evaluation & selection: `yardstick` & `tidyposterior`



#########################################################################
###                          III.a `states` data set                  ###
#########################################################################


## we already split the data in section `I.a`
main_train__states 
main_test__states 

## 1. If you want to split the data using only `tidymodels` ecosystem, one can use
##        package `resample` 
# set.seed(123)
# train_test_split_states1_tidymodels <- rsample::initial_split(states, prop = 0.7)
# main_train__states <- rsample::training(train_test_split_states1_tidymodels) %>% 
#      select (-State) 
# main_test__states <- rsample::testing(train_test_split_states1_tidymodels) %>% 
#      select (-State) 
 

## 2. Resample the training data set with `vfold_cv()` function from 
## `rsample` package. 
## In 5-fold cross-validation, the set would consist of the 5 different 
## resamples of the original data.

set.seed(123)  # Run to replicate this post
df_split_states_tidymodels <- rsample::vfold_cv(main_train__states, 
          v = 5, repeats = 1, strata = NULL)


## Like `modelr`, the resamples are stored in data-frame-like tibble object 
##   in the `splits` column as an object that has class `rsplit`.
df_split_states_tidymodels
df_split_states_tidymodels$splits[[1]]


##        Terminology for the two partitions that comprise a resample:

## - The `analysis` data are those that we selected in the resample; these data are 
##        used to fit a model or calculate a statistic (this is the training data of
##        each fold) - i.e. the training sets inside the initial training set
# ex: next function will display the `analysis` subset of the first of the five folds
rsample::analysis(df_split_states_tidymodels$splits[[1]])
# see that the number of rows of the data frame is the first value displayed by 
#   `df_split_states_tidymodels$splits[[1]]`

## - The `assessment` data are usually the section of the original data not covered 
## by the analysis set.  These data are often used to evaluate the performance 
## of a model that was fit to the analysis data ((this is the testing data of
##        each fold) - i.e. the testing sets inside the initial training set
# ex: next function will display the `assessment` subset of the first of the five folds
rsample::assessment(df_split_states_tidymodels$splits[[1]])  
# see that the number of rows of the data frame is the second value displayed by 
#   `df_split_states_tidymodels$splits[[1]]`

## (Labeling `analysis` and `assessment` is much preferable to `trainining` and 
##   `testing` since we already divide the initial data set into a training
##   subset (`main_train__states`) and a  testing subset (`main_test__states`)


## we'll add the special column with model's formula and also 
##  the main test set (`main_test__states`) for each fold, 
##   in order to compare the model performance with other techniques/packages

# Build the model formula:
#    `Murder ~ Population + Income + Illiteracy + Life_Exp +
#          HS_Grad Frost + Area`
response <- "Murder"
predictors <- setdiff(names(main_train__states), c(response, 'State'))
formula_ = paste(response, paste(predictors, collapse = ' + '), sep = ' ~ ')

# add a special column for the model formula (this is not so useful 
# now, but when fitting multiple models/fomulas for the same data set)
df_split_states_tidymodels <- df_split_states_tidymodels %>%
     mutate (formula = formula_) %>%
     mutate (main_test_set = list(main_test__states))



## 3. build a `recipe`, ...
## 4. ...prep it...
## 5. ... and bake it
##
## A recipe object is a container that holds all the steps 
##   that should be performed to go from the raw data set to 
##   the set that is fed into model a algorithm.
##   
# The three main functions of package `recipes` are `recipe()`, `prep()`, and `bake()`.

# 3. `recipe()` defines the operations on the data and the associated roles. 

# 4. Once the preprocessing steps are defined, parameters are estimated using `prep()`. 

# 5. Once the data are ready for transformation, 
#    the `bake()` function applies the operations.
# `bake()` takes a trained recipe and applies the operations 
#    to a data set to create a design matrix.

## 6. Fit the model with package `parnsip`
## 
## 7. Evaluate the model performance with `yardstick`
##



##
## We'll create a function for estimating model performance 
##    on each fold test set (`assessment` data set)
##   (we'll use a `recipe-prep-bake` "chain" for each fold `assessment`
##   subset)    
##   

#formula <- formula_
#split <- df_split_states_tidymodels$splits[[1]]

f_lm_states1_assessment <- function(split, formula){
   
     # extract the analysis set...
     analysis_set <- rsample::analysis(split)
     # ... and the assessment (validation) set
     assessment_set <- assessment(split)

     # 3. define the recipe
     analysis_recipe <- recipes::recipe(as.formula(formula), data = analysis_set)
     
     # 4. prepare the analysis set; estimate parameters (nothing spectacularly here,
     #    since we didn't scale/dummy-field/... any predictor or the outcome)
     analysis_prep <- recipes::prep(analysis_recipe, training = analysis_set)

     # 5. bake (apply computations to the both subsets) and get the
     #    processed version (again, in this case, this is not spectacular)
     #    of both `analysis` and `validation` subsets in each fold      
     analysis_processed <- bake(analysis_prep, new_data = analysis_set)
     assessment_processed <- bake(analysis_prep, new_data = assessment_set)
    
     # 6. fit the model (on the "baked" `analysis` subset)
     model <-  parsnip::linear_reg() %>%
          parsnip::set_engine("lm") %>%
          parsnip::fit(formula(analysis_recipe), data = analysis_processed)
     
     # return the observed versus predicted values in `assessment` subsets
     tibble::tibble("observed" = assessment_processed$Murder,
        "predicted" = unlist(predict(model, new_data = assessment_processed)))
}

## check the function 
temp_assessment <- f_lm_states1_assessment (
     split = df_split_states_tidymodels$splits[[1]],
     formula = formula_
)


##
## Similarly, we'll create a function for estimating model performance 
##    on the main test set
##   
# test_set <- df_split_states_tidymodels$main_test_set[[1]]

f_lm_states1_test <- function(split, formula, test_set) {
     
     # extract the analysis set
     analysis_set <- rsample::analysis(split)

     # 3. define the recipe
     analysis_recipe <- recipes::recipe(as.formula(formula), data = analysis_set)
     
     # 4. prepare the analysis set; estimate parameters (nothing spectacularly here,
     #    since we didn't scale/dummy-field/... any predictor or the outcome)
     analysis_prep <- recipes::prep(analysis_recipe, training = analysis_set)

     # 5. bake (apply computations to the both subsets) and get the data
     analysis_processed <- bake(analysis_prep, new_data = analysis_set)
     test_processed <- bake(analysis_prep, new_data = test_set)
    
     # 6. fit the model (on the "baked" `analysis` subset)
     model <-  parsnip::linear_reg() %>%
          parsnip::set_engine("lm") %>%
          parsnip::fit(formula(analysis_recipe), data = analysis_processed)
     
     # return the observed versus predicted values for the `test` subset
     tibble::tibble("observed" = test_processed$Murder,
        "predicted" = unlist(predict(model, new_data = test_processed)))
}


## check the function 
temp_test <- f_lm_states1_test (
     split = df_split_states_tidymodels$splits[[1]],
     formula = formula_, 
     test_set = df_split_states_tidymodels$main_test_set[[1]]
)


## Use these two functions for adding two columns of `df_split_states_tidymodels`
df_split_states_tidymodels <- df_split_states_tidymodels %>%
     mutate(results_assessment = map2(.x = .$splits, .y = .$formula,
          ~ f_lm_states1_assessment (split = .x, formula = .y))) %>% 
     mutate(results_test = pmap(list(.$splits, .$formula, .$main_test_set),
          f_lm_states1_test))  


##
## 7. Evaluate the model performance with `yardstick` package

df_split_states_tidymodels <- df_split_states_tidymodels %>%
     mutate(
          assessment_rmse_tidymodels = map_dbl(.x = .$results_assessment,
               ~ yardstick::rmse_vec (.x$observed, .x$predicted)), 
          test_rmse_tidymodels = map_dbl(.x = .$results_test,
               ~ yardstick::rmse_vec (.x$observed, .x$predicted)),        
          assessment_rsq_tidymodels = map_dbl(.x = .$results_assessment,
               ~ yardstick::rsq_vec (.x$observed, .x$predicted)), 
          test_rsq_tidymodels = map_dbl(.x = .$results_test,
               ~ yardstick::rsq_vec (.x$observed, .x$predicted))       
          
          )


metrics_states1_tidymodes <- df_split_states_tidymodels %>% 
     select (assessment_rmse_tidymodels:test_rsq_tidymodels) %>%
     ungroup() %>%
     summarise(
          mean_rmse_assessment_tidymodels = mean(assessment_rmse_tidymodels),
               sd_rmse_assessment_tidymodels = sd(assessment_rmse_tidymodels),
          mean_rmse_test_tidymodels = mean(test_rmse_tidymodels),
               sd_rmse_test_tidymodels = sd(test_rmse_tidymodels),
          mean_r2_assessment_tidymodels = mean(assessment_rsq_tidymodels),
               sd_r2_assessment_tidymodels = sd(assessment_rsq_tidymodels),
          mean_r2_test_tidymodels = mean(test_rsq_tidymodels),
               sd_r2_test_tidymodels = sd(test_rsq_tidymodels)
          ) %>%
     gather(statistic, value)   %>%
     arrange(model)


metrics_states1_modelr
metrics_states1_tidymodes



###############################################################
##             The same stuff, but "pipe"-oriented

# the function that returs a (linear) model
f_lm <- function(formula, data) {
     parsnip::linear_reg() %>%
          parsnip::set_engine("lm") %>%
          parsnip::fit(formula = formula, data = data)
}

# check the function
the_recipe <- recipes::recipe(formula = as.formula(formula_), 
     data = analysis(df_split_states_tidymodels$splits[[1]]))
the_data = analysis(df_split_states_tidymodels$splits[[1]])      
formula(the_recipe)

f_lm(formula(the_recipe), the_data)


##                  here the steps are combined
set.seed(123)  # Run to replicate this post
df_split_states_tidymodels_v2 <- rsample::vfold_cv(main_train__states, 
     v = 5, repeats = 1, strata = NULL) %>%
     mutate (main_test_set = list(main_test__states)) %>%
     mutate(formula = list(as.formula(formula_))) %>%
     mutate(recipe = map2(.x = .$splits, .y = .$formula,     ## recipe
          ~ recipes::recipe(.y, data = analysis(.x)))) %>%
     mutate(prep = map2(.x = .$recipe, .y = .$splits,     ## prep
          ~ recipes::prep(.x, data = analysis(.y))))  %>%
     mutate(                                                      ## bake
          analysis_processed = map2(.x = .$prep, .y = .$splits, 
               ~ recipes::bake(.x, analysis(.y))),
          assessment_processed = map2(.x = .$prep, .y = .$splits, 
               ~ recipes::bake(.x, assessment(.y))), 
          test_processed = map2(.x = .$prep, .y = .$main_test_set, 
               ~ recipes::bake(.x, .y))   ) %>%
     mutate (the_model = map2(.x = .$formula, .y = .$analysis_processed,  ## model fit
              ~ f_lm(.x, .y))) %>%
     mutate(                               # predicted values of `Murder` in assessment
                                           #  and main test set
          predicted_assessment = map2(.$the_model, .$assessment_processed, predict),
          predicted_test = map2(.$the_model, .$test_processed, predict)
          ) %>%
     mutate(                                               ## rmse and r2
          assessment_rmse_tidymodels = map2_dbl(
               .x = .$assessment_processed, .y = .$predicted_assessment,                                 
               ~ yardstick::rmse_vec (.x$Murder, .y$`.pred`)),
          test_rmse_tidymodels = map2_dbl(
               .x = .$test_processed, .y = .$predicted_test,                                 
               ~ yardstick::rmse_vec (.x$Murder, .y$`.pred`)),
          assessment_rsq_tidymodels = map2_dbl(
               .x = .$assessment_processed, .y = .$predicted_assessment,                                 
               ~ yardstick::rsq_vec (.x$Murder, .y$`.pred`)),
          test_rsq_tidymodels = map2_dbl(
               .x = .$test_processed, .y = .$predicted_test,                                 
               ~ yardstick::rsq_vec (.x$Murder, .y$`.pred`))
          ) 

# get the two metrics (RMSE and R2)
metrics_states1_tidymodes_v2 <- df_split_states_tidymodels_v2 %>% 
     select (assessment_rmse_tidymodels:test_rsq_tidymodels) %>%
     ungroup() %>%
     summarise(
          mean_rmse_assessment_tidymodels = mean(assessment_rmse_tidymodels),
               sd_rmse_assessment_tidymodels = sd(assessment_rmse_tidymodels),
          mean_rmse_test_tidymodels = mean(test_rmse_tidymodels),
               sd_rmse_test_tidymodels = sd(test_rmse_tidymodels),
          mean_r2_assessment_tidymodels = mean(assessment_rsq_tidymodels),
               sd_r2_assessment_tidymodels = sd(assessment_rsq_tidymodels),
          mean_r2_test_tidymodels = mean(test_rsq_tidymodels),
               sd_r2_test_tidymodels = sd(test_rsq_tidymodels)
          ) %>%
     gather(statistic, value)   %>%
     arrange(model)



#########################################################################
###                      III.b `insurance` data set                   ###
#########################################################################

## we already split the data in section `I.a`
main_train__insurance 
main_test__insurance 

# the function that returs a (linear) model defined in section III.a
f_lm <- function(formula, data) {
     parsnip::linear_reg() %>%
          parsnip::set_engine("lm") %>%
          parsnip::fit(formula = formula, data = data)
}

formula_ <- 'charges ~ .'

set.seed(123)  # Run to replicate this post
df_split_insurance_tidymodels <- rsample::vfold_cv(main_train__insurance, 
     v = 5, repeats = 1, strata = NULL) %>%
     mutate (main_test_set = list(main_test__insurance)) %>%
     mutate(formula = list(as.formula(formula_))) %>%
     mutate(recipe = map2(.x = .$splits, .y = .$formula,     ## recipe
          ~ recipes::recipe(.y, data = analysis(.x)))) %>%
     mutate(prep = map2(.x = .$recipe, .y = .$splits,     ## prep
          ~ recipes::prep(.x, data = analysis(.y))))  %>%
     mutate(                                                      ## bake
          analysis_processed = map2(.x = .$prep, .y = .$splits, 
               ~ recipes::bake(.x, analysis(.y))),
          assessment_processed = map2(.x = .$prep, .y = .$splits, 
               ~ recipes::bake(.x, assessment(.y))), 
          test_processed = map2(.x = .$prep, .y = .$main_test_set, 
               ~ recipes::bake(.x, .y))   ) %>%
     mutate (the_model = map2(.x = .$formula, .y = .$analysis_processed,  ## model fit
              ~ f_lm(.x, .y))) %>%
     mutate(                               # predicted values of `Murder` in assessment
                                           #  and main test set
          predicted_assessment = map2(.$the_model, .$assessment_processed, predict),
          predicted_test = map2(.$the_model, .$test_processed, predict)
          ) %>%
     mutate(                                               ## rmse and r2
          assessment_rmse_tidymodels = map2_dbl(
               .x = .$assessment_processed, .y = .$predicted_assessment,                                 
               ~ yardstick::rmse_vec (.x$charges, .y$`.pred`)),
          test_rmse_tidymodels = map2_dbl(
               .x = .$test_processed, .y = .$predicted_test,                                 
               ~ yardstick::rmse_vec (.x$charges, .y$`.pred`)),
          assessment_rsq_tidymodels = map2_dbl(
               .x = .$assessment_processed, .y = .$predicted_assessment,                                 
               ~ yardstick::rsq_vec (.x$charges, .y$`.pred`)),
          test_rsq_tidymodels = map2_dbl(
               .x = .$test_processed, .y = .$predicted_test,                                 
               ~ yardstick::rsq_vec (.x$charges, .y$`.pred`))
          ) 


# get the two metrics (RMSE and R2)
metrics_insurance_tidymodes <- df_split_insurance_tidymodels %>% 
     select (assessment_rmse_tidymodels:test_rsq_tidymodels) %>%
     ungroup() %>%
     summarise(
          mean_rmse_assessment_tidymodels = mean(assessment_rmse_tidymodels),
               sd_rmse_assessment_tidymodels = sd(assessment_rmse_tidymodels),
          mean_rmse_test_tidymodels = mean(test_rmse_tidymodels),
               sd_rmse_test_tidymodels = sd(test_rmse_tidymodels),
          mean_r2_assessment_tidymodels = mean(assessment_rsq_tidymodels),
               sd_r2_assessment_tidymodels = sd(assessment_rsq_tidymodels),
          mean_r2_test_tidymodels = mean(test_rsq_tidymodels),
               sd_r2_test_tidymodels = sd(test_rsq_tidymodels)
          ) %>%
     gather(statistic, value)   %>%
     arrange(model)


