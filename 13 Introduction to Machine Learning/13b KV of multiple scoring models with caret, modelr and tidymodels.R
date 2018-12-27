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
###          13b. Introduction to Machine Learning with `caret`,         ### 
###      `modelr` and `tidymodels` packages.                             ### 
###       Cross-validation of multiple scoring (regression) models       ###   
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
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
### O. Import and prepare the data sets; also build a tibble with     ###
###       all possible combinations of predictors_states              ###
###  I. Estimate performance of multiple linear regression models     ###
###       with cross-validation  using `caret` package                ###
###  II. Estimate performance of multiple linear regression models    ###
###       with cross-validation  using `modelr` package               ###
###  III. Estimate performance of multiple linear regression models   ###
###       with cross-validation  using `tidymodels` packages          ###
#########################################################################


#########################################################################
#### O. Import and prepare the data sets; also build a tibble with    ###
###                all possible combinations of predictors_states     ###
########################################################################

#########################################################################
###     0.a. `States` (USA) data set - see scripts `11b` and `13a`    ###
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

## We'll reuse the code for main split from script `13a`:

## Set seed for reproducibility
set.seed(123)

## Split the data into train/test using an index of row numbers:
index__states <- caret::createDataPartition(y = states$Murder, p = .7, list = FALSE)
main_train__states <- states[index__states, ] %>%
     select (-State)
main_test__states <- states[-index__states, ] %>%
     select (-State)

# descriptive statistics
states %>%
     skimr::skim()

main_train__states %>%
     skimr::skim()

main_test__states %>%
     skimr::skim()



#########################################################################
###  Build a data frame with all possible predictor combinations for 
###                      `states` data set

## one-variable predictors set
predictors_states <- states %>% 
     select (-State, -Murder) %>%
     names() %>%
     sort()

## two-variable predictors set
two_predictors_states <- tibble(predictor1 = predictors_states, 
          foo = 1) %>%
     full_join(tibble(predictor2 = predictors_states, foo = 1)) %>%
     filter (predictor1 != predictor2) %>%
     select (-foo) %>%
     mutate (id = row_number()) %>%
     gather(predictor_no, predictor_name, -id) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)


## three-variable predictors set
three_predictors_states_init <- tibble(predictor1 = predictors_states, 
          foo = 1) %>%
     full_join(tibble(predictor2 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor3 = predictors_states, foo = 1)) %>%
     select (-foo) %>%
     mutate (id = row_number()) %>%
     gather(predictor_no, predictor_name, -id) %>%
     distinct(id, predictor_name) %>%
     arrange(id, predictor_name) 

three_predictors_states <- three_predictors_states_init %>%
     semi_join( three_predictors_states_init %>%
                         group_by(id) %>%
                         tally() %>%
                         filter (n == 3) 
             ) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)


## four-variable predictors set
four_predictors_states_init <- tibble(predictor1 = predictors_states, 
          foo = 1) %>%
     full_join(tibble(predictor2 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor3 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor4 = predictors_states, foo = 1)) %>%
     select (-foo) %>%
     mutate (id = row_number()) %>%
     gather(predictor_no, predictor_name, -id) %>%
     distinct(id, predictor_name) %>%
     arrange(id, predictor_name) 

four_predictors_states <- four_predictors_states_init %>%
     semi_join( four_predictors_states_init %>%
                         group_by(id) %>%
                         tally() %>%
                         filter (n == 4) 
             ) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)


## five-variable predictors set
five_predictors_states_init <- tibble(predictor1 = predictors_states, 
          foo = 1) %>%
     full_join(tibble(predictor2 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor3 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor4 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor5 = predictors_states, foo = 1)) %>%
     select (-foo) %>%
     mutate (id = row_number()) %>%
     gather(predictor_no, predictor_name, -id) %>%
     distinct(id, predictor_name) %>%
     arrange(id, predictor_name) 

five_predictors_states <- five_predictors_states_init %>%
     semi_join( five_predictors_states_init %>%
                         group_by(id) %>%
                         tally() %>%
                         filter (n == 5) 
             ) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)


## six-variable predictors set
six_predictors_states_init <- tibble(predictor1 = predictors_states, 
          foo = 1) %>%
     full_join(tibble(predictor2 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor3 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor4 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor5 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor6 = predictors_states, foo = 1)) %>%
     select (-foo) %>%
     mutate (id = row_number()) %>%
     gather(predictor_no, predictor_name, -id) %>%
     distinct(id, predictor_name) %>%
     arrange(id, predictor_name) 

six_predictors_states <- six_predictors_states_init %>%
     semi_join( six_predictors_states_init %>%
                         group_by(id) %>%
                         tally() %>%
                         filter (n == 6) 
             ) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)


## seven-variable predictors set
seven_predictors_states_init <- tibble(predictor1 = predictors_states, 
          foo = 1) %>%
     full_join(tibble(predictor2 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor3 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor4 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor5 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor6 = predictors_states, foo = 1)) %>%
     full_join(tibble(predictor7 = predictors_states, foo = 1)) %>%
     select (-foo) %>%
     mutate (id = row_number()) %>%
     gather(predictor_no, predictor_name, -id) %>%
     distinct(id, predictor_name) %>%
     arrange(id, predictor_name) 

seven_predictors_states <- seven_predictors_states_init %>%
     semi_join( seven_predictors_states_init %>%
                         group_by(id) %>%
                         tally() %>%
                         filter (n == 7) 
             ) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)


## bind all predictor combinations
all_predictors_states <- bind_rows(
     tibble(predictor_list = predictors_states),
     two_predictors_states,
     three_predictors_states,
     four_predictors_states,
     five_predictors_states,
     six_predictors_states,
     seven_predictors_states
)

rm(two_predictors_states, three_predictors_states_init, 
   three_predictors_states, four_predictors_states_init, 
   four_predictors_states, five_predictors_states_init,
   five_predictors_states, six_predictors_states_init, 
   six_predictors_states,seven_predictors_states_init, 
   seven_predictors_states)



#######################################################################
### 	         0.b.  insurance` data set - see script `13a`          ###
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


## Set seed for reproducibility
set.seed(123)

## Split the data into train/test using an index of row numbers:
index__insurance <- caret::createDataPartition(y = insurance$charges, 
          p = .8, list = FALSE)
main_train__insurance <- insurance[index__insurance, ]
main_test__insurance <- insurance[-index__insurance, ]


# descriptive statistics
insurance %>%
     skimr::skim()

main_train__insurance %>%
     skimr::skim()

main_test__insurance %>%
     skimr::skim()


#########################################################################
###  Build a data frame with all possible predictor combinations for 
###                           `insurance` data set

## one-variable predictors_insurance set
predictors_insurance <- insurance %>% 
     select (-charges) %>%
     names() %>%
     sort()

all_predictors_insurance <- tibble()

# Compared with the solution for `states` data set, we'll try something
#    more general

# build `m` (where m = number of variables) data frames of 
# `predictors_insurance`
for (i in 1:length(predictors_insurance)) {
     temp <- tibble(predictor = predictors_insurance) %>%
          set_names(paste0(names(.), i)) %>% 
          mutate (foo = 1)
     assign(names(temp), temp)
}


# now build all the possible combinations
for (n_of_variables_in_predictor in 1:length(predictors_insurance)) {
     
     # full join all the `predictor<<i>>` tibbles, 
     #    where i = 1, ..., `n_of_variables_in_predictor`
     for (i in 1:n_of_variables_in_predictor) {
          if (i == 1) {
               temp <- predictor1
          }  else {
               temp <- temp %>%
                    full_join(get(paste0('predictor', i)))
          }
     }
     
     # build the raw data frame with `n_of_variables_in_predictor` predictors
     temp <- temp %>%
          select (-foo) %>%
          mutate (id = row_number()) %>%
          gather(predictor_no, predictor_name, -id) %>%
          distinct(id, predictor_name) %>%
          arrange(id, predictor_name) 
          
     # refine the tibble, removing duplicated combinations
     temp <- temp %>%
          semi_join( temp %>%
                         group_by(id) %>%
                         tally() %>%
                         filter (n == n_of_variables_in_predictor) 
             ) %>%
     arrange(id, predictor_name) %>%
     group_by(id) %>%
     summarise(predictor_list = paste(predictor_name, collapse = " + ")) %>%
     ungroup() %>%
     distinct(predictor_list)
    
     # add current predictors tibble to the main one
     all_predictors_insurance <- bind_rows(all_predictors_insurance, 
          temp)
     
}

# remove 
rm(list = ls(pattern = '^predictor'))




#########################################################################
###  I. Estimate performance of multiple linear regression models     ###
###            with cross-validation  using `caret` package           ###
#########################################################################


#########################################################################
###                      I.a. `States` (USA) data set                 ###
#########################################################################

## we already split the data in section `0.a`
main_train__states 
main_test__states 

# we also set up the resampling, here repeated CV
tr_states1 <- trainControl(method = "cv", number = 5)

#########################################################################
##                  Build a tibble with all model predictions
## we-ll reuse some stuff from section `I.a`
## 

# initialize the tibble
states_all_models_predictions_caret <- tibble()

# build the tibble, looping through the predictors_states (this loop can take a
# couple of minutes)
for (i in 1:nrow(all_predictors_states))  {
     crt_formula <- paste('Murder', 
          paste(all_predictors_states$predictor_list[i], 
               collapse = ' + '), sep = ' ~ ')

     # train the current model using 5-fold CV
     crt_model <- train(formula(crt_formula), data = main_train__states, 
                    method = "lm", trControl = tr_states1, trace = FALSE)

     states_all_models_predictions_caret <- bind_rows(
          states_all_models_predictions_caret,
          tibble(fitted = predict(crt_model, main_test__states)) %>%
               transmute (model = crt_formula, 
                          observed = main_test__states$Murder, fitted)
     )
}

#########################################################################
##    Evaluate the performance of the models on the main test set
##                                 using RMSE

states_all_models_predictions_caret %>%
     group_by(model) %>%
     summarise(mean_rmse = caret::RMSE (fitted, observed)) %>%
     arrange(mean_rmse)
     

#########################################################################
###                        I.b. `insurance` data set                  ###
#########################################################################

## we already split the data in section `0.b`
main_train__insurance
main_test__insurance

# we also set up the resampling, here 10-fold CV
tr_insurance1 <- trainControl(method = "cv", number = 10)

#########################################################################
##                  Build a tibble with all model predictions
## we-ll reuse some stuff from section `I.a`
## 

# initialize the tibble
insurance_all_models_predictions_caret <- tibble()

# build the tibble, looping through the predictors_states (this loop can take a
# couple of minutes)
for (i in 1:nrow(all_predictors_insurance))  {
     crt_formula <- paste('charges', paste(
          all_predictors_insurance$predictor_list[i], 
               collapse = ' + '), sep = ' ~ ')

     # train the current model using 10-fold CV
     crt_model <- train(formula(crt_formula), data = main_train__insurance, 
                    method = "lm", trControl = tr_insurance1, trace = FALSE)

     insurance_all_models_predictions_caret <- bind_rows(
          insurance_all_models_predictions_caret,
          tibble(fitted = predict(crt_model, main_test__insurance)) %>%
               transmute (model = crt_formula, 
                          observed = main_test__insurance$charges, 
                          fitted)
     )
}

#########################################################################
##    Evaluate the performance of the models on the main test set
##                                 using RMSE

insurance_all_models_predictions_caret %>%
     group_by(model) %>%
     summarise(mean_rmse = caret::RMSE (fitted, observed)) %>%
     arrange(mean_rmse)




#########################################################################
###  II. Estimate performance of multiple linear regression models    ###
###       with cross-validation  using `modelr` package               ###
#########################################################################

#########################################################################
###                      II.a. `States` (USA) data set                ###
#########################################################################

# we did the main split in section `I.a`

# now, we'll create 5 folds from `main_train__states` subset 
set.seed(123)  # Run to replicate this post
df_split_states_modelr <- main_train__states %>%
     modelr::crossv_kfold (k = 5) %>%
     transmute (analysis = train, assessment = test, .id) %>%
     mutate(main_test__states = list(main_test__states)) %>%
     print()


## build all the models and folds
states_models_and_folds_all_modelr <- df_split_states_modelr %>%
     mutate (foo = 1) %>%
     full_join(all_predictors_states %>%
                    mutate (foo = 1)) %>%
     select (-foo) %>%
     mutate (formula = paste('Murder ~', predictor_list)) %>%    
     mutate(model = map2 (.x = formula, .y = analysis, 
                          ~ lm(formula = .x, data = .y)))


## compute the predicted values with `augment` function from `broom` package. 
states_models_and_folds_all_modelr <- states_models_and_folds_all_modelr %>% 
     mutate(predicted_assessment = map2(model, assessment, 
          ~ augment(.x, newdata = .y)))  %>%
     mutate(predicted_test = map2(model, main_test__states, 
          ~ augment(.x, newdata = .y)))


## To extract the relevant information from these predicted results, 
## we’ll unnest the data frames:
states_predicted_assessment_all_modelr <- states_models_and_folds_all_modelr %>%
  unnest(predicted_assessment)

states_predicted_test_all_modelr <- states_models_and_folds_all_modelr %>%
  unnest(predicted_test)


###                      Evaluate the model performance

## metrics with  `modelr`
states_models_and_folds_all_modelr <- states_models_and_folds_all_modelr %>% 
     mutate (
          rmse_modelr_assessement = map2_dbl(model, assessment, modelr::rmse),
          rmse_modelr_test = map2_dbl(model, main_test__states, modelr::rmse),
          rsq_modelr_assessement = map2_dbl(model, assessment, modelr::rsquare),
          rsq_modelr_test = map2_dbl(model, main_test__states, modelr::rsquare)             
             )

names(states_models_and_folds_all_modelr)

states_models_and_folds_all_modelr %>%
     select(`.id`, formula, rmse_modelr_assessement:rsq_modelr_test) %>%
     print()
     

## order the cross-validated models on `rmse_modelr_test`
states_ranked_models_by_rmse_test_modelr <- states_models_and_folds_all_modelr %>%
     group_by(formula) %>%
     summarise(mean__test_rmse = mean(rmse_modelr_test)) %>%
     arrange(mean__test_rmse)


states_ranked_models_by_rsq_test_modelr <- states_models_and_folds_all_modelr %>%
     group_by(formula) %>%
     summarise(mean__test_rsq = mean(rsq_modelr_test)) %>%
     arrange(desc(mean__test_rsq))


## metrics with `yardstick` 
class_metrics <- metric_set(rmse, rsq, rsq_trad)

states_metrics_yardstick_all_modelr <- states_predicted_test_all_modelr %>%
     group_by(formula) %>%
     class_metrics(Murder, estimate = .fitted)

states_metrics_yardstick_all_modelr %>%
     spread (".metric", ".estimate") %>%
     arrange(rmse)



#########################################################################
###                        II.b. `insurance` data set                ###
#########################################################################

# we did the main split in section `I.b`

# now, we'll create 10 folds from `main_train__insurance` subset 
set.seed(123)  # Run to replicate this post
df_split_insurance_modelr <- main_train__insurance %>%
     modelr::crossv_kfold (k = 10) %>%
     transmute (analysis = train, assessment = test, .id) %>%
     mutate(main_test__insurance = list(main_test__insurance)) %>%
     print()


## build all the models and folds
insurance_models_and_folds_all_modelr <- df_split_insurance_modelr %>%
     mutate (foo = 1) %>%
     full_join(all_predictors_insurance %>%
                    mutate (foo = 1)) %>%
     select (-foo) %>%
     mutate (formula = paste('charges ~', predictor_list)) %>%    
     mutate(model = map2 (.x = formula, .y = analysis, 
                          ~ lm(formula = .x, data = .y)))


## compute the predicted values with `augment` function from `broom` package. 
insurance_models_and_folds_all_modelr <- insurance_models_and_folds_all_modelr %>% 
     mutate(predicted_assessment = map2(model, assessment, 
          ~ augment(.x, newdata = .y)))  %>%
     mutate(predicted_test = map2(model, main_test__insurance, 
          ~ augment(.x, newdata = .y)))


## To extract the relevant information from these predicted results, 
## we’ll unnest the data frames:
insurance_predicted_assessment_all_modelr <- insurance_models_and_folds_all_modelr %>%
  unnest(predicted_assessment)

insurance_predicted_test_all_modelr <- insurance_models_and_folds_all_modelr %>%
  unnest(predicted_test)


###                      Evaluate the model performance

## metrics with  `modelr`
insurance_models_and_folds_all_modelr <- insurance_models_and_folds_all_modelr %>% 
     mutate (
          rmse_modelr_assessement = map2_dbl(model, assessment, modelr::rmse),
          rmse_modelr_test = map2_dbl(model, main_test__insurance, modelr::rmse),
          rsq_modelr_assessement = map2_dbl(model, assessment, modelr::rsquare),
          rsq_modelr_test = map2_dbl(model, main_test__insurance, modelr::rsquare)             
             )

names(insurance_models_and_folds_all_modelr)

insurance_models_and_folds_all_modelr %>%
     select(`.id`, formula, rmse_modelr_assessement:rsq_modelr_test) %>%
     print()
     

## order the cross-validated models on `rmse_modelr_test`
insurance_ranked_models_by_rmse_test_modelr <- insurance_models_and_folds_all_modelr %>%
     group_by(formula) %>%
     summarise(mean__test_rmse = mean(rmse_modelr_test)) %>%
     arrange(mean__test_rmse)


insurance_ranked_models_by_rsq_test_modelr <- insurance_models_and_folds_all_modelr %>%
     group_by(formula) %>%
     summarise(mean__test_rsq = mean(rsq_modelr_test)) %>%
     arrange(desc(mean__test_rsq))


## metrics with `yardstick` 
class_metrics <- metric_set(rmse, rsq, rsq_trad)

insurance_metrics_yardstick_all_modelr <- insurance_predicted_test_all_modelr %>%
     group_by(formula) %>%
     class_metrics(charges, estimate = .fitted)

insurance_metrics_yardstick_all_modelr %>%
     spread (".metric", ".estimate") %>%
     arrange(rmse)



#########################################################################
###  III. Estimate performance of multiple linear regression models   ###
###       with cross-validation  using `tidymodels` packages          ###
#########################################################################

#########################################################################
###                     III.a.  `States` (USA) data set                ###
#########################################################################

## we already split the data in section `I.a`
main_train__states 
main_test__states 

# we'll re-use the function that returs a (linear) model 
# built in section `I.c` of script `13a`
f_model <- function(formula, data) {
     parsnip::linear_reg() %>%
          parsnip::set_engine("lm") %>%
          parsnip::fit(formula = formula, data = data)
}

# ... and the `all_predictors_states` data frame
all_predictors_states

##                  here the steps are combined
set.seed(123)  # Run to replicate this post
df_split_states_all_tidymodels <- rsample::vfold_cv(main_train__states, 
               v = 5, repeats = 1, strata = NULL) %>%
     mutate (main_test_set = list(main_test__states)) %>%
     mutate (foo = 1) %>%
     full_join(all_predictors_states %>%
                    mutate (foo = 1)) %>%
     select (-foo) %>%
     mutate (formula = paste("Murder ~", predictor_list)) %>%
     mutate(recipe = map2(.x = .$splits, .y = .$formula,     ## recipe
          ~ recipes::recipe(as.formula(.y), data = analysis(.x)))) %>%
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
              ~ f_model(as.formula(.x), .y))) %>%
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
metrics_states1_all_tidymodes <- df_split_states_all_tidymodels %>% 
     select (formula, assessment_rmse_tidymodels:test_rsq_tidymodels) %>%
     group_by(formula) %>%
     summarise(
          mean_rmse_assessment_tidymodels = mean(assessment_rmse_tidymodels),
               sd_rmse_assessment_tidymodels = sd(assessment_rmse_tidymodels),
          mean_rmse_test_tidymodels = mean(test_rmse_tidymodels),
               sd_rmse_test_tidymodels = sd(test_rmse_tidymodels),
          mean_r2_assessment_tidymodels = mean(assessment_rsq_tidymodels),
               sd_r2_assessment_tidymodels = sd(assessment_rsq_tidymodels),
          mean_r2_test_tidymodels = mean(test_rsq_tidymodels),
               sd_r2_test_tidymodels = sd(test_rsq_tidymodels)
          ) 


