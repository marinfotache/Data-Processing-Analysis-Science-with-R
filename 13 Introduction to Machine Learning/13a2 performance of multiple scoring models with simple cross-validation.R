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
###         13.a.2. Performance evaluation of multiple scoring  models   ###
###    with the `tidymodels` ecosystem using simple Cross-Validation     ### 
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 22.11.2019

options(scipen = 999)
library(tidyverse)
library(tidymodels)


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
###                         I. `States` data set                   ###
#########################################################################
## for the description and EDA - see script `09c...``
## for linier models fit - see scripts `11a2...` and `11a5....`

# load the data and formulas saves in script `13a1b...``
load(file = 'df_and_the_formulas__states.RData')
glimpse(states)
glimpse(the_formulas__states)



#########################################################################
###        I.a Model building and assessment with `tidymodels`        ###
#########################################################################


#########################################################################
###   1. Train/test split, then cross-validation of the train subset  ###

# initial train-test split
set.seed(seed = 1234) 
train_test_split__states_3 <-
  rsample::initial_split(
    data = states,     
    prop = 0.80
  ) 
train_test_split__states_3

train_tbl__states_3 <- train_test_split__states_3 %>% training() 
test_tbl__states_3  <- train_test_split__states_3 %>% testing()


## now, create 5 times a 5 fold cross-validation on the main train subset
set.seed(12345)
cv_train__states3 <- vfold_cv(train_tbl__states_3, v = 5, repeats = 5)


#########################################################################
###               2. Combine all formulas with the folds              ###
the_pipe_init__states3 <- the_formulas__states %>%
     mutate (foo = 1) %>%
     full_join(cv_train__states3 %>% mutate (foo = 1))


#########################################################################
###                    3. The same simple recipe

simple_recipe__states_3 <- function(formula, dataset) {
     recipe(formula, 
            data = dataset) %>%
     prep(data = dataset)
}

#  we build incrementally data frames containing the main ingredients of the models

#  this commands will take a while (where `while` could be minutes)  
the_pipe__states_3 <- the_pipe_init__states3 %>% 
     mutate (analysis_data = map(splits, analysis), 
             assessment_data = map(splits, assessment) ) %>%
     mutate (prep = map2( .x = .$the_formula, .y = analysis_data, 
                          .f =  simple_recipe__states_3 )) %>%
     mutate (
          analysis_juiced = map(.x = .$prep, .f = juice),
          assessment_baked = map2(.x = .$prep, .y = .$assessment_data, .f = bake)
          )     


#########################################################################
###                      4. Fit the models

# the same function as in the previous script
lm_model__states_3 <- function(formula, dataset) {
     linear_reg() %>%
          set_engine("lm") %>%
          fit(as.formula(formula), data = dataset)
}


##  this commands will take a (larger) while !!!!! 

## I saved it and upload into `DataSets` directory (on GitHub),
## so you can download it into your working directory and load it:
# load ('the_pipe_with_models__states_3.RData')

the_pipe_with_models__states_3 <- the_pipe__states_3 %>%
     mutate (the_model = map2(.x = .$the_formula, .y = .$analysis_juiced,
                              .f = lm_model__states_3))

# here I saved the data frame for saving your time
# save(the_pipe_with_models__states_3, file = 'the_pipe_with_models__states_3.RData')



#######################################################################
###             5. Performance assessment of the models
#######################################################################

perf_metrics__states_3 <- metric_set(rmse, rsq, ccc)

# function that returnes (as tibble) the performance metrics for each model 
f_assessment__states_3 <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked) %>%
          # Add the baked assessment data back in
          bind_cols(assessment_baked) %>% 
          perf_metrics__states_3(Murder, .pred)
}

# get the prediction and three performance metrics for each model
the_pipe_with_metrics__states_3 <- the_pipe_with_models__states_3 %>%
     mutate (the_metrics = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_assessment__states_3))
     

# get the model performance as data frame for all folds
performance__states_3 <- the_pipe_with_metrics__states_3 %>%
     select (model_id, formula, id, id2, the_metrics) %>%
     unnest(the_metrics)
glimpse(performance__states_3)

# average the metrics 
average_model_performance__states_3 <- performance__states_3 %>%
     group_by(model_id, formula, .metric) %>%
     summarise (average_estimate = mean(.estimate)) %>%
     ungroup()



# In script `13a1a...` for model 1 the RMSE was 1.981765

# compare that value with the current results
average_model_performance__states_3 %>%
     filter (formula == 'Murder ~ Area + Frost + Illiteracy + Life_Exp + Population' &
                  .metric == 'rmse') %>%
     pull(average_estimate)





