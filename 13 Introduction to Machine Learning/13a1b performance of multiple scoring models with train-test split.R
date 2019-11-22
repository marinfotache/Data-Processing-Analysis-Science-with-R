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
###         13.a.1.b. Performance evaluation of multiple scoring         ###
###  models with the `tidymodels` ecosystem using a train-test split     ### 
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 21.11.2019

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
states <- as_tibble(state.x77, rownames = 'State')
names(states) <- str_replace_all(names(states), ' |\\.', '_')

# remove `States` column, as it is not relevant for the models
states <- states %>%
     select (-State)


#########################################################################
###        I.a Model building and assessment with `tidymodels`        ###
#########################################################################

#########################################################################
###                0. Build a data frame all predictor combinations 
### taken from script `11a5....` 

# this is the vector with all possible (simple) predictors
predictors <- setdiff(names(states), c('State', 'Murder'))

# start with just one predictor
df_one_predictor <- tibble(predictor = predictors) 

previous_df_predictors <- df_one_predictor %>%
     transmute(predictor_1 = predictor, foo = 1)

df_all_possible_predictors <- df_one_predictor %>%
     transmute (model_id = 1000000 + row_number(), predictor_1 = predictor)
     
# i <- 2
for (i in 2:length(predictors)) {
     # we cross join `previous_df_predictors` with `preod`
     col_name_ <- paste0('predictor_', i)
          
     crt_df_predictors <- previous_df_predictors %>%
          mutate (foo = 1) %>%
          full_join(df_one_predictor %>% mutate (foo = 1)) %>%
          select (-foo) %>%
          mutate (!!col_name_ := predictor) %>%
          select (-predictor) %>%
          mutate (model_id = i * 1000000 + row_number()) %>%
          pivot_longer(-model_id, names_to = "predictor_series", 
                       values_to = "predictor") %>%
          select (model_id, predictor) %>%
          arrange(model_id, predictor) %>%
          group_by(model_id, predictor) %>%
          summarise(n_of_occurences = n()) %>%
          filter (n_of_occurences == 1) %>%
          select (-n_of_occurences)%>%
          arrange(model_id, predictor) %>%
          group_by(model_id) %>%
          mutate (attrib_name = paste0('predictor_', row_number())) %>%
          ungroup() %>%
          tidyr::pivot_wider(names_from = "attrib_name", 
                             values_from = "predictor") %>%
          dplyr::select (-model_id) %>%
          distinct(.) %>%
          mutate(n_of_non_na = rowSums(!is.na(.))) %>%
          filter(n_of_non_na == i) %>%
          select (-n_of_non_na) %>%
          mutate(model_id = i * 1000000 + row_number())

     
     df_all_possible_predictors <- bind_rows(df_all_possible_predictors,
                     crt_df_predictors                        )
     
     previous_df_predictors <- crt_df_predictors %>%
          select (-model_id)
     
}     

the_formulas__states <- df_all_possible_predictors %>%
     tidyr::unite("formula", predictor_1:predictor_7,
                  sep = ' + ', na.rm = TRUE, remove = FALSE) %>%
     transmute (model_id, formula = paste('Murder', formula, sep = ' ~ ')) %>%
     mutate(the_formula = map(.$formula, as.formula))
     
glimpse(the_formulas__states)

# save the formulas data frame for later use
getwd()
save(states, the_formulas__states, file = 'df_and_the_formulas__states.RData')


#########################################################################
###                        1. Train and test split                   ###
# rsample provides a streamlined way to create a randomised 
#    training and test split of the original data.
set.seed(seed = 1234) 
train_test_split__states_2 <-
  rsample::initial_split(
    data = states,     
    prop = 0.66
  ) 
train_test_split__states_2

train_tbl__states_2 <- train_test_split__states_2 %>% training() 
test_tbl__states_2  <- train_test_split__states_2 %>% testing()


#########################################################################
###                            2. A simple recipe

simple_recipe__states_2 <- function(formula, dataset) {
     recipe(formula, 
            data = dataset) %>%
     prep(data = dataset)
}

#  we build incrementally data frames containing the main ingredients of the models
the_pipe__states_2 <- the_formulas__states %>% 
     mutate (train = list(train_tbl__states_2), test = list(test_tbl__states_2)) %>%
     mutate (prep = map2( .x = .$the_formula, .y = train, .f =  simple_recipe__states_2 )) %>%
     mutate (
          train_juiced = map(.x = .$prep, .f = juice),
          test_baked = map2(.x = .$prep, .y = .$test, .f = bake)
          )     
     

#########################################################################
###                      3. Fit the models

lm_model__states_2 <- function(formula, dataset) {
     linear_reg() %>%
          set_engine("lm") %>%
          fit(as.formula(formula), data = dataset)
}

# this could take some minutes
the_pipe_with_models__states_2 <- the_pipe__states_2 %>%
     mutate (the_model = map2(.x = .$the_formula, .y = .$train_juiced,
                              .f = lm_model__states_2))


#######################################################################
###             4. Performance assessment of the models
#######################################################################

perf_metrics__states_2 <- metric_set(rmse, rsq, ccc)

# function that returnes (as tibble) the performance metrics for each model 
f_assessment__states_2 <- function(the_model, test_baked) {

     the_model %>%
          predict(new_data = test_baked) %>%
          # Add the baked assessment data back in
          bind_cols(test_baked) %>% 
          perf_metrics__states_2(Murder, .pred)
}

# get the prediction and three performance metrics for each model
the_pipe_with_metrics__states_2 <- the_pipe_with_models__states_2 %>%
     mutate (the_metrics = map2(.x = .$the_model, .y = .$test_baked, 
               .f = f_assessment__states_2))
     

# get the model performance as data frame
performance__states_2 <- the_pipe_with_metrics__states_2 %>%
     select (model_id, formula, the_metrics) %>%
     unnest(the_metrics)


# order the models by `Concordance Correlation Coefficient`
temp1 <- performance__states_2 %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(.estimate))

# order the models by `R Squared`
temp2 <- performance__states_2 %>%
     filter (.metric == 'rsq') %>%
     arrange(desc(.estimate))


# Remember :-) that in previous script for model 1 the RMSE was 1.981765
# compare above value with the current results
performance__states_2 %>%
     filter (formula == 'Murder ~ Area + Frost + Illiteracy + Life_Exp + Population' &
                  .metric == 'rmse') %>%
     pull(.estimate)



