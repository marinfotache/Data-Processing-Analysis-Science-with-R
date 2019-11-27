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
###       13.b.1.b. Performance evaluation of multiple claasification    ###
###  models with the `tidymodels` ecosystem using a train-test split     ### 
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 27.11.2019

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
load('ml_datasets.RData')


#####################################################################
###                      II.1 Heart disease                       ###
#####################################################################
## for the description and EDA - see script `09c...``
## for logistic regression models fit - see scripts `11b...` 
glimpse(heart)


#########################################################################
###        II.1.a Model building and assessment with `tidymodels`     ###
#########################################################################

#########################################################################
###             0. Build a data frame all predictor combinations 

# this is the vector with all possible (simple) predictors
predictors <- setdiff(names(heart), c('AHD'))

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

the_formulas__heart <- df_all_possible_predictors %>%
     tidyr::unite("formula", predictor_1:predictor_7,
                  sep = ' + ', na.rm = TRUE, remove = FALSE) %>%
     transmute (model_id, formula = paste('AHD', formula, sep = ' ~ ')) %>%
     mutate(the_formula = map(.$formula, as.formula))
     
glimpse(the_formulas__heart)

# save the formulas data frame for later use
getwd()
#save(heart, the_formulas__heart, file = 'df_and_the_formulas__heart.RData')


#########################################################################
###                        1. Train and test split                   ###

# train-test split
set.seed(seed = 1234) 
train_test_split__heart_2 <-
  rsample::initial_split(
    data = heart,     
    prop = 0.66
  ) 
train_test_split__heart_2

train_tbl__heart_2 <- train_test_split__heart_2 %>% training() 
test_tbl__heart_2  <- train_test_split__heart_2 %>% testing()


#########################################################################
###                            2. A simple recipe

simple_recipe__heart_2 <- function(formula, dataset) {
     recipe(formula, 
            data = dataset) %>%
     prep(data = dataset)
}

#  we build incrementally data frames containing the main ingredients of the models
the_pipe__heart_2 <- the_formulas__heart %>% 
     mutate (train = list(train_tbl__heart_2), test = list(test_tbl__heart_2)) %>%
     mutate (prep = map2( .x = .$the_formula, .y = train, .f =  simple_recipe__heart_2 )) %>%
     mutate (
          train_juiced = map(.x = .$prep, .f = juice),
          test_baked = map2(.x = .$prep, .y = .$test, .f = bake)
          )     
     

#########################################################################
###                      3. Fit the models

glm_model__heart_2 <- function(formula, dataset) {
     logistic_reg(mode = "classification") %>%
          set_engine("glm") %>%
          fit(as.formula(formula), data = dataset)
}

# this could take many, many, many minutes..., so you can dowload it from 
# OneDrive (~ 273 MB)...
# https://1drv.ms/u/s!AgPvmBEDzTOSiPA_EhubOwfRdTZ1ng?e=Pyr39R

# ...and then:
# load(file = 'the_pipe_with_models__heart_2.RData')

the_pipe_with_models__heart_2 <- the_pipe__heart_2 %>%
     mutate (the_model = map2(.x = .$the_formula, .y = .$train_juiced,
                              .f = glm_model__heart_2))

#no need to run this; a used it for saving the above data set
#save(the_pipe_with_models__heart_2, file = 'the_pipe_with_models__heart_2.RData')


#######################################################################
###             4. Performance assessment of the models
#######################################################################

######################################################################
###                      4.1 Model predictions


# function that returns (as tibble) the predictions as classes
f_pred_class__heart_2 <- function(the_model, test_baked) {
     the_model %>%
          predict(new_data = test_baked, type = "class") %>%
          # Add the baked test data back in
          bind_cols(test_baked  %>% select(AHD) )  
}

# function that returns (as tibble) the predictions as probabilities
f_pred_prob__heart_2 <- function(the_model, test_baked) {
     the_model %>%
          predict(new_data = test_baked, type = "prob") %>%
          # Add the baked test data back in
          bind_cols(test_baked  %>% select(AHD) )  
}


predictions_glm__heart_2 <- the_pipe_with_models__heart_2 %>%
     mutate (
          predict_class = map2(.x = .$the_model, .y = .$test_baked, 
               .f = f_pred_class__heart_2),
          predict_prob = map2(.x = .$the_model, .y = .$test_baked, 
               .f = f_pred_prob__heart_2)            
             )


#########################################################################
###    4.2 Compute Accuracy, precision, recall, F1-score and AUC for
###    each model

f_performance_glm__heart_2 <- function (predict_class, predict_prob) {
     bind_rows(
        predict_class %>%
               metrics(truth = AHD, estimate = .pred_class), 
        precision(predict_class, AHD, .pred_class),
        recall(predict_class, AHD, .pred_class),
        f_meas(predict_class, AHD, .pred_class),
        roc_auc(predict_prob, AHD, .pred_Yes)
            ) 
}
     

# get all the metrics for all the models     
performance_glm__heart2 <- predictions_glm__heart_2 %>%
     mutate (the_metrics = map2(.x = .$predict_class, .y = .$predict_prob, 
               .f = f_performance_glm__heart_2)) %>%
     select (model_id, formula, the_metrics) %>%
     unnest(the_metrics)


# extract top 5 models, by ROC_AUC
top_5__heart_2 <- performance_glm__heart2 %>%
     filter (.metric == 'roc_auc') %>%
     arrange(desc(.estimate)) %>%
     top_n(5, .estimate)



# extract the coefficients of the best model
library(broom)

temp <- the_pipe_with_models__heart_2 %>%
     filter (formula == top_5__heart_2$formula[1]) %>%
     pull(the_model[[1]]) %>%
     pluck(1) %>%
     broom::tidy() %>%
     mutate(odds_ratio = exp(estimate)) 
View(temp)

     


