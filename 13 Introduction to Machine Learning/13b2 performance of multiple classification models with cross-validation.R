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
###         13.b.2. Performance evaluation of multiple classification    ###
### models with the `tidymodels` ecosystem using simple Cross-Validation ### 
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 28.11.2019

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
## for validation based on simple train-test split - 
##             see scripts `13b1a...` and `13b1b...` 

# repeated cross-validation of the logistic regression (in tidymodels)
# models for this data set is also available at:
# https://rileyking.netlify.com/post/heart-disease-prediction-from-patient-data-in-r/


glimpse(heart)

# we will cross-validate only the top 10 models based on 
# ROC_AUC metric (see script `13b1b`)
load(file = 'df_and_the_formulas__heart.RData')
glimpse(the_formulas__heart)
load(file = 'performance_glm__heart2.RData')
glimpse(performance_glm__heart2)
head(performance_glm__heart2)

selected_models__heart__3 <- performance_glm__heart2 %>%
     filter (.metric == 'roc_auc') %>%
     arrange(desc(.estimate)) %>%
     top_n(10, .estimate)


#########################################################################
###        I.a Model building and assessment with `tidymodels`        ###
#########################################################################

#########################################################################
###   1. Train/test split, then cross-validation of the train subset  ###

# initial train-test split
set.seed(seed = 1234) 
train_test_split__heart_3 <-
  rsample::initial_split(
    data = heart,     
    prop = 0.66
  ) 
train_test_split__heart_3

train_tbl__heart_3 <- train_test_split__heart_3 %>% training() 
test_tbl__heart_3  <- train_test_split__heart_3 %>% testing()


## now, create 5 times a 10 fold cross-validation on the main train subset
set.seed(12345)
cv_train__heart_3 <- vfold_cv(train_tbl__heart_3, v = 10, repeats = 5)


#########################################################################
###      2. Combine all selected models formulas with the folds       ###
the_pipe_init__heart_3 <- selected_models__heart__3 %>%
     select (model_id, formula) %>%
     mutate(the_formula = map(.$formula, as.formula)) %>%
     mutate (foo = 1) %>%
     full_join(cv_train__heart_3 %>% mutate (foo = 1))


#########################################################################
###     3. We slightly change the recipe for missing values imputation

recipe__heart_3 <- function(formula, dataset) {
     recipe(formula, 
            data = dataset) %>%
     step_knnimpute(all_predictors(), neighbors = 3) %>%  # missing values treatment
     prep(data = dataset)
}

#  we build incrementally data frames containing the main ingredients of the models

#  this commands will take a while (where `while` could be minutes)  
the_pipe__heart_3 <- the_pipe_init__heart_3 %>% 
     mutate (analysis_data = map(splits, analysis), 
             assessment_data = map(splits, assessment) ) %>%
     mutate (prep = map2( .x = .$the_formula, .y = analysis_data, 
                          .f =  recipe__heart_3 )) %>%
     mutate (
          analysis_juiced = map(.x = .$prep, .f = juice),
          assessment_baked = map2(.x = .$prep, .y = .$assessment_data, .f = bake)
          )     


#########################################################################
###                      4. Fit the models

# the same function as in the previous script
glm_model__heart_3 <- function(formula, dataset) {
     logistic_reg(mode = "classification") %>%
          set_engine("glm") %>%
          fit(as.formula(formula), data = dataset)
}


##  this commands will take a (larger) while !!!!! 
the_pipe_with_models__heart_3 <- the_pipe__heart_3 %>%
     mutate (the_model = map2(.x = .$the_formula, .y = .$analysis_juiced,
                              .f = glm_model__heart_3))



#######################################################################
###             5. Performance assessment of the models
#######################################################################

######################################################################
###                      5.1 Model predictions

# function that returns (as tibble) the predictions as classes
f_pred_class__heart_3 <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked, type = "class") %>%
          # Add the baked test data back in
          bind_cols(assessment_baked  %>% select(AHD) )  
}

# function that returns (as tibble) the predictions as probabilities
f_pred_prob__heart_3 <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked, type = "prob") %>%
          # Add the baked test data back in
          bind_cols(assessment_baked  %>% select(AHD) )  
}


predictions_glm__heart_3 <- the_pipe_with_models__heart_3 %>%
     mutate (
          predict_class = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_pred_class__heart_3),
          predict_prob = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_pred_prob__heart_3)            
             )


#########################################################################
###    5.2 Compute Accuracy, precision, recall, F1-score and AUC for
###    each KV model

f_performance_glm__heart_3 <- function (predict_class, predict_prob) {
     bind_rows(
        predict_class %>%
               metrics(truth = AHD, estimate = .pred_class), 
        precision(predict_class, AHD, .pred_class),
        recall(predict_class, AHD, .pred_class),
        f_meas(predict_class, AHD, .pred_class),
        roc_auc(predict_prob, AHD, .pred_Yes)
            ) 
}
     

# get all the metrics for all KV models     
performance_glm__heart_3 <- predictions_glm__heart_3 %>%
     mutate (the_metrics = map2(.x = .$predict_class, .y = .$predict_prob, 
               .f = f_performance_glm__heart_3)) %>%
     select (model_id, id, id2, formula, the_metrics) %>%
     unnest(the_metrics)

# save for later use
save(performance_glm__heart_3, file = 'performance_glm__heart_3.RData')


# extract top 5 models, by aggregating (among folds) their ROC_AUC
top_5__heart_3 <- performance_glm__heart_3 %>%
     filter (.metric == 'roc_auc') %>%
     group_by(model_id, formula) %>%
     summarise(estimate = mean(.estimate)) %>%
     arrange(desc(estimate)) %>%
     top_n(5, estimate)



# extract the coefficients of the best model
library(broom)

temp <- the_pipe_with_models__heart_3 %>%
     filter (formula == top_5__heart_3$formula[1]) %>%
     pull(the_model[[1]]) %>%
     pluck(1) %>%
     broom::tidy() %>%
     mutate(odds_ratio = exp(estimate)) 
View(temp)








