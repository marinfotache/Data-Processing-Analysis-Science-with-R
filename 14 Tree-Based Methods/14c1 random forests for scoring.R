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
###                 14c1. Random Forests for Scoring                     ###
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 07.01.2020

library(tidyverse)
library(caret)
library(randomForest)
library(ranger)
library(tidymodels)


############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
#setwd('/Users/admin/SkyDrive/DataSets')
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
###  O. Import and prepare the data sets                              ###
###  I. Random Forests for Regression Trees                           ###
###    I.1. Random Forests for Regression Trees with `randomForest`   ###
###    and `ranger`                                                   ###
###    I.2. Random Forests for Regression Trees with `tidymodels`     ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

### All the data sets and models (including variable importance plots
### and performance metrics) were saved at the end of scripts `14a1`
###  and `14b1`
### We'll procees from this point
load(file = '14a1_states.RData')
load(file = '14b1_states.RData')
load(file = '14a1_insurance.RData')
load(file = '14b1_insurance.RData')


#########################################################################
###              I. Random Forests for Regression Trees               ###
#########################################################################
## packages: `randomForest` and `ranger`

## hyperparameter: `mtry` - number of randomly selected predictors
##   to choose from at each split (node)
## For regression setting, Breiman recommends setting `mtry` on 
##   1/3 of the number of predictors (that is the default value
##        chosen by `randomForest` and `ranger`) (Kuhn & Johnson, 213, p.199)

## other parameter: `ntree` - number of trees for the forest
## (Kuhn & Johnson, 213, p.200) suggest `ntree` at least 1000

#########################################################################
###           I.1. Random Forests for Regression Trees with           ###
###                       `randomForest` and `ranger`                 ###
#########################################################################

#########################################################################
###                              I.1.a. `states`                      ###
#########################################################################

#########################################################################
##                       The random forests model 
set.seed(123)
states_rf_rf <- randomForest::randomForest(Murder ~ ., ntree = 1000,
                          data = train_tbl__states)
print(states_rf_rf)
plot(states_rf_rf)

#########################################################################
#         Evaluathe model performance on the test subset 

# predicting the test data sets
predict_states_rf_rf <- predict(states_rf_rf, test_tbl__states)

# compute RMSE
rmse_states_rf_rf <- caret::RMSE(pred = predict_states_rf_rf, 
                          obs = test_tbl__states$Murder)
rmse_states_rf_rf

# compute R2
r2_states_rf_rf <- caret::R2 (predict_states_rf_rf, 
                                  test_tbl__states$Murder)
r2_states_rf_rf


#########################################################################
#                             Display variable's importance
set.seed(123)
vi_states_rf_rf <- randomForest::importance(states_rf_rf) %>%
     data.frame() %>% 
     mutate(variable = row.names(.)) %>%
     rename(importance = IncNodePurity) %>%
     arrange(desc(importance))

plot_vi_states_rf_rf <- ggplot(data = vi_states_rf_rf, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - States - Random Forests") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_rf_rf




#########################################################################
###                            I.1.b. `insurance`                     ###
#########################################################################

#########################################################################
##                       The random forests model 
set.seed(123)
insurance_rf_rf <- randomForest::randomForest(charges ~ ., 
          ntree = 1000,data = main_train__insurance)
## error ! `char` variables must be converted into factors

set.seed(123)
insurance_rf_rf <- randomForest::randomForest(charges ~ ., 
          ntree = 1000,
          data = main_train__insurance %>% 
                mutate_if(is.character, as.factor))

print(insurance_rf_rf)
plot(insurance_rf_rf)


#########################################################################
#         Evaluathe model performance on the test subset 

# predicting the test data sets
predict_insurance_rf_rf <- predict(insurance_rf_rf, 
     main_test__insurance %>% 
          mutate_if(is.character, as.factor))

# compute RMSE
rmse_insurance_rf_rf <- caret::RMSE(pred = predict_insurance_rf_rf, 
                          obs = main_test__insurance$charges)
rmse_insurance_rf_rf

# compute R2
r2_insurance_rf_rf <- caret::R2 (predict_insurance_rf_rf, 
                                  main_test__insurance$charges)
r2_insurance_rf_rf


#########################################################################
#                             Display variables importance
set.seed(123)
vi_insurance_rf_rf <- randomForest::importance(insurance_rf_rf) %>%
     data.frame() %>% 
     mutate(variable = row.names(.)) %>%
     rename(importance = IncNodePurity) %>%
     arrange(desc(importance))

plot_vi_insurance_rf_rf <- ggplot(data = vi_insurance_rf_rf, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 5.0e+10, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - insurance - Random Forests") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_rf_rf




#########################################################################
###    I.2. Random Forests for Regression Trees with `tidymodels`     ###
#########################################################################

#########################################################################
###                              I.2.a. `states`                      ###
#########################################################################
## (re)create a 5-times - 5-fold cross-validation on the main train subset
set.seed(12345)
cv_train__states <- vfold_cv(train_tbl__states, v = 5, repeats = 3)

# recipe
recipe__states <- function(dataset) {
     recipe(Murder ~ ., data = dataset) %>%
     prep(data = dataset)
}

# we'll search for best `random forest` model, varying two  hyperparameters:
#  `mtry`
#  `ntree`
grid <- tibble(mtry = seq(1,6, by = 1)) %>%
     mutate (foo = 1) %>%
     inner_join(
          tibble(trees = seq(500, 2000, by = 500)) %>%
               mutate (foo = 1)  ) 

#
#  start the pipe   
the_pipe__states <- cv_train__states %>% 
     mutate(foo = 1) %>%
     inner_join(grid) %>%
     select (-foo) %>%
     mutate (analysis_data = map(splits, analysis), 
             assessment_data = map(splits, assessment) ) %>%
     mutate (prep = map(analysis_data, recipe__states )) %>%
     mutate (
          analysis_juiced = map(.x = .$prep, .f = juice),
          assessment_baked = map2(.x = .$prep, .y = .$assessment_data, .f = bake)
          )     

# fit the model
rf_model__states <- function(dataset, mtry, trees) {
     rand_forest( mtry = mtry, trees = trees, mode = "regression") %>%
     set_engine("ranger") %>%
     fit(Murder ~ ., data = dataset)
}

# it may take many minutes...
set.seed(12345)
the_pipe__states <- the_pipe__states %>%
     mutate (the_model = pmap(list(analysis_juiced, mtry, trees), 
                              rf_model__states))

# Performance assessment of the models
perf_metrics__states <- metric_set(rmse, rsq, ccc)

# function that returnes (as tibble) the performance metrics for each model 
f_assessment__states <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked) %>%
          # Add the baked assessment data back in
          bind_cols(assessment_baked) %>% 
          perf_metrics__states(Murder, .pred)
}

# get the prediction and three performance metrics for each model
the_pipe__states <- the_pipe__states %>%
     mutate (the_metrics = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_assessment__states))

names(the_pipe__states)
     
# get the model performance as data frame for all folds
performance__states <- the_pipe__states %>%
     select (id, id2, mtry, trees, the_metrics) %>%
     unnest(the_metrics)
glimpse(performance__states)

# average the metrics 
average_model_performance__states_rf <- performance__states %>%
     group_by(mtry, trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_states_ccc_rf <- average_model_performance__states_rf %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best ccc (0.641)
# mtry = 3
# trees = 1000	

# fit the model for the entire training set
set.seed(12345)
rf_model_states <- rand_forest( mtry = 3, trees = 1000, mode = "regression") %>% 
     set_engine("ranger", importance = "permutation") %>%
     fit(Murder ~ ., data = juice(recipe__states(train_tbl__states)))

###
##                             Variables importance
names(rf_model_states$fit$variable.importance)

vi_states_rf_tidy <- rf_model_states$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(rf_model_states$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_states_rf_tidy <- ggplot(data = vi_states_rf_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `states-random_forests-tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_rf_tidy


###   save the final model for using in further scripts
save(rf_model_states, train_tbl__states, test_tbl__states,
     average_model_performance__states_rf,
     vi_states_rf_tidy, plot_vi_states_rf_tidy,
     file = '14c1_states.RData')


# compare `ccc` for best `rpart` and `bagging` models
average_model_performance__states_rpart %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)

average_model_performance__states_bagg %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)
     
average_model_performance__states_rf %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)

# compare variable's importance     
     
plot_vi_states_rpart_tidy

plot_vi_states_bagg_tidy

plot_vi_states_rf_tidy


#########################################################################
###                           I.2.b. `insurance`                      ###
#########################################################################

## create a 3 times 5 fold cross-validation on the main train subset
set.seed(12345)
cv_train__insurance <- vfold_cv(main_train__insurance, v = 5, repeats = 3)

# recipe
recipe__insurance <- function(dataset) {
     recipe(charges ~ ., data = dataset) %>%
     prep(data = dataset)
}

# we'll search for best `random forest` model, varying two  hyperparameters:
#  `mtry`
#  `ntree`
grid <- tibble(mtry = seq(1,6, by = 1)) %>%
     mutate (foo = 1) %>%
     inner_join(
          tibble(trees = seq(500, 2000, by = 500)) %>%
               mutate (foo = 1)  ) 

#
#  start the pipe   
the_pipe__insurance <- cv_train__insurance %>% 
     mutate(foo = 1) %>%
     inner_join(grid) %>%
     select (-foo) %>%
     mutate (analysis_data = map(splits, analysis), 
             assessment_data = map(splits, assessment) ) %>%
     mutate (prep = map(analysis_data, recipe__insurance )) %>%
     mutate (
          analysis_juiced = map(.x = .$prep, .f = juice),
          assessment_baked = map2(.x = .$prep, .y = .$assessment_data, .f = bake)
          )     

# fit the model
rf_model__insurance <- function(dataset, mtry, trees) {
     rand_forest( mtry = mtry, trees = trees, mode = "regression") %>%
     set_engine("ranger") %>%
     fit(charges ~ ., data = dataset)
}

# it may take many minutes...
set.seed(12345)
the_pipe__insurance <- the_pipe__insurance %>%
     mutate (the_model = pmap(list(analysis_juiced, mtry, trees), 
                              rf_model__insurance))

# Performance assessment of the models
perf_metrics__insurance <- metric_set(rmse, rsq, ccc)

# function that returnes (as tibble) the performance metrics for each model 
f_assessment__insurance <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked) %>%
          # Add the baked assessment data back in
          bind_cols(assessment_baked) %>% 
          perf_metrics__states(charges, .pred)
}

# get the prediction and three performance metrics for each model
the_pipe__insurance <- the_pipe__insurance %>%
     mutate (the_metrics = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_assessment__insurance))

# get the model performance as data frame for all folds
performance__insurance <- the_pipe__insurance %>%
     select (id, id2, mtry, trees, the_metrics) %>%
     unnest(the_metrics)

# average the metrics 
average_model_performance__insurance_rf <- performance__insurance %>%
     group_by(mtry, trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_insurance_ccc_rf <- average_model_performance__insurance_rf %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best ccc (0.919)
# mtry = 4
# trees = 2000	

# fit the model for the entire training set
set.seed(12345)
rf_model_insurance <- rand_forest( mtry = 4, trees = 2000, mode = "regression") %>% 
     set_engine("ranger", importance = "permutation") %>%
     fit(charges ~ ., data = juice(recipe__insurance(main_train__insurance)))

###
##                             Variables importance

vi_insurance_rf_tidy <- rf_model_insurance$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(rf_model_insurance$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_insurance_rf_tidy <- ggplot(data = vi_insurance_rf_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance-random_forests-tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_rf_tidy


###   save the final model for using in further scripts
save(rf_model_insurance, main_train__insurance, main_test__insurance,
     average_model_performance__insurance_rf,
     vi_insurance_rf_tidy, plot_vi_insurance_rf_tidy,
     file = '14c1_insurance.RData')


# compare `ccc` for best `rpart` and `bagging` models
average_model_performance__insurance_rpart %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)

average_model_performance__insurance_bagg %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)
     
average_model_performance__insurance_rf %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)

# compare variable's importance     
     
plot_vi_insurance_rpart_tidy

plot_vi_insurance_bagg_tidy

plot_vi_insurance_rf_tidy

