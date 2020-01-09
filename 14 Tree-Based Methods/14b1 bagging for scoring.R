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
###                 14b1. Bagginng for Regression Trees                  ###
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 07.01.2020


library(tidyverse)

library(caret)     # for confusion matrix, ...
#library(caTools) # for splitting the date set into train/test
                  # and also for plotting the ROC curve
library(ipred)
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
###  I. Bagging Regression Trees                                      ###
###    I.1. Bagging Regression Trees with `ipred`,                    ###
###    I.2. Bagging Regression Trees with `tidymodels`                ###
#########################################################################




#########################################################################
###  O. Import and prepare the data sets  (see also script 14a1)      ###
#########################################################################
load(file = '14a1_states.RData')
load(file = '14a1_insurance.RData')



#########################################################################
###                    I. Bagging Regression Trees                    ###
#########################################################################
## package `ipred`
## the main tuning parameter is `nbagg` - the number of bootstap samples
## to aggregate. (Kuhn & Johnson, 2013, p.197) suggest using up to 50 
## bagging ensembles


#########################################################################
###               I.1. Bagging Regression Trees with `ipred`          ###
#########################################################################


#########################################################################
###                              I.1.a. `states`                      ###
#########################################################################

#########################################################################
#                             The bagging model 
set.seed(123)
states_bag_ipred <- ipred::bagging(Murder ~ ., data = train_tbl__states, 
     nbagg = 50)


#########################################################################
#                             Display variables importance
# this may take some time
set.seed(123)
vi_states_bag_ipred <- varImp(states_bag_ipred) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_states_bag_ipred <- ggplot(data = vi_states_bag_ipred, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - States - Bagging - ipred") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_bag_ipred


#########################################################################
#         Evaluathe model performance on the test subset and compare 

# predicting the test data sets
predict_states_bag_ipred <- predict(states_bag_ipred, test_tbl__states)

# compute RMSE
rmse_states_bag_ipred <- caret::RMSE(pred = predict_states_bag_ipred, 
                          obs = test_tbl__states$Murder)
rmse_states_bag_ipred

# compute R2
r2_states_bag_ipred <- caret::R2 (predict_states_bag_ipred, 
                                  test_tbl__states$Murder)
r2_states_bag_ipred



#########################################################################
###                              I.1.b. `insurance`                    ###
#########################################################################

#########################################################################
#                             The bagging model 
set.seed(123)
insurance_bag_ipred <- ipred::bagging(charges ~ ., 
     data = main_train__insurance, nbagg = 500)

#########################################################################
#                             Display variables importance
# this may take some time
set.seed(123)
vi_insurance_bag_ipred <- varImp(insurance_bag_ipred) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_insurance_bag_ipred <- ggplot(data = vi_insurance_bag_ipred, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - insurance - Bagging - ipred") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_bag_ipred



#########################################################################
#         Evaluath model performance on the test subset 

# predicting the test data sets
predict_insurance_bag_ipred <- predict(insurance_bag_ipred, 
          main_test__insurance)

# compute RMSE
rmse_insurance_bag_ipred <- caret::RMSE(pred = predict_insurance_bag_ipred, 
                          obs = main_test__insurance$charges)
rmse_insurance_bag_ipred

# compute R2
r2_insurance_bag_ipred <- caret::R2 (predict_insurance_bag_ipred, 
                                  main_test__insurance$charges)
r2_insurance_bag_ipred


# compare performance with previous models
rmse_insurance_rpart
rmse_insurance_rpart_caret
rmse_insurance_bag_ipred

r2_insurance_rpart
r2_insurance_rpart_caret
r2_insurance_bag_ipred



#########################################################################
###         I.2. Bagging Regression Trees with `tidymodels`           ###
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

# we'll search for best `bagging` model, varying only one  hyperparameter:
#  `trees`
grid <- tibble(trees = seq(10, 200, by = 20)) %>%
     mutate(foo = 1)

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
bagg_model__states <- function(dataset, trees) {
     rand_forest(trees = trees, mode = "regression", mtry = .preds()) %>%
     set_engine("ranger", importance = 'permutation') %>%
     fit(Murder ~ ., data = dataset)
}

# it may take many minutes...
set.seed(12345)
the_pipe__states <- the_pipe__states %>%
     mutate (the_model = map2(analysis_juiced, trees, 
                              bagg_model__states))

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
     select (id, id2, trees, the_metrics) %>%
     unnest(the_metrics)
glimpse(performance__states)

# average the metrics 
average_model_performance__states_bagg <- performance__states %>%
     group_by(trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_states_ccc_bagg <- average_model_performance__states_bagg %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best ccc (0.621)
# trees = 110

# fit the model for the entire training set
set.seed(12345)
bagg_model_states <- rand_forest( trees = 110, 
                                  mode = "regression", mtry = .preds()) %>% 
     set_engine("ranger", importance = "permutation") %>%
     fit(Murder ~ ., data = juice(recipe__states(train_tbl__states)))

###
##                             Variables importance
names(bagg_model_states$fit$variable.importance)

vi_states_bagg_tidy <- bagg_model_states$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(bagg_model_states$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_states_bagg_tidy <- ggplot(data = vi_states_bagg_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `states_bagging_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_bagg_tidy


###   save the final model for using in further scripts
save(bagg_model_states, train_tbl__states, test_tbl__states,
     average_model_performance__states_bagg,
     vi_states_bagg_tidy, plot_vi_states_bagg_tidy,
     file = '14b1_states.RData')


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
     
# compare variable's importance     
     
plot_vi_states_rpart_tidy

plot_vi_states_bagg_tidy



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


# we'll search for best `bagging` model, varying only one  hyperparameter:
#  `trees`
grid <- tibble(trees = seq(10, 200, by = 20)) %>%
     mutate(foo = 1)

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
bagg_model__insurance <- function(dataset, trees) {
     rand_forest(trees = trees, mtry = .preds(), mode = "regression") %>%
     set_engine("ranger") %>%
     fit(charges ~ ., data = dataset)
}


# it may take many minutes...
set.seed(12345)
the_pipe__insurance <- the_pipe__insurance %>%
     mutate (the_model = map2(analysis_juiced, trees, 
                              bagg_model__insurance))

# Performance assessment of the models
perf_metrics__insurance <- metric_set(rmse, rsq, ccc)

# function that returnes (as tibble) the performance metrics for each model 
f_assessment__insurance <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked) %>%
          # Add the baked assessment data back in
          bind_cols(assessment_baked) %>% 
          perf_metrics__insurance(charges, .pred)
}

# get the prediction and three performance metrics for each model
the_pipe__insurance <- the_pipe__insurance %>%
     mutate (the_metrics = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_assessment__insurance))

names(the_pipe__insurance)
     
# get the model performance as data frame for all folds
performance__insurance <- the_pipe__insurance %>%
     select (id, id2, trees, the_metrics) %>%
     unnest(the_metrics)

# average the metrics 
average_model_performance__insurance_bagg <- performance__insurance %>%
     group_by(trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_insurance_ccc_bagg <- average_model_performance__insurance_bagg %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best ccc (0.916)
# trees = 150	

# fit the model for the entire training set
set.seed(12345)
bagg_model_insurance <- rand_forest( trees = 150, mode = "regression", 
                                      mtry = .preds()) %>% 
     set_engine("ranger", importance = "permutation") %>%
     fit(charges ~ ., data = juice(recipe__insurance(main_train__insurance)))

###
##                             Variables importance
names(bagg_model_insurance$fit$variable.importance)

vi_insurance_bagg_tidy <- bagg_model_insurance$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(bagg_model_insurance$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_insurance_bagg_tidy <- ggplot(data = vi_insurance_bagg_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 50000000, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance_bagging_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_bagg_tidy


###   save the final model for using in further scripts
save(bagg_model_insurance, main_train__insurance, main_test__insurance,
     average_model_performance__insurance_bagg,
     vi_insurance_bagg_tidy, plot_vi_insurance_bagg_tidy,
     file = '14b1_insurance.RData')


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
     
# compare variable's importance     
     
plot_vi_insurance_rpart_tidy

plot_vi_insurance_bagg_tidy


