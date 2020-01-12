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
###                   14d1. Boosting  for Scoring                        ###
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 11.01.2020

library(tidyverse)
library(caret)     
#library(randomForest)
library(gbm)
library(xgboost)
#library(plotmo)
library(tidymodels)
options(scipen = 999)


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
###  I. Boosting for Regression Trees                                 ###
###    I.1. Generalized Boosted Regression Modeling with `gbm`        ###
###    I.2. XGBoost for Regression Trees with `tidymodels`            ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

### All the data sets and models (including variable importance plots
### and performance metrics) were saved at the end of scripts `14a1`,
###  `14b1 and `14c1`
### We'll procees from this point
load(file = '14a1_states.RData')
load(file = '14b1_states.RData')
load(file = '14c1_states.RData')
load(file = '14a1_insurance.RData')
load(file = '14b1_insurance.RData')
load(file = '14c1_insurance.RData')


#########################################################################
###    I.1. Generalized Boosted Regression Modeling with `gbm`        ###
#########################################################################

#########################################################################
###              I.1. GBM for Regression Trees with `gbm`             ###
#########################################################################
browseVignettes(package = "gbm")

#########################################################################
###                              I.1.a. `states`                      ###
#########################################################################

# for reproducibility
set.seed(123)
# train GBM model
states_gbm <- gbm::gbm(
     formula = Murder ~ .,
     distribution = "gaussian",
     data = train_tbl__states,
     n.trees = 10000,
     interaction.depth = 1,
     shrinkage = 0.001,
     cv.folds = 5,  
     verbose = TRUE
     )  

### Does not work!
# Error in gbm.fit(x = x, y = y, offset = offset, 
#    distribution = distribution,  : 
# The data set is too small or the subsampling rate is too large: 
#    `nTrain * bag.fraction <= n.minobsinnode`



#########################################################################
###                             I.1.b. `insurance`                    ###
#########################################################################

set.seed(123)
insurance_gbm <- gbm::gbm(formula = charges ~ ., 
     distribution = "gaussian", 
     data = main_train__insurance,
     n.trees = 10000,
     interaction.depth = 5,
     shrinkage = 0.001,
     cv.folds = 10,  
     verbose = TRUE)

# error; as in the random forest model, all variables of type char
# must be converted into factors 
set.seed(123)
# this will take some time
insurance_gbm <- gbm::gbm(formula = charges ~ ., 
     distribution = "gaussian", 
     data = main_train__insurance %>% 
                               mutate_if(is.character, as.factor),
     n.trees = 10000,
     interaction.depth = 5,
     shrinkage = 0.001,
     cv.folds = 10,  
     verbose = TRUE)

insurance_gbm
summary(insurance_gbm)

# plot loss function as a result of n trees added to the ensemble
gbm::gbm.perf(insurance_gbm, method = "cv")

plotmo::plotres(insurance_gbm)


#########################################################################
#                             Display variables importance
# variable importance for the gradient boosting
set.seed(123)
vi_insurance_gbm <- summary(insurance_gbm) %>%
     data.frame() %>%
     rename(variable = var, importance = rel.inf) %>%
     arrange(desc(importance))

plot_vi_insurance_gbm <- ggplot(data = vi_insurance_gbm, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 20, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - insurance - GBM") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_gbm


#########################################################################
##             Prediction using a GBM model
#  The `gbm` package uses a predict() function that requires specifiyng 
#  the number of trees used in the prediction. 

# predicting the test data sets
predict_insurance_gbm <- predict(object = insurance_gbm, 
                  newdata = main_test__insurance %>% 
                    mutate_if(is.character, as.factor),
                  n.trees = 10000)

# compute RMSE
rmse_insurance_gbm <- caret::RMSE(pred = predict_insurance_gbm, 
                          obs = main_test__insurance$charges)
rmse_insurance_gbm

# compute R2
r2_insurance_gbm <- caret::R2 (predict_insurance_gbm, 
                                  main_test__insurance$charges)
r2_insurance_gbm
#



#########################################################################
###    I.2. XGBoost for Regression Trees with `tidymodels`            ###
#########################################################################
### XGBoost is one of the most recent and performant implementation
### of the boosting method
### we will not build models with the package `xgboost`, since it requires some
### data pre-processing operations;
### instead, we'll build some `xgb` models with `tidymodels`

### for some details about xgboost algorithm, see:
### https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

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

# we'll search for best `xgboost` model, 
# varying (only) three  hyperparameters:
#  `lear_rate`
#  `mtry`
#  `trees`
grid <- tibble(learn_rate = seq(0.01, 0.35, by = 0.05)) %>%
     mutate (foo = 1) %>%
     inner_join(
          tibble(mtry = seq(1,6, by = 1)) %>%
               mutate (foo = 1)) %>%
     inner_join(
          tibble(trees = seq(100, 2000, by = 500)) %>%
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
xgb_model__states <- function(dataset, learn_rate, mtry, trees) {
     boost_tree(learn_rate = learn_rate, mtry = mtry, 
                trees = trees, mode = "regression") %>%
     set_engine("xgboost") %>%
     fit(Murder ~ ., data = dataset)
}


# it may take many, many, many minutes...
set.seed(12345)
the_pipe__states <- the_pipe__states %>%
     mutate (the_model = pmap(list(analysis_juiced, learn_rate, mtry, trees), 
                              xgb_model__states))

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
     select (id, id2, learn_rate, mtry, trees, the_metrics) %>%
     unnest(the_metrics)
glimpse(performance__states)

# average the metrics 
average_model_performance__states_xgb <- performance__states %>%
     group_by(learn_rate, mtry, trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_states_ccc_xgb <- average_model_performance__states_xgb %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best ccc (0.89)
# learn_rate = 0.01
# mtry = 3
# trees = 100

# fit the model for the entire training set
set.seed(12345)
xgb_model_states <- boost_tree( learn_rate = 0.26, mtry = 3, trees = 100, 
          mode = "regression") %>% 
     set_engine("xgboost", importance = "permutation") %>%
     fit(Murder ~ ., data = juice(recipe__states(train_tbl__states)))



###
##                             Variables importance
library(xgboost)
xgb.importance(model=xgb_model_states$fit) %>% head()

names(xgb_model_states$fit$variable.importance)

# notice that the variable importance is extracted differently (from
# random forests), using `xgboost` package
vi_states_xgb_tidy <- xgboost::xgb.importance(model=xgb_model_states$fit) %>%
     as_tibble() %>%
     transmute (variable = Feature, importance = Gain) %>%
     arrange(desc(importance))

plot_vi_states_xgb_tidy <- ggplot(data = vi_states_xgb_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.3, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `states-xgboost-tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_xgb_tidy


###   save the final model for using in further scripts
save(xgb_model_states, train_tbl__states, test_tbl__states,
     average_model_performance__states_xgb,
     vi_states_xgb_tidy, plot_vi_states_xgb_tidy,
     file = '14d1_states.RData')


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

average_model_performance__states_xgb %>%
     filter (.metric == 'ccc') %>%
     top_n(1, average_estimate) %>%
     pull(average_estimate) %>%
     head(1)


# compare variable's importance     
     
plot_vi_states_rpart_tidy

plot_vi_states_bagg_tidy

plot_vi_states_rf_tidy

plot_vi_states_xgb_tidy

