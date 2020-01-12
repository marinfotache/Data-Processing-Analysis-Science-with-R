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
###                   14d2. Boosting  for Classification                 ###
############################################################################


### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 11.01.2020

library(tidyverse)
library(caret)     # for confusion matrix, ...
library(caTools) # for plotting the ROC curve
library(gbm)
#library(plotmo)
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
###  II. Boosting for Classification Trees                            ###
###    II.1. GBM for Classification Trees with `gbm`                  ###
###    II.2. XGBoost for Classification Trees with `tidymodels`       ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

### All the data sets and models (including variable importance plots
### and performance metrics) were saved at the end of scripts `14a2`,
###  `14b2 and `14c2`
### We'll procees from this point
load(file = '14a2_heart.RData')
load(file = '14b2_heart.RData')
load(file = '14c2_heart.RData')

#########################################################################
###                       `Heart` disease data set                    ###
#########################################################################
Heart <- read_csv('Heart.csv') %>%
     select (-X1) %>%
     mutate(AHD = factor(AHD))
glimpse(Heart)

# there are missing values...
anyNA(Heart)

# ...but not for the outcome variable ('AHD)
anyNA(Heart$AHD)

# impute the missing values using `random forest` method
set.seed(123)
Heart_mice <- Heart %>%
     mutate_if(is.character, as.factor) %>%
     mice::mice(., method = 'rf' ) %>%
     mice::complete()
glimpse(Heart_mice)


###  Train and test split                   
set.seed(seed = 1234) 
# notice `strata` option
train_test_split__heart <-
  rsample::initial_split(
    data = Heart_mice,     
    prop = 0.8,
    strata = 'AHD'
  ) 
train_test_split__heart

main_train__Heart <- train_test_split__heart %>% 
     training() 
main_test__Heart  <- train_test_split__heart %>% 
     testing()



### We'll keep a copy of the initial data set
###  Train and test split                   
set.seed(seed = 1234) 
# notice `strata` option
train_test_split__heart_init <-
  rsample::initial_split(
    data = Heart,     
    prop = 0.8,
    strata = 'AHD'
  ) 
train_test_split__heart

main_train__heart <- train_test_split__heart %>% 
     training() 
main_test__heart  <- train_test_split__heart %>% 
     testing()




#########################################################################
###             II. Boosting for Classification Trees                 ###
#########################################################################


#########################################################################
###            II.1. GBM for Classification Trees with `gbm`          ###
#########################################################################

#########################################################################
##                            Heart data set

set.seed(123)
heart_gbm <- gbm::gbm(formula = AHD ~ ., 
     distribution = "bernoulli", 
     data = main_train__Heart %>%
          mutate_if(is.character, as.factor),
     n.trees = 10000,
     interaction.depth = 5,
     shrinkage = 0.001,
     cv.folds = 10,  
     verbose = TRUE)
# Error in checkForRemoteErrors(val) : 
#  5 nodes produced errors; first error: Bernoulli requires the response to be in {0,1}

set.seed(123)
heart_gbm <- gbm::gbm(formula = AHD ~ ., 
     distribution = "bernoulli", 
     data = main_train__Heart %>%
          mutate_if(is.character, as.factor) %>%
          mutate(AHD = if_else(AHD == 'Yes', 1L, 0L)),
     n.trees = 10000,
     interaction.depth = 5,
     shrinkage = 0.001,
     cv.folds = 10,  
     verbose = TRUE)

heart_gbm
summary(heart_gbm)

# plot loss function as a result of n trees added to the ensemble
gbm::gbm.perf(heart_gbm, method = "cv")

#########################################################################
#                             Display variables importance

# variable importance for the gradient boosting
set.seed(123)
vi_heart_gbm <- summary(heart_gbm) %>%
     data.frame() %>%
     rename(variable = var, importance = rel.inf) %>%
     arrange(desc(importance))

plot_vi_heart_gbm <- ggplot(data = vi_heart_gbm, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 10, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - Heart - GBM") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_gbm


#########################################################################
#         Evaluate model performance on the test subset and compare 
#         it with previous models

## predicting the test data sets
# ... as class

predict_heart_gbm_class <- if_else(predict(object = heart_gbm, 
                  newdata = main_test__Heart %>%
                       mutate(AHD = if_else(AHD == 'Yes', 1L, 0L)),
                  n.trees = 10000,
                  type = 'response') >= .5, 1, 0)

predict_heart_gbm_prob <- predict(object = heart_gbm, 
                  newdata = main_test__Heart %>%
                       mutate(AHD = if_else(AHD == 'Yes', 1L, 0L)),
                  n.trees = 10000,
                  type = 'response')

# Confusion Matrix
cm_heart_gbm <- caret::confusionMatrix(table(
     data = predict_heart_gbm_class,
     reference = if_else(main_test__Heart$AHD  == 'Yes', 1L, 0L)), 
     positive='1')


# calculate AUC, and draw ROC
auc_heart_gbm <- caTools::colAUC(predict_heart_gbm_prob, 
                if_else(main_test__Heart$AHD  == 'Yes', 1L, 0L), 
                plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_heart_gbm



#########################################################################
###    II.2. XGBoost for Classification Trees with `tidymodels`       ###
#########################################################################


#########################################################################
###                 II.2.a. `Heart` disease data set                  ###
# we'll use the initial data set, since we'll treat the missing values
#     inside of the recipe
glimpse(main_train__heart)
glimpse(main_test__heart)


## create 3 times a 5 fold cross-validation on the main train subset
set.seed(12345)
cv_train__heart <- vfold_cv(main_train__heart, v = 5, repeats = 3)



###  the recipe treats the missing values
recipe__heart <- function(dataset) {
     recipe(AHD ~ ., data = dataset) %>%
     step_knnimpute(all_predictors(), neighbors = 3) %>%  # missing values treatment
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
          tibble(trees = seq(100, 1000, by = 500)) %>%
               mutate (foo = 1)  ) 

#
#  start the pipe   
the_pipe__heart <- cv_train__heart %>% 
     mutate(foo = 1) %>%
     inner_join(grid) %>%
     select (-foo) %>%
     mutate (analysis_data = map(splits, analysis), 
             assessment_data = map(splits, assessment) ) %>%
     mutate (prep = map(analysis_data, recipe__heart )) %>%
     mutate (
          analysis_juiced = map(.x = .$prep, .f = juice),
          assessment_baked = map2(.x = .$prep, .y = .$assessment_data, .f = bake)
          )     

# fit the model
xgb_model__heart <- function(dataset, learn_rate, mtry, trees) {
     boost_tree(learn_rate = learn_rate, mtry = mtry, 
                trees = trees, mode = "classification") %>%
     fit(AHD ~ ., data = dataset)
}


# it may take many, many. many minutes...
the_pipe__heart <- the_pipe__heart %>%
     mutate (the_model = pmap(list(analysis_juiced, learn_rate, mtry, trees), 
                              xgb_model__heart))


###                 Model predictions
# function that returns (as tibble) the predictions as classes
f_pred_class__heart <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked, type = "class") %>%
          # Add the baked test data back in
          bind_cols(assessment_baked  %>% select(AHD) )  
}

# function that returns (as tibble) the predictions as probabilities
f_pred_prob__heart <- function(the_model, assessment_baked) {
     the_model %>%
          predict(new_data = assessment_baked, type = "prob") %>%
          # Add the baked test data back in
          bind_cols(assessment_baked  %>% select(AHD) )  
}

# add the predictions to the `pipe`
the_pipe__heart <- the_pipe__heart %>%
     mutate (
          predict_class = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_pred_class__heart),
          predict_prob = map2(.x = .$the_model, .y = .$assessment_baked, 
               .f = f_pred_prob__heart)            
             )


#########################################################################
###    Compute accuracy, precision, recall, F1-score and AUC for
###    each KV model
f_performance__heart <- function (predict_class, predict_prob) {
     bind_rows(
        predict_class %>%
               metrics(truth = AHD, estimate = .pred_class), 
        precision(predict_class, AHD, .pred_class),
        recall(predict_class, AHD, .pred_class),
        f_meas(predict_class, AHD, .pred_class),
        roc_auc(predict_prob, AHD, .pred_Yes)
            ) 
}

names(the_pipe__heart)     
# get all the metrics for all KV models     
performance_xgb__heart <- the_pipe__heart %>%
     mutate (the_metrics = map2(.x = .$predict_class, .y = .$predict_prob, 
               .f = f_performance__heart)) %>%
     select (id, id2, learn_rate, mtry, trees, the_metrics) %>%
     unnest(the_metrics)

glimpse(performance_rf__heart)

# average the metrics 
average_model_performance__heart_xgb <- performance_xgb__heart %>%
     group_by(learn_rate, mtry, trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `roc_auc`
top_heart_auc_xgb <- average_model_performance__heart_xgb %>%
     filter (.metric == 'roc_auc') %>%
     arrange(desc(average_estimate))

# model with best AUC (0.90):
# earn_rate = 0.01
# mtry = 3
# trees = 600

# fit the model for the entire training set
set.seed(123)

xgb_model__heart <- boost_tree(learn_rate= 0.01, mtry = 3, 
          trees = 600, mode = "classification") %>%
     set_engine("xgboost", importance = "permutation") %>%
     fit(AHD ~ ., data = juice(recipe__heart(main_train__heart)))

###
##                             Variable's importance
# notice that the variable importance is extracted differently (from
# random forests), using `xgboost` package
vi_heart_xgb_tidy <- xgboost::xgb.importance(model = xgb_model__heart$fit) %>%
     as_tibble() %>%
     transmute (variable = Feature, importance = Gain) %>%
     arrange(desc(importance))

plot_vi_heart_xgb_tidy <- ggplot(data = vi_heart_xgb_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.09, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Var. importance - `heart_xgboost_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_xgb_tidy

# compare with 
plot_vi_heart_rf_tidy

plot_vi_heart_bagg_tidy

plot_vi_heart_rpart_tidy


###   save the final model for using in further scripts
save(xgb_model__heart, main_train__heart, main_test__heart,
     average_model_performance__heart_xgb,
     vi_heart_xgb_tidy, plot_vi_heart_xgb_tidy,
     file = '14d2_heart.RData')



