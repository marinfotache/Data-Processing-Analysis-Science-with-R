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
###                               14d. Boosting                          ###
############################################################################

### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 15.01.2019



library(tidyverse)
library(caret)     # for confusion matrix, ...
library(caTools) # for splitting the date set into train/test
                  # and also for plotting the ROC curve
library(randomForest)
library(gbm)

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
###    I.2. GBM for Regression Trees with `caret`                     ###
###  II. Boosting for Classification Trees                            ###
###    II.1. GBM for Classification Trees with `gbm`                  ###
###    II.2. GBM for Classification Trees with `caret`                ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

### All the data sets and models (including variable importance plots
### and performance metrics) were saved at the end of script `14b`
### 
### We'll procees from this point
load(file = '14c_random_forests.RData')



#########################################################################
###    I.1. Generalized Boosted Regression Modeling with `gbm`        ###
#########################################################################

#########################################################################
###              I.1. GBM for Regression Trees with `gbm`             ###
#########################################################################

#########################################################################
###                              I.1.a. `states`                      ###
#########################################################################

#########################################################################
##                  The Gradient Boosting Machine model
browseVignettes(package = "gbm")

# for reproducibility
set.seed(123)
# train GBM model
states_gbm <- gbm::gbm(
     formula = Murder ~ .,
     distribution = "gaussian",
     data = main_train__states,
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

#########################################################################
##                  The Gradient Boosting Machine model
browseVignettes(package = "gbm")

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
gbm.perf(insurance_gbm, method = "cv")


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

## compare with the variable importance in previous models
plot_vi_insurance_rf_rf
plot_vi_insurance_bag_ipred
plot_vi_insurance_rpart_caret
plot_vi_insurance_rpart


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

# compare performance metrics among models
rmse_insurance_rpart
rmse_insurance_rpart_caret
rmse_insurance_bag_ipred
rmse_insurance_rf_rf
rmse_insurance_gbm

r2_insurance_rpart
r2_insurance_rpart_caret
r2_insurance_bag_ipred
r2_insurance_rf_rf
r2_insurance_gbm


#########################################################################
###             II. Boosting for Classification Trees                 ###
#########################################################################


#########################################################################
###            II.1. GBM for Classification Trees with `gbm`          ###
#########################################################################

#########################################################################
##                  The Gradient Boosting Machine model
browseVignettes(package = "gbm")

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

## compare with the variable importance in previous models
plot_vi_heart_rf_caret
plot_vi_heart_rf_rf
plot_vi_heart_bag_caret
plot_vi_heart_bag_ipred
plot_vi_heart_rpart_caret
plot_vi_heart_rpart


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


# compare performance with previous models 
cm_heart_rpart
cm_heart_rpart_caret
cm_heart_bag_ipred
cm_heart_rf_rf
cm_heart_gbm

auc_heart_rpart
auc_heart_rpart_caret
auc_heart_bag_ipred
auc_heart_rf_rf
auc_heart_gbm



#########################################################################
###    II.2. GBM for Classification Trees with `caret`                ###
#########################################################################

## see
## https://quantdev.ssri.psu.edu/tutorials/ensemble-methods-bagging-random-forests-boosting
## 

