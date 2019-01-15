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
###                              14c. Random Forests                     ###
############################################################################

### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 08.01.2019

### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 08.01.2019


library(tidyverse)
library(caret)     # for confusion matrix, ...
# ROCR package for ROC curve plotting:
#library(ROCR)
#library(Metrics) # for `auc` function
#library(caTools) # for splitting the date set into train/test
                  # and also for plotting the ROC curve
library(ipred)
library(randomForest)
library(ranger)


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
###    I.2. Random Forests for Regression Trees with `caret`          ###
###  II. Random Forests for Classification Trees                      ###
###    II.1. Random Forests for Classification Trees with             ###
###       `randomForest`                                              ###
###    II.2. Random Forests for Classification Trees with `caret`     ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

### All the data sets and models (including variable importance plots
### and performance metrics) were saved at the end of script `14b`
### 
### We'll procees from this point
load(file = '14b_bagging.RData')


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
###    I.1. Random Forests for Regression Trees with `randomForest`   ###
#########################################################################

#########################################################################
###                              I.1.a. `states`                      ###
#########################################################################

#########################################################################
##                       The random forests model 
set.seed(123)
states_rf_rf <- randomForest::randomForest(Murder ~ ., ntree = 1000,
                          data = main_train__states)
print(states_rf_rf)
plot(states_rf_rf)

#########################################################################
#                             Display variables importance
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

## compare with the variable importance in previous models
plot_vi_states_bag_caret
plot_vi_states_bag_ipred
plot_vi_states_ctree
plot_vi_states_rpart_caret
plot_vi_states_rpart


#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with previous models

# predicting the test data sets
predict_states_rf_rf <- predict(states_rf_rf, main_test__states)

# compute RMSE
rmse_states_rf_rf <- caret::RMSE(pred = predict_states_rf_rf, 
                          obs = main_test__states$Murder)
rmse_states_rf_rf

# compute R2
r2_states_rf_rf <- caret::R2 (predict_states_rf_rf, 
                                  main_test__states$Murder)
r2_states_rf_rf


# compare performance metrics among various models

rmse_states_rpart
rmse_states_rpart_caret
rmse_states_bag_ipred
rmse_states_bag_caret
rmse_states_rf_rf

r2_states_rpart
r2_states_rpart_caret
r2_states_bag_ipred
r2_states_bag_caret
r2_states_rf_rf



#########################################################################
###                            I.1.b. `insurance`                     ###
#########################################################################

#########################################################################
##                       The random forests model 
set.seed(123)
insurance_rf_rf <- randomForest::randomForest(charges ~ ., ntree = 1000,
                          data = main_train__insurance)
## error ! `char` variables must be converted into factors

set.seed(123)
insurance_rf_rf <- randomForest::randomForest(charges ~ ., ntree = 1000,
                          data = main_train__insurance %>% 
                               mutate_if(is.character, as.factor))

print(insurance_rf_rf)
plot(insurance_rf_rf)


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

## compare with the variable importance in previous models
plot_vi_insurance_bag_ipred
plot_vi_insurance_rpart_caret
plot_vi_insurance_rpart



#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with bagging and CART

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


# compare performance metrics among models 

rmse_insurance_rpart
rmse_insurance_rpart_caret
rmse_insurance_bag_ipred
rmse_insurance_rf_rf

r2_insurance_rpart
r2_insurance_rpart_caret
r2_insurance_bag_ipred
r2_insurance_rf_rf



#########################################################################
###             II. Random Forests for Classification Trees           ###
#########################################################################
## packages: `randomForest` and `ranger`

## hyperparameter: `mtry` - number of randomly selected predictors
##   to choose from at each split (node)
## For regression setting, Breiman recommends setting `mtry` on 
##   square root of the number of predictors (that is the default value
##        chosen by `randomForest` and `ranger`) (Kuhn & Johnson, 213, p.387)

## other parameter: `ntree` - number of trees for the forest
## (Kuhn & Johnson, 213, p.200) suggest `ntree` at least 1000



#########################################################################
###    II.1. Random Forests for Classification Trees with             ###
###       `randomForest`                                              ###
#########################################################################

#########################################################################
###                       `Heart` disease data set                    ###
###                       
#########################################################################
##                       The random forests model 
set.seed(123)
heart_rf_rf <- randomForest::randomForest(AHD ~ ., ntree = 1000,
                          data = main_train__Heart)

print(heart_rf_rf)
plot(heart_rf_rf)


#########################################################################
#                             Display variables importance
set.seed(123)
vi_heart_rf_rf <- randomForest::importance(heart_rf_rf) %>%
     data.frame() %>% 
     mutate(variable = row.names(.)) %>%
     rename(importance = MeanDecreaseGini) %>%
     arrange(desc(importance))

plot_vi_heart_rf_rf <- ggplot(data = vi_heart_rf_rf, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 10, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - Heart - Random Forests") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rf_rf

## compare with the variable importance in previous models
plot_vi_heart_bag_caret
plot_vi_heart_bag_ipred
plot_vi_heart_rpart_caret
plot_vi_heart_rpart

#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with previous models

## predicting the test data sets
# ... as class
predict_heart_rf_rf_class <- predict(heart_rf_rf, 
     main_test__Heart, type = 'class', na.action = na.omit)

# ... as probabilities
predict_heart_rf_rf_prob <- predict(heart_rf_rf, 
     main_test__Heart, type = 'prob', na.action = na.omit)

# Confusion Matrix
cm_heart_rf_rf <- caret::confusionMatrix(table(
     data = predict_heart_rf_rf_class,
     reference = main_test__Heart$AHD), positive='Yes')
cm_heart_rf_rf


# calculate AUC, and draw ROC
auc_heart_rf_rf <- caTools::colAUC(predict_heart_rf_rf_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_heart_rf_rf


# compare performance with previous models 
cm_heart_rpart
cm_heart_rpart_caret
cm_heart_bag_ipred
cm_heart_rf_rf

auc_heart_rpart
auc_heart_rpart_caret
auc_heart_bag_ipred
auc_heart_rf_rf



#########################################################################
###    II.2. Random Forests for Classification Trees with `caret`     ###
#########################################################################

#########################################################################
###                       `Heart` disease data set                    ###
set.seed(123)
heart_rf_caret_train <- train(
     AHD ~ .,
     data = main_train__Heart,
     method = "rf",
     trControl =  trainControl(method = "cv", 
          number = 5, classProbs = TRUE, 
          summaryFunction = twoClassSummary,
          savePredictions = "final"),
     metric = "ROC",
     importance = TRUE
  )

# summary of the model
heart_rf_caret_train
heart_rf_caret_train$finalModel
heart_rf_caret <- heart_rf_caret_train$finalModel


#########################################################################
##    Variable importance for `heart_rf_caret` 
set.seed(123)
vi_heart_rf_caret <- randomForest::importance(heart_rf_caret) %>%
     data.frame() %>% 
     mutate(variable = row.names(.)) %>%
     rename(importance = MeanDecreaseAccuracy) %>%
     arrange(desc(importance))

plot_vi_heart_rf_caret <- ggplot(data = vi_heart_rf_caret, 
               aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 10, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `heart_random forest_caret` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rf_caret

# compare with variable importance for `previous models 
plot_vi_heart_rf_rf
plot_vi_heart_bag_caret
plot_vi_heart_bag_ipred
plot_vi_heart_rpart_caret
plot_vi_heart_rpart


#########################################################################
#              Evaluate model performance on the test subset 

## predicting the test data sets...

# ... as class
predict_heart_rf_caret_class <- predict(heart_rf_caret_train, 
     main_test__Heart, type = 'raw')

# ... as probabilities
predict_heart_rf_caret_prob <- predict(heart_rf_caret_train, 
     main_test__Heart, type = 'prob')

# Confusion Matrix
cm_heart_rf_caret <- caret::confusionMatrix(table(
     data = predict_heart_rf_caret_class,
     reference = main_test__Heart$AHD), positive='Yes')
cm_heart_rf_caret


# calculate AUC, and draw ROC
auc_heart_rf_caret <- caTools::colAUC(predict_heart_rf_caret_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_heart_rf_caret


# Compare confusion matrices and AUCs
cm_heart_rpart
cm_heart_rpart_caret
cm_heart_bag_ipred
cm_heart_bag_caret
cm_heart_rf_rf
cm_heart_rf_caret


auc_heart_rpart
auc_heart_rpart_caret
auc_heart_bag_ipred
auc_heart_bag_caret
auc_heart_rf_rf
auc_heart_rf_caret


#########################################################################
###      Save all the objects in current session for further scripts
#########################################################################
ls()
save.image(file = '14c_random_forests.RData')



