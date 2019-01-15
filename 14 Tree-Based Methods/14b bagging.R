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
###          14b. Bagginng for Classification and Regression Trees       ###
############################################################################

### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 08.01.2019


library(tidyverse)
library(rpart)
library(caret)     # for confusion matrix, ...
#library(caTools) # for splitting the date set into train/test
                  # and also for plotting the ROC curve
library(ipred)


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
###    I.2. Bagging Regression Trees with `caret`                     ###
###  II. Bagging Classification Trees                                 ###
###    II.1. Bagging Classification Trees with `ipred`                ###
###    II.2. Bagging Classification Trees with `caret`                ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

### All the data sets and models (including variable importance plots
### and performance metrics) were saved at the end of script `14a`
### 
### We'll procees from this point
load(file = '14a_CART.RData')


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
states_bag_ipred <- ipred::bagging(Murder ~ ., data = main_train__states, 
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

# compare it with the variable importance in the CART case
plot_vi_states_rpart
plot_vi_states_rpart_caret


#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with previous CART models

# predicting the test data sets
predict_states_bag_ipred <- predict(states_bag_ipred, main_test__states)

# compute RMSE
rmse_states_bag_ipred <- caret::RMSE(pred = predict_states_bag_ipred, 
                          obs = main_test__states$Murder)
rmse_states_bag_ipred

# compute R2
r2_states_bag_ipred <- caret::R2 (predict_states_bag_ipred, 
                                  main_test__states$Murder)
r2_states_bag_ipred


# compare performance for CART and bagging models 
rmse_states_rpart
rmse_states_rpart_caret
rmse_states_bag_ipred

r2_states_rpart
r2_states_rpart_caret
r2_states_bag_ipred


#########################################################################
###                              I.1.b. `insurace`                    ###
#########################################################################

#########################################################################
#                             The bagging model 
set.seed(123)
insurance_bag_ipred <- ipred::bagging(charges ~ ., 
     data = main_train__insurance, nbagg = 50)

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

# compare the the variable importance in the CART case
plot_vi_insurance_rpart
plot_vi_insurance_rpart_caret


#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with CART

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
###              I.2. Bagging Regression Trees with `caret`           ###
#########################################################################

#########################################################################
###                              I.2.a. `states`                      ###
#########################################################################

#########################################################################
#                   the bagging model with `caret`
set.seed(123)
states_bag_caret_train <- train(
  Murder ~ .,
  data = main_train__states,
  method = "treebag",
  trControl =  trainControl(method = "cv",  number = 5),  # 5-fold cross validation
  importance = TRUE
  )

# summary of the model
states_bag_caret_train
states_bag_caret_train$finalModel
states_bag_caret <- states_bag_caret_train$finalModel


#                             Display variables importance

# this may take some time
set.seed(123)
vi_states_bag_caret <- varImp(states_bag_caret) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_states_bag_caret <- ggplot(data = vi_states_bag_caret, 
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


## compare with `ipred` bagging model and previous models
plot_vi_states_bag_caret
plot_vi_states_bag_ipred
plot_vi_states_rpart_caret
plot_vi_states_rpart


#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with CART

# predicting the test data sets
predict_states_bag_caret <- predict(states_bag_caret, main_test__states)
predict_states_bag_caret

# compute RMSE
rmse_states_bag_caret <- caret::RMSE(pred = predict_states_bag_caret, 
                          obs = main_test__states$Murder)
rmse_states_bag_caret

# compute R2
r2_states_bag_caret <- caret::R2 (predict_states_bag_caret, 
                                  main_test__states$Murder)
r2_states_bag_caret


# compare performance for CART and bagging models 

rmse_states_rpart
rmse_states_rpart_caret
rmse_states_bag_ipred
rmse_states_bag_caret

r2_states_rpart
r2_states_rpart_caret
r2_states_bag_ipred
r2_states_bag_caret


#########################################################################
###                  II. Bagging Classification Trees                 ###
#########################################################################

#########################################################################
###          II.1. Bagging Classification Trees with `ipred`          ###
#########################################################################

#########################################################################
###                       `Heart` disease data set                    ###
###                       
#########################################################################
#                             The bagging model 
set.seed(123)
heart_bag_ipred <- ipred::bagging(AHD  ~ ., data = main_train__Heart, 
     nbagg = 50)

#########################################################################
#                             Display variables importance
# this may take some time
set.seed(123)
vi_heart_bag_ipred <- varImp(heart_bag_ipred) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_heart_bag_ipred <- ggplot(data = vi_heart_bag_ipred, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - Heart - Bagging - ipred") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_bag_ipred

# compare it with the variable importance for previous models
plot_vi_heart_rpart_caret
plot_vi_heart_rpart


#########################################################################
#         Evaluathe model performance on the test subset and compare 
#         it with previous models

## predicting the test data sets
# ... as class
predict_heart_bag_ipred_class <- predict(heart_bag_ipred, 
     main_test__Heart, type = 'class', na.action = na.omit)

# ... as probabilities
predict_heart_bag_ipred_prob <- predict(heart_bag_ipred, 
     main_test__Heart, type = 'prob', na.action = na.omit)

# Confusion Matrix
cm_heart_bag_ipred <- caret::confusionMatrix(table(
     data = predict_heart_bag_ipred_class,
     reference = main_test__Heart$AHD), positive='Yes')
cm_heart_bag_ipred


# calculate AUC, and draw ROC
auc_heart_bag_ipred <- caTools::colAUC(predict_heart_bag_ipred_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_heart_bag_ipred


# compare performance with previous models 
cm_heart_rpart
cm_heart_rpart_caret
cm_heart_bag_ipred

auc_heart_rpart
auc_heart_rpart_caret
auc_heart_bag_ipred



#########################################################################
###        II.2. Bagging Classification Trees with `caret`            ###
#########################################################################

#########################################################################
###                       `Heart` disease data set                    ###

set.seed(123)
heart_bag_caret_train <- train(
     AHD ~ .,
     data = main_train__Heart,
     method = "treebag",
     trControl =  trainControl(method = "cv", 
          number = 5, classProbs = TRUE, 
          summaryFunction = twoClassSummary,
          savePredictions = "final"),
     metric = "ROC",
     importance = TRUE
  )

# summary of the model
heart_bag_caret_train
heart_bag_caret_train$finalModel
heart_bag_caret <- heart_bag_caret_train$finalModel



#########################################################################
##    Variable importance for `heart_bag_caret` 
set.seed(123)
vi_heart_bag_caret <- varImp(heart_bag_caret) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_heart_bag_caret <- ggplot(data = vi_heart_bag_caret, 
               aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 30, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `heart_bag_caret` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_bag_caret

# compare with variable importance for `previous models 
plot_vi_heart_bag_ipred
plot_vi_heart_rpart_caret
plot_vi_heart_rpart


#########################################################################
#              Evaluate model performance on the test subset 

## predicting the test data sets...

# ... as class
predict_heart_bag_caret_class <- predict(heart_bag_caret_train, 
     main_test__Heart, type = 'raw')

# ... as probabilities
predict_heart_bag_caret_prob <- predict(heart_bag_caret_train, 
     main_test__Heart, type = 'prob')

# Confusion Matrix
cm_heart_bag_caret <- caret::confusionMatrix(table(
     data = predict_heart_bag_caret_class,
     reference = main_test__Heart$AHD), positive='Yes')
cm_heart_bag_caret


# calculate AUC, and draw ROC
auc_heart_bag_caret <- caTools::colAUC(predict_heart_bag_caret_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_heart_bag_caret


# Compare confusion matrices and AUCs
cm_heart_rpart
cm_heart_rpart_caret
cm_heart_bag_ipred
cm_heart_bag_caret

auc_heart_rpart
auc_heart_rpart_caret
auc_heart_bag_ipred
auc_heart_bag_caret




#########################################################################
###      Save all the objects in current session for further scripts
#########################################################################
ls()
save.image(file = '14b_bagging.RData')
