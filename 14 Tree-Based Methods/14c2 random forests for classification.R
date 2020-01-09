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
###             14c2. Random Forests for Classification Trees            ###
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 09.01.2020

library(tidyverse)
library(caret)     
library(ipred)
library(ranger)
library(caTools)
library(tidymodels)
library(randomForest)
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
###  II. Random Forests for Classification Trees                      ###
###   II.1. Random Forests for Classification Trees with              ###
###       `randomForest` and `ranger`                                 ###
###   II.2. Random Forests for Classification Trees with `tidymodels` ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################
load(file = '14a2_heart.RData')
load(file = '14b2_heart.RData')

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
###             II. Random Forests for Classification Trees           ###
#########################################################################
## packages: `randomForest` and `ranger`

## hyperparameter: `mtry` - number of randomly selected predictors
##   to choose from at each split (node)
## For classification setting, Breiman recommends setting `mtry` on 
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


#########################################################################
#         Evaluate model performance on the test subset and compare 
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




#########################################################################
###   II.2. Random Forests for Classification Trees with `tidymodels` ###
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
rf_model__heart <- function(dataset, mtry, trees) {
     rand_forest(mtry = mtry, trees = trees, mode = "classification") %>%
     set_engine("ranger") %>%
     fit(AHD ~ ., data = dataset)
}


# it may take many minutes...
the_pipe__heart <- the_pipe__heart %>%
     mutate (the_model = pmap(list(analysis_juiced, mtry, trees), 
                              rf_model__heart))


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
performance_rf__heart <- the_pipe__heart %>%
     mutate (the_metrics = map2(.x = .$predict_class, .y = .$predict_prob, 
               .f = f_performance__heart)) %>%
     select (id, id2, mtry, trees, the_metrics) %>%
     unnest(the_metrics)

glimpse(performance_rf__heart)

# average the metrics 
average_model_performance__heart_rf <- performance_rf__heart %>%
     group_by(mtry, trees, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `roc_auc`
top_heart_auc_rf <- average_model_performance__heart_rf %>%
     filter (.metric == 'roc_auc') %>%
     arrange(desc(average_estimate))

# model with best AUC (0.90)
# mtry = 1
# trees = 2000

# fit the model for the entire training set
set.seed(123)
rf_model_heart <- rand_forest(mtry = 1, trees = 2000,
                              mode = "classification") %>%
     set_engine("ranger", importance = "permutation") %>%
     fit(AHD ~ ., data = juice(recipe__heart(main_train__heart)))

###
##                             Variables importance
set.seed(123)
names(rf_model_heart$fit$variable.importance)
vi_heart_rf_tidy <- rf_model_heart$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(rf_model_heart$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_heart_rf_tidy <- ggplot(data = vi_heart_rf_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.02, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Var. importance - `heart_random-forests_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rf_tidy

# compare with 
plot_vi_heart_bagg_tidy

plot_vi_heart_rpart_tidy


###   save the final model for using in further scripts
save(rf_model_heart, main_train__heart, main_test__heart,
     average_model_performance__heart_rf,
     vi_heart_rf_tidy, plot_vi_heart_rf_tidy,
     file = '14c2_heart.RData')
