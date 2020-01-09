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
###                     14a2. CART for Classification                    ###
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 09.01.2020


library(tidyverse)
# install.packages('mice')
library(mice)   # for missing value treatment
library(caret)   
library(rpart)
library(rpart.plot)
library(partykit)
library(caTools) # for plotting the ROC curve
options(scipen = 999)
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
###  II. Classification Trees                                         ###
###    II.1. Classification Trees with `rpart`, `partykit`            ###
###    II.2. Classification Trees with `tidymodels`                   ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################
### see also script `09c...`

#########################################################################
###                       `Heart` disease data set                    ###
###                          (for classification)                     ###
### details about this data set and EDA are to be found in script `09c...`

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
###                  Credit Scoring (G.Sanchez) data set              ###
###                          (for classification)                     ###
### some details about this data set are to be found in script `09c...`

cs_sanchez <- readr::read_csv('CreditScoring_GastonSanchez.csv')
glimpse(cs_sanchez)

# there are no missing values...
anyNA(cs_sanchez)

###  Train and test split                   
set.seed(seed = 1234) 
# notice `strata` option
train_test_split__cs_sanchez <-
  rsample::initial_split(
    data = cs_sanchez,     
    prop = 0.8,
    strata = 'Status'
  ) 
train_test_split__cs_sanchez

main_train__cs_sanchez <- train_test_split__cs_sanchez %>% 
     training() 
main_test__cs_sanchez  <- train_test_split__cs_sanchez %>% 
     testing()



#########################################################################
###                        II. Classification Trees                   ###
#########################################################################


#########################################################################
###         II.1. Classification Trees with `rpart`, `partykit`       ###
#########################################################################

# Recap - main parameters of `rpart`
#  - `method` will be 
#         * `anova` for regression
#         * class` for classification
#  - `minsplit` is the minimum number of observations that must 
#         exist in a node in order for a split to be attempted 
#  - `minbucket` is the minimum number of observations in any 
#         terminal node
#  - `cp` is the complexity parameter; any split that does not 
#         decrease the overall lack of fit by a factor of cp 
#         is not attempted       
#    - `maxdepth` - set the maximum depth of any node of the 
#         final tree


#########################################################################
###                 II.1.a. `Heart` disease data set                  ###
glimpse(main_train__Heart)
glimpse(main_test__Heart)

#########################################################################
#                           Build the unpruned tree
set.seed(123)
heart_rpart_1_unpruned <- rpart::rpart (AHD ~ ., 
     data = main_train__Heart, 
     method = 'class', control = rpart.control(cp = 0))
summary (heart_rpart_1_unpruned)

# The rel error of each iteration of the tree is the fraction 
#    of mislabeled elements in the iteration relative to the 
#    fraction of mislabeled elements in the root. 
rpart.plot(heart_rpart_1_unpruned)

# plot with `partykit`
plot(partykit::as.party(heart_rpart_1_unpruned))

# cross validation results 
printcp(heart_rpart_1_unpruned)
# The cross validation error rates and standard deviations 
# are displayed in the columns `xerror` and `xstd` respectively.

plotcp(heart_rpart_1_unpruned)

# extract the complexity parameter for the optimal model (the lowest `xerror`)
heart_rpart_1_unpruned$cptable[which.min(heart_rpart_1_unpruned$cptable[,"xerror"]),"CP"]
# cp = 0.0179

as_tibble(heart_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     

#########################################################################
#        Build the pruned tree with suggested `cp` for min `xerror`
set.seed(123)
heart_rpart_2 <- rpart::rpart (AHD ~ ., 
     data = main_train__Heart, method = 'class', 
     control = rpart.control(cp = 0.0179))   
summary (heart_rpart_2)

# now the plot is simpler
rpart.plot::rpart.plot(heart_rpart_2)

# plot with `partykit`
plot(partykit::as.party(heart_rpart_2))


#########################################################################
#        Build the pruned tree with `cp` for the minimim
#    number of splits for all trees with `xerror` within one standard 
#    error of the minimum `xerror` 

# extract the cp the tree with the lowest `split` for `xerror` not min,
#    but within a standard deviation
temp <- as.tibble(heart_rpart_1_unpruned$cptable) %>%
     mutate (min_xerror = min(xerror)) %>%
     mutate (sd_min_xerror = if_else(xerror == min_xerror, 
               xstd, 0)) %>%
     mutate(sd_min_xerror = max(sd_min_xerror)) %>%
     mutate (xerror_begin_range = min_xerror - sd_min_xerror, 
             xerror_end_range = min_xerror + sd_min_xerror) %>%
     filter (xerror >= xerror_begin_range & xerror <= xerror_end_range )  %>%
     top_n(1, -nsplit) 
temp

# cp = 0.0179 , so...

set.seed(123)
heart_rpart_3 <- rpart::rpart (AHD ~ ., 
     data = main_train__Heart, method = 'class', 
     control = rpart.control(cp = 0.0179))   
summary (heart_rpart_3)

# now the plot is a bit simpler
rpart.plot::rpart.plot(heart_rpart_3)

# relative to...
rpart.plot::rpart.plot(heart_rpart_2)

# plot with `partykit`
plot(partykit::as.party(heart_rpart_3))


#########################################################################
# Display variables importance for `heart_rpart2` and heart_rpart3` models

set.seed(123)
vi_heart_rpart_2 <- varImp(heart_rpart_2) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance)) 

plot_vi_heart_rpart_2 <- ggplot(data = vi_heart_rpart_2, 
               aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 30, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `heart_rpart_2` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rpart_2

set.seed(123)
vi_heart_rpart_3 <- varImp(heart_rpart_3) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance)) 

plot_vi_heart_rpart_3 <- ggplot(data = vi_heart_rpart_3, 
               aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 30, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `heart_rpart_3` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rpart_3


#########################################################################
#              Evaluate models performance on the test subset 

## predicting the test data sets...

# ... as class
predict_heart_rpart_2_class <- predict(heart_rpart_2, 
     main_test__Heart, type = 'class')
predict_heart_rpart_3_class <- predict(heart_rpart_3, 
     main_test__Heart, type = 'class')


# ... as probabilities
predict_heart_rpart_2_prob <- predict(heart_rpart_2, 
     main_test__Heart, type = 'prob')
predict_heart_rpart_3_prob <- predict(heart_rpart_3, 
     main_test__Heart, type = 'prob')


# Confusion Matrix
cm_heart_rpart_2 <- caret::confusionMatrix(
     data = predict_heart_rpart_2_class,
     reference = main_test__Heart$AHD,   positive='Yes')
cm_heart_rpart_3 <- caret::confusionMatrix(
     data = predict_heart_rpart_3_class,
     reference = main_test__Heart$AHD,   positive='Yes')

cm_heart_rpart_2
cm_heart_rpart_3



# calculate AUC, and draw ROC

auc_heart_rpart_2 <- caTools::colAUC(predict_heart_rpart_2_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
auc_heart_rpart_2
# reference line (50%)
abline(0, 1, lty = 2)

auc_heart_rpart_3 <- caTools::colAUC(predict_heart_rpart_3_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
auc_heart_rpart_3
# reference line (50%)
abline(0, 1, lty = 2)


### we'll get the simplest model as the final model
heart_rpart <- heart_rpart_3
vi_heart_rpart <- vi_heart_rpart_3
plot_vi_heart_rpart <- plot_vi_heart_rpart_3
predict_heart_rpart_class <- predict_heart_rpart_3_class
predict_heart_rpart_prob <- predict_heart_rpart_3_prob
cm_heart_rpart <- cm_heart_rpart_3
auc_heart_rpart <- auc_heart_rpart_3


# remove some intermediary objects
rm(list=ls(pattern="rpart_1"))
rm(list=ls(pattern="rpart_2"))
rm(list=ls(pattern="rpart_3"))

ls()



#########################################################################
###           II.1.b. Credit Scoring (G.Sanchez) data set             ###
glimpse(main_train__cs_sanchez)
glimpse(main_test__cs_sanchez)
table(main_train__cs_sanchez$Status)
table(main_test__cs_sanchez$Status)

#########################################################################
#                           Build the unpruned tree
set.seed(123)
css_rpart_1_unpruned <- rpart (Status ~ ., 
     data = main_train__cs_sanchez, 
     method = 'class', control = rpart.control(cp = 0))
summary (css_rpart_1_unpruned)

# a bushy tree...(run only if you have plenty of time...)
rpart.plot(css_rpart_1_unpruned)

# plot with `partykit`
plot(partykit::as.party(css_rpart_1_unpruned))

# cross validation results 
printcp(css_rpart_1_unpruned)

plotcp(css_rpart_1_unpruned)

# extract the complexity parameter for the optimal model (the lowest `xerror`)
css_rpart_1_unpruned$cptable[which.min(css_rpart_1_unpruned$cptable[,"xerror"]),"CP"]
# cp = 0.008

as_tibble(css_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     

#########################################################################
#        Build the pruned tree with suggested `cp` for min `xerror`
set.seed(123)
css_rpart_2 <- rpart::rpart (Status ~ ., 
     data = main_train__cs_sanchez, method = 'class', 
     control = rpart.control(cp = 0.008))   
css_rpart_2

# now the plot is simpler (still bushy and taking a lot of time)
rpart.plot::rpart.plot(css_rpart_2)

# plot with `partykit`
plot(partykit::as.party(css_rpart_2))


#########################################################################
#        Build the pruned tree with `cp` for the minimim
#    number of splits for all trees with `xerror` within one standard 
#    error of the minimum `xerror` 

# extract the cp the tree with the lowest `split` for `xerror` not min,
#    but within a standard deviation
temp <- as.tibble(css_rpart_1_unpruned$cptable) %>%
     mutate (min_xerror = min(xerror)) %>%
     mutate (sd_min_xerror = if_else(xerror == min_xerror, 
               xstd, 0)) %>%
     mutate(sd_min_xerror = max(sd_min_xerror)) %>%
     mutate (xerror_begin_range = min_xerror - sd_min_xerror, 
             xerror_end_range = min_xerror + sd_min_xerror) %>%
     filter (xerror >= xerror_begin_range & xerror <= xerror_end_range )  %>%
     top_n(1, -nsplit) 
temp

# cp = 0.0145
# 
set.seed(123)
css_rpart_3 <- rpart::rpart (Status ~ ., 
     data = main_train__cs_sanchez, method = 'class', 
     control = rpart.control(cp = 0.0145))   
css_rpart_3

# now the plot is much simpler 
rpart.plot::rpart.plot(css_rpart_3)

# plot with `partykit`
plot(partykit::as.party(css_rpart_3))


#########################################################################
##    Variable importance for `css_rpart_2` and `css_rpart_3` models

# for `css_rpart_2`
set.seed(123)
vi_css_rpart_2 <- varImp(css_rpart_2) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance)) 

plot_vi_css_rpart_2 <- ggplot(data = vi_css_rpart_2, 
               aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 30, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `css_rpart_2` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_css_rpart_2

# for `css_rpart_3`
set.seed(123)
vi_css_rpart_3 <- varImp(css_rpart_3) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance)) 

plot_vi_css_rpart_3 <- ggplot(data = vi_css_rpart_3, 
               aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 30, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `css_rpart_3` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_css_rpart_3


#########################################################################
#              Evaluate models performance on the test subset 

## predicting the test data sets...

# ... as class
predict_css_rpart_2_class <- predict(css_rpart_2, 
     main_test__cs_sanchez, type = 'class')
predict_css_rpart_3_class <- predict(css_rpart_3, 
     main_test__cs_sanchez, type = 'class')

# ... as probabilities
predict_css_rpart_2_prob <- predict(css_rpart_2, 
     main_test__cs_sanchez, type = 'prob')
predict_css_rpart_3_prob <- predict(css_rpart_3, 
     main_test__cs_sanchez, type = 'prob')



# Confusion Matrices
cm_css_rpart_2 <- caret::confusionMatrix(table(
     data = predict_css_rpart_2_class,
     reference = main_test__cs_sanchez$Status), positive='bad')
cm_css_rpart_3 <- caret::confusionMatrix(table(
     data = predict_css_rpart_3_class,
     reference = main_test__cs_sanchez$Status), positive='bad')

cm_css_rpart_2
cm_css_rpart_3

# calculate AUC, and draw ROC

auc_css_rpart_2 <- caTools::colAUC(predict_css_rpart_2_prob, 
                main_test__cs_sanchez$Status, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_css_rpart_2


auc_css_rpart_3 <- caTools::colAUC(predict_css_rpart_3_prob, 
                main_test__cs_sanchez$Status, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_css_rpart_3


### we'll get the simplest model as the final model
css_rpart <- css_rpart_3
vi_css_rpart <- vi_css_rpart_3
plot_vi_css_rpart <- plot_vi_css_rpart_3
predict_css_rpart_class <- predict_css_rpart_3_class
predict_css_rpart_prob <- predict_css_rpart_3_prob
cm_css_rpart <- cm_css_rpart_3
auc_css_rpart <- auc_css_rpart_3


# remove some intermediary objects
rm(list=ls(pattern="rpart_1"))
rm(list=ls(pattern="rpart_2"))
rm(list=ls(pattern="rpart_3"))

ls()



#########################################################################
###              II.2. Classification Trees with `tidymodels`         ###
#########################################################################

#########################################################################
###                 II.1.a. `Heart` disease data set                  ###
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



# we'll search for best `rpart` model, varying three hyperparameters:
#  `cost_complexity`,
#  `min_n` 
# `tree_depth`
grid <- tibble(cost_complexity = seq(0, 1, by = 0.2)) %>%
     mutate (foo = 1) %>%
     inner_join(
          tibble(min_n = seq(5, 15, by = 5)) %>%
               mutate (foo = 1)  ) %>%
     inner_join(
          tibble(tree_depth = seq(5, 30, by = 6)) %>%
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
cart_model__heart <- function(dataset, cost_complexity, min_n, tree_depth) {
     decision_tree(cost_complexity = cost_complexity, min_n = min_n, 
          tree_depth = tree_depth, mode = "classification") %>%
     set_engine("rpart") %>%
     fit(AHD ~ ., data = dataset)
}

# it may take many minutes...
the_pipe__heart <- the_pipe__heart %>%
     mutate (the_model = pmap(list(analysis_juiced,cost_complexity, min_n, tree_depth), 
                              cart_model__heart))

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
performance_rpart__heart <- the_pipe__heart %>%
     mutate (the_metrics = map2(.x = .$predict_class, .y = .$predict_prob, 
               .f = f_performance__heart)) %>%
     select (id, id2, cost_complexity, min_n, tree_depth, the_metrics) %>%
     unnest(the_metrics)

glimpse(performance_rpart__heart)

# average the metrics 
average_model_performance__heart_rpart <- performance_rpart__heart %>%
     group_by(cost_complexity, min_n, tree_depth, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `roc_auc`
top_heart_auc_rpart <- average_model_performance__heart_rpart %>%
     filter (.metric == 'roc_auc') %>%
     arrange(desc(average_estimate))

# model with best ccc
# cp = 0.0	
# min_n = 15	
# tree_depth = 11	

# fit the model for the entire training set
dt_model_heart <- decision_tree( cost_complexity = 0, 
     min_n = 15, tree_depth = 11, mode = "classification") %>% 
     set_engine("rpart") %>%
     fit(AHD ~ ., data = juice(recipe__heart(main_train__heart)))

# now the plot is quite easy to interpret
rpart.plot::rpart.plot(dt_model_heart$fit)

###
##                             Variables importance
set.seed(123)
names(dt_model_heart$fit$variable.importance)

vi_heart_rpart_tidy <- dt_model_heart$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(dt_model_heart$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_heart_rpart_tidy <- ggplot(data = vi_heart_rpart_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Var. importance - `heart_rpart_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rpart_tidy

# compare with... 
plot_vi_heart_rpart


###   save the final model for using in further scripts
save(dt_model_heart, main_train__heart, main_test__heart,
     average_model_performance__heart_rpart,
     vi_heart_rpart_tidy, plot_vi_heart_rpart_tidy,
     file = '14a2_heart.RData')



# # extract the coefficients of the best model
# library(broom)
# 
# temp <- the_pipe_with_models__heart_3 %>%
#      filter (formula == top_5__heart_3$formula[1]) %>%
#      pull(the_model[[1]]) %>%
#      pluck(1) %>%
#      broom::tidy() %>%
#      mutate(odds_ratio = exp(estimate)) 
# View(temp)





