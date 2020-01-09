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
###        14a1. Classification and Regression Trees for Scoring         ###
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/14%20Tree-Based%20Methods/14%20Tree%20Based%20Methods.pptx
############################################################################
## last update: 07.01.2020

library(tidyverse)
library(caret)   
library(rpart)
library(rpart.plot)
library(partykit)
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
###  I. Regression Trees                                              ###
###    I.1. Regression Trees with `rpart`, `partykit`                 ###
###    I.2. Regression Trees with `tidymodels`                        ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

#########################################################################
###                          `States` data set                        ###
#########################################################################
## for the description and EDA - see script `09c...``
## for liniar models fit - see scripts `11a2...` and `11a5....`
states <- as_tibble(state.x77, rownames = 'State')
names(states) <- str_replace_all(names(states), ' |\\.', '_')

# remove `States` column, as it is not relevant for the models
states <- states %>%
     select (-State)

# no missing values
anyNA(states)

###                         Train and test split                   ###
set.seed(seed = 1234) 
train_test_split__states <-
  rsample::initial_split(
    data = states,     
    prop = 0.66
  ) 
train_test_split__states

train_tbl__states <- train_test_split__states %>% 
     training() 
test_tbl__states  <- train_test_split__states %>% 
     testing()



#######################################################################
### 	                    ` insurance` data set                      ###
###                    (for scoring/regression)                     ###
## for the description and EDA - see script `09c...``
insurance <- readr::read_csv('insurance.csv')

# no missing values
anyNA(insurance)

## Set seed for reproducibility
## Split the data into train/test using caret:
set.seed(seed = 1234) 
train_test_split__insurance <-
  rsample::initial_split(
    data = insurance,     
    prop = 0.8
  ) 
train_test_split__insurance

main_train__insurance <- train_test_split__insurance %>% 
     training() 
main_test__insurance  <- train_test_split__insurance %>% 
     testing()

# descriptive statistics
main_train__insurance %>%
     skimr::skim()

main_test__insurance %>%
     skimr::skim()


#########################################################################
###                      `House Prices` (Ames, Iowa)                  ###
###           
### https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data          
###                    (for scoring/regression)                       ###
#########################################################################

kaggle_house_ames <- readr::read_csv('kaggle_house_ames_2_train.csv',
          col_names = TRUE)
glimpse(kaggle_house_ames)

kaggle_house_ames %>%
     skimr::skim()

# remove variables without variability (with only two values and
# one of these two values occurs only once)
# (this is the case with `Utilities`)
test <- kaggle_house_ames %>%
     select_if (function (var) n_distinct(var) > 2 |
          (tibble (col = var) %>%
               group_by(col) %>%
               summarise(n = n()) %>%
               top_n(-1, n) %>%
               ungroup() %>%
               head(1) %>%
               dplyr::select(n) %>%
               pull()) > 1
               )   

# display the variables to be removed
setdiff(names(kaggle_house_ames), names(test))

kaggle_house_ames <- test
names(kaggle_house_ames)




#########################################################################
###                          I. Regression Trees                      ###
#########################################################################

#########################################################################
###             I.1. Regression Trees with `rpart`, `partykit`        ###
#########################################################################

# Main parameters of `rpart`
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
###                              I.1.a. `states`                      ###
#########################################################################

#########################################################################
##                           Build an unprunned tree
set.seed(123)
states_rpart_1_unpruned <- rpart::rpart (Murder ~ ., 
     data = train_tbl__states, 
     method = 'anova', 
     control = rpart.control(cp = 0))   ## cp = 0
summary (states_rpart_1_unpruned)

rpart.plot::rpart.plot(states_rpart_1_unpruned)

# plot better plots with `partykit`
plot(partykit::as.party(states_rpart_1_unpruned))

# When rpart grows a tree it performs 10-fold cross validation 
# on the data. 
# To see the cross validation results use the printcp() function.
printcp(states_rpart_1_unpruned)
# The cross validation error rates and standard deviations 
# are displayed in the columns `xerror` and `xstd` respectively.

plotcp(states_rpart_1_unpruned)

# extract the complexity parameter for the optimal model (the lowest `xerror`)
states_rpart_1_unpruned$cptable[which.min(states_rpart_1_unpruned$cptable[,"xerror"]),"CP"]
# cp = 0!

str(states_rpart_1_unpruned)

as_tibble(states_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     


#########################################################################
#    In this case, the pruned tree is the same with the unpruned one
#              (with suggested `cp` from the above plot)
states_rpart <- states_rpart_1_unpruned


#########################################################################
#                             Display variables importance
states_rpart$variable.importance

# plot the variable importance
set.seed(123)
vi_states_rpart <- caret::varImp(states_rpart) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_states_rpart <- ggplot(data = vi_states_rpart, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.5, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - States - CART - rpart") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)
print(plot_vi_states_rpart)

summary(lm(Murder ~ ., data = states ))


#########################################################################
#              Revealing the Classification Trees high variance
## Split again the dataset into the Training set and Test set
## Set another seed 
set.seed(789)
###                         Train and test split                   ###
train_test_split__states2 <-
  rsample::initial_split(
    data = states,     
    prop = 0.66
  ) 
train_test_split__states2

train_tbl__states2 <- train_test_split__states2 %>% 
     training() 
test_tbl__states2  <- train_test_split__states2 %>% 
     testing()

states_rpart_2_unpruned <- rpart::rpart (Murder ~ ., 
     data = train_tbl__states2, 
     method = 'anova', 
     control = rpart.control(cp = 0))   ## cp = 0
summary (states_rpart_2_unpruned)

rpart.plot::rpart.plot(states_rpart_2_unpruned)
# compare with the first `rpart` model:
rpart.plot::rpart.plot(states_rpart_1_unpruned)

printcp(states_rpart_2_unpruned)

# The cross validation error rates and standard deviations 
# are displayed in the columns `xerror` and `xstd` respectively.

plotcp(states_rpart_2_unpruned)

# extract the complexity parameter for the optimal model (the lowest `xerror`)
states_rpart_2_unpruned$cptable[which.min(states_rpart_2_unpruned$cptable[,"xerror"]),"CP"]
#  cp = 0.153013
# 
as_tibble(states_rpart_2_unpruned$cptable) %>%
     top_n(1, -xerror) 



#########################################################################
#              Evaluate model performance on the test subset 

# predicting the test data sets
predict_states_rpart <- predict(states_rpart, test_tbl__states)

# compute RMSE
rmse_states_rpart <- caret::RMSE(pred = predict_states_rpart, 
                          obs = test_tbl__states$Murder)
rmse_states_rpart

# compute R2
r2_states_rpart <- caret::R2 (predict_states_rpart, test_tbl__states$Murder)
r2_states_rpart




#########################################################################
###                           I.1.b. `insurance`                      ###
#########################################################################
glimpse(main_train__insurance)
#########################################################################
##                           Build an unprunned tree
set.seed(123)
insurance_rpart_1_unpruned <- rpart::rpart (charges ~ ., 
     data = main_train__insurance, method = 'anova', 
     control = rpart.control(cp = 0))   
summary (insurance_rpart_1_unpruned)

# this plot is illisible (it may takes a couple of minutes to display)
rpart.plot::rpart.plot(insurance_rpart_1_unpruned)

# also `partykit` plot is indigestible
plot(partykit::as.party(insurance_rpart_1_unpruned))

# cross validation results 
printcp(insurance_rpart_1_unpruned)

# The cross validation error rates and standard deviations 
# are displayed in the columns `xerror` and `xstd` respectively.
plotcp(insurance_rpart_1_unpruned)

# extract the complexity parameter for the optimal model (the lowest `xerror`)
insurance_rpart_1_unpruned$cptable[which.min(insurance_rpart_1_unpruned$cptable[,"xerror"]),"CP"]
# 0.001883154

as.tibble(insurance_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     


#########################################################################
#        Build the pruned tree (with suggested `cp` for min `xerror`)
set.seed(123)
insurance_rpart_2 <- rpart::rpart (charges ~ ., 
     data = main_train__insurance, method = 'anova', 
     control = rpart.control(cp = 0.00188))   
summary (insurance_rpart_2)

# now the plot slightly more lisible (still, quite bushy) 
rpart.plot::rpart.plot(insurance_rpart_2)

# plot with `partykit`
plot(partykit::as.party(insurance_rpart_2))


#########################################################################
#        Build the pruned tree with `cp` for the minimim
#    number of splits for all trees with `xerror` within one standard 
#    error of the minimum `xerror` model ("one-standard-error" (1SE) rule) 

# Breiman et al. (1984) suggested that in actual practice, 
#    its common to instead use the smallest tree 
#    within 1 standard deviation of the minimum 
#    cross validation error 

# extract the cp the tree with the lowest `split` for `xerror` not min,
#    but within a standard deviation of the min `xerror`
temp <- as.tibble(insurance_rpart_1_unpruned$cptable) %>%
     mutate (min_xerror = min(xerror)) %>%
     mutate (sd_min_xerror = if_else(xerror == min_xerror, 
               xstd, 0)) %>%
     mutate(sd_min_xerror = max(sd_min_xerror)) %>%
     mutate (xerror_begin_range = min_xerror - sd_min_xerror, 
             xerror_end_range = min_xerror + sd_min_xerror) %>%
     filter (xerror >= xerror_begin_range & xerror <= xerror_end_range )  %>%
     top_n(1, -nsplit) 
temp
# cp = 0.00245

set.seed(123)
insurance_rpart_3 <- rpart::rpart (charges ~ ., 
     data = main_train__insurance, method = 'anova', 
     control = rpart.control(cp = 0.00245))   
summary (insurance_rpart_3)

# now the plot is quite easy to interpret
rpart.plot::rpart.plot(insurance_rpart_3, digits = -3)

# plot with `partykit`
plot(partykit::as.party(insurance_rpart_3))




#########################################################################
#              Display variables importance for both 
#    `insurance_rpart_2`  and `insurance_rpart_3` models

set.seed(123)
vi_insurance_rpart_2 <- varImp(insurance_rpart_2) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_insurance_rpart_2 <- ggplot(data = vi_insurance_rpart_2, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance_rpart_2` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_rpart_2

# plot the variable importance
set.seed(123)
vi_insurance_rpart_3 <- varImp(insurance_rpart_3) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_insurance_rpart_3 <- ggplot(data = vi_insurance_rpart_3, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance_rpart_3` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_rpart_3


#########################################################################
#              Evaluate both models performance on the test subset 

# predicting the test data sets
predict_insurance_rpart_2 <- predict(insurance_rpart_2, 
                                     main_test__insurance)
predict_insurance_rpart_3 <- predict(insurance_rpart_3, 
                                     main_test__insurance)

# compute RMSEs
rmse_insurance_rpart_2 <- caret::RMSE(pred = predict_insurance_rpart_2, 
                          obs = main_test__insurance$charges)
rmse_insurance_rpart_3 <- caret::RMSE(pred = predict_insurance_rpart_3, 
                          obs = main_test__insurance$charges)

rmse_insurance_rpart_2
rmse_insurance_rpart_3

# compute R2
r2_insurance_rpart_2 <- caret::R2 (predict_insurance_rpart_2, 
                                   main_test__insurance$charges)
r2_insurance_rpart_3 <- caret::R2 (predict_insurance_rpart_3, 
                                   main_test__insurance$charges)

r2_insurance_rpart_2
r2_insurance_rpart_3


# we'll get the simplest model as the final model
insurance_rpart <- insurance_rpart_3
vi_insurance_rpart <- vi_insurance_rpart_3
plot_vi_insurance_rpart <- plot_vi_insurance_rpart_3
predict_insurance_rpart <- predict_insurance_rpart_3
rmse_insurance_rpart <- rmse_insurance_rpart_3
r2_insurance_rpart <- r2_insurance_rpart_3

# remove some intermediart objects
rm(list=ls(pattern="insurance_rpart_1"))
rm(list=ls(pattern="insurance_rpart_2"))
rm(list=ls(pattern="insurance_rpart_3"))

ls()



#########################################################################
###                  I.2. Regression Trees with `tidymodels`          ###
#########################################################################

#########################################################################
###                              I.2.a. `states`                      ###
#########################################################################


## create 3 times a 5 fold cross-validation on the main train subset
set.seed(12345)
cv_train__states <- vfold_cv(train_tbl__states, v = 5, repeats = 3)


# recipe
recipe__states <- function(dataset) {
     recipe(Murder ~ ., data = dataset) %>%
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
cart_model__states <- function(dataset, cost_complexity, min_n, tree_depth) {
     decision_tree(cost_complexity = cost_complexity, min_n = min_n, 
          tree_depth = tree_depth, mode = "regression") %>%
     set_engine("rpart") %>%
     fit(Murder ~ ., data = dataset)
}

# it may take many minutes...
the_pipe__states <- the_pipe__states %>%
     mutate (the_model = pmap(list(analysis_juiced,cost_complexity, min_n, tree_depth), 
                              cart_model__states))

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
     select (id, id2, cost_complexity, min_n, tree_depth, the_metrics) %>%
     unnest(the_metrics)
glimpse(performance__states)

# average the metrics 
average_model_performance__states_rpart <- performance__states %>%
     group_by(cost_complexity, min_n, tree_depth, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_states_ccc_rpart <- average_model_performance__states_rpart %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best ccc
# cp = 0.0	
# min_n = 10	
# tree_depth = 5	

# fit the model for the entire training set
dt_model_states <- decision_tree( cost_complexity = 0, 
     min_n = 10, tree_depth = 5, mode = "regression") %>% 
     set_engine("rpart") %>%
     fit(Murder ~ ., data = juice(recipe__states(train_tbl__states)))

# now the plot is quite easy to interpret
rpart.plot::rpart.plot(dt_model_states$fit)

###
##                             Variables importance
set.seed(123)
names(dt_model_states$fit$variable.importance)

vi_states_rpart_tidy <- dt_model_states$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(dt_model_states$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_states_rpart_tidy <- ggplot(data = vi_states_rpart_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `states_rpart_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_rpart_tidy

# compare with... 
plot_vi_states_rpart


###   save the final model for using in further scripts
save(dt_model_states, train_tbl__states, test_tbl__states,
     average_model_performance__states_rpart,
     vi_states_rpart_tidy, plot_vi_states_rpart_tidy,
     file = '14a1_states.RData')



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
cart_model__insurance <- function(dataset, cost_complexity, min_n, tree_depth) {
     decision_tree(cost_complexity = cost_complexity, min_n = min_n, 
          tree_depth = tree_depth, mode = "regression") %>%
     set_engine("rpart") %>%
     fit(charges ~ ., data = dataset)
}

# it may take many minutes...
the_pipe__insurance <- the_pipe__insurance %>%
     mutate (the_model = pmap(list(analysis_juiced,cost_complexity, min_n, tree_depth), 
                              cart_model__insurance))

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

names(the_pipe__insurance)
     
# get the model performance as data frame for all folds
performance__insurance <- the_pipe__insurance %>%
     select (id, id2, cost_complexity, min_n, tree_depth, the_metrics) %>%
     unnest(the_metrics)
glimpse(performance__insurance)

# average the metrics 
average_model_performance__insurance_rpart <- performance__insurance %>%
     group_by(cost_complexity, min_n, tree_depth, .metric) %>%
     summarise (average_estimate = mean(.estimate, na.rm = TRUE)) %>%
     ungroup()

# display the best models, ranked by `ccc`
top_insurance_ccc <- average_model_performance__insurance %>%
     filter (.metric == 'ccc') %>%
     arrange(desc(average_estimate))

# model with best average ccc (0.917)
# cp = 0.0	
# min_n = 15	
# tree_depth = 5	

# fit the model for the entire training set
dt_model_insurance <- decision_tree( cost_complexity = 0, 
     min_n = 15, tree_depth = 5, mode = "regression") %>% 
     set_engine("rpart") %>%
     fit(charges ~ ., data = juice(recipe__insurance(main_train__insurance)))

# now the plot is quite easy to interpret
rpart.plot::rpart.plot(dt_model_insurance$fit)

###
##                             Variables importance
set.seed(123)
names(dt_model_insurance$fit$variable.importance)

vi_insurance_rpart_tidy <- dt_model_insurance$fit$variable.importance %>%
     as_tibble() %>%
     transmute (variable = names(dt_model_insurance$fit$variable.importance), 
                importance = value) %>%
     arrange(desc(importance))

plot_vi_insurance_rpart_tidy <- ggplot(data = vi_insurance_rpart_tidy, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance_rpart_tidymodels`") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_rpart_tidy

# compare with... 
plot_vi_insurance_rpart


###   save the final model for using in further scripts
save(dt_model_insurance, main_train__insurance, main_test__insurance,
     average_model_performance__insurance_rpart,
     vi_insurance_rpart_tidy, plot_vi_insurance_rpart_tidy,
     file = '14a1_insurance.RData')

