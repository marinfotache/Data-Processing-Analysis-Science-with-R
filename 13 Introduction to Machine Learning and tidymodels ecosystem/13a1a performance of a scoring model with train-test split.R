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
###         13.a.1.a. Performance evaluation of a pair of scoring        ###
###  models with the `tidymodels` ecosystem using a train-test split     ### 
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 06.12.2021

options(scipen = 999)
library(tidyverse)
# install.packages('tidymodels')
library(tidymodels)


############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')



#########################################################################
###                         I. `States` data set                   ###
#########################################################################
## for the description and EDA - see script `09c...``
## for linier models fit - see scripts `11a2...` and `11a5....`
states <- as_tibble(state.x77, rownames = 'State')
names(states) <- str_replace_all(names(states), ' |\\.', '_')

# remove `States` column, as it is not relevant for the models
states <- states %>%
     select (-State)


#########################################################################
###        I.a Model building and assessment with `tidymodels`        ###
#########################################################################

#########################################################################
###                         1 Train and test split                   ###
# rsample provides a streamlined way to create a randomized 
#    training and test split of the original data.
set.seed(seed = 1234) 
train_test_split__states_1 <- rsample::initial_split(
    data = states,     
    prop = 0.66
  ) 
train_test_split__states_1

train_tbl__states_1 <- train_test_split__states_1 %>%  training() 
test_tbl__states_1  <- train_test_split__states_1 %>%  testing()



#########################################################################
###                         2. Two simple recipes

recipe1__states_1 <- recipes::recipe(Murder ~ Area + Frost + Illiteracy + 
  Life_Exp + Population, data = train_tbl__states_1) 

recipe2__states_1 <- recipes::recipe(Murder ~ HS_Grad + Income + Population, 
            data = train_tbl__states_1) 


#########################################################################
###                      3. Create the models

# `parsnip` offers a unified API that allows access to 
#    several machine learning packages without the need 
#    to learn the syntax of each individual one.

# With 3 simple steps you can:
#    - set the type of model you want to fit (here is a 
#    linear regression)
#    -  decide which computational engine to use (`lm` in this case)
#    -  spell out the exact model specification to fit 
# lm_model1__states_1 <- linear_reg() %>%
#   set_engine("lm") %>%
#   fit(Murder ~ Area + Frost + Illiteracy + Life_Exp + Population, 
#       data = juice(recipe1_prepped__states_1))
# 
# lm_model2__states_1 <- linear_reg() %>%
#   set_engine("lm") %>%
#   fit(Murder ~ HS_Grad + Income + Population, 
#       data = juice(recipe2_prepped__states_1))

lm_model1__states_1 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Murder ~ Area + Frost + Illiteracy + Life_Exp + Population, 
      data = train_tbl__states_1)

lm_model2__states_1 <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Murder ~ HS_Grad + Income + Population, 
      data = train_tbl__states_1)


# model results
lm_model1__states_1 %>% extract_fit_engine() %>% summary()
lm_model2__states_1 %>% extract_fit_engine() %>% summary()


# extract predictors coefficients in the tidy format
lm_model1__states_1 %>% extract_fit_engine() %>% tidy()
lm_model2__states_1 %>% extract_fit_engine() %>% tidy()

# model overall statistics in the tidy format
lm_model1__states_1 %>% extract_fit_engine() %>% glance()
lm_model2__states_1 %>% extract_fit_engine() %>% glance()




#######################################################################
###             4. Performance assessment of the models
#######################################################################

# The `yardstick` package provides an easy way to calculate 
#    several assessment measures. 
# But before I can evaluate my model’s performance, 
#    I need to calculate some predictions by passing 
#    the `test_baked` data to the `predict` function.




######################################################################
###                      4.1 Model predictions

# Numeric predictions always in a df
# with column `.pred`
# test1_pred__states_1 <- lm_model1__states_1 %>%
#   predict(test1_baked__states_1) %>%
#   bind_cols(test1_baked__states_1) %>% 
#   dplyr::select(Murder, .pred) 
#   
# test2_pred__states_1 <- lm_model2__states_1 %>%
#   predict(test2_baked__states_1) %>%
#   bind_cols(test2_baked__states_1) %>%
#   dplyr::select(Murder, .pred) 


test1_pred__states_1 <- predict(lm_model1__states_1, new_data = test_tbl__states_1) %>%
    bind_cols(test_tbl__states_1) %>% 
    select(Murder, .pred) 
  
test2_pred__states_1 <- predict(lm_model2__states_1, new_data = test_tbl__states_1) %>%
    bind_cols(test_tbl__states_1) %>%
    dplyr::select(Murder, .pred) 




######################################################################
###                      4.2 Performance metrics
# yardstick loaded by tidymodels
perf_metrics__states_1 <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test1_pred__states_1  %>% 
  perf_metrics__states_1(truth = Murder, estimate = .pred)

test2_pred__states_1  %>% 
  perf_metrics__states_1(truth = Murder, estimate = .pred)


### aggregate in a tibble
performance__states_1 <- bind_rows(
     test1_pred__states_1  %>% 
          perf_metrics__states_1(truth = Murder, estimate = .pred) %>%
          transmute (model = 'Murder ~ Area + Frost + Illiteracy + Life_Exp + Population',
                     .metric, .estimate),
     test2_pred__states_1  %>% 
          perf_metrics__states_1(truth = Murder, estimate = .pred) %>%
          transmute (model = 'Murder ~ HS_Grad + Income + Population',
                     .metric, .estimate)
     ) %>%
     arrange(.metric, model)


# check the RMSE 
# 

# model 1
check_model1 <- lm(Murder ~ Area + Frost + Illiteracy + Life_Exp + Population, 
                       data = train_tbl__states_1)
summary(check_model1)

pred1 <- predict (check_model1, newdata = test_tbl__states_1)
SSE1 <- sum((test_tbl__states_1$Murder - pred1)^2)
RMSE1 = sqrt(SSE1 / length(pred1))
RMSE1

performance__states_1 %>% 
     filter (model == 'Murder ~ Area + Frost + Illiteracy + Life_Exp + Population' &
                  .metric == 'rmse') %>%
     pull(.estimate)


# model 2
check_model2 <- lm(Murder ~ HS_Grad + Income + Population, 
                       data = train_tbl__states_1)
summary(check_model2)

pred2 <- predict (check_model2, newdata = test_tbl__states_1)
SSE2 <- sum((test_tbl__states_1$Murder - pred2)^2)
RMSE2 = sqrt(SSE2 / length(pred2))
RMSE2

performance__states_1 %>% 
     filter (model == 'Murder ~ HS_Grad + Income + Population' &
                  .metric == 'rmse') %>%
     pull(.estimate)



