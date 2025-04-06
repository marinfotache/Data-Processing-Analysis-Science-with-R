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
###  13.c.2.a. Performance evaluation of a pair of classification        ###
###  models with the `tidymodels` ecosystem using a train-test split     ### 
###  (an older version)
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 09.01.2025

options(scipen = 999)
library(tidyverse)
library(tidymodels)


############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')
load('ml_datasets.RData')

#####################################################################
###                      II.1 Heart disease                       ###
#####################################################################
## for the description and EDA - see script `09c...``
## for logistic regression models fit - see scripts `11b...` 
glimpse(heart)


#########################################################################
###        I.a Model building and assessment with `tidymodels`        ###
#########################################################################

# ML model based on logistic regression for this data set is also available at:
# https://rileyking.netlify.com/post/heart-disease-prediction-from-patient-data-in-r/


#########################################################################
###                         1 Train and test split                   ###
# randomised training and test split of the original data with `rsample`
set.seed(seed = 1234) 
train_test_split__heart_1 <-
  rsample::initial_split(
    data = heart,     
    prop = 0.66
  ) 
train_test_split__heart_1

train_tbl__heart_1 <- train_test_split__heart_1 %>% training() 
test_tbl__heart_1  <- train_test_split__heart_1 %>% testing()


#########################################################################
###                            2. Two simple recipes

recipe1__heart_1 <- function(dataset) {
     recipe(ahd ~ ., 
            data = dataset) %>%
     prep(data = dataset)
}
     
recipe2__heart_1 <- function(dataset) {
     recipe(ahd ~ age + sex + chol + rest_ecg + max_hr, 
            data = dataset) %>%
     prep(data = dataset)
}


#  prepare the recipe
recipe1_prepped__heart_1 <- recipe1__heart_1(dataset = train_tbl__heart_1)
recipe2_prepped__heart_1 <- recipe2__heart_1(dataset = train_tbl__heart_1)


# In the `bake` step, all preprocessing operations are applied
#    to the test data subsets.
test1_baked__heart_1  <- bake(recipe1_prepped__heart_1, new_data = test_tbl__heart_1)
test2_baked__heart_1  <- bake(recipe2_prepped__heart_1, new_data = test_tbl__heart_1)



#########################################################################
###                      3. Fit the models with `parsnip`  
glm_model1__heart_1 <-   logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(ahd ~ ., 
      data = juice(recipe1_prepped__heart_1))

glm_model2__heart_1 <-   logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(ahd ~ age + sex + chol + rest_ecg + max_hr,
      data = juice(recipe2_prepped__heart_1))


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

#########################################################################
###             D1 Performance assessment both models

# first, predictions as classes (they are necessary for computing accuracy,
#   kappa, f1 score, ...)
predictions_glm1__heart_1__class <- glm_model1__heart_1 %>%
     predict(new_data = test1_baked__heart_1) %>%
     bind_cols(test1_baked__heart_1 %>% select(ahd))
head(predictions_glm1__heart_1__class)

predictions_glm2__heart_1__class <- glm_model2__heart_1 %>%
     predict(new_data = test2_baked__heart_1) %>%
     bind_cols(test2_baked__heart_1 %>% select(ahd))
head(predictions_glm2__heart_1__class)


# second, predictions as class probabilities (necessary for computing
#  Area Under Curve - ROC/AUC)
predictions_glm1__heart_1__prob <- glm_model1__heart_1 %>%
     predict(new_data = test1_baked__heart_1, type = "prob") %>%
     bind_cols(test1_baked__heart_1 %>% select(ahd))
head(predictions_glm1__heart_1__prob)


predictions_glm2__heart_1__prob <- glm_model2__heart_1 %>%
     predict(new_data = test2_baked__heart_1, type = "prob") %>%
     bind_cols(test2_baked__heart_1 %>% select(ahd))
head(predictions_glm2__heart_1__prob)



#########################################################################
###                         D2 Confusion Matrix

#  Confusion Matrix - logistic regression - model1
predictions_glm1__heart_1__class %>%
     conf_mat(ahd, .pred_class) %>%
     pluck(1) %>%
     as_tibble() %>%
     ggplot(aes(Prediction, Truth, alpha = n)) +
     geom_tile(show.legend = FALSE) +
     geom_text(aes(label = n), colour = "red", alpha = 1, size = 8)


#  Confusion Matrix - logistic regression - model2
predictions_glm2__heart_1__class %>%
     conf_mat(ahd, .pred_class) %>%
     pluck(1) %>%
     as_tibble() %>%
     ggplot(aes(Prediction, Truth, alpha = n)) +
     geom_tile(show.legend = FALSE) +
     geom_text(aes(label = n), colour = "red", alpha = 1, size = 8)


#########################################################################
###           D3 Accuracy, precision, recall, F1-score

performance__heart_1 <- bind_rows(
    bind_rows(
        predictions_glm1__heart_1__class %>%
            metrics(truth = ahd, estimate = .pred_class), 
        precision(predictions_glm1__heart_1__class, ahd, .pred_class),
        recall(predictions_glm1__heart_1__class, ahd, .pred_class),
        f_meas(predictions_glm1__heart_1__class, ahd, .pred_class),
        roc_auc(predictions_glm1__heart_1__prob, ahd, .pred_No)
            ) %>%
    mutate (model = 'ahd ~ .'), 
    bind_rows(
        predictions_glm2__heart_1__class %>%
            metrics(truth = ahd, estimate = .pred_class), 
        precision(predictions_glm2__heart_1__class, ahd, .pred_class),
        recall(predictions_glm2__heart_1__class, ahd, .pred_class),
        f_meas(predictions_glm2__heart_1__class, ahd, .pred_class),
        roc_auc(predictions_glm2__heart_1__prob, ahd, .pred_No)
            ) %>%
    mutate (model = 'ahd ~ age + sex + vhol + rest_ecg + max_hr')
                ) %>%
    select (model, everything()) %>%
    arrange(.metric, model)


gridExtra::grid.table(performance__heart_1)
  

# Visualize the ROC curve using ggplot2 `manually`
bind_rows(
    roc_curve(predictions_glm1__heart_1__prob, ahd, .pred_No) %>%
        mutate (model = 'ahd ~ .'), 
    roc_curve(predictions_glm2__heart_1__prob, ahd, .pred_No) %>%
        mutate (model = 'ahd ~ age + sex + vhol + rest_ecg + max_hr')
      ) %>%
ggplot(aes(x = 1 - specificity, y = sensitivity, fill = model, color = model)) +
geom_path() +
geom_abline(lty = 3) +
coord_equal() +
theme_bw() +
theme(legend.position = c(0.6, 0.15))  
  


# with autoplot
bind_rows(
    predictions_glm1__heart_1__prob %>% 
        mutate (model = 'ahd ~ .'), 
    predictions_glm2__heart_1__prob %>% 
        mutate (model = 'ahd ~ age + sex + vhol + rest_ecg + max_hr')
     ) %>%
group_by(model) %>%
roc_curve(., truth = ahd, .pred_No) %>%
autoplot() +
theme_bw() +
theme(legend.position = c(0.6, 0.15))  



