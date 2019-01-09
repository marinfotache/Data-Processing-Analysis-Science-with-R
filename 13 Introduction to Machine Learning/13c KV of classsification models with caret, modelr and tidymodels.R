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
###            13b. Introduction to Machine Learning with `caret`,        ### 
###            `modelr` and `tidymodels` packages - classification        ###   

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/13%20Introduction%20to%20Machine%20Learning/13_Introduction%20to%20Machine%20Learning.pptx
############################################################################
## last update: 27.12.2018


# needed packages
library(tidyverse)
#install.packages('skimr')
library(skimr)
library(caret)
library(caTools)
library(pROC)
#library(readxl)
library(modelr)
#install.packages('tidymodels')
library(tidymodels)
#library(Metrics)
# install.packages('pryr')
#library(pryr)
options(scipen = 6)


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
###  I. Estimate Performance of a Logistic Regression Model with      ###
###     Cross-Validation using `caret`                                ###
###  II. Estimate Performance of a Logistic Regression Model with     ###
###     Cross-Validation using `modelr`                               ###
###  III. Estimate Performance of a Logistic Regression Model with    ###
###     Cross-Validation using `tidymodels`                           ###
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

#########################################################################
###                       `Heart` disease data set                    ###
###                          (for classification)                     ###
###       sources:
### https://archive.ics.uci.edu/ml/datasets/heart+Disease
###       description: 
### https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/heart-disease.names 
### 

# ### -- Attribute documentation:
#    `Age`: age in years
#    `Sex`: sex (1 = male; 0 = female)
#    `ChestPain`: chest pain type
#              -- Value 1: typical angina
#              -- Value 2: atypical angina
#              -- Value 3: non-anginal pain
#              -- Value 4: asymptomatic
#    `RestBP`: resting blood pressure (in mm Hg on admission to the 
#         hospital)
#    `Chol`: serum cholestoral in mg/dl
#    `Fbs`: (fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)
#    `RestECG`: resting electrocardiographic results. 
#              0: normal, 
#              1: having ST-T wave abnormality (T wave inversions and/or ST, 
#                   elevation or depression of > 0.05 mV) 
#              2: showing probable or definite left ventricular hypertrophy 
#                   by Estes' criteria
#    `MaxHR`: maximum heart rate achieved
#    `ExAng`: exercise induced angina (1 = yes; 0 = no)
#    `Oldpeak`: ST depression induced by exercise relative to rest
#    `Slope`: the slope of the peak exercise ST segment 
#              1: upsloping, 
#              2: flat, 
#              3: downsloping
#    `Ca`: number of major vessels (0-3) colored by flourosopy 
#    `Thal`:    
#              3 = normal; 
#              6 = fixed defect; 
#              7 = reversable defect 
#    `AHD`: diagnosis of heart disease (angiographic disease status). 
#              0: < 50 
#              1: > 50 
#         (in any major vessel: attributes 59 through 68 are vessels)              


Heart <- read_csv('Heart.csv') %>%
     select (-X1) %>%
     mutate(AHD = factor(AHD))
glimpse(Heart)


Heart %>%
     skimr::skim()


# remove the observations with missing values (we can afford 
#    here, since we loose only 6)
Heart_no_na <- read_csv('Heart.csv') %>%
     select (-X1) %>%
     mutate(AHD = factor(AHD)) %>%
     na.omit()

glimpse(Heart_no_na)
table(Heart_no_na$AHD)

Heart_no_na %>%
     skimr::skim()


#########################################################################
###                  Credit Scoring (G.Sanchez) data set              ###
### taken from 
### https://github.com/gastonstat/CreditScoring

##        Variables:
## `Status` - credit status
## `Seniority` - job seniority (years)
## `Home` - type of home ownership
## `Time` - time of requested loan
## `Age` - client's age
## `Marital` - marital status
## `Records` - existance of records
## `Job` - type of job
## `Expenses` - amount of expenses
## `Income` - amount of income
## `Assets` - amount of assets
## `Debt` - amount of debt
## `Amount` - amount requested of loan
## `Price` - price of good

cs_sanchez <- readr::read_csv('CreditScoring_GastonSanchez.csv')
glimpse(cs_sanchez)

cs_sanchez %>%
     skimr::skim()



#########################################################################
###  I. Estimate Performance of a Logistic Regression Model with      ###
###     Cross-Validation using `caret`                                ###
#########################################################################


## The trainControl() function in caret can be adjusted to use 
##   AUC (instead of accuracy), to tune the parameters of trained models.
## The twoClassSummary() convenience function allows for this to be done easily.

## When using twoClassSummary(), be sure to always include the argument
##   `classProbs = TRUE`


#########################################################################
###                   I.a `Heart` disease data set                    ###
glimpse(Heart_no_na)
table(Heart_no_na$AHD)
Heart_no_na %>%
     skimr::skim()


## Set seed for reproducibility
set.seed(123)

## Split the data into train/test using an index of row numbers:
index_heart <- createDataPartition(y = Heart_no_na$AHD, p = .7, list = FALSE)
main_train_heart <- Heart_no_na[index_heart, ]
main_test_heart <- Heart_no_na[-index_heart, ]

# Set up the resampling, here 5-fold KV
logistic_heart_ctrl <- trainControl(method = "cv", 
     number = 5,
     classProbs = TRUE, summaryFunction = twoClassSummary,
     savePredictions = "final")

logistic_heart1_caret <- train(AHD ~ ., data = main_train_heart, 
     trControl = logistic_heart_ctrl,
     method = "glm", family = "binomial", metric = "ROC")

logistic_heart1_caret
summary(logistic_heart1_caret)


# Find out which variables contribute to predictive accuracy:
varImp(logistic_heart1_caret)

plot(varImp(logistic_heart1_caret))


##             Assessing the model performance

# confusion matrix for the test set
caret::confusionMatrix(predict(logistic_heart1_caret, main_test_heart, 
     type = "raw"), main_test_heart$AHD)


# Predict probabilites, calculate AUC, and draw ROC
predicted_prob_logistic_heart1_caret <- predict(logistic_heart1_caret, 
          main_test_heart, type = "prob")

## for computing and plotting the `ROC`, package `caTools` is needed
caTools::colAUC(predicted_prob_logistic_heart1_caret, 
                main_test_heart$AHD, plotROC = TRUE)




#########################################################################
###               I.b Credit Scoring (G.Sanchez) data set             ###
glimpse(cs_sanchez)
cs_sanchez %>%
     skimr::skim()

## Set seed for reproducibility
set.seed(123)

## Split the data into train/test using an index of row numbers:
index_credit <- createDataPartition(y = cs_sanchez$Status, 
                    p = .7, list = FALSE)
main_train_credit <- cs_sanchez[index_credit, ]
main_test_credit <- cs_sanchez[-index_credit, ]

# Set up the resampling, here 5-fold KV
logistic_credit_ctrl <- trainControl(method = "cv", 
     number = 5,
     classProbs = TRUE, summaryFunction = twoClassSummary,
     savePredictions = "final")

logistic_credit_caret <- train(Status ~ ., data = main_train_credit, 
     trControl = logistic_credit_ctrl,
     method = "glm", family = "binomial", metric = "ROC")

logistic_credit_caret
summary(logistic_credit_caret)


# Find out which variables contribute to predictive accuracy:
varImp(logistic_credit_caret)

plot(varImp(logistic_credit_caret))


##             Assessing the model performance

# confusion matrix for the test set
caret::confusionMatrix(table(predict(logistic_credit_caret, main_test_credit, 
     type = "raw"), main_test_credit$Status))


# Predict probabilites, calculate AUC, and draw ROC
predicted_prob_logistic_credit_caret <- predict(logistic_credit_caret, 
          main_test_credit, type = "prob")

## for computing and plotting the `ROC`, package `caTools` is needed
caTools::colAUC(predicted_prob_logistic_credit_caret, 
                main_test_credit$Status, plotROC = TRUE)



#########################################################################
###  II. Estimate Performance of a Logistic Regression Model with     ###
###     Cross-Validation using `modelr`                               ###
#########################################################################

#########################################################################
###                  II.a `Heart` disease data set                    ###

# we already split the date in section `I.a`
main_train_heart 
main_test_heart 

main_train_heart %>%
     skimr::skim()

main_test_heart %>%
     skimr::skim()

#########################################################################
##                       Model fit for each fold
## split the data into 5 folders and build the model for each fold and add the
## main test set into each fold
logistic_heart_modelr <- main_train_heart %>%
     crossv_kfold(5) %>% ## Make the folds and train the models 
     mutate(model = purrr::map(train,                       ## fit a logistic model
          ~ glm(AHD ~ ., data = ., family=binomial))) %>%   ## for each fold
     mutate (main_test_heart = list(main_test_heart))


#########################################################################
##        Model performance on each fold test data (validation) 

## get the model predictions for each fold on validation data (the fold's test set)
logistic_heart_modelr_results_validation <- logistic_heart_modelr %>%
     mutate (results_within_folds = map2(model, test, 
               ~augment(.x, newdata = .y, type.predict = "response"))) %>%
     unnest(results_within_folds) %>%
     transmute(.id, observed_AHD = AHD,
               predicted_AHD_within_folds__prob = .fitted,
               predicted_AHD_within_folds__class = 
                    if_else(predicted_AHD_within_folds__prob >= 0.5,
                         'Yes', 'No'))


## computing and plot the `ROC` for each fold
## 
logistic_heart_modelr_results_validation %>%
     group_by(.id) %>%
     summarise (auc = caTools::colAUC(predicted_AHD_within_folds__prob,
                                      observed_AHD, plotROC = TRUE))

# ... or with function `roc` in package `pROC`
logistic_heart_modelr_results_validation %>%
     group_by(.id) %>%
     summarise (auc = pROC::roc(observed_AHD, predicted_AHD_within_folds__prob)$auc)


#########################################################################
##                  Model performance on each main test data 

## get the model predictions for each fold on validation data (the fold's test set)
logistic_heart_modelr_results_test <- logistic_heart_modelr %>%
     mutate (results_main_test = map2(model, main_test_heart, 
               ~augment(.x, newdata = .y, type.predict = "response"))) %>%
     unnest(results_main_test) %>%
     transmute(.id, observed_AHD = AHD,
               predicted_AHD__prob = .fitted,
               predicted_AHD__class = 
                    if_else(predicted_AHD__prob >= 0.5,
                         'Yes', 'No'))


## computing and plot the `ROC` for each fold
logistic_heart_modelr_results_test %>%
     group_by(.id) %>%
     summarise (auc = caTools::colAUC(predicted_AHD__prob,
                                      observed_AHD, plotROC = TRUE))

logistic_heart_modelr_results_test %>%
     group_by(.id) %>%
     summarise (auc = pROC::roc(observed_AHD, predicted_AHD__prob)$auc)



#########################################################################
###  III. Estimate Performance of a Logistic Regression Model with    ###
###     Cross-Validation using `tidymodels`                           ###
#########################################################################

# https://cdn.rawgit.com/ClaytonJY/tidymodels-talk/145e6574/slides.html#38

#########################################################################
###                  II.a `Heart` disease data set                    ###

# we already split the date in section `I.a`
main_train_heart 
main_test_heart 

main_train_heart %>%
     skimr::skim()

main_test_heart %>%
     skimr::skim()


rec <- main_train_heart %>%
  recipe(AHD ~ .) %>%
  step_bin2factor(all_outcomes())
rec

rec %>%
  prep() %>%
  bake(main_train_heart)







