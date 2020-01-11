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
###                14a. Classification and Regression Trees              ###
############################################################################

### See also the presentation:
### xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
############################################################################
## last update: 15.01.2019

library(tidyverse)
# install.packages('mice')
library(mice)   # for missing value treatment
library(caret)   
library(rpart)
library(rpart.plot)
library(partykit)
#install.packages('party')
#library(party) # for conditional inference tree (ctree)
library(caTools) # for plotting the ROC curve
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
###    I.2. Regression Trees with `caret`                             ###
###    I.3. Regression Trees with `tidymodels` (not yet completed)    ###
###  II. Classification Trees                                         ###
###    II.1. Classification Trees with `rpart`, `partykit`            ###
###    II.2. Classification Trees with `caret`                        ###
###    II.3. Classification Trees with `tidymodels` (not yet          ###
###       completed)                                                  ###
###  III. Conditional inference trees (ctrees)                        ###     
#########################################################################


#########################################################################
###                 O. Import and prepare the data sets               ###
#########################################################################

#########################################################################
###           `states` (USA) data set (see also scripts 13a, 13b)     ###
###                    (for scoring/regression)                       ###
#########################################################################
states <- as_tibble(state.x77, rownames = 'State')
names(states) <- str_replace_all(names(states), ' |\\.', '_')
# no missing values
anyNA(states)

## Set seed for reproducibility
set.seed(123)
## Split the data into train/test using caret:
index__states <- caret::createDataPartition(y = states$Murder, 
     p = .7, list = FALSE)
main_train__states <- states[index__states, ] %>%
     select (-State)
main_test__states <- states[-index__states, ] %>%
     dplyr::select (-State)

# descriptive statistics
main_train__states %>%
     skimr::skim()

main_test__states %>%
     skimr::skim()


#######################################################################
### 	                    ` insurance` data set                      ###
###                    (for scoring/regression)                     ###
### data available on 
### https://github.com/stedy/Machine-Learning-with-R-datasets

## Variables
## `age`: age of primary beneficiary
## `sex`: insurance contractor gender, female, male
## `bmi`: Body mass index, providing an understanding of body, 
##   weights that are relatively high or low relative to height, 
##   objective index of body weight (kg / m ^ 2) using the ratio 
##   of height to weight, ideally 18.5 to 24.9
## `children`: Number of children covered by health insurance / Number 
##   of dependents
## `smoker`: Smoking
## `region`: the beneficiary's residential area in the US, northeast, southeast, 
##        southwest, northwest.
## `charges`: Individual medical costs billed by health insurance
insurance <- readr::read_csv('insurance.csv')

# no missing values
anyNA(insurance)

## Set seed for reproducibility
set.seed(123)
## Split the data into train/test using caret:
index__insurance <- caret::createDataPartition(y = insurance$charges, 
     p = .8, list = FALSE)
main_train__insurance <- insurance[index__insurance, ]
main_test__insurance <- insurance[-index__insurance, ] 


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


## the analysis will be completed during lectures


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

## Set seed for reproducibility
set.seed(123)
## Split the data into train/test using caret:
index__Heart <- caret::createDataPartition(y = Heart_mice$AHD, 
     p = .8, list = FALSE)
main_train__Heart <- Heart_mice[index__Heart, ]
main_test__Heart <- Heart_mice[-index__Heart, ] 


# descriptive statistics
main_train__Heart %>%
     skimr::skim()

main_test__Heart %>%
     skimr::skim()


#########################################################################
###                  Credit Scoring (G.Sanchez) data set              ###
###                          (for classification)                     ###
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

# there are no missing values...
anyNA(cs_sanchez)


## Set seed for reproducibility
set.seed(123)
## Split the data into train/test using caret:
index__cs_sanchez <- caret::createDataPartition(y = cs_sanchez$Status, 
     p = .8, list = FALSE)
main_train__cs_sanchez <- cs_sanchez[index__cs_sanchez, ]
main_test__cs_sanchez <- cs_sanchez[-index__cs_sanchez, ] 

# descriptive statistics
main_train__cs_sanchez %>%
     skimr::skim()

main_test__cs_sanchez %>%
     skimr::skim()


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
     data = main_train__states, 
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
vi_states_rpart <- varImp(states_rpart) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_states_rpart <- ggplot(data = vi_states_rpart, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - States - Regression Trees - rpart") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)
print(plot_vi_states_rpart)

summary(lm(Murder ~ ., data = states %>% select (-State)))

#########################################################################
#              Revealing the Classification Trees high variance
## Split again the dataset into the Training set and Test set
## Set another seed 
set.seed(789)
## Split the data into train/test using caret:
index__states2 <- caret::createDataPartition(y = states$Murder, 
     p = .7, list = FALSE)
main_train__states2 <- states[index__states2, ] %>%
     select (-State)
main_test__states2 <- states[-index__states2, ] %>%
     select (-State)

# descriptive statistics
main_train__states2 %>%
     skimr::skim()

main_test__states2 %>%
     skimr::skim()


#                        fit another tree on the current split
set.seed(789)
states2_rpart_1_unpruned <- rpart::rpart (Murder ~ ., 
     data = main_train__states2, 
     method = 'anova', control = rpart.control(cp = 0))   
summary (states2_rpart_1_unpruned)
rpart.plot::rpart.plot(states2_rpart_1_unpruned)
# compare with the first `rpart` model:
rpart.plot::rpart.plot(states_rpart_1_unpruned)

printcp(states2_rpart_1_unpruned)

# The cross validation error rates and standard deviations 
# are displayed in the columns `xerror` and `xstd` respectively.

plotcp(states2_rpart_1_unpruned)

# extract the complexity parameter for the optimal model (the lowest `xerror`)
states2_rpart_1_unpruned$cptable[which.min(states2_rpart_1_unpruned$cptable[,"xerror"]),"CP"]
# again cp = 0
# 
as.tibble(states2_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 

rm(list=ls(pattern="states2"))
rm(list=ls(pattern="states_rpart_1_unpruned"))


#########################################################################
#              Evaluate model performance on the test subset 

# predicting the test data sets
predict_states_rpart <- predict(states_rpart, main_test__states)

# compute RMSE
rmse_states_rpart <- caret::RMSE(pred = predict_states_rpart, 
                          obs = main_test__states$Murder)
rmse_states_rpart

# compute R2
r2_states_rpart <- caret::R2 (predict_states_rpart, main_test__states$Murder)
r2_states_rpart


# all the objects will be saved at the end of this script

predict_states_rpart2 <- predict(states2_rpart_1_unpruned, 
                                 main_test__states)
rmse_states_rpart2 <- caret::RMSE(pred = predict_states_rpart2, 
                          obs = main_test__states$Murder)
rmse_states_rpart
rmse_states_rpart2



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
# cp = 0.000847

as.tibble(insurance_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     


#########################################################################
#        Build the pruned tree (with suggested `cp` for min `xerror`)
set.seed(123)
insurance_rpart_2 <- rpart::rpart (charges ~ ., 
     data = main_train__insurance, method = 'anova', 
     control = rpart.control(cp = 0.000847))   
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
# cp = 0.00524 

set.seed(123)
insurance_rpart_3 <- rpart::rpart (charges ~ ., 
     data = main_train__insurance, method = 'anova', 
     control = rpart.control(cp = 0.00524))   
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

# all the objects will be saved at the end of this script



#########################################################################
###                I.2. Regression Trees with `caret`                 ###
#########################################################################

#########################################################################
###                              I.2.a. `states`                      ###
#########################################################################

#########################################################################
##                             The model
set.seed(123)
states_rpart_caret_train <- caret::train( Murder ~., 
     data = main_train__states, 
     trControl = trainControl(method = "cv", number = 5), 
     method = "rpart")

states_rpart_caret_train

states_rpart_caret <- states_rpart_caret_train$finalModel

rpart.plot::rpart.plot(states_rpart_caret)

# `partykit` plot
plot(partykit::as.party(states_rpart_caret))

# display the complexity parameter
plot(states_rpart_caret_train)

# Print the best tuning parameter cp that
# minimize the model RMSE
states_rpart_caret_train$bestTune


#########################################################################
##                             Variables importance
set.seed(123)
vi_states_rpart_caret <- varImp(states_rpart_caret) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_states_rpart_caret <- ggplot(data = vi_states_rpart_caret, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance_rpart_caret` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_rpart_caret

# compare with... 
plot_vi_states_rpart


#########################################################################
#              Evaluate both models performance on the test subset 

# predicting the test data sets
predict_states_rpart_caret <- predict(states_rpart_caret_train, 
                                      main_test__states)

# compute RMSE
rmse_states_rpart_caret <- caret::RMSE(pred = predict_states_rpart_caret, 
     obs = main_test__states$Murder)

# compute R2
r2_states_rpart_caret <- caret::R2 (predict_states_rpart_caret, 
     main_test__states$Murder)


# compare
rmse_states_rpart
rmse_states_rpart_caret

r2_states_rpart
r2_states_rpart_caret

# all the objects will be saved at the end of this script


#########################################################################
###                           I.2.b. `insurance`                      ###
#########################################################################

#########################################################################
##                             The model
set.seed(123)
# train the model using 10-fold CV
insurance_rpart_caret_train <- caret::train(charges ~ ., 
     data = main_train__insurance,
     trControl = caret::trainControl(method = "cv", number = 10), 
     method = "rpart")

insurance_rpart_caret_train

insurance_rpart_caret <- insurance_rpart_caret_train$finalModel
insurance_rpart_caret

rpart.plot::rpart.plot(insurance_rpart_caret, digits = -3)

# plot with `partykit`
plot(partykit::as.party(insurance_rpart_caret))

# display the complexity parameter
plot(insurance_rpart_caret_train)

# Print the best tuning parameter cp that
# minimize the model RMSE
insurance_rpart_caret_train$bestTune


#########################################################################
##                             Variables importance
set.seed(123)
vi_insurance_rpart_caret <- varImp(insurance_rpart_caret) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance))

plot_vi_insurance_rpart_caret <- ggplot(data = vi_insurance_rpart_caret, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `insurance_rpart_caret` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_insurance_rpart_caret

# compare it with... 
plot_vi_insurance_rpart


#########################################################################
#               Evaluate both models performance on the test subset 

# predicting the test data sets
predict_insurance_rpart_caret <- predict(insurance_rpart_caret_train, 
               main_test__insurance)

# compute RMSE
rmse_insurance_rpart_caret <- caret::RMSE(pred = predict_insurance_rpart_caret, 
     obs = main_test__insurance$charges)

# compute R2
r2_insurance_rpart_caret <- caret::R2 (predict_insurance_rpart_caret, 
     main_test__insurance$charges)

# compare
rmse_insurance_rpart
rmse_insurance_rpart_caret

r2_insurance_rpart
r2_insurance_rpart_caret

# all the objects will be saved at the end of this script


#########################################################################
###                        II. Classification Trees                   ###
#########################################################################

#########################################################################
###         II.1. Classification Trees with `rpart`, `partykit`       ###
#########################################################################

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
# cp = 0

as.tibble(heart_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     

#########################################################################
#        Build the pruned tree with suggested `cp` for min `xerror`
set.seed(123)
heart_rpart_2 <- rpart::rpart (AHD ~ ., 
     data = main_train__Heart, method = 'class', 
     control = rpart.control(cp = 0))   
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

# cp = 0.00893 , so...

set.seed(123)
heart_rpart_3 <- rpart::rpart (AHD ~ ., 
     data = main_train__Heart, method = 'class', 
     control = rpart.control(cp = 0.00893))   
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


# all the objects will be saved at the end of this script


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
# cp = 0.002

as.tibble(css_rpart_1_unpruned$cptable) %>%
     top_n(1, -xerror) 
     

#########################################################################
#        Build the pruned tree with suggested `cp` for min `xerror`
css_rpart_2 <- rpart::rpart (Status ~ ., 
     data = main_train__cs_sanchez, method = 'class', 
     control = rpart.control(cp = 0.002))   
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

# cp = 0.004
# 
set.seed(123)
css_rpart_3 <- rpart::rpart (Status ~ ., 
     data = main_train__cs_sanchez, method = 'class', 
     control = rpart.control(cp = 0.004))   
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


# all the objects will be saved at the end of this script


#########################################################################
###                II.2. Classification Trees with `caret`            ###
#########################################################################


#########################################################################
###                 II.2.a. `Heart` disease data set                  ###
glimpse(main_train__Heart)
main_train__Heart$AHD

# Set up the resampling, here 5-fold KV
heart_rpart_ctrl <- trainControl(method = "cv", 
     number = 5,
     classProbs = TRUE, summaryFunction = twoClassSummary,
     savePredictions = "final")

set.seed(123)
heart_rpart_caret_train <- train(AHD ~ ., data = main_train__Heart, 
     method = "rpart", trControl = heart_rpart_ctrl, 
     metric = "ROC", na.action = na.pass)
 
heart_rpart_caret <- heart_rpart_caret_train$finalModel    

heart_rpart_caret
rpart.plot(heart_rpart_caret)

# plot with `partykit`
plot(partykit::as.party(heart_rpart_caret))


#########################################################################
##    Variable importance for `heart_rpart_caret$finalModel` 
set.seed(123)
vi_heart_rpart_caret <- varImp(heart_rpart_caret) %>%
     tibble::rownames_to_column() %>%
     rename(variable = rowname, importance = Overall) %>%
     arrange(desc(importance)) 

plot_vi_heart_rpart_caret <- ggplot(data = vi_heart_rpart_caret, 
     aes(x = reorder(variable, importance), fill = variable)) +
  	     geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	     geom_text(aes(x=variable, y=importance,  
  		     label=variable, hjust=ifelse(importance > 30, 1.1, -0.1)), 
               position = position_dodge(width=1)) +
  	     scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - `heart_rpart_caret` model ") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_heart_rpart_caret

#########################################################################
#              Evaluate model performance on the test subset 

## predicting the test data sets...

# ... as class
predict_heart_rpart_caret_class <- predict(heart_rpart_caret_train, 
     main_test__Heart, type = 'raw', na.action = na.omit)

# ... as probabilities
predict_heart_rpart_caret_prob <- predict(heart_rpart_caret_train, 
     main_test__Heart, type = 'prob')

# Confusion Matrix
cm_heart_rpart_caret <- caret::confusionMatrix(table(
     data = predict_heart_rpart_caret_class,
     reference = main_test__Heart$AHD), positive='Yes')
cm_heart_rpart_caret


# calculate AUC, and draw ROC
auc_heart_rpart_caret <- caTools::colAUC(predict_heart_rpart_caret_prob, 
                main_test__Heart$AHD, plotROC = TRUE)
# reference line (50%)
abline(0, 1, lty = 2)
auc_heart_rpart_caret


# compare the metrics
cm_heart_rpart
cm_heart_rpart_caret

auc_heart_rpart
auc_heart_rpart_caret


# all the objects will be saved at the end of this script


#########################################################################
###             III. Conditional inference trees (ctrees)             ###     
#########################################################################
### Conditional inference tree (ctree) (Hothorn et al. 2006) uses 
### significance test methods to select and split recursively 
### the most related predictor variables to the outcome. 
### At each splitting step, the algorithm stops if there is no dependency
### between predictor variables and the outcome variable. 
### Otherwise the variable that is the most associated to the outcome 
### is selected for splitting.


#########################################################################
###                              III.a. `states`                      ###
#########################################################################
set.seed(123)
states_ctree <- partykit::ctree(Murder ~ ., data = main_train__states)
summary(states_ctree)
states_ctree

plot(states_ctree)

#########################################################################
#                             Display variables importance

# plot the variable importance
set.seed(123)
vi_states_ctree <- partykit::varimp(states_ctree) %>%
     tibble(variable = names(.), importance = .) %>%
     arrange(desc(importance))

plot_vi_states_ctree <- ggplot(data = vi_states_ctree, 
       aes(x = reorder(variable, importance), fill = variable)) +
  	geom_bar(stat="identity", 
  		aes(y=importance), position="dodge") +
  	geom_text(aes(x=variable, y=importance,  
  		label=variable, hjust=ifelse(importance > 0.75, 1.1, -0.1)), 
      position = position_dodge(width=1)) +
  	scale_y_continuous(labels = waiver()) + coord_flip() +
 		ggtitle("Variable importance - States - ctree - partykit") +
		theme(axis.text.y = element_blank(), 
			text=element_text(size=12)) +
	   	xlab("variable") + ylab("importance") +
		scale_fill_discrete(guide=FALSE)

plot_vi_states_ctree

#########################################################################
#              Evaluate model performance on the test subset 

# predicting the test data sets
predict_states_ctree <- predict(states_ctree, main_test__states)

# compute RMSE
rmse_states_ctree <- caret::RMSE(pred = predict_states_ctree, 
                          obs = main_test__states$Murder)
rmse_states_ctree
rmse_states_rpart
rmse_states_rpart_caret

# compute R2
r2_states_ctree <- caret::R2 (predict_states_ctree, main_test__states$Murder)
r2_states_ctree
r2_states_rpart




#########################################################################
###      Save all the objects in current session for further scripts
#########################################################################
ls()
rm(test, temp)
save.image(file = '14a_CART.RData')


