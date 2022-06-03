########################################################################
### Case study: EDA + ML - Performance analysis PostgreSQL-Hadoop/Hive 
########################################################################
## last update: 31.03.2022

options(scipen = 999)
library(tidyverse) 
library(corrr)
library(corrgram)
library(readxl)
library(DataExplorer)
library(inspectdf)
library(ggstatsplot)
library(ranger)   # for Random Forest models
library(xgboost)  # for XGBoost models
library(broom)
library(tidymodels)
library(vip)


############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

load(file = 'ih2016.RData')
glimpse(ih2016)

############################################################################
###                       I. Prepare the data set
############################################################################

### keep only some variables of interest
df <- ih2016 %>%
     transmute (query_id = queryId, duration, scale_factor = scale,
                dbserver, nof_joins, nof_attributes_select, 
                nof_attributes_where, avg_nof_rows, max_nof_rows, 
                n_of_in = nof_all_in_s, n_of_between = nof_all_between_s,
                n_of_in_values = nof_all_in_values)



############################################################################
###                       II. Exploratory Data Analysis
############################################################################


# descriptive statistics
df %>%
     select (-query_id) %>%
     skimr::skim()


#################################################################
##     Display the number missing values for each variable     ## 

missing_vals <- df %>%
     select (-query_id) %>%
     map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
     tibble(variable = names(.), n_missing = .) %>%
     mutate (percent_missing = round(n_missing * 100 / 
               nrow(df), 2))

# now, the plot
ggplot(missing_vals, 
     aes (x = variable, y = n_missing, fill = variable)) +
     geom_col() +
     coord_flip() +
     geom_text(aes(label = paste0(percent_missing, '%'), size = 3.5, 
               hjust = if_else(percent_missing > 3, 1.02, -0.03), 
               vjust = 0.5))  +
     theme(legend.position="none") + # this will remove the legend
     scale_y_continuous(limits = c(0,170), breaks = seq(0, 170, 20)) 



#################################################################
##     Display the frequency (and the percent) of the          ##
##           values for each character/factor variable         ##

# first, compute the frequencies for each categorical variables and values
eda_factors <- df %>%
     select (-query_id) %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(df),2)) %>%
     arrange(variable, value)
View(eda_factors)

glimpse(eda_factors)


# plot the factors values 
eda_factors %>%
     group_by(variable) %>%
     summarise(n_of_values = n()) %>%
#     filter (n_of_values < 20) %>%    
     ungroup() %>%
     dplyr::select (variable) %>%
     inner_join(eda_factors) %>%
ggplot(., aes(x = value, y = n_value, fill = value)) +
     geom_col() +
     geom_text (aes(label = paste0(round(percent,0), '%'), 
                  vjust = if_else(n_value > 300, 1.5, -0.5))) +
    facet_wrap(~ variable, scale = "free") +
    theme(legend.position="none") + # this will remove the legend
    theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5)) +
    theme(strip.text.x = element_text(size = 14)) +
    xlab("") + ylab("frequency") 



#################################################################
##    Display the distribution (as histograms,                 ##
##   density plots and boxplots) of each numeric variable      ##

num_variables <- df %>%
     select (-query_id) %>%
     select_if(., is.numeric ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) 
View(num_variables)


# separate histogram for each numeric value; free scale
num_variables %>%
ggplot(., aes(x = value, fill = variable)) +
     geom_histogram() +
     facet_wrap(~ variable, scale = "free") +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("") + ylab("frequency") 

table(num_variables$variable)

num_variables %>%
     filter (variable %in% c('avg_nof_rows', 'max_nof_rows', 'n_of_between')) %>%
ggplot(., aes(x = value, fill = variable)) +
     geom_histogram() +
     facet_wrap(~ variable, scale = "free") +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 10)) +
     xlab("") + ylab("frequency") 



# separate density plot for each numeric value; free scale
num_variables %>%
ggplot(., aes(x = value, fill = variable, alpha = 0.5)) +
     geom_density() +
     facet_wrap(~ variable, scale = "free") +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("") + ylab("frequency") 



# boxplot (free scale)
num_variables %>%
ggplot(., aes(y = value)) +
     geom_boxplot() +
     facet_wrap(~ variable, scales = 'free') +
     theme(legend.position="none") + # this will remove the legend
     xlab("") + ylab("value") +
     theme(axis.text.x = element_blank()) 



#################################################################
##    Display correlation  among all numeric variables         ##
## with package `corrr` (part of tidymodels)                   ##

# display correlations
temp <-  df %>%
     select (-query_id) %>%
     select_if(is.numeric) %>%
     corrr::correlate()  # Create correlation data frame 
View(temp)


# ... a better look...
temp <-  df %>%
     select (-query_id) %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     corrr::rearrange() %>%  # rearrange by correlations
     corrr::shave() # Shave off the upper triangle for a clean result
View(temp)
     

# the correlation plot
 df %>%
     select (-query_id) %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     corrr::rplot()


# another series of correlation plot

corrplot::corrplot(cor( df %>% select (-query_id) %>% 
                             select_if(is.numeric), 
             method = "spearman"), method = "number", type = "upper")

glimpse(df)
df_ok <- df %>%
     select (-max_nof_rows, -n_of_in, -avg_nof_rows, 
             -n_of_between)

corrplot::corrplot(cor( df_ok %>% select (-query_id) %>% 
                             select_if(is.numeric), 
             method = "spearman"), method = "number", type = "upper")



# scatter plots of the outcome vs. all other variables
df %>%
     select (-query_id, -dbserver) %>%
     pivot_longer(-duration, names_to = 'Predictor', values_to = "Value") %>%
ggplot(., aes(x = Value, y = duration)) +
     facet_wrap(~ Predictor, scale = "free_x") +
     geom_point() +
     geom_smooth(col = "darkgreen") +
     geom_smooth(method = "lm", col = "red") +
     theme_bw() +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("")  



############################################################################
###                      III. Inferential Statistics
############################################################################
glimpse(df)
table(df$scale_factor)

############################################################################
###                     Research Question no 1 (RQ1)
### Does duration significantly differ between PostgreSQL (XL) and 
### Hadoop/Hive?

### test: t-test 
### t-test assumptions:
###  (1) duration must be normal
###  (2) duration must have a homogenic variance (for both servers)
###  

### 	           Shapiro-Wilk Test for Normality                 ###
## 	test the normality of variable `duration`                 
# H0: the distribution of `duration` is normal
# Ha: variable `duration` values do NOT follow a normal distribution
shapiro.test(df$duration)
# W = 0.8838, p-value < 0.00000000000000022
# As the p-value is smaller than 0.05,  H0 is rejected.
# `duration`  distribution is far from normal


## As the distribution of duration is not normal, instead of t-test
## the Mann-Whitney (U) test will be used 
## H0: both servers have similar performance

wilcox.test(duration ~ dbserver , data = df)
# W = 3517508, p-value < 0.00000000000000022

# H0 is rejected: performance differences between Pg and Hive are
# statistically significant


# https://indrajeetpatil.github.io/ggstatsplot/
#

# overall analysis
ggbetweenstats(
     data = df, 
     x = dbserver,
     y = duration,
     type = 'np'
)


# analysis on each scale factor
table(df$scale_factor)

# RQ1a
#  analysis for scale_factor = 0.1
ggbetweenstats(
     data = df %>% filter (scale_factor == 0.1), 
     x = dbserver,
     y = duration,
     type = 'np'
)


# RQ1b
#  analysis for scale_factor = 0.25
ggbetweenstats(
     data = df %>% filter (scale_factor == 0.25), 
     x = dbserver,
     y = duration,
     type = 'np'
)

# RQ1c
#  analysis for scale_factor = 0.5
ggbetweenstats(
     data = df %>% filter (scale_factor == 0.5), 
     x = dbserver,
     y = duration,
     type = 'np'
)


# RQ1d
#  analysis for scale_factor = 1
ggbetweenstats(
     data = df %>% filter (scale_factor == 1), 
     x = dbserver,
     y = duration,
     type = 'np'
)



############################################################################
###                     Research Question no 2 (RQ2)
### Is `duration` associated with the number  of joins?

### H0: No association!

ggscatterstats(
     data = df , 
     x = nof_joins,
     y = duration,
     type = 'np')

## p-value < 0.001, effect size = 0.18, CI 95% [0.15, 021]
## H0 is rejected; variables are associated (weak to medium)


###                   Research Question no 2a (RQ2a)
### Is `duration` associated with the number  of joins in PostgreSQL?
ggscatterstats(
     data = df %>% filter(dbserver == 'PostgreSQL'), 
     x = nof_joins,
     y = duration,
     type = 'np')

## p-value < 0.001, effect size = 0.21, CI 95% [0.17, 025]
## H0 is rejected; variables are associated (weak to medium)


###            Research Question no 2b (RQ2b)
### Is `duration` associated with the number  of joins in Hadoop/Hive?
ggscatterstats(
     data = df %>% filter(dbserver == 'Hive'), 
     x = nof_joins,
     y = duration,
     type = 'np')

## p-value < 0.001, effect size = 0.31, CI 95% [0.26, 0.35]
## H0 is rejected; variables are associated (weak to medium)




############################################################################
###                      IV. Machine Learning models
############################################################################


# just a regression ...
lm_ih <- lm(duration ~ ., data = df_ok %>% select (-query_id))
summary(lm_ih)



##########################################################################
###                             Main split of the data           
###
set.seed(1234)
splits   <- initial_split(df_ok %>% select (-query_id), prop = 0.75)
train_tbl <- training(splits)
test_tbl  <- testing(splits)


## cross-validation folds
set.seed(1234)
cv_train <- vfold_cv(train_tbl, v = 5, repeats = 1)
cv_train



##########################################################################
###                        The recipe for data preparation             ###
### not all steps (in the following recipe) are really necessary 
### in this case, but in many other situations they are really useful
the_recipe <- recipe(duration ~ ., data = train_tbl) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>% # dummification of the predictors
    step_impute_knn(all_predictors(), neighbors = 3) %>%   # ... when having missing values
    step_zv(all_predictors()) # this removes predictors with zero variance



#########################################################################
###                           Model Specification

## Random Forest models
rf_spec <- rand_forest(
     mtry = tune(),    # first hyper-parameter to be tuned
     trees = 500,
     min_n = tune()     # second hyper-parameter to be tuned
          ) %>%
     set_engine("ranger") %>%
     set_mode("regression")

rf_spec


### XGBoost models
xgb_spec <- boost_tree(
    trees = 700, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                         ## step size
    ) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")

xgb_spec


#########################################################################
###                           Assemble the workflows

wf_rf <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(the_recipe)

wf_xgb <- workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(the_recipe)



#########################################################################
###                      Grids for hyper-parameter tuning

set.seed(1234)
rf_grid <- dials::grid_random(
    finalize(mtry(), train_tbl %>% select (-duration)),
    min_n(),  
    size = 50)  # the number should be larger, but it would take longer
rf_grid


set.seed(1234)
xgb_grid <- dials::grid_random(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_tbl %>% select (-duration)),
    learn_rate(),
    size = 150   # the number should be larger, but it would take longer
)
xgb_grid



#########################################################################
###   Fit the models for all k-fold folders and hyper-parameters grid
doParallel::registerDoParallel()


set.seed(1234)
rf_resamples <- wf_rf %>% 
    tune_grid(
      resamples = cv_train,
      grid = rf_grid
              )
rf_resamples


set.seed(1234)
xgb_resamples <- wf_xgb %>% 
    tune_grid(
      resamples = cv_train,
      grid = xgb_grid
    )
xgb_resamples

# In case of errors, one can extract additional information with ...
#temp <- xgb_resamples$.notes[[1]][1]
#temp$.notes[1]



#########################################################################
### Explore the results and choose the best hyper-parameter combination


# performance metrics (mean) across folds for each grid line
temp <- rf_resamples %>% collect_metrics()
autoplot(rf_resamples)

rf_resamples %>% 
    collect_metrics() %>%
    filter(`.metric` == 'rsq') %>%
    summarise(avg_rsq = mean(mean))


xgb_resamples %>% collect_metrics()
autoplot(xgb_resamples)

xgb_resamples %>% 
    collect_metrics() %>%
    filter(`.metric` == 'rsq') %>%
    summarise(avg_rsq = mean(mean))




# choose the best hyper-parameter combination
best_rf <- rf_resamples %>% select_best("rmse")
best_rf

best_xgb <- xgb_resamples %>% select_best("rmse")
best_xgb



#########################################################################
###        Finalize the workflows with the best performing parameters

final_wf_rf <- wf_rf %>% 
     finalize_workflow(best_rf)

final_wf_xgb <- wf_xgb %>% 
    finalize_workflow(best_xgb)


## fit the final models on the entire train data set

set.seed(1234)
final_rf_train <- final_wf_rf %>%
    fit(data = train_tbl) 
final_rf_train

set.seed(1234)
final_xgb_train <- final_wf_xgb %>%
    fit(data = train_tbl) 
final_xgb_train



#########################################################################
###     The moment of truth: model performance on the test data set

### Function last_fit() fits the finalized workflow one last time 
### to the training data and evaluates one last time on the testing data.

set.seed(1234)
test__rf <- final_wf_rf %>% last_fit(splits) 
test__rf %>% collect_metrics() 

set.seed(1234)
test__xgb <- final_wf_xgb %>% last_fit(splits) 
test__xgb %>% collect_metrics() 



#########################################################################
###                        Variable importance
#########################################################################
library(vip)

set.seed(1234)
rf_imp_spec <- rf_spec %>%
    finalize_model(best_rf) %>%
    set_engine("ranger", importance = "permutation")

workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(rf_imp_spec) %>%
    fit(train_tbl) %>%
    extract_fit_parsnip() %>%
    vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))



set.seed(1234)
xgb_imp_spec <- xgb_spec %>%
    finalize_model(best_xgb) %>%
    set_engine("xgboost", importance = "permutation")

workflow() %>%
    add_recipe(the_recipe) %>%
    add_model(xgb_imp_spec) %>%
    fit(train_tbl) %>%
    extract_fit_parsnip() %>%
    vip(aesthetics = list(alpha = 0.8, fill = "red"))



