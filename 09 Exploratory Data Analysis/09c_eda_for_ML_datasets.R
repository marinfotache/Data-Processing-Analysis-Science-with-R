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
###            09c Exploratory Data Analysis for Datasets to be          ###
###   further used in Inferential Statistics and Machine Learning        ###
###   (see next chapters/sections)                                       ###
############################################################################
## last update: 16-12-2024

options(scipen = 999)
library(tidyverse) 
library(corrr)
library(corrgram)
library(readxl)
library(DataExplorer)
library(inspectdf)


############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

############################################################################


#####################################################################
###	                              Agenda                         ###	
#####################################################################
###	I. EDA for datasets to be used for scoring (regression)      ###	
###	     I.1 States(USA)                                         ###	
### 	     I.2 Insurance                                           ###
###       I.3 House Prices (Ames, Iowa)                           ###
### 	     I.4 Hrubaru2015                                         ###
###	II. EDA for datasets to be used for classification           ###	
###       II.1 Heart disease                                      ###
###       II.2 Credit scoring (G.Sanchez version)                 ###
#####################################################################



#####################################################################
###	I. EDA for datasets to be used for scoring (regression)      ###	
#####################################################################

#####################################################################
###	                      I.1 States(USA)                        ###	
#####################################################################
### inspired by R. Kabacoff - R in Action (Manning), 2013, 2015 
#
# state.x77 dataset in the base package 
# Explore the relationship between a state’s murder rate and 
#    other characteristics of the state, including population, 
#    illiteracy rate, average income, and frost levels (mean number 
#    of days below freezing).
#####################################################################

states <- as_tibble(state.x77, rownames = 'State')
names(states) <- str_replace_all(names(states), ' |\\.', '_')
head(states)
class(states)
glimpse(states)

# descriptive statistics
states %>%
     skimr::skim()


#################################################################
##     Display the number missing values for each variable     ## 

missing_vals <- states %>%
     map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
     tibble(variable = names(.), n_missing = .) %>%
     mutate (percent_missing = round(n_missing * 100 / 
               nrow(states), 2))

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
##     Display the frequency (and the percent) of the  ##
##           values for each character/factor variable         ##

# first, compute the frequencies for each categorical variables and values
eda_factors <- states %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(states),2)) %>%
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
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(strip.text.x = element_text(size = 14)) +
    xlab("") + ylab("frequency") 



#################################################################
##    Display the distribution (as histograms,                 ##
##   density plots and boxplots) of each numeric variable      ##

num_variables <- states %>%
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

# correlations
temp <- states %>%
     select_if(is.numeric) %>%
     corrr::correlate()  # Create correlation data frame 
View(temp)

# the correlation plot
corrplot::corrplot(cor(states %>% dplyr::select (-State) %>% select_if(is.numeric), 
             method = "spearman"), method = "number", type = "upper",
             tl.cex = 0.75, number.cex = .75)


# Get a comprehensive report about variables distribution and correlation
config <- configure_report(
     add_introduce = TRUE, 
     add_plot_intro = TRUE,     
     add_plot_missing = TRUE,     
     add_plot_str = TRUE,
     add_plot_histogram = TRUE, 
     add_plot_density = TRUE,
     add_plot_qq = TRUE,
     add_plot_prcomp = FALSE,
     add_plot_boxplot = TRUE,
     add_plot_scatterplot = TRUE
) 

DataExplorer::create_report(states, config = config)


# scatter plots of the outcome vs. all other variables
states %>%
     dplyr::select (-State) %>%
     pivot_longer(-Murder, names_to = 'Predictor', values_to = "Value") %>%
ggplot(., aes(x = Value, y = Murder)) +
     facet_wrap(~ Predictor, scale = "free_x") +
     geom_point() +
     geom_smooth(col = "darkgreen") +
     geom_smooth(method = "lm", col = "red") +
     theme_bw() +
     theme(strip.text.x = element_text(size = 11)) +
     xlab("")  

# save all the data sets in this script into single file for
# further use     
save(states, file = 'ml_datasets.RData')



#####################################################################
### 	                      I.2 Insurance                       ###
#####################################################################
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

# descriptive statistics
insurance %>%
     skimr::skim()



#################################################################
##     Display the number missing values for each variable     ## 

missing_vals <- insurance %>%
     map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
     tibble(variable = names(.), n_missing = .) %>%
     mutate (percent_missing = round(n_missing * 100 / 
               nrow(insurance), 2))


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
##     Display the frequency (and the percent) of the  ##
##           values for each character/factor variable         ##

# first, compute the frequencies for each categorical variables and values
eda_factors <- insurance %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(insurance),2)) %>%
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
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(strip.text.x = element_text(size = 14)) +
    xlab("") + ylab("frequency") 


#################################################################
##    Display the distribution (as histograms,                 ##
##   density plots and boxplots) of each numeric variable      ##

num_variables <- insurance %>%
     select_if(., is.numeric ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) 
View(num_variables)

table(insurance$children)

insurance %>%
     group_by(children) %>%
     tally()


# separate histogram for each numeric value; free scale
num_variables %>%
     filter (variable == 'children') %>%
ggplot(., aes(x = value, fill = variable)) +
     geom_histogram(bins = 5, col = 'black') +
     facet_wrap(~ variable, scale = "free") +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("") + ylab("frequency") 


# separate density plot for each numeric value; free scale
num_variables %>%
     filter (variable != 'children') %>%
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



# examine bivariate relationships
insurance %>%
     select_if(is.numeric) %>%
     cor()


corrplot::corrplot(cor(insurance %>%
     select_if(is.numeric) , 
          method = "spearman"), method = "number", type = "upper",
          tl.cex = 0.75, number.cex = .75)



# scatter plots of the outcome vs. all other valiables
insurance %>%
     select_if(is.numeric) %>%
     pivot_longer(-charges, names_to = 'Predictor', 
                  values_to = "Value") %>%
ggplot(., aes(x = Value, y = charges)) +
     facet_wrap(~ Predictor, scale = "free_x") +
     geom_point() +
     geom_smooth(col = "darkgreen") +
     geom_smooth(method = "lm", col = "red") +
     theme_bw() +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("")  


insurance_lm1 <- lm(charges ~ ., data = insurance)
summary(insurance_lm1)



#####################################################################
###                I.3 House Prices (Ames, Iowa)                  ###
#####################################################################
### https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data          
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



#####################################################################
### 	                    I.4 Hrubaru 2016                      ###
#####################################################################
### the data set prepared by Hrubaru & Fotache (see.
### ....)


load('Hrubaru_2016-02.RData')
rm(results_details.ok_rand)
glimpse(results_ih_2016)


### correlation plot (nonparametric)
#citation('corrplot')
library(corrplot)
corrplot.mixed(corr=cor(results_ih_2016 %>% select_if(., is.numeric), 
                        method = "spearman"), 
     upper = 'ellipse', tl.pos='lt')


##
## to be continued during lectures with the removal of highly correlated predictors.
##



#####################################################################
###	     II. EDA for datasets to be used for classification      ###	
#####################################################################


#####################################################################
###                      II.1 Heart disease                       ###
#####################################################################
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

# EDA for this data set is also available at:
# https://rileyking.netlify.com/post/heart-disease-prediction-from-patient-data-in-r/

heart <- read_csv('Heart.csv')  |>
     janitor::clean_names() |>
     select(-x1) |>
     mutate(
          sex = case_when(
               sex == 0 ~ "Female",
               sex == 1 ~ "Male",
               is.na(sex) ~ 'unknown',
               .default = 'other'),
          fbs = case_when(
               fbs == 0 ~ "No",
               fbs == 1 ~ "Yes",
               .default = 'ERROR!'),
          ex_ang = case_when(
               ex_ang == 0 ~ "No",
               ex_ang == 1 ~ "Yes",
               .default = 'ERROR!'),
          slope = factor (slope, levels = c(1, 2, 3))
          ) |>
     mutate_if(is.character, as.factor)

glimpse(heart)

# Descriptive statistics
heart %>%
     skimr::skim()


#################################################################
##     Display the number missing values for each variable     ## 
missing_vals <- heart %>%
     map_int(., ~ sum(is.na(.) | . == 'N/A')) %>%
     tibble(variable = names(.), n_missing = .) %>%
     mutate (percent_missing = round(n_missing * 100 / 
               nrow(heart), 2))

# now, the plot
ggplot(missing_vals, 
     aes (x = variable, y = n_missing, fill = variable)) +
     geom_col() +
     coord_flip() +
     geom_text(aes(label = paste0(percent_missing, '%'), size = 3.5, 
               hjust = if_else(percent_missing > 5, 1.02, -0.03), 
               vjust = 0.5))  +
     theme(legend.position="none") + # this will remove the legend
     scale_y_continuous(limits = c(0,7), breaks = seq(0, 7, 1)) 


#################################################################
##         Display the frequency (and the percent) of the      ##
##           values for each character/factor variable         ##

# first, compute the frequencies for each categorical variables and values
eda_factors <- heart %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(heart),2)) %>%
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
                  vjust = if_else(n_value > 100, 1.5, -0.5))) +
    facet_wrap(~ variable, scale = "free") +
     theme(legend.position="none") + # this will remove the legend
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(strip.text.x = element_text(size = 12)) +
    xlab("") + ylab("frequency") 



#################################################################
##    Display the distribution (as histograms,                 ##
##   density plots and boxplots) of each numeric variable      ##
num_variables <- heart %>%
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

glimpse(heart)
# separate density plot for each numeric value with more than 30 values ; 
# free scale
# 
num_variables %>%
     group_by(variable) %>%
     summarise(n_distinct = n_distinct(value)) %>%
     ungroup()

num_variables %>%
     filter (variable %in% (
          num_variables %>%
               group_by(variable) %>%
               summarise(n_distinct = n_distinct(value)) %>%
               ungroup() %>%
               filter (n_distinct > 30) %>%
               pull(variable))
             ) %>%  
ggplot(., aes(x = value, fill = variable )) +
     geom_density(alpha = 0.5) +
     facet_wrap(~ variable, scale = "free") +
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("") + ylab("frequency") +
     theme(legend.position="none")  # this will remove the legend


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

#  correlations
temp <- heart %>%
     select_if(is.numeric) %>%
     corrr::correlate()  # Create correlation data frame 
View(temp)


# ... a better look...
temp <- heart %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     corrr::rearrange() %>%  # rearrange by correlations
     corrr::shave() # Shave off the upper triangle for a clean result
View(temp)
     


# the correlation plot
corrplot::corrplot(cor(heart %>% select_if (is.numeric) %>% na.omit(), 
             method = "spearman"), method = "number", type = "upper",
             tl.cex = 0.95, number.cex = .85)

# the network plot
heart %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     network_plot(min_cor = .2)

# Get a comprehensive report about variables distribution and correlation
config <- configure_report(
     add_introduce = TRUE, 
     add_plot_intro = TRUE,     
     add_plot_missing = TRUE,     
     add_plot_str = TRUE,
     add_plot_histogram = TRUE, 
     add_plot_density = TRUE,
     add_plot_qq = TRUE,
     add_plot_prcomp = FALSE,
     add_plot_boxplot = TRUE,
     add_plot_scatterplot = TRUE
) 

DataExplorer::create_report(heart, config = config)


#################################################################
###         Display the distribution of data, grouped by      ###
###                      the outcome levels                   ###
#################################################################


#################################################################
##     Display the number missing values for each variable     ## 
## grouped by the outcome levels       
missing_vals <- heart %>%
     mutate (id = row_number()) %>%
     mutate_all(as.factor) %>%
     pivot_longer(c(-id,-ahd), names_to = "variable", values_to = "value")  %>%
     group_by(variable, ahd) %>%
     summarise(n_missing = sum(is.na(value) | value == 'N/A'))  %>%
     mutate (percent_missing = round(n_missing * 100 / 
               nrow(heart), 2))

# now, the plot
ggplot(missing_vals, 
     aes (x = variable, y = n_missing, fill = variable)) +
     geom_col() +
     coord_flip() +
     geom_text(aes(label = paste0(percent_missing, '%'), size = 3.5, 
               hjust = if_else(percent_missing > 5, 1.02, -0.03), 
               vjust = 0.5))  +
     facet_wrap(~ ahd, labeller = "label_both") +
     theme(strip.text.x = element_text(size = 12)) +
     theme(legend.position="none") + # this will remove the legend
     scale_y_continuous(limits = c(0,7), breaks = seq(0, 7, 1)) 


#################################################################
##         Display the frequency (and the percent) of the      ##
##           values for each character/factor variable         ##
##                  grouped by the outcome levels       

# first, compute the frequencies for each categorical variables and values
eda_factors <- heart %>%
     mutate_if(is.factor, as.character) %>%
     select_if(., is.character ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(c(-id,-ahd), names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, ahd, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(heart),2)) %>%
     arrange(variable, ahd, value)
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
                  vjust = if_else(n_value > 100, 1.5, -0.5))) +
    facet_wrap(~ variable + ahd, scale = "free", labeller = 'label_both') +
     theme(legend.position="none") + # this will remove the legend
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(strip.text.x = element_text(size = 12)) +
    xlab("") + ylab("frequency") 


# plot the factors values as a grid plot (better visualisation)
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
                  vjust = if_else(n_value > 100, 1.5, -0.5))) +
     facet_grid(ahd ~ variable, scale = "free", labeller = 'label_both') +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
     theme(strip.text = element_text(size = 12)) +
     xlab("") + ylab("frequency") 


#################################################################
##    Display the distribution (as histograms,                 ##
##   density plots and boxplots) of each numeric variable      ##
num_variables <-      bind_cols(
     heart %>% select (ahd), 
     heart %>% select_if(., is.numeric)) %>%
     mutate (id = row_number()) %>%
     pivot_longer(c(-id,-ahd), names_to = "variable", values_to = "value" ) 
View(num_variables)


# separate histogram for each numeric value; free scale
num_variables %>%
ggplot(., aes(x = value, fill = variable)) +
     geom_histogram() +
     facet_grid(ahd ~ variable, scale = "free", labeller = 'label_both') +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text = element_text(size = 12)) +
     xlab("") + ylab("frequency") 

glimpse(heart)


# separate density plot for each numeric value with more 
# than 30 values ; free scale
# 
num_variables %>%
     group_by(variable, ahd) %>%
     summarise(n_distinct = n_distinct(value)) %>%
     ungroup()

num_variables %>%
     filter (variable %in% (
          num_variables %>%
               group_by(variable) %>%
               summarise(n_distinct = n_distinct(value)) %>%
               ungroup() %>%
               filter (n_distinct > 30) %>%
               pull(variable))
             ) %>%  
ggplot(., aes(x = value, fill = variable )) +
     geom_density(alpha = 0.5) +
     facet_grid(ahd ~ variable, scale = "free", labeller = 'label_both') +
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text = element_text(size = 12)) +
     xlab("") + ylab("frequency") +
     theme(legend.position="none")  # this will remove the legend


# boxplot (free scale)
num_variables %>%
ggplot(., aes(y = value)) +
     geom_boxplot() +
     facet_grid(variable ~ ahd, scale = "free") +
     theme(legend.position="none") + # this will remove the legend
     xlab("") + ylab("value") +
     theme(axis.text.x = element_blank()) 



#################################################################
## mosaic plots of the outcome vs. all other nominal variables
library(ggmosaic)
names(heart)


vars <- setdiff(names(heart %>% select_if (is.factor)), 'ahd')

plots <- list()

var <- vars[1]
for (var in vars) {
     temp <- heart %>%
          transmute (ahd, var1 = heart[[var]]) 
     glimpse(temp)
     
     plot <- ggplot(data = temp) + 
     geom_mosaic (aes(weight = 1, x = product(ahd, var1), 
                   fill=ahd)) +
     theme(legend.position="none") + # this will remove the legend
     xlab(var) + ylab("AHD") +
     theme(axis.text.x=element_text(angle=45, hjust= 1, size = 10)) 
          
     plots <- c(plots, list(plot))
     #print(plot)
}

#install.packages('Rmisc')
library(Rmisc)
multiplot(plotlist = plots, cols = 3)



#################################################################
##        Since there are a few missing values, we can
##        remove these observations; we'll keep both versions
##        of the data set
#################################################################
getwd()
heart__na_omit <- heart %>%
     na.omit()

# add the current datasets to the file
save(states, 
     heart, heart__na_omit,
     file = 'ml_datasets.RData')




#####################################################################
###       II.2 Credit scoring (G.Sanchez version)                 ###
#####################################################################
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





