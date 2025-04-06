#######################################################################
###                    Al.I. Cuza University of Iași                ###
###       Faculty of Economics and Business Administration          ###
###   Department of Accounting, Information Systems and Statistics  ###
#######################################################################
###
#######################################################################
###        Data Processing/Analysis/Science with R                  ###
#######################################################################
###
#######################################################################
###                 Case study 1: Insurance data set                ###
#######################################################################
### last update: 2024-03-30

options(scipen = 999)
library(tidyverse) 
library(corrr)
library(corrgram)
library(readxl)
library(ggstatsplot)

# install.packages("lindia")
library(lindia)

#######################################################################
###          Download the necessary data sets for this script
#######################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


#######################################################################
###                      A. Data set description
#######################################################################

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
glimpse(insurance)



#######################################################################
###                    B.  Exploratory Data Analysis
#######################################################################

# descriptive statistics
insurance %>%
     skimr::skim()


#######################################################################
##         Display the number missing values for each variable       ## 

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


#######################################################################
##            Display the frequency (and the percent) of the         ##
##                values for each character/factor variable         ##

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


table(insurance$children)

insurance %>%
     group_by(children) %>%
     tally()

# include children in variabiles of type string/character
glimpse(insurance)
eda_factors <- insurance |>
     select(sex, children:region) |>
     mutate(children = as.character(children)) |>
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(insurance),2)) %>%
     arrange(variable, value)

ggplot(eda_factors, aes(x = value, y = n_value, fill = value)) +
     geom_col() +
     geom_text (aes(label = paste0(round(percent,0), '%'), 
                  vjust = if_else(n_value > 300, 1.5, -0.5))) +
    facet_wrap(~ variable, scale = "free") +
    theme(legend.position="none") + # this will remove the legend
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(strip.text.x = element_text(size = 14)) +
    xlab("") + ylab("frequency") 



#######################################################################
##            Display the distribution (as histograms,               ##
##         density plots and boxplots) of each numeric variable      ##

num_variables <- insurance %>%
     select_if(., is.numeric ) %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id, names_to = "variable", values_to = "value" ) 

View(num_variables)

table(insurance$children)

insurance %>%
     group_by(children) %>%
     tally()


# # separate histogram for children
# num_variables %>%
#      filter (variable == 'children') %>%
# ggplot(., aes(x = value, fill = variable)) +
#      geom_histogram(bins = 5, col = 'black') +
#      facet_wrap(~ variable, scale = "free") +
#      theme(legend.position="none") + # this will remove the legend
#      theme(axis.text.x = element_text(size = 9)) +
#      theme(strip.text.x = element_text(size = 12)) +
#      xlab("") + ylab("frequency") 


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


shapiro.test(insurance$charges)

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




#######################################################################
###                     Linear Regression Model
#######################################################################
table(insurance$region)

insurance_lm1 <- lm(charges ~ ., data = insurance)
summary(insurance_lm1)

# # install.packages('parameters')
library(parameters)
# install.packages('see')
library(see)
plot(parameters(insurance_lm1))
plot(insurance_lm1, show_labels = TRUE, size_text = 4)
#plot(model_parameters(insurance_lm1, effects = "fixed"))


# visualize diagnostic plots with package `lindia`
lindia::gg_diagnose(insurance_lm1)

# Check for Multicollinearity - Variance Inflation Factor
#install.packages('performance')
library(performance)
performance::check_collinearity(insurance_lm1)

# Check for Normal Distributed Residuals
check_normality(insurance_lm1)
plot(check_normality(insurance_lm1), type = "density")
plot(check_normality(insurance_lm1), type = "qq")

# Check for Heteroscedasticity
check_heteroscedasticity(insurance_lm1)
plot(check_heteroscedasticity(insurance_lm1))

# Check for Outliers
performance::check_outliers(insurance_lm1)

#!!!!!!!!!!!!!!!!!!!!!!!
# Overall Model Check
check_model(insurance_lm1)

# install.packages('gglm')
library(gglm) 
gglm(insurance_lm1) # Plot the four main diagnostic plots



# prediction
df <- data.frame(age = 45, sex = 'male', bmi = 35, 
          children = 2, smoker = 'yes', region = 'northeast')

predict(insurance_lm1, newdata = df)

hist(insurance$charges)

