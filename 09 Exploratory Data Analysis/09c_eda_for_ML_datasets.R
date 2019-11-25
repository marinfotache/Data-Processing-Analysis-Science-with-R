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
## last update: 21.11.2019

options(scipen = 999)
library(tidyverse) 
library(corrr)
library(corrgram)
library(readxl)
library(DataExplorer)
library(inspectdf)


############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


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
    guides(fill=FALSE) +
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
     guides(fill=FALSE) +
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("") + ylab("frequency") 


# separate density plot for each numeric value; free scale
num_variables %>%
ggplot(., aes(x = value, fill = variable, alpha = 0.5)) +
     geom_density() +
     facet_wrap(~ variable, scale = "free") +
     guides(fill=FALSE) +
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("") + ylab("frequency") 


# boxplot (free scale)
num_variables %>%
ggplot(., aes(y = value)) +
     geom_boxplot() +
     facet_wrap(~ variable, scales = 'free') +
     guides(fill=FALSE) +
     xlab("") + ylab("value") +
     theme(axis.text.x = element_blank()) 


#################################################################
##    Display correlation  among all numeric variables         ##
## with package `corrr` (part of tidymodels)                   ##

# display correlations
temp <- states %>%
     select_if(is.numeric) %>%
     corrr::correlate()  # Create correlation data frame 
View(temp)


# ... a better look...
temp <- states %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     corrr::rearrange() %>%  # rearrange by correlations
     corrr::shave() # Shave off the upper triangle for a clean result
View(temp)
     

# display even better... 
fashion(temp)

# the correlation plot
states %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     corrr::rplot()


# another series of correlation plot

corrplot::corrplot(cor(states %>% dplyr::select (-State), 
             method = "spearman"), method = "number", type = "upper")

corrgram::corrgram(states %>% dplyr::select (-State) %>% select_if(is.numeric),
     lower.panel=panel.conf, upper.panel=panel.pts,
     diag.panel=panel.density)


# the network plot
states %>%
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

DataExplorer::create_report(states, config = config)


# scatter plots of the outcome vs. all other valiables
states %>%
     dplyr::select (-State) %>%
     pivot_longer(-Murder, names_to = 'Predictor', values_to = "Value") %>%
ggplot(., aes(x = Value, y = Murder)) +
     facet_wrap(~ Predictor, scale = "free_x") +
     geom_point() +
     geom_smooth(col = "darkgreen") +
     geom_smooth(method = "lm", col = "red") +
     theme_bw() +
     theme(strip.text.x = element_text(size = 12)) +
     xlab("")  

 
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
heart_init <- read_csv('Heart.csv')
glimpse(heart_init)

# prepare the data set, by removing the first column 
# and recoding the factors (also convertinf the character
# variables into factor)
heart <- heart_init %>%
     select (-X1) %>%
     mutate(
          Sex = recode (Sex, `0` = "Female", `1` = "Male"),
          Fbs = recode (Fbs, `0` = "No", `1` = "Yes"),
          RestECG = factor (RestECG, levels = c(0, 1, 2)),
          ExAng = recode (ExAng, `0` = "No", `1` = "Yes"),
          Slope = factor (Slope, levels = c(1, 2, 3))
          ) %>%
     mutate_if(is.character, as.factor)

glimpse(heart_init)
glimpse(heart)
rm(heart_init)

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
    guides(fill=FALSE) +
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
     guides(fill=FALSE) +
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
     guides(fill=FALSE) 


# boxplot (free scale)
num_variables %>%
ggplot(., aes(y = value)) +
     geom_boxplot() +
     facet_wrap(~ variable, scales = 'free') +
     guides(fill=FALSE) +
     xlab("") + ylab("value") +
     theme(axis.text.x = element_blank()) 


#################################################################
##    Display correlation  among all numeric variables         ##
## with package `corrr` (part of tidymodels)                   ##

# display correlations
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
     

# display even better... 
fashion(temp)

# the correlation plot
heart %>%
     select_if(is.numeric) %>%
     corrr::correlate() %>%
     corrr::rplot()


# another series of correlation plot

corrplot::corrplot(cor(heart %>% select_if (is.numeric) %>% na.omit(), 
             method = "spearman"), method = "number", type = "upper")

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
     pivot_longer(c(-id,-AHD), names_to = "variable", values_to = "value")  %>%
     group_by(variable, AHD) %>%
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
     facet_wrap(~ AHD, labeller = "label_both") +
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
     pivot_longer(c(-id,-AHD), names_to = "variable", values_to = "value" ) %>%
     mutate (value = coalesce(value, 'N/A')) %>%
     group_by(variable, AHD, value) %>%
     summarise (n_value = n()) %>%
     ungroup() %>%
     mutate (percent = round(n_value * 100 / nrow(heart),2)) %>%
     arrange(variable, AHD, value)
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
    facet_wrap(~ variable + AHD, scale = "free", labeller = 'label_both') +
    guides(fill=FALSE) +
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
    facet_grid(AHD ~ variable, scale = "free", labeller = 'label_both') +
    guides(fill=FALSE) +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(strip.text = element_text(size = 12)) +
    xlab("") + ylab("frequency") 


#################################################################
##    Display the distribution (as histograms,                 ##
##   density plots and boxplots) of each numeric variable      ##
num_variables <-      bind_cols(
     heart %>% select (AHD), 
     heart %>% select_if(., is.numeric)) %>%
     mutate (id = row_number()) %>%
     pivot_longer(c(-id,-AHD), names_to = "variable", values_to = "value" ) 
View(num_variables)


# separate histogram for each numeric value; free scale
num_variables %>%
ggplot(., aes(x = value, fill = variable)) +
     geom_histogram() +
    facet_grid(AHD ~ variable, scale = "free", labeller = 'label_both') +
     guides(fill=FALSE) +
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text = element_text(size = 12)) +
     xlab("") + ylab("frequency") 

glimpse(heart)


# separate density plot for each numeric value with more 
# than 30 values ; free scale
# 
num_variables %>%
     group_by(variable, AHD) %>%
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
     facet_grid(AHD ~ variable, scale = "free", labeller = 'label_both') +
     theme(axis.text.x = element_text(size = 9)) +
     theme(strip.text = element_text(size = 12)) +
     xlab("") + ylab("frequency") +
     guides(fill=FALSE) 


# boxplot (free scale)
num_variables %>%
ggplot(., aes(y = value)) +
     geom_boxplot() +
     facet_grid(variable ~ AHD, scale = "free") +
     guides(fill=FALSE) +
     xlab("") + ylab("value") +
     theme(axis.text.x = element_blank()) 



#################################################################
## mosaic plots of the outcome vs. all other nominal variables
library(ggmosaic)
names(heart)


vars <- setdiff(names(heart %>% select_if (is.factor)), 'AHD')

plots <- list()

for (var in vars) {
     temp <- heart %>%
          transmute (AHD, var1 = heart[[var]]) 
     
     plot <- ggplot(data = temp) + 
     geom_mosaic (aes(weight = 1, x = product(AHD, var1), 
                   fill=factor(AHD))) +
     guides(fill=FALSE) +
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
##        romove those observations; we'll keep both versions
##        of the data set
#################################################################
getwd()
heart__na_omit <- heart %>%
     na.omit()

