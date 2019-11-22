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

     
