################################################################################
#########                Regression Models Selection                ############
################################################################################
####  last update: 14.11.2017

library(tidyverse)
options(scipen=10)
library (stringr)

## install.packages('devtools')
#library(devtools)
#devtools::install_github("dgrtwo/broom")
library (broom)

# install.packages('devtools')
#library(devtools)
# remove.packages('ggrepel')
#devtools::install_github("slowkow/ggrepel")
#install.packages("ggrepel")
library(ggrepel)
library(corrplot)
library(car)


###################################################################################
###                      Case study: `States` (USA)
#  Example taken from Kabacoff's R in Action (Manning), 2013, 2015 
# state.x77 dataset in the base package 
# Explore the relationship between a state’s murder rate and other characteristics of
#   the state, including population, illiteracy rate, average income, 
#   and frost levels (mean number of days below freezing).
###################################################################################

# lm() function requires a data frame;state.x77 dataset is contained in a matrix, 
#    so one must convert it:
states <- as.data.frame(state.x77)
states$State <- row.names(states)
names(states) <- str_replace_all(names(states), ' |\\.', '_')
head(states)


# examine bivariate relationships
cor(states %>% select (-State))

# install.packages('corrplot')
corrplot::corrplot(cor(states %>% select (-State), 
             method = "spearman"), method = "number", type = "upper")

car::scatterplotMatrix(states %>% select (-State), spread=FALSE, 
                  lty.smooth=2, main="Scatter Plot Matrix")
warnings()



################################################################
##     `All-in Model` - the model containing all variables    ##
##   (except `State`)                                         ## 
################################################################

states_lm1 <- lm(Murder ~ ., data=states %>% select (-State))
summary(states_lm1)

####################################################
## with the `broom` package, we can collect simply
##    and rigurously the main model parameters

model_overall <- glance(states_lm1)
model_overall
model_predictors <- tidy(states_lm1)
model_predictors
model_details <- augment(states_lm1)
model_details

# Residuals Sum of Squares
rss <- sum((states$Murder - predict(states_lm1))^2)
rss

states_lm1$residuals
sum(states_lm1$residuals^2)


# Mean Squared Error
mse <- mean((states$Murder - predict(states_lm1))^2)
mse

# Density plot of the residuals
title_ <- paste0('Residuals Distribution for ALL-IN Model')
ggplot(data = model_details, aes(x = .resid)) + 
	geom_density(colour="black") + 
     xlab("Residuals") + ylab("Density") +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey() +
     theme(strip.text.x = element_text(size = 9)) +
     scale_y_continuous() +
     scale_x_continuous(breaks=seq(-4, 4, .5)) +
     theme(axis.text.x  = element_text(angle=45, vjust=.5, hjust=.5)) 

# Box plot of the residuals
ggplot(data = model_details, aes(x = 1, y = .resid)) + 
	geom_boxplot() + 
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey() + 
     theme(axis.text.x=element_blank()) +
     scale_y_continuous(breaks = seq(-3, 3, .5)) +
     xlab("Residuals") 


# for getting the model formula, instead of ...
states_lm1$call
# ... we'll gather information from...
states_lm1$terms[[1]]
states_lm1$terms[[2]]
states_lm1$terms[[3]]


# this is the vector with all possible (simple) predictors
predictors <- setdiff(names(states), c('State', 'Murder'))



####################################################################
####      Task:                                                 ####
####      Find the best model (what `best` is ?) that explains  ####
####      and/or predict the outcomene (Murder rate)            ####
####################################################################


####################################################################
###            Model selection                                   ###
####################################################################
###         a. Backward elimination                              ### 
###         b. Forward selection                                 ### 
###         c. Bidirectional elimination                         ### 
###         d. Score comparison (for all possible models)        ### 
####################################################################



####################################################################
###         a. Backward elimination                              ### 
####################################################################
##   a.1. Choose a significance level (usually 0.05 or 0.10)
##   a.2. Start with `all-in` model
##   a.3. Remove least significant predictor (the predictor
##        with the highest p-value)
##   a.4. Build the model (without the predictor found in a.3.)
##   a.5. Check if there is at least one predictor with 
##      the p-value above the significance level. 
##      If so, go to a.3
##      In not, STOP.        

current_predictors <- predictors
the_backward_models <- tibble()
removed_predictors <- tibble()
models_residuals <- tibble()
current_step <- 0


## a.1. Choose a significance level (usually 0.05 or 0.10)
significance_level <- 0.05

# loop variable
stop <- FALSE

while (!stop)
{
     current_step <- current_step + 1
     
     # fit the model
     formula_ <- paste('Murder ~', paste(current_predictors, collapse = ' + '))
     current_model <- lm(formula = formula_, data = states %>% select (-State))    
     # summary(current_model)
     
     # add the overall information about the data into the dataframe
     #  `the_backward_models`
     the_backward_models <- bind_rows(the_backward_models,
          glance(current_model) %>%
               mutate (predictors = paste(current_predictors, collapse = ' + '),
                       step = current_step))
     
     # find the least significant predictor
     predictor_details <- tidy(current_model)

     the_least_significant_predictor <- predictor_details %>%
          filter (term != '(Intercept)') %>%
          mutate (max_p_value = max(p.value), step = current_step) %>%
          filter (p.value == max_p_value) %>%
          head(1)
     

     # the the p-value of the least significant predictor is smaller
     # than the significance level, then STOP
     if (the_least_significant_predictor[1,]$p.value < significance_level )
          break
     
     # here, we still have predictors with p-value above 
     #   the significance level

     # we'll remove it from the model ...
     current_predictors <- setdiff(current_predictors, 
                              the_least_significant_predictor[1,]$term )
     
     # ... and store it in the `removed_predictors` data frame
     removed_predictors <- bind_rows(removed_predictors, 
               the_least_significant_predictor         )
     
     # store the model residuals
     models_residuals <- bind_rows(models_residuals, 
          augment(states_lm1) %>%
          mutate(step = current_step, n_of_predictors = length(current_predictors)) )     

}     


# The resulted model is:
final_model_backard <- current_model 
summary(final_model_backard)


# Compute the residuals sum of squares (RSS) 
# for each step of the backward elimination
models_residuals <- models_residuals %>%
     mutate (rss = .resid ^ 2)


# Density plot of the Residuals (for checking the normality)
title_ <- paste0('Residuals - Backward Elimination')
ggplot(data = models_residuals, 
     aes(x = .resid, color = factor(n_of_predictors) )) + 
	geom_density(alpha = 0.2) + 
     xlab("Residuals") + ylab("Density") +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey() +
     scale_y_continuous() +
     scale_x_continuous(breaks=seq(-4, 4, .5)) +
     theme(axis.text.x  = element_text(angle=45, vjust=.5, hjust=.5))  + 
	theme(legend.position = "bottom", legend.direction = "horizontal") 


# Box plot of the residuals sum of squares for models on each step of 
# the backaward elimination
title_ <- paste0('Residuals Sum of Squares - Backward Elimination')
ggplot(data = models_residuals, 
          aes(x = factor(n_of_predictors), y = rss)) + 
	geom_boxplot() +
     ggtitle(title_) +
     xlab("Number of predictors in the model") +
     theme_bw() +
     scale_fill_grey() + 
     scale_y_continuous() 


# plot the BIC for all the models
title_ <- 'BIC (Bayesian Information Criterion) Evolution for Each Step\n of Backward Selection'    
ggplot(the_backward_models, aes(x = step, y = BIC)) +
     geom_point(col = 'red') +
     geom_text_repel(aes(label = predictors), size = 3.5) +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(breaks=seq(180, 280, 1)) 


title_ <- 'BIC, AIC and Adjusted R^2 for Each Step\n of Backward Selection'    
ggplot(the_backward_models, aes(x = step, y = BIC)) +
     geom_point(col = 'red', size = 2) +
     geom_point(aes(x = step, y = AIC), col = 'darkgreen', size = 2) +
     geom_point(aes(x = step, y = adj.r.squared), col = 'magenta', size = 2) +
     geom_text(aes(x = step, y = step * 10 + 300, label = str_wrap(predictors, width = 20)), 
          size = 3.5, hjust = 0.5) +
     geom_text_repel(aes(x = step, BIC, label = paste0('BIC=', round(BIC, 3)) ), 
          size = 3.5, col = 'red') +
     geom_text_repel(aes(x = step, AIC, label = paste0('AIC=', round(BIC, 3)) ), 
          size = 3.5, col = 'darkgreen') +
     geom_text_repel(aes(x = step, adj.r.squared, label = paste0('adj.r.squared=', 
          round(adj.r.squared, 3)) ), 
          size = 3.5, col = 'magenta') +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(limits = c(0, 350), breaks=seq(200, 250, 5)) +
     scale_x_continuous(limits = c(0, 5), breaks=seq(0, 5, 1)) 


