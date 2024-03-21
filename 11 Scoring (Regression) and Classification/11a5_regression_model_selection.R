################################################################################
#########                Regression Models Selection                ############
################################################################################
####  last update: 21.12.2020

library(tidyverse)
library(ggrepel)
library(broom)
#library(corrplot)
#library(car)
#library(QuantPsyc)
library(janitor)

############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

options(scipen=999, digits=4)



###################################################################################
###                      Case study: `States` (USA)
#  Example taken from Kabacoff's R in Action (Manning), 2013, 2015 
# state.x77 dataset in the base package 
# Explore the relationship between a state’s murder rate and other characteristics of
#   the state, including population, illiteracy rate, average income, 
#   and frost levels (mean number of days below freezing).
###################################################################################
states <- as.data.frame(state.x77)
states$State <- row.names(states)
names(states) <- str_replace_all(names(states), ' |\\.', '_')
head(states)

# janitor::clean_names()

# 
# for Exploratory Data Analysis, see script 09c...
# ... see previous scripts


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

#idx_figure <- 101

# Density plot of the residuals
title_ <- paste0('Residuals Distribution for ALL-IN Model')
plot <- ggplot(data = model_details, aes(x = .resid)) + 
	geom_density(colour="black") + 
     xlab("Residuals") + ylab("Density") +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey() +
     theme(strip.text.x = element_text(size = 9)) +
     scale_y_continuous() +
     scale_x_continuous(breaks=seq(-4, 4, .5)) +
     theme(axis.text.x  = element_text(angle=45, vjust=.5, hjust=.5)) 
print (plot)
#ggsave(filename = paste0('Fig_', idx_figure,  "_", 
#     title_,"_a.pdf"), dpi=1200)


# Box plot of the residuals
plot <- ggplot(data = model_details, aes(x = 1, y = .resid)) + 
	geom_boxplot() + 
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey() + 
     theme(axis.text.x=element_blank()) +
     scale_y_continuous(breaks = seq(-3, 3, .5)) +
     xlab("Residuals") 
print (plot)



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
####      and/or predict the outcome (Murder rate)             ####
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

while (!stop) {
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
plot <- ggplot(data = models_residuals, 
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
print (plot)


# Box plot of the residuals sum of squares for models on each step of 
# the backaward elimination
title_ <- paste0('Residuals Sum of Squares - Backward Elimination')
plot <- ggplot(data = models_residuals, 
          aes(x = factor(n_of_predictors), y = rss)) + 
	geom_boxplot() +
     ggtitle(title_) +
     xlab("Number of predictors in the model") +
     theme_bw() +
     scale_fill_grey() + 
     scale_y_continuous() 
print (plot)


# plot the BIC for all the models
title_ <- 'BIC (Bayesian Information Criterion) Evolution for Each Step\n of Backward Selection'    
plot <- ggplot(the_backward_models, aes(x = step, y = BIC)) +
     geom_point(col = 'red') +
     geom_text_repel(aes(label = predictors), size = 3.5) +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(breaks=seq(180, 280, 1)) 
print(plot)


title_ <- 'BIC, AIC and Adjusted R^2 for Each Step\n of Backward Selection'    
plot <- ggplot(the_backward_models, aes(x = step, y = BIC)) +
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
print(plot)



################################################################
##                 b. Forward selection                       ## 
################################################################
## b.1. Choose a significance level (usually 0.05 or 0.10).
## b.2  Begin with the null model (a model that contains an 
##       intercept but no predictors).
## b.3. Fit all the models, by adding to the current (kept) 
##   predictor(s), each variable from the remaining (un-kept)
##   predictors.
## b.4. Select, from all models built in b.3, the model with the 
##   lowest Residual Sum of Squares (the best model).
## b.5. Test if in the best model (resulted in b.3),
##   its corresponding additional variable has a p-value above
##   the significance level (0.05):
##   If NO, then keep all the current predictors and go to b.3
##   If YES, then STOP
##   
the_forward_models <- tibble()

# two set of predictors...
# ... one for predictor to be kept in the final model
kept_predictors <- c()
# ... and the rest (pending, predictors to be processed)
remaining_predictors <- predictors


# in this data frame we'll log the sequence of adding predictors
# in the model
added_predictors <- tibble()

# here, we'll log the residuals of each modele
models_residuals <- tibble()

current_step <- 0

## Choose a significance level (usually 0.05 or 0.10)
significance_level <- 0.05


stop <- FALSE

while (!stop) {
     current_step <- current_step + 1
     
     # build the modeles containing the set of predictors to be
     # kept plus each of the remaining predictors
     current_models <- tibble()
     
     # loop through all the remaining predictors
     # i <- 1
     for (i in 1:length(remaining_predictors)) {
          # current set of predictors is composed of `kept` predictors + 
          # one of the remaining predictors
          formula_ <- paste('Murder ~', paste(
               c(kept_predictors, remaining_predictors[i]), collapse = ' + '))
          # fit the model
          current_model <- lm(formula = formula_, data = states %>% select (-State))   
          # summary(current_model)
          # get the generall information about the model
          model_info <- glance(current_model)
          names(model_info) <- paste0('model_', names(model_info))
          
          # get the information about the model containing the newly added
          # predictor
          current_predictor <- tidy(current_model) %>%
               filter (term == remaining_predictors[i]) 
          names(current_predictor) <- paste0('crt_predictor_',  names(current_predictor))
          
          all_info_crt_model <- cbind(current_predictor, model_info)
                    
          # add this row to the competing models
          current_models <- bind_rows(current_models, all_info_crt_model)
          
          # also, store the models residuals
          models_residuals <- bind_rows(models_residuals, 
               test <-   augment(current_model) %>%
                    select (.resid) %>%
                    mutate (foo = 1)  %>%
                    inner_join(all_info_crt_model %>% mutate (foo = 1)) %>%
               mutate (step = current_step, formula = formula_ )  %>%
               select (-foo) )
     }
     
     # find the most significant predictor (the predictor of the model with 
     # the lowest RSS (Residual Sum of Squares))
     the_most_significant_predictor <- current_models %>%
          mutate (min_rss = min(model_deviance), step = current_step) %>%
          filter (model_deviance == min_rss) %>%
          head(1)
     

     # the the p-value of the most significant predictor is larger
     # than the significance level, then STOP
     if (the_most_significant_predictor[1,]$crt_predictor_p.value > significance_level )
          break
     
     # here, we still have predictors with p-value smaller than 
     #   the significance level

     # we'll add the new predictor to the model ...
     kept_predictors <- c(kept_predictors, 
          the_most_significant_predictor[1,]$crt_predictor_term )
     
     
     # update the remaining predictors
     remaining_predictors <- setdiff(remaining_predictors, 
                    the_most_significant_predictor[1,]$crt_predictor_term)
     
     # ... and store it in the `removed_predictors` data frame
     added_predictors <- bind_rows(added_predictors, 
               the_most_significant_predictor         )
     
     # add the overall information about the data into the dataframe
     #  `the_forward_models`
     the_forward_models <- bind_rows(the_forward_models,
          the_most_significant_predictor %>%
               mutate (predictors = paste(kept_predictors, collapse = ' + '),
                       step = current_step))
     
}     

# The resulted model is:
# 
kept_predictors
# 
formula_ <- paste('Murder ~', paste(kept_predictors, collapse = ' + '))
final_model_forward <- lm(formula = formula_, data = states %>% select (-State))   
summary(final_model_forward)

# Compare it the for model obtained through backard eliminatio
summary(final_model_backard)


models_residuals <- models_residuals %>%
     mutate(predictors = str_replace_all(formula,  'Murder ~ ', ''), 
            rss = .resid ^ 2)


# Density plot of the residuals (for checking the normality)
title_ <- paste0('Residuals - Forward Selection')
plot <- ggplot(data = models_residuals, 
     aes(x = .resid, color = predictors )) + 
	geom_density(alpha = 0.2) + 
     facet_wrap( ~ factor(step)) +
     xlab("Residuals") + ylab("Density") +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey() +
     scale_y_continuous() +
     scale_x_continuous(breaks=seq(-4, 4, .5)) +
     theme(axis.text.x  = element_text(angle=45, vjust=.5, hjust=.5))  + 
	theme(legend.position = "bottom", legend.direction = "horizontal") 
print (plot)


# Boxplot of the RSS
title_ <- paste0('RSS - Forward Selection')
plot <- ggplot(data = models_residuals, 
          aes(x = predictors, y = rss)) + 
	geom_boxplot() +
     facet_wrap( ~ factor(step), scale = 'free') +
     ggtitle(title_) +
     xlab("  ") +
     theme_bw() +
     scale_fill_grey() + 
     theme(axis.text.x  = element_text(angle=45, vjust=.5, hjust=0.5))   +
     scale_y_continuous() 
print (plot)


# plot the BIC for all the models
title_ <- 'BIC (Bayesian Information Criterion) Evolution for Each Step\n of Forward Selection'    
plot <- ggplot(the_forward_models, aes(x = step, y = model_BIC)) +
     geom_point(col = 'red') +
     geom_text_repel(aes(label = predictors), size = 3.5) +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(breaks=seq(180, 280, 1)) 
print(plot)


title_ <- 'BIC, AIC and Adjusted R^2 for Each Step\n of Forward Selection'    
plot <- ggplot(the_forward_models, aes(x = step, y = model_BIC)) +
     geom_point(col = 'red', size = 2) +
     geom_point(aes(x = step, y = model_AIC), col = 'darkgreen', size = 2) +
     geom_point(aes(x = step, y = model_adj.r.squared), col = 'magenta', size = 2) +
     geom_text(aes(x = step, y = step * 10 + 300, label = str_wrap(predictors, width = 25)), 
          size = 3.5, hjust = 0.5) +
     geom_text_repel(aes(x = step, model_BIC, label = paste0('BIC=', round(model_BIC, 3)) ), 
          size = 3.5, col = 'red') +
     geom_text_repel(aes(x = step, model_AIC, label = paste0('AIC=', round(model_AIC, 3)) ), 
          size = 3.5, col = 'darkgreen') +
     geom_text_repel(aes(x = step, model_adj.r.squared, label = paste0('adj.r.squared=', 
          round(model_adj.r.squared, 3)) ), 
          size = 3.5, col = 'magenta') +
     ggtitle(title_) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(limits = c(0, 350), breaks=seq(200, 250, 5)) +
     scale_x_continuous(limits = c(0, 5), breaks=seq(0, 5, 1)) 
print(plot)





################################################################
##       D. Score comparison (for all possible models)        ## 
################################################################


############################################################
###  Warming up:     
###  Build all the models with just one predictor
###
the_models <- tibble()

# model identifier
model_id_ <- 1

# i <- 1
for (i in 1:length(predictors)) {
     crt_predictors <- predictors[i]
     crt_dataset <- states[c('Murder', crt_predictors)]
     crt_model <- lm(Murder ~ ., data = crt_dataset)
     crt_model_overall <- glance(crt_model)


     the_models <- bind_rows(the_models, bind_cols(
          tibble(model_id = model_id_, 
                 formula = paste(crt_model$terms[[2]], crt_model$terms[[1]], 
                                 crt_model$terms[[3]]),
                 predictor_list = crt_predictors  ),
          crt_model_overall)
          )     
     model_id_ <- model_id_ + 1
}
          
# examine the models (AIC, BIC, ...)
# discussion



############################################################
###       Build a data frame all predictor combinations 
###
# this is the vector with all possible (simple) predictors
predictors <- setdiff(names(states), c('State', 'Murder'))

# start with just one predictor
df_one_predictor <- tibble(predictor = predictors) 

previous_df_predictors <- df_one_predictor %>%
     transmute(predictor_1 = predictor, foo = 1)

df_all_possible_predictors <- df_one_predictor %>%
     transmute (model_id = 1000000 + row_number(), predictor_1 = predictor)
     
# i <- 2
for (i in 2:length(predictors)) {
     # we cross join `previous_df_predictors` with `preod`
     col_name_ <- paste0('predictor_', i)
          
     crt_df_predictors <- previous_df_predictors %>%
          mutate (foo = 1) %>%
          full_join(df_one_predictor %>% mutate (foo = 1)) %>%
          select (-foo) %>%
          mutate (!!col_name_ := predictor) %>%
          select (-predictor) %>%
          mutate (model_id = i * 1000000 + row_number()) %>%
          pivot_longer(-model_id, names_to = "predictor_series", 
                       values_to = "predictor") %>%
          select (model_id, predictor) %>%
          arrange(model_id, predictor) %>%
          group_by(model_id, predictor) %>%
          summarise(n_of_occurences = n()) %>%
          filter (n_of_occurences == 1) %>%
          select (-n_of_occurences)%>%
          arrange(model_id, predictor) %>%
          group_by(model_id) %>%
          mutate (attrib_name = paste0('predictor_', row_number())) %>%
          ungroup() %>%
          tidyr::pivot_wider(names_from = "attrib_name", 
                             values_from = "predictor") %>%
          dplyr::select (-model_id) %>%
          distinct(.) %>%
          mutate(n_of_non_na = rowSums(!is.na(.))) %>%
          filter(n_of_non_na == i) %>%
          select (-n_of_non_na) %>%
          mutate(model_id = i * 1000000 + row_number())

     
     df_all_possible_predictors <- bind_rows(df_all_possible_predictors,
                     crt_df_predictors                        )
     
     previous_df_predictors <- crt_df_predictors %>%
          select (-model_id)
     
}     


################################################################
###  Now, build the models based on `dl_all_possible_predictors`

the_models <- tibble()
the_model_residuals <- tibble()

# i <- 1
for (i in 1:nrow(df_all_possible_predictors)) {
     model_id_ <- df_all_possible_predictors[i,]$model_id
     crt_predictors <- df_all_possible_predictors[i,2:ncol(df_all_possible_predictors)]
     crt_predictors <- str_c(crt_predictors)
     crt_predictors <- crt_predictors[!is.na(crt_predictors)]
     
     crt_dataset <- states[c('Murder', crt_predictors)]
     crt_model <- lm(Murder ~ ., data = crt_dataset)
     crt_model_overall <- glance(crt_model)
     outcome_ <- crt_model$terms[[2]]
     tilda_ <- crt_model$terms[[1]]
     predictor_expression <-  as.character(crt_model$terms[[3]])
     if (length(predictor_expression) > 1)
          predictor_expression <- paste(predictor_expression[2:length(predictor_expression)],
               collapse = paste0(' ', predictor_expression[1], ' ' ))
     
     formula_ = paste(outcome_, tilda_, predictor_expression)


     the_models <- bind_rows(the_models, 
          bind_cols(
               tibble(model_id = model_id_, 
                 formula = formula_ [1],
                 predictor_list = list(crt_predictors) ),
               crt_model_overall)
          )     
     
     the_model_residuals <- bind_rows(the_model_residuals,
          augment(crt_model) %>%
               select (.resid) %>%
               mutate (model_id = model_id_)    ) 

}

the_models <- the_models %>%
     mutate(n_of_predictors = map_int(predictor_list, length),
          predictors = str_replace_all(formula,  'Murder ~ ', ''))
        

# sort the models
the_models <- the_models %>%
     arrange(desc(adj.r.squared))

the_models <- the_models %>%
     arrange(AIC)

the_models <- the_models %>%
     arrange(BIC)


# Now, examine the best model
# 
model_row <- the_models[1,]

model <- lm(formula = model_row[1,]$formula, data = states)
summary(model)


# compare ...
summary(final_model_backard)


# plot the BIC for all the models
plot <- ggplot(the_models, aes(x = n_of_predictors, y = BIC)) +
     geom_point(col = 'red') +
     geom_text_repel(aes(label = predictors), size = 3.5) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(breaks=seq(200, 280, 5)) 
print(plot)


# the chart is too busy, 
# so we'll display oly 25% of the best models (lowest BIC)
#
fewer_models <- the_models %>%
     mutate(first_quartile = quantile (BIC, .25)) %>%
     filter (BIC <= first_quartile)
 
title <- 'Top Model in the First Quartile as the Lowest BIC'    
plot <- ggplot(fewer_models, aes(x = n_of_predictors, y = BIC)) +
     geom_point(col = 'red') +
     geom_text_repel(aes(label = paste0 (predictors, ' (BIC=', round(BIC,3), ')')), 
               size = 3.5) +
     theme_bw() +
     scale_fill_grey()  +
     scale_y_continuous(breaks=seq(200, 225, 1)) 
print(plot)




####################################################################
###            Model selection with package `leaps`              ###
####################################################################
# install.packages('leaps')
library(leaps)


# function `regsubsets` 
models_leaps <- regsubsets( Murder ~.,  data=states %>% select (-State), 
     nvmax = 8) # nvmax is the maximum number of predictors to be 
                # included in the model (default is 8)

# attributes of the `models_leaps` object
models_leaps$np
models_leaps$d
models_leaps$first
models_leaps$last
models_leaps$vorder
models_leaps$rss
models_leaps$method
models_leaps$xnames
models_leaps$coef

     
## attributes of the summary

leaps_summary <- summary(models_leaps)
leaps_summary

# compare with the final graphics for the forward selection
# 

leaps_summary$which
leaps_summary$rsq
leaps_summary$rss
leaps_summary$adjr2
leaps_summary$bic
leaps_summary$outmat


# plot (by default, the plot uses BIC)
plot(models_leaps)


# plot using the adjusted R square
plot(models_leaps, scale = "adjr2")


# extract the best model based on the adjusted R square
best_model_index <- which.max(leaps_summary$adjr2)
best_model_index

### print the coefficients of the best model
coef(models_leaps, best_model_index)


## The regsubset() function does NOT not provide details about the models, 
## the predictors (p-values);
## This information must be found out by fitting `manually` the models




####################################################################
###            Forward selection with package `leaps`            ###

models_leaps_forward <- regsubsets( Murder ~.,  data=states %>% select (-State), 
     nvmax = 8, method="forward") 

forward_leaps_summary <- summary(models_leaps_forward)
forward_leaps_summary

# plot (by default, the plot uses BIC)
plot(models_leaps_forward)

# plot using the adjusted R square
plot(models_leaps_forward, scale = "adjr2")


####################################################################
###            Blackward selection with package `leaps`          ###

models_leaps_backward <- regsubsets( Murder ~.,  data=states %>% select (-State), 
     nvmax = 8, method="backward") 

backward_leaps_summary <- summary(models_leaps_backward)
backward_leaps_summary

# plot (by default, the plot uses BIC)
plot(models_leaps_backward)

# plot using the adjusted R square
plot(models_leaps_backward, scale = "adjr2")




####################################################################
###            Model selection with package `MASS`               ###
###  (Variable selection is based only on AIC)                   ###        
####################################################################
library(MASS)

model_mass <- lm( Murder ~.,  data = states %>% dplyr::select (-State) ) 

# Stepwise Regression in both directions
step_mass_both <- stepAIC(model_mass, direction = 'both')

# Display results
step_mass_both$anova 

# Backward Regression 
step_mass_backward <- stepAIC(model_mass, direction = 'backward')

# Display results
step_mass_backward$anova 




####################################################################
###             Predictor importance within the models           ###
####################################################################


####################################################################
###         Predictor importance with package `relaimpo`         ###
# install.packages('relaimpo')
library(relaimpo)


# Calculate Relative Importance for Each Predictor
calc.relimp(states_lm1,type=c("lmg","last","first","pratt"),
   rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(states_lm1, b = 1000, type = c("lmg", 
  "last", "first", "pratt"), rank = TRUE, 
  diff = TRUE, rela = TRUE)
str(boot)
warnings()

# print result
booteval.relimp(boot)

# plot result
plot(booteval.relimp(boot,sort=TRUE)) 



