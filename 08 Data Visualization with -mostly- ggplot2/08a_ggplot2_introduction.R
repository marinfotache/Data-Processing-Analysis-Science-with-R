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
###                           8a. Introduction to ggplot2               ####
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/08%20Data%20Visualization%20with%20-mostly-%20ggplot2/08_ggplot2.pptx
############################################################################

## last update: 27.11.2022
##
library(tidyverse)
library(readxl)
library(htmltab)
library(xml2)

# giving up scientific notation (1.6e+07)
options(scipen=999, digits=4)


############################################################################
###            Download the necessary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and
# set the directory where you dowloaded the data files as the
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')

# check if the current directory is ok
getwd()
############################################################################



#######################################################################
###                                Agenda                           ###
#######################################################################
###	  I. Basic syntax                                              ###
###       I.1 (At least) Three layers                               ###
###       I.2 Main `geom`s                                          ###
###       I.3 Main title and the subtitle                           ###
###   	I.4 Axis labels/text                                      ###
###	 II. Beyond the basics                                         ###
###	     II.1 Chart rotation                                       ###
###	     II.2 Text on the plot	                                 ###
###		II.3 Legend                                               ###
###		II.4 Faceting                                             ###
###	 III. Useful companions to ggplot2                             ###
###	     III.1 `scales`                                            ###
###	     III.2 `ggrepel`	                                      ###
###	IV. Saving charts                                              ###
#######################################################################
###


### Some tutorials/examples:
# http://r-statistics.co/ggplot2-Tutorial-With-R.html
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
# https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#themes


#######################################################################
###	                    I. Basic syntax                            ###
#######################################################################


#######################################################################
###                 I.1 (At least) Three layers                     ###
#######################################################################

## Every ggplot2 chart is build on different layers.
## Three layers are compulsory
## - data (set)
## - aestetics mapping (aesthetic means here “something you can see”):
##   * position (i.e., on the x and y axes)
##   * color (“outside” color)
##   * fill (“inside” color)
##   * shape (of points)
##   * linetype
##   * size
## - geoms — visual marks that represent data points
##   * points (geom_point, for scatter plots, dot plots, etc)
##   * lines (geom_line)
##   * boxplot (geom_boxplot


###  Import the exchange rates published by National Bank of Romania (BNR)
# see also script `06a`
#load('exchange_rates.RData')

url <- 'http://www.bnr.ro/Exchange-rates-15192.aspx'
exchange_rates <- htmltab::htmltab(doc = url, which = 1, 
          encoding = "UTF-8")
head(exchange_rates)
View(exchange_rates)
names(exchange_rates)[1] <- 'Date'


###########################################################################
## Task 1:
## Draw a plot with the evolution (by dates - `Date`) of
## exchange rate for EUR (`EUR`)

glimpse(exchange_rates)

ggplot(
     exchange_rates %>%
          mutate (Date = lubridate::dmy(Date),
                       EUR = as.numeric(EUR)),     # the data source
     aes (x = Date, y = EUR)) +                    # aestetics
     geom_point()                                  # `geom`s

# not so lisible (see next sections)

ggplot(
     exchange_rates %>%
          mutate (Date = lubridate::dmy(Date),
                       EUR = as.numeric(EUR)),     # the data source
     aes (x = Date, y = EUR)) +                    # aestetics
     geom_line()                                  # `geom`s



###########################################################################
## Task 2:
## Draw a plot with the comparative evolution (by dates - `Date`) of
## exchange rate for EUR, USD and GBP


## solution 1 - three (sumperimposed graphics)
data <- exchange_rates %>%
     transmute (Date = lubridate::dmy(Date), EUR, USD, GBP) %>%
     mutate_if(is.character, as.numeric)
glimpse(data)


# not-so-lisible:
ggplot(
     data,     # the data source
     aes (x = Date )) +                # general `aestetics``
     geom_point(aes (y = EUR, colour = EUR)) +  # `geom` for EUR
     geom_point(aes (y = USD, colour = USD) ) + # `geom` for USD
     geom_point(aes (y = GBP, colour = GBP) )  # `geom` for GBP


# more lisible, but wring labels on y-axis and legend title:
ggplot(
     data,     # the data source
     aes (x = Date )) +                # general `aestetics``
     geom_point(aes (y = EUR, colour = factor('EUR'))) +  # `geom` for EUR
     geom_point(aes (y = USD, colour = factor('USD'))) + # `geom` for USD
     geom_point(aes (y = GBP, colour = factor('GBP')))  # `geom` for GBP



## solution 2 - transform data from `wider` to `longer` format and
##    use `color` aestetic

# first, transform data
data <- exchange_rates %>%
     transmute (Date = lubridate::dmy(Date), EUR, USD, GBP) %>%
     mutate_if(is.character, as.numeric) %>%
     pivot_longer(-Date, names_to = 'currency', values_to = "exchange_rate") %>%
     mutate(currency = factor(currency))
glimpse(data)

# now, the graph (notice `color` aestetic):
ggplot(
     data,                                                     # the data source
     aes (x = Date, y = exchange_rate, color = currency )) +   # general `aestetics`
     geom_point()                                              # a single `geom`


## solution 3 - facet_wrap (using the same transformed data)
ggplot(
     data,                                                   # the data source
     aes (x = Date, y = exchange_rate, color = currency)) +  # general `aestetics`
     geom_point() +                                          # a single `geom`
     facet_wrap( ~ currency)   # display each currency in a separate panel




#######################################################################
###                           I.2 Main `geom`s                      ###
#######################################################################

# Note:
# We are interested mainly in syntax; some examples are not suitable
# for real-world (data) analysis), but they can be adapted for
# serving more relevant cases

data <- exchange_rates %>%
     transmute (Date = lubridate::dmy(Date), EUR, USD, GBP) %>%
     mutate_if(is.character, as.numeric) %>%
     pivot_longer(-Date, names_to = 'currency', values_to = "exchange_rate") %>%
     mutate(currency = factor(currency))
glimpse(data)


# ...we already used `geom_point`


# ... `geom_line`
ggplot(
     data,                                                   # the data source
     aes (x = Date, y = exchange_rate, color = currency)) +  # general `aestetics`
     geom_line()                                  # `geom` for a line plot



# ... `geom_col` (`col` from `column`), with general `colour` aestetic
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_col()

# legend not usable here!

# ... `geom_bar` (see also script `08_b...`)
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)) %>%
          ungroup(),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity")


# ... `geom_histogram` (see also script `08_c...`)
ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_histogram(alpha = .5)                   # `transparency/translucency`

# ... `geom_histogram` with `facets`
ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_histogram(alpha = 0.5)   +    #  `alpha` is for setting the "transparency/translucency"
     facet_wrap( ~ currency)


# ... `geom_histogram` with `facets` and free scales
ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_histogram()   +
     facet_wrap( ~ currency, scales = 'free')


# ... `geom_density` with `facets` and free scales
ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_density(alpha = 0.5)   +
     facet_wrap( ~ currency, scales = 'free')



# ... `geom_boxplot`
ggplot(
     data,
     aes (x = currency, y = exchange_rate)) +
     geom_boxplot()



# ... `geom_point` for scatterplots
ggplot(
     data,
     aes (x = currency, y = exchange_rate)) +
     geom_point()


##
## see also next three scripts (08b..., 08c..., and 08d... )
##   for details (and better examples)
##


#######################################################################
###   	         I.3 Main title and the subtitle                  ###
#######################################################################

## Setting the title and subtitle with `ggtitle`
ggplot(
     data,                                                   # the data source
     aes (x = Date, y = exchange_rate, color = currency)) +  # general `aestetics`
     geom_line() +                                # `geom` for a line plot
     ggtitle("Exchange Rates - RON vs. EUR/USD/GBP",
             subtitle = paste(min(data$Date), max(data$Date), sep = ' - '))


## Setting the title, subtitle and caption with `labs` (from `labels`)
ggplot(
     data,                                                   # the data source
     aes (x = Date, y = exchange_rate, color = currency)) +  # general `aestetics`
     geom_line() +  # `geom` for a line plot
     labs(title = "Exchange Rates - RON vs. EUR/USD/GBP",
             subtitle = paste(min(data$Date), max(data$Date), sep = ' - '),
             caption = "Source: BNR")


## Formatting the titles (font, justification, ...)
##
## theme(
##        plot.title = element_text(),
##        plot.subtitle.title = element_text(),
##        plot.caption = element_text()
##        )
##
## Arguments of the function element_text() includes:
##  - color,
##  - size,
##  - face (“plain”, “italic”, “bold”, “bold.italic”),
##  - family
##  - lineheight (space between two lines of text elements):
##             a number between 0 and 1 (Useful for multi-line plot titles)
##   - hjust and vjust: number in [0, 1], for horizontal and
##        vertical adjustment of titles
##             * hjust = 0.5: Center the plot titles.
##             * hjust = 1: Place the plot title on the right
##             * hjust = 0: Place the plot title on the left


# also notice `\n` for wrapping the title
ggplot(
     data,                                                   # the data source
     aes (x = Date, y = exchange_rate, color = currency)) +  # general `aestetics`
     geom_line() +                                # `geom` for a line plot
     labs(title = "Exchange Rates \nRON vs. EUR/USD/GBP",
             subtitle = paste(min(data$Date), max(data$Date), sep = ' - '),
             caption = "Source: BNR") +
     theme(
        plot.title = element_text(color = "darkblue", size = 14,
                                  face = "bold", hjust = 0.5, lineheight = 1.2),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 8)
        )



#######################################################################
###   	    I.4 Axis labels/text  (size, justification)           ###
#######################################################################


# labels and the format cand be changed with the same `labs` and `theme`
# options


# we continue the last example, changing the axis labels and text format
ggplot(
     data,                                                  # the data source
     aes (x = Date, y = exchange_rate, color = currency)) + # general `aestetics`
     geom_line() +                                          # `geom` for a line plot
     labs(
          title = "Exchange Rates \nRON vs. EUR/USD/GBP",
          subtitle = paste(min(data$Date), max(data$Date), sep = ' - '),
          caption = "Source: BNR") +
     theme(
        plot.title = element_text(color = "darkblue", size = 13,
                                  face = "bold", hjust = 0.5, lineheight = 1.2),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 8)
        ) +
     # here we change the laxes labels
     xlab("Day of the exchange rates") +
     ylab("Exchange Rate (1 currency = ? RON)" ) +
     # here we change the angle of text on the x axis (45%)
     theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))



# remove the axis/axes label
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_col() +
     xlab("") # this remove the x-axis label/title


# remove the axis/axes text
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_col() +
     theme(axis.text.x = element_blank())




#######################################################################
###	                 II. Beyond the basics                         ###
#######################################################################

#######################################################################
###	                     II.1 Chart rotation                       ###
#######################################################################


# horizontal plot (`coord_flip`)
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     coord_flip()



#######################################################################
###	                    II.2 Text on the plot                   ###	#######################################################################

# Text on the chart
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency,
          label = currency)) +  # `label` will display the text on the chart
     geom_bar(stat="identity") +
     coord_flip() +
     geom_text()


#  `geom_label` instead of `geom_text`; label defined on general aestetics
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency,
          label = currency)) +  # `label` will display the text on the chart
     geom_bar(stat="identity") +
     coord_flip() +
     geom_label()


#  `geom_label`; label defined inside `geom_label`
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     coord_flip() +
     geom_label(aes(label = currency))


#  place the text inside the bar (with a larger size)
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     coord_flip() +
     geom_text(aes(label = currency), size = 13, hjust = 1.1)  # notice `hjust`!


#  add another text with the average exchange rate
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     coord_flip() +
     geom_text(aes(label = currency), size = 11, hjust = 1.1)  +
     geom_text(aes(label = round(mean_rate,2)), size = 5, hjust = -0.1)  # notice `hjust`!


#  vertical barplot (notice the `justifications`)
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     geom_text(aes(label = currency), size = 10,
               hjust = 0.5, vjust = 1.3)  +
     geom_text(aes(label = round(mean_rate,2)), size = 5,
               hjust = 0.5, vjust = -0.2)



#######################################################################
###	                        II.3 Legend                         ###
#######################################################################

# remove the legend
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     geom_text(aes(label = currency), size = 10,
               hjust = 0.5, vjust = 1.3)  +
     geom_text(aes(label = round(mean_rate,2)), size = 5,
               hjust = 0.5, vjust = -0.2)  +
     theme(axis.text.x = element_blank()) + # this will remove the text on the x-axis
     xlab("") + # this will remove x-axis label/title
     theme(legend.position="none")  # this will remove the legend


# change the legend title and position
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     geom_text(aes(label = currency), size = 10,
               hjust = 0.5, vjust = 1.3)  +
     geom_text(aes(label = round(mean_rate,2)), size = 5,
               hjust = 0.5, vjust = -0.2)  +
     theme(axis.text.x = element_blank()) + # this will remove the text on the x-axis
     xlab("") + # this will remove x-axis label/title
     labs(fill = 'Select currencies:') + # this will change the legend title
     theme(legend.position="bottom")  # this will change the legend position


# remove the legend title (and keep the legend)
ggplot(
     data %>%
          group_by (currency) %>%
          summarise (mean_rate = mean(exchange_rate)),
     aes (x = currency, y = mean_rate, fill = currency)) +
     geom_bar(stat="identity") +
     geom_text(aes(label = currency), size = 10,
               hjust = 0.5, vjust = 1.3)  +
     geom_text(aes(label = round(mean_rate,2)), size = 5,
               hjust = 0.5, vjust = -0.2)  +
     theme(axis.text.x = element_blank()) + # this will remove the text on the x-axis
     xlab("") + # this will remove x-axis label/title
     theme(legend.position="bottom") + # this will change the legend position
     theme(legend.title=element_blank()) # this will remove the legend title




#######################################################################
###	                           II.4 Faceting                       ###
#######################################################################

 # we already saw the next plot:
ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_density()   +
     facet_wrap( ~ currency, scales = "free") +
     theme(legend.position="none")  # this will remove the legend


# change the format of panel titles
ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_density()   +
     facet_wrap( ~ currency, scales = "free") +
     theme(legend.position="none") + # this will remove the legend
     theme(strip.text = element_text(size = 14) )





# see also script `06a`
##  Import montly earnings from the (Romanian) National Institute for Statistics
url <- 'http://www.insse.ro/cms/ro/content/castiguri-salariale-din-1991-serie-lunara'
net_earning <- htmltab::htmltab(doc = url, which = 1, encoding = "UTF-8")
glimpse(net_earning)
head(net_earning)
names(net_earning)[1] <- 'Year'

# load('net_earning.RData')
data_earning <- net_earning %>%
     pivot_longer(-Year, names_to = 'month', values_to = "net_earning") %>%
     mutate (net_earning = map_chr(.x = .$net_earning,
               ~ str_remove_all(.x, '\\.') )) %>%
     mutate( net_earning = if_else(Year <= 2004,
                                   as.numeric(net_earning) / 10000,
                                   as.numeric(net_earning) )
     ) %>%
     group_by(Year) %>%
     mutate (new_month = paste0(
          str_pad(row_number(), width = 2, side = 'left', pad = '0'),
          '-', month)) %>%
     ungroup()

glimpse(data_earning)

# with facet_wrap
ggplot(data_earning,
       aes (x = new_month, y = net_earning, fill = new_month)) +
     geom_col() +
     facet_wrap(~ Year) +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) #


# with facet_grid
ggplot(data_earning,
       aes (x = new_month, y = net_earning, fill = new_month)) +
     geom_col() +
     facet_grid(. ~ Year) +
     theme(legend.position="none") + # this will remove the legend
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) #



#######################################################################
###	            III. Useful companions to ggplot2                  ###
#######################################################################


#######################################################################
###	                      III.1 `scales`                           ###
#######################################################################

# add a scale on the y-axis
ggplot(
     data,                         # the data source
     aes (x = Date, y = exchange_rate,
          color = currency)) +    # general `aestetics`
     geom_line() +                # `geom` for a line plot
     labs(
          title = "Exchange Rates \nRON vs. EUR/USD/GBP",
          subtitle = paste(min(data$Date), max(data$Date), sep = ' - '),
          caption = "Source: BNR") +
     theme(
        plot.title = element_text(color = "darkblue", size = 13,
                                  face = "bold", hjust = 0.5, lineheight = 1.2),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 8)
        ) +
     # here we change the laxes labels
     xlab("Day of the exchange rates") +
     ylab("Exchange Rate (1 currency = ? RON)" ) +
     # here we change the angle of text on the x axis (45%)
     theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
     scale_y_continuous(limits = c(4,5.5), breaks = seq(4, 5.5, 0.1))




#######################################################################
###	                         III.2 `ggrepel`	            ###
#######################################################################
### see next scripts

#######################################################################
###	                         IV. Saving charts                   ###
#######################################################################

getwd()

# how to save graphics on disc
imageDirectory <- getwd()
imageFile <- paste(imageDirectory, "exchange rates.png",sep="/")
ggsave(file = imageFile)

# for including multiple charts on the same figure, see package `patchwork`
library(scales)
library(patchwork)

g1 <- ggplot(
     data,                         # the data source
     aes (x = Date, y = exchange_rate,
          color = currency)) +    # general `aestetics`
     geom_line() +                # `geom` for a line plot
     labs(
          title = "Exchange Rates \nRON vs. EUR/USD/GBP",
          subtitle = paste(min(data$Date), max(data$Date), sep = ' - '),
          caption = "Source: BNR") +
     theme(
        plot.title = element_text(color = "darkblue", size = 13,
                                  face = "bold", hjust = 0.5, lineheight = 1.2),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 8)
        ) +
     # here we change the laxes labels
     xlab("Day of the exchange rates") +
     ylab("Exchange Rate (1 currency = ? RON)" ) +
     # here we change the angle of text on the x axis (45%)
     theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
     scale_y_continuous(limits = c(4,6), breaks = seq(4, 6, 0.1))

g2 <- ggplot(
     data,
     aes (x = exchange_rate, fill = currency)) +
     geom_density(alpha = 0.5)   +
     facet_wrap( ~ currency, scales = "free") +
     theme(legend.position="none") + # this will remove the legend
     theme(strip.text = element_text(size = 14) )

x <- g1 + g2 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("a_plot.pdf", plot = x,  device = "pdf") 
ggsave("a_plot.png", plot = x,  device = "png") 


