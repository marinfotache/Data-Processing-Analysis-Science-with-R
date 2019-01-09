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
###                06b. Introduction to web scraping with R              ###   

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/06%20XML-%20JSON-%20Web%20Scrapping-%20APIs/06_html_xml_json__web_scrap__apis.pptx
############################################################################
## last update: 05.01.2019


# needed packages
library(tidyverse)
# Activate the `XML` library
#library(XML)
#library(methods)
library(htmltab)
library(rvest)


############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
###  I. Import HTML tables with `rvest` package                       ###
###       I.a. Dealing with one table per web page                    ###
###       I.b. Dealing with multiple tables per web page              ###
###  II. Extract non-tabular data from web pages                      ###
###  III. Dealing with tables scattered on multiple web pages         ###
#########################################################################


#########################################################################
###              I. Import HTML tables with  `rvest` package          ###
#########################################################################
### This time, we'll locate the table within the web page             ###

#########################################################################
###              I.a. Dealing with one table per web page             ###
#########################################################################

###
### Task: Extract the Romanian names frequency from the web page
### below:
url <- "http://www.name-statistics.org/ro/numedefamiliecomune.php"

# remember that that the solution in script `06a` was...
#roNF1 <- XML::readHTMLTable(url,which=1)

# next solution is based on `rvest`

# read the web page
romanian_names_raw <- xml2::read_html("http://www.name-statistics.org/ro/numedefamiliecomune.php") 

# extract the table
romanian_names <- romanian_names_raw %>%
     rvest::html_nodes("table") %>%
     .[[1]] %>%     # there one table within the page
     rvest::html_table(, fill=TRUE ,header=TRUE)
     


#########################################################################
###          I.b. Dealing with multiple tables per web page           ###
#########################################################################

##  Wikipedia main page on Romania

## read the page
wikipedia_romania_url <- 'https://en.wikipedia.org/wiki/Romania'
wikipedia_romania_raw <- xml2::read_html(wikipedia_romania_url) 

## extract all the tables within the page 
wikipedia_romania_tables <- wikipedia_romania_raw %>%
     rvest::html_nodes("table")


## we are interested in `Foreign-born population` table
# we know that this is the 6th of the tables
foreign_born <- wikipedia_romania_tables %>%
     .[[6]] %>%
     rvest::html_table(, fill=TRUE ,header=TRUE)



#########################################################################
###             II. Extract non-tabular data from web pages           ###
#########################################################################
### for scrapping using CSS, SelectorGadget etc. - see the recommended
### resources in the GitHub presentation indicated at the beginning of
### this script


###
###  Task 1: Using Wikipedia page 'https://en.wikipedia.org/wiki/President_of_Romania',
## extract the name of the current president of Romania
wikipedia_romania_president <- 'https://en.wikipedia.org/wiki/President_of_Romania' %>%
     xml2::read_html() %>%
     rvest::html_nodes('td b') %>%
     html_text() %>%
     tibble() %>%
     filter(str_detect(., "Incumbent")) %>%
     str_replace(., 'Incumbent', '') 

wikipedia_romania_president


###
###  Task 2: Using Wikipedia page 'https://en.wikipedia.org/wiki/Romania',
## extract the text of all section `Education`

## read the page
wikipedia_romania_url <- 'https://en.wikipedia.org/wiki/Romania'
wikipedia_romania_raw <- xml2::read_html(wikipedia_romania_url) 


wikipedia_romania_science <- wikipedia_romania_raw %>% 
     rvest::html_nodes("h3,p") %>%
     rvest::html_text() %>%
     tibble (text = .) %>%
     mutate (row_number = row_number()) %>%
     mutate(
          row_number_education = if_else(text == 'Education', row_number, 0L), 
          row_number_healthcare = if_else(text == 'Healthcare', row_number, 0L) 
          ) %>%
     mutate(
          row_number_education = max(row_number_education), 
          row_number_healthcare = max(row_number_healthcare)
          ) %>%
     filter (row_number > row_number_education & row_number < row_number_healthcare) %>%
     select(text) %>%
     pull()



#########################################################################
###       III. Dealing with tables scattered on multiple web pages    ###
#########################################################################

###
### Task: Get the list of wars from  Wikipedia

wikipedia_wars_url <- "https://en.wikipedia.org/wiki/Timeline_of_wars"
wikipedia_wars_raw <- wikipedia_wars_url %>%
     read_html() 

links_and_titles <- wikipedia_wars_raw %>%
     html_nodes("ul li a") %>%
     head(8) %>%
     html_attrs() %>%    # get a list
     tibble(link = map_chr(., 'href'),     # create the data frame from the list
            title = map_chr(., 'title'))    %>%
     select (-`.`)

View(links_and_titles)      

# build the tibble by looping through `links_and_titles`
wikipedia_wars_df <- tibble()
for (i in 1:nrow(links_and_titles)) {
     
     # set the wikipedia page address
     crt_adress <- paste0('https://en.wikipedia.org', links_and_titles$link[i])
     
     # extract the table from the current address
     crt_tibble <- xml2::read_html(crt_adress) %>%
          html_nodes("table.wikitable") %>%
          .[[1]]   %>%
          rvest::html_table(, fill=TRUE ,header=FALSE)
     
     # get the column names ( table header is on two rows)
     if (i == 1) 
          column_names <- crt_tibble[2,]
     
     # set the column names and extract all rows except the first two
     crt_tibble <- crt_tibble %>%
          set_names(column_names) %>%
          tail(nrow(crt_tibble) - 2)
     
     # add current table to the final table
     wikipedia_wars_df <- bind_rows(wikipedia_wars_df, crt_tibble)
}

View(wikipedia_wars_df)




