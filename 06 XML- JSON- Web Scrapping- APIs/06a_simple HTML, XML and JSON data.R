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
###       06a. HTML tables, simple XML and JSON Data Management in R     ###   
############################################################################

### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/06%20XML-%20JSON-%20Web%20Scrapping-%20APIs/06_html_xml_json__web_scrap__apis.pptx
############################################################################
## last update: 13.03.2021


# needed packages
library(tidyverse)
library(htmltab)
#library(methods)
library(xml2)

# install.packages('flatxml')
library(flatxml)

# install.packages("xml2relational", dependencies = TRUE)
library(xml2relational)

# install.packages("xmlconvert", dependencies = TRUE)
library(xmlconvert)


# install.packages('rjson')
# library(rjson)
library(jsonlite)
#install.packages('data.tree')
#library(data.tree)

#install.packages('tidyjson')
library(tidyjson)



############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)-1 googledrive/DataSets')


#########################################################################
###                                Agenda                             ###
#########################################################################
###  I. Import HTML tables                                            ###
###       I.a. Import HTML tables with `htmltab` package              ###
###       I.b. Import HTML tables with `XML` package                  ###
###  II. Simple XML data management                                   ###
###       II.a. Import and manage simple XML files                    ###
###            with `XML`/`XML2` packages                             ###
###       II.b. Import and manage simple XML files                    ###
###            with `flatxml` package                                 ###
###  III. JSON data management with `jsonlite` and tidyjson` packages ### 
###       III.1 Simple/rectangular data                               ###
###       III.2 Single-level nested data                              ###
###       III.3 Multi-level nested data                               ###            
#########################################################################



#########################################################################
###                      I. Import HTML tables                        ###
#########################################################################

#########################################################################
###            I.a. Import HTML tables with `htmltab` package         ###
#########################################################################


# see also script `03b`, section `II.2`
##  Import montly earnings from the (Romanian) National Institute for Statistics
url <- 'http://www.insse.ro/cms/ro/content/castiguri-salariale-din-1991-serie-lunara'
net_earning <- htmltab::htmltab(doc = url, which = 1, encoding = "UTF-8")
glimpse(net_earning)
head(net_earning)
names(net_earning)[1] <- 'Year'


###  Import the exchange rates published by National Bank of Romania (BNR)
# see also script `03b`, section `II.2`
url <- 'http://www.bnr.ro/Exchange-rates-15192.aspx'
exchange_rates <- htmltab::htmltab(doc = url, which = 1, 
          encoding = "UTF-8")
head(exchange_rates)
View(exchange_rates)
names(exchange_rates)[1] <- 'Date'


#########################################################################
##        Average daily maximum and minimum temperatures 
##        for the eight largest cities in Romania
##  source: Wikipedia
url <- 'https://en.wikipedia.org/wiki/Romania'
# the table of interest is the 2nd on the page
main_cities_temperature <- htmltab::htmltab(url,which=2)


#########################################################################
##                        Administrative divisions of Romania
##  source: Wikipedia
url <- 'https://en.wikipedia.org/wiki/Romania'
# the table of interest is the 3rd on the page
main_admin_regions_ro <- htmltab::htmltab(url, which = 3)




#########################################################################
###                   II. Simple XML data management                  ###
#########################################################################
###  By `simple` we mean data in a (quasi) rectangular format, i.e.   ###
###  without deep nested levels                                       ###


#########################################################################
###       II.a. Import and manage simple XML files                    ###
###          with `xml2relational` and  `xmlconvert` packages         ###
#########################################################################


#########################################################################
##                       Romanian counties 
url <- 'https://data.gov.ro/dataset/8ec29b07-1c5d-40a2-bfa7-a1f01d482d86/resource/16639504-4f56-423b-9188-85b103049437/download/nomjudete.xml'

romanian_counties_init <- xml2relational::toRelational(url)
class(romanian_counties_init)
romanian_counties_init
romanian_counties <- romanian_counties_init[[2]]

names(romanian_counties)

romanian_counties_init2 <- xmlconvert::xml_to_df(file = url, records.tags = "NOM_JUDETE")
glimpse(romanian_counties_init2)

romanian_counties2 <- romanian_counties_init2 %>%
     mutate (id = row_number()) %>%
     pivot_longer(-id) %>%
     mutate (rec_list = str_split(value, '\\||\\|\\|')) %>%
     rowwise() %>%
     mutate (
          COD = as.integer(str_remove(rec_list[[2]], 'COD~')),
          DENUMIRE = str_remove(rec_list[[4]], 'DENUMIRE~'),
          AUTO = str_remove(rec_list[[6]], 'AUTO~'),
          ORDINE = as.integer(str_remove(rec_list[[8]], 'ORDINE~'))
          )

glimpse(romanian_counties2)


#########################################################################
##                                   CD catalog
url <- 'https://www.w3schools.com/xml/cd_catalog.xml'
cd_init <- xml2relational::toRelational(url)
cd <- cd_init[[2]]


cd_init2 <- xmlconvert::xml_to_df(file = url, records.tags = "CATALOG")
glimpse(cd_init2)

cd_2 <- cd_init2 %>%
     mutate (id = 1) %>%
     pivot_longer(-id) %>%
     mutate (rec_list = str_split(value, '\\||\\|\\|')) %>%
     rowwise() %>%
     mutate (
          TITLE = str_remove(rec_list[[2]], 'TITLE~'),
          ARTIST = str_remove(rec_list[[4]], 'ARTIST~'),
          COUNTRY = str_remove(rec_list[[6]], 'COUNTRY~'),
          COMPANY = str_remove(rec_list[[8]], 'COMPANY~'),
          PRICE = as.numeric(str_remove(rec_list[[10]], 'PRICE~')),
          YEAR = as.integer(str_remove(rec_list[[12]], 'YEAR~'))
          )



#########################################################################
###    II.b. Import and manage simple XML files with `xml2` package   ###
#########################################################################

# read the xml data
cd_data <- xml2::read_xml(url)


## some useful functions 

xml2::xml_name(cd_data)

xml2::xml_children(cd_data)


# Find all `CD` nodes anywhere in the document
cds <- xml2::xml_find_all(cd_data, ".//CD")
xml2::xml_path(cds)
xml2::xml_text(cds)

# Find all `TITLE` nodes anywhere in the document
titles <- xml2::xml_find_all(cd_data, ".//TITLE")
titles
xml2::xml_path(titles)
xml2::xml_text(titles)


# covert the XML file into a tibble
cd_tibble <- tibble (
     TITLE = cd_data %>% 
          xml_find_all("//TITLE") %>%
          xml_text("TITLE") %>% as.character(),
     ARTIST = cd_data %>% 
          xml_find_all("//ARTIST") %>%
          xml_text("ARTIST") %>% as.character(),
     COUNTRY = cd_data %>% 
          xml_find_all("//COUNTRY") %>%
          xml_text("COUNTRY") %>% as.character(),
     COMPANY = cd_data %>% 
          xml_find_all("//COMPANY") %>%
          xml_text("COMPANY") %>% as.character(),
     PRICE = cd_data %>% 
          xml_find_all("//PRICE") %>%
          xml_text("PRICE") %>% as.numeric(),
     YEAR = cd_data %>% 
          xml_find_all("//YEAR") %>%
          xml_text("YEAR") %>% as.integer()
)

View(cd_tibble)



#########################################################################
###       II.c. Import and manage simple XML files                    ###
###            with `flatxml` package                                 ###
#########################################################################

#########################################################################
##                                   CD catalog
cd_url <- 'https://www.w3schools.com/xml/cd_catalog.xml'
cd_data_flatxml_init <- flatxml::fxml_importXMLFlat(cd_url)
View(cd_data_flatxml_init)

cd_data_flatxml_final <- cd_data_flatxml_init %>%
     filter (!is.na(value.)) %>%
     transmute (attribute = elem., value = value., 
             elementid = elemid., rowid = row_number()) %>%
     mutate (rowid_ok = if_else(attribute == 'TITLE', rowid, 0L)) %>%
     mutate(row_id_final = cumsum(rowid_ok)) %>%
     select(row_id_final, attribute, value) %>%
     spread(attribute, value)

View(cd_data_flatxml_final)



#########################################################################
##                                   Hamlet script
## here a structure is more complicated
hamlet_url <- 'http://www.chilkatsoft.com/xml-samples/hamlet.xml'
hamlet_flatxml_init <- flatxml::fxml_importXMLFlat(hamlet_url)

View(hamlet_flatxml_init)

hamlet_flatxml_final <- hamlet_flatxml_init %>%
     select (-attr. ) %>%
     filter (elemid. >= hamlet_flatxml_init %>%             # extract only the dialogue
                              filter(elem. == 'ACT') %>%    #  section
                              select(elemid.) %>%
                              min()  ) %>%
     mutate (elem. = if_else(elem. == 'TITLE',                # `TITLE` appears for both
                     paste(lag(elem., 1), elem., sep = '_'),  #  `ACT` and `SCENE`, so
                     elem.) ) %>%                                 # change it
     filter (!is.na(value.))  %>%                      # remove empty lines
     mutate (rowid_act_ok = if_else(str_detect(value., '^ACT '), elemid., 0)) %>% # extract
     mutate(rowid_act_final = cumsum(rowid_act_ok)) %>%                           # the act rowid
     group_by(rowid_act_final) %>%
     mutate (rowid_scene_ok = if_else(str_detect(value., '^SCENE '),    # extract
                    elemid., 0)) %>%                                    # the scene rowid
     mutate(rowid_scene_final = cumsum(rowid_scene_ok)) %>%                           
     ungroup() %>%
     group_by(rowid_act_final, rowid_scene_final) %>%
     mutate (rowid_stagedir_ok = if_else(elem. == 'STAGEDIR',    # extract
                    elemid., 0)) %>%                             # the stagedir rowid
     mutate(rowid_stagedir_final = cumsum(rowid_stagedir_ok)) %>%                           
     ungroup() %>%
     group_by(rowid_act_final, rowid_scene_final, rowid_stagedir_final) %>%
     mutate (rowid_speaker_ok = if_else(elem. == 'SPEAKER',    # extract
                    elemid., 0)) %>%                             # the speaker rowid
     mutate(rowid_speaker_final = cumsum(rowid_speaker_ok)) %>%                           
     ungroup()  %>%
     group_by(rowid_act_final, rowid_scene_final, rowid_stagedir_final, 
              rowid_speaker_final) %>%
     mutate(rowid_line_final = row_number() - 1) %>%
     ungroup() %>%
     select(rowid_act_final, rowid_scene_final, rowid_stagedir_final,
            rowid_speaker_final, rowid_line_final,
            attribute = elem., value = value.) %>%
     spread(attribute, value) %>%
     dplyr::select(ACT_TITLE, SCENE_TITLE, STAGEDIR, SPEAKER, LINE) %>%
     mutate (row_number = row_number())
     
View(hamlet_flatxml_final)



#########################################################################
###  III. JSON data management with `jsonlite` and tidyjson` packages ###                      #########################################################################

## see also:
## https://cran.r-project.org/web/views/WebTechnologies.html
## https://arxiv.org/pdf/1403.2805.pdf


#########################################################################
###                     III.1 Simple/rectangular data                 ###
#########################################################################


## with `jsonlite`
nobel_countries_url <- 'http://api.nobelprize.org/v1/country.json'
nobel_countries_jsonlite <- jsonlite::fromJSON(nobel_countries_url, 
          flatten = TRUE)
nobel_countries_jsonlite
nobel_countries_df_jsonlite <- dplyr::bind_rows(nobel_countries_jsonlite)
glimpse(nobel_countries_df_jsonlite)

## with `tidyjson`
nobel_countries_df_tydyjson <- paste(jsonlite::toJSON(
          jsonlite::fromJSON(nobel_countries_url)),
                                  collapse = ' ') %>%
     tidyjson::as.tbl_json() %>%
     tidyjson::gather_object() %>%
     tidyjson::gather_array() %>%
     tidyjson::spread_values(
          country_name = jstring("name"),
          country_code = jstring("code")
     )
glimpse(nobel_countries_df_tydyjson)



#########################################################################
###                III.2 Single-level nested data                     ###
#########################################################################

url <- 'http://api.nobelprize.org/v1/prize.json'

## with `jsonlite` - we'll use `unnest`
nobel_prizes_url <- 'http://api.nobelprize.org/v1/prize.json'
nobel_prizes_jsonlite <- jsonlite::fromJSON(nobel_prizes_url, 
               flatten = TRUE)
nobel_prizes_jsonlite
nobel_prizes_df_jsonlite <- dplyr::bind_rows(nobel_prizes_jsonlite, 
          .id = 'prizes') %>%
     unnest(laureates)

glimpse(nobel_prizes_df_jsonlite)


## with `tidyjson` - we'll use `enter_object` with additional `gather_array` 
##   and `spread_values`
nobel_prizes_df_tydyjson <- paste(jsonlite::toJSON(jsonlite::fromJSON(nobel_prizes_url)),
                                  collapse = ' ') %>%
     tidyjson::as.tbl_json() %>%
     tidyjson::gather_object() %>%
     tidyjson::gather_array() %>%
     tidyjson::spread_values(
          year = jstring("year"),
          category = jstring("category"),
          overall_motivation = jstring("overallMotivation")
          )   %>%        
     enter_object("laureates") %>%
     tidyjson::gather_array() %>%
     tidyjson::spread_values(
          laureate_id = jstring("id"),
          laureate_firstname = jstring("firstname"),
          laureate_lastname = jstring("surname"),
          laureate_motivation = jstring("motivation"),
          laureate_share = jstring("share")
          )   

glimpse(nobel_prizes_df_tydyjson)



#########################################################################
###                     III.3 Multi-level nested data                 ###            
#########################################################################


## with `jsonlite` - we'l use `unnest`
nobel_laureates_url <- 'http://api.nobelprize.org/v1/laureate.json'
nobel_laureates_jsonlite <- jsonlite::fromJSON(nobel_laureates_url, 
          flatten = TRUE)
nobel_laureates_jsonlite

nobel_laureates_df_jsonlite <- dplyr::bind_rows(nobel_prizes_jsonlite, 
                                                .id = 'laureates_data') %>%
     unnest(laureates)

View(nobel_laureates_df_jsonlite)


## with `tidyjson` - we'll multiple `enter_object`s  with additional `gather_array`s 
##   and `spread_values`s
nobel_laureates_df_tydyjson <- paste(jsonlite::toJSON(jsonlite::fromJSON(nobel_laureates_url)),
                                  collapse = ' ') %>%
     tidyjson::as.tbl_json() %>%
     tidyjson::gather_object() %>%
     tidyjson::gather_array() %>%
     tidyjson::spread_values(
          laureate_id = jstring("id"),
          laureate_firstname = jstring("firstname"),
          laureate_lastname = jstring("surname"),
          laureate_dob = jstring("born"),
          laureate_dod = jstring("died"),
          laureate_birth_country = jstring("bornCountry")
          )   %>%        
     enter_object("prizes") %>%
     tidyjson::gather_array() %>%
     tidyjson::spread_values(
          prize_year = jstring("year"),
          prize_category = jstring("category"),
          prize_motivation = jstring("motivation")
          )  %>%
     enter_object("affiliations") %>%
     tidyjson::gather_array() %>%
     tidyjson::spread_values(
          afiliation_name = jstring("name"),
          afiliation_city = jstring("city"),
          afiliation_country = jstring("country")
          )
View(nobel_laureates_df_tydyjson)          
          


