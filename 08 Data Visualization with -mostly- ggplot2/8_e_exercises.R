library (vcd)
library(tidyverse)
library(readxl)
library(lubridate)
options(scipen = 999)
setwd('/Users/nicolairomanov/Documents/projects/Data-Processing-Analysis-Science-with-R-master/DataSets')

#1
file <- "netflix_titles.csv"
netflix_titles <- read_csv(file, col_names = TRUE, skip = 0)
glimpse(netflix_titles)
table(netflix_titles$type)

#2
tv_shows <- netflix_titles %>%
  filter (
    type == 'TV Show' &
    release_year > 2000
  ) %>%
  mutate(
    release_year = factor(release_year)
  )
tv_shows_agg <- tv_shows %>%
  dplyr::group_by(release_year) %>%
  summarise(n_of_tv_shows = n()) %>%
  ungroup()
movies <- netflix_titles %>%
  filter (
    type == 'Movie' &
    release_year > 2000
  ) %>%
  mutate(
    release_year = factor(release_year)
  )
movies_agg <- movies %>%
  dplyr::group_by(release_year) %>%
  summarise(n_of_movies = n()) %>%
  ungroup()

#3
ggplot(
  data = tv_shows, 
  aes(x = release_year, fill = release_year)
) +
geom_bar()

#4
ggplot(
  data = movies_agg, 
  aes(x = release_year, y = n_of_movies, fill = release_year)
) +
geom_bar(stat="identity")

#5
ggplot(
  movies,
  aes (x = release_year, fill = release_year)
) +
  geom_bar() +
  coord_flip() + 
  theme(legend.position="none") + 
  xlab("Year") + ylab("Number of movies") +
  ggtitle("Number of Neflix movies since 2000, by year") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = seq(0, 800, 50)) 


#6 
ggplot(
  data = tv_shows %>%
    filter(!is.na(rating)) %>%
    mutate(rating = factor(rating)),
  aes(x = release_year, fill = rating)
) +
  geom_bar() +
  coord_flip() +
  xlab("Year") + ylab("Number of TV shows") +
  ggtitle("Structure of the TV shows") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  theme(legend.position="bottom")


#7
ggplot(
  data = tv_shows %>%
    filter(!is.na(rating)) %>%
    mutate(rating = factor(rating)),
  aes(x = release_year, fill = rating)
) +
  geom_bar(position=position_dodge()) +
  xlab("Year") + ylab("Number of TV shows") +
  ggtitle("Structure of the TV shows") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  theme(legend.position="bottom")


#8
ggplot(
  data = tv_shows %>%
    filter(!is.na(rating)) %>%
    mutate(
      rating = factor(rating)
    ),
  aes(x = release_year, fill = rating)
) +
  geom_bar(position = position_dodge()) +
  coord_flip() +
  theme_bw() +
  xlab("Year") + ylab("Number of TV shows") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )
  ) +
  ggtitle("Rating of tv shows\nBy Year") +
  facet_wrap( ~ rating) +
  theme(legend.position="none")


filtered_movies <- movies %>%
  filter(
    as.numeric(as.character(release_year)) > 2010 &
      rating %in% c("TV-14", "TV-MA", "TV-PG", "TV-Y", "TV-Y7") &
      !is.na(rating) &
      !is.na(listed_in)
  ) %>%
  mutate(
    rating = factor(rating),
    genre = factor(str_split(listed_in, ",") %>% sapply(`[`, 1))
  ) %>%
  filter(
    genre %in% c("Dramas", "Comedies", "Horror Movies", "Documentaries", "Thrillers")
  )

#9
ggplot(
  data = filtered_movies,
  aes(x = release_year, fill = rating)
) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  xlab("Year") + ylab("Number of movies") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )
  ) +
  ggtitle("Rating of movies\nBy Year") +
  facet_wrap( genre ~ rating, labeller = label_both) +
  theme(legend.position="none")

#10
ggplot(
  data = filtered_movies,
  aes(x = release_year, fill = rating)
) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  xlab("Year") + ylab("Number of Movies") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 )
  ) +
  ggtitle("Rating of movies\nBy Year") +
  facet_grid( genre ~ rating, labeller = label_both) +
  theme(legend.position="none")

#11
file <- "vgsales.csv"
video_game_sales <- read_csv(file, col_names = TRUE, skip = 0)
glimpse(video_game_sales)
table(video_game_sales$Platform)


#12
ggplot(
  video_game_sales %>% 
    filter (
      !is.na(NA_Sales) &
      Publisher == "Sony Computer Entertainment"
      ),
  aes(x = NA_Sales)
) +
  geom_histogram(color = "white") +
  ggtitle("Histogram of NA Sales for Sony Copmuter Entertainment")


#13
ggplot(
  video_game_sales %>% 
    filter (
      !is.na(EU_Sales) &
      Genre == "Fighting"
    ),
  aes(x = EU_Sales)
) +
  geom_histogram(
    fill="lightblue",
    binwidth = 0.2,
    alpha = .6 
  ) +
  geom_vline(
    aes (xintercept = mean(EU_Sales, na.rm=T)),
    color="red", 
    linetype="dashed", 
    size= .5
  ) +
  ggtitle("Histogram of EU Sales for Fighting genre")


#14
ggplot(
  video_game_sales %>% 
    filter (
      !is.na(Global_Sales) &
      Publisher == "Electronic Arts"
    ),
  aes(x = Global_Sales)
) +
  geom_histogram(fill="coral", colour="white", aes(y = after_stat(density)), alpha = .5) +
  geom_density(alpha = .5) +
  geom_vline(
    aes (xintercept = mean(Global_Sales, na.rm=T)),
    color="red",
    linetype="dashed",
    size=.5
  )  +
  ggtitle("Superimposed Histogram and Density Curve of sales") +
  xlab("sales in millions")


#15 
ggplot(
  video_game_sales %>% 
    filter (
      !is.na(NA_Sales) &
      !is.na(EU_Sales) &
      Publisher == "Activision"
    ),
  aes(x = NA_Sales)
) +    # global `aes`; used for first histogram
  geom_histogram(col = "red", fill = "red", alpha = 0.4) +
  geom_histogram(
    aes(x = EU_Sales), 
    col = "yellow", # the second histogram needs
    fill = "yellow",
    alpha = 0.4
  ) +      #  its own `aes`
  xlab("sales in millions") +
  ggtitle("Superimosed Histograms of NA Sales (red) and EU Sales (yellow) \nVideo Game Sales")

#16
eu_na_long <- video_game_sales %>%
  filter (
    !is.na(EU_Sales) & 
    !is.na(NA_Sales) &
    Publisher == "Activision"
  ) %>%
  mutate (row_num = row_number()) %>%
  select (row_num, NA_Sales, EU_Sales) %>%
  gather (parameter, value, -row_num)

ggplot(
  eu_na_long,
  aes(x = value, fill = parameter, col = parameter)
) +
  geom_histogram(alpha = .4) +
  xlab("sales in millions") +
  ggtitle("Superimosed Histograms of NA Sales and EU Sales \nActivison Video Game Sales")


#17
ggplot(
  eu_na_long,
  aes(x = value, fill = parameter, col = parameter)
) +
geom_histogram( alpha = 0.5) +
  xlab("sales in millions") +
  ggtitle("Superimosed Histograms of NA Sales and EU Sales \nActivison Video Game Sales") +
  facet_wrap(~ parameter) +
  theme(legend.position = "none")

#18
ggplot(
  video_game_sales %>% 
    filter (
      !is.na(Other_Sales) &
      !is.na(EU_Sales) &
      Genre == "Racing"
    ),
  aes(x = EU_Sales)
) +    # global `aes`; used for first histogram
  geom_density(col = "red", fill = "red", alpha = 0.4) +
  geom_density(
    aes(x = Other_Sales), 
    col = "yellow", # the second histogram needs
    fill = "yellow",
    alpha = 0.4
  ) +      #  its own `aes`
  xlab("sales in millions") +
  ggtitle("Superimosed Histograms of Other Sales (red) and EU Sales (yellow) \nVideo Game Sales")

#19
eu_other_long <- video_game_sales %>%
  filter (
    !is.na(Other_Sales) &
      !is.na(EU_Sales) &
      Genre == "Racing"
  ) %>%
  mutate (row_num = row_number()) %>%
  select (row_num, EU_Sales, Other_Sales) %>%
  gather (parameter, value, -row_num)

ggplot(
  eu_other_long,
  aes(x = value, fill = parameter, col = parameter)
) +
  geom_histogram(alpha = .4) +
  xlab("sales in millions") +
  ggtitle("Superimosed Histograms of EU Sales and Other Sales \nRacing Genre Video Game Sales")

#20
ggplot(
  eu_other_long,
  aes(x = value, fill = parameter, col = parameter)
) +
  geom_histogram( alpha = 0.5) +
  xlab("sales in millions") +
  ggtitle("Superimosed Histograms of Eu Sales and Other Sales \nRacing Genre Video Game Sales") +
  facet_wrap(~ parameter) +
  theme(legend.position = "none")

#21
ggplot(
  video_game_sales %>% 
    filter (!is.na(Global_Sales) & Genre == "Shooter" & Platform == "PS4"),
  aes(x = 0, y = Global_Sales)
) +
  geom_boxplot() +
  ggtitle("Boxplot of Global Sales") +
  ylab("sales in millions") + xlab("") +
  theme(axis.text.x = element_blank()) 


#22
ggplot(
  video_game_sales %>% 
    filter (
      !is.na(NA_Sales) &
      !is.na(EU_Sales) &
      !is.na(Other_Sales)
    ),
  aes(x = 'NA_Sales', y = NA_Sales)
) +  # global `aes`; used for first boxplot
  geom_boxplot() + # this box takes the settings from the global `aes`
  geom_boxplot(aes(x = 'EU_Sales', y = EU_Sales)) +  #  own `aes`
  geom_boxplot(aes(x = 'Other Sales', y = Other_Sales)) + #  own `aes`
  ylab("sales in millions") + xlab("region") +
  ggtitle("NA, EU, and Other region Sales")

#23
library(corrplot)

video_game_sales %>%
  select (NA_Sales:Global_Sales) %>%
  filter(complete.cases(.)) %>% # remove all observations with NA values
  cor(.) %>%
  corrplot.mixed(., number.cex=0.75, tl.cex=0.6 )

#24
video_game_sales %>%
  select (NA_Sales:Global_Sales) %>%
  filter(complete.cases(.)) %>% # remove all observations with NA values
  cor(.) %>%
  corrplot(., method = 'number', type = 'lower', number.cex=0.75, tl.cex=0.6)

