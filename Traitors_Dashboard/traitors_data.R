## Scrape Traitors Data

suppressPackageStartupMessages({
    library(rvest)
    library(httr)
    library(tidyverse)
    library(tidylog) # Debug only
    library(xml2)
})

# taken from https://www.kaggle.com/datasets/max-mind/world-cities-database
lat_longs <- 
    read_csv("~/Martin Ingram R Work/Cities_Long_Lat/GB_Lat_Long.csv") %>% 
    select(City, Latitude, Longitude) %>% 
    add_row(City = 'cork', 
            Latitude = 51.8985,
            Longitude = -8.4756) %>% 
    add_row(City = 'bury st edmunds',
            Latitude = 52.2463,
            Longitude = 0.7111) %>% 
    add_row(City = 'swanley',
            Latitude = 51.3965, 
            Longitude = 0.1773) %>% 
    add_row(City = 'brussels',
            Latitude = 50.8476,
            Longitude = 4.3572)

# Wiki Pages
traitors_s1_page <- "https://en.wikipedia.org/wiki/The_Traitors_(British_series_1)"

traitors_s2_page <- "https://en.wikipedia.org/wiki/The_Traitors_(British_series_2)"

# Scrape Data -------------------------------------------------------------

traitors_tables <- 
    read_html(GET(traitors_s1_page,
                           timeout(10))) %>%  # read the page
    html_table(header = TRUE) %>%  # pull tables
    c(
    read_html(GET(traitors_s2_page,
                  timeout(10))) %>%
    html_table(header = TRUE))

# Combine 2 seasons
names(traitors_tables)[1:6] <- paste0('S1 - ', 1:6)
names(traitors_tables)[7:13] <- paste0('S2 - ', 1:7)

# Process Season 1 data
s1_contestant_data <- traitors_tables[['S1 - 2']] %>% 
    add_row(Contestant = 'Aisha Birley', Age = '23',
            From = 'Manchester,England', 
            Occupation = 'Masters Graduate',
            Affiliation = 'Faithful',
            Finish = 'Murdered(Episode 2)') %>% 
    mutate(Finish = case_when(
        .default = Finish, 
        row_number() < 9 ~ Affiliation)) %>% 
    mutate(Affiliation = case_when(
        .default = Affiliation, 
        row_number() < 9 ~ `...11`)) %>% 
    select(Contestant, Age, From, Occupation, Affiliation, Finish) %>% 
    filter(Contestant != 'Contestant') %>% 
    mutate(Finish = str_remove_all(Finish, 
                                   pattern = '\\[[:digit:]\\]')) %>% 
    mutate(Last_Episode = str_extract_all(Finish, 
                                          pattern = ' .*')) %>% 
    mutate(Last_Episode = as.numeric(str_remove_all(Last_Episode, 
                                          pattern = '\\)'))) %>% 
    mutate(Final_Status = str_remove_all(Finish, 
                                          pattern = '\\(.*')) %>% 
    mutate(City = str_to_lower(str_squish(
        sub(From, pattern = ',.*', replacement = ""))),
        .after = 'From') %>% 
    mutate(Country = str_squish(
               sub(From, pattern = '.*,', replacement = "")), .after = 'City') %>% 
    mutate(City = 
               str_remove_all(City,
                    pattern = 'north |south |west |east ')) %>% 
    left_join(lat_longs, by = 'City') %>% 
    select(Name = Contestant, Age, Occupation,
           City, Country, Latitude, Longitude, 
           Affiliation, Final_Status, Last_Episode) %>% 
    mutate(Affiliation = case_when( # remove duplicates
        .default = Affiliation, 
        Name == "Kieran Tompsett" ~ 'Faithful-Traitor')) %>% 
    distinct() %>% 
    filter(Latitude != '53.733333') %>% 
    filter(Latitude != '55.833333')

Player_Status <- s1_contestant_data %>% 
    select(Name, Affiliation, Last_Episode) %>% 
    ungroup() %>% 
    mutate(Name = sub(Name, pattern = ' .*', replacement = '')) %>% 
    mutate(Name = recode(Name, 'Madelyn' = 'Maddy'))

s1_episode_data <-  traitors_tables[['S1 - 4']] %>% 
    janitor::clean_names() %>% 
    select(-c('x', 'episode', 'episode_2'), Name = episode_3) %>% 
    filter(row_number() > 6) %>% 
    rename('x13' = x12_5)

names(s1_episode_data) <- str_replace_all(names(s1_episode_data), 
                                          pattern = 'x', 
                                          replacement = 'Episode: ')

s1_episode_data <- s1_episode_data %>% 
    tidyr::pivot_longer(cols = -Name, names_to = 'Episode', values_to = 'Status') %>% 
    mutate(coded_status = case_when(
        .default = 1, 
        grepl(x = Status, pattern = 'Murdered|Banished|Eliminated') ~ 0
    )) %>% 
    group_by(Episode) %>% 
    mutate(survivor_count = sum(coded_status)) %>% 
    mutate(Episode = str_remove_all(Episode, pattern = "Episode: ")) %>% 
    mutate(Episode = str_replace_all(Episode,
                        pattern = '_', replacement = ".")) %>% 
    mutate(Episode = as.numeric(Episode))


s1_episode_survivor_counts <- s1_episode_data %>% 
    select(Episode, survivor_count) %>% 
    distinct()

# Need to add in last episode data to get counts correct

traitor_counts <- s1_episode_data %>% 
    select(Name, Episode) %>% 
    left_join(Player_Status) %>% 
    mutate(Affiliation = case_when(
        .default = Affiliation, 
        Affiliation == 'Faithful-Traitor' & 
            grepl(Episode, pattern = '10|11|12|End') ~ 'Traitor', 
        Affiliation == 'Faithful-Traitor' & 
            !grepl(Episode, pattern = '10|11|12|End') ~ 'Faithful')) %>% 
    group_by(Episode) %>% 
    mutate(Traitor_Count = sum(str_count(Affiliation, 'Traitor')))

# Map ---------------------------------------------------------------------

# https://rstudio.github.io/leaflet/

map_data <- s1_contestant_data %>% 
    select(City, Country, lng = Longitude, lat = Latitude) %>% 
    group_by(across(everything())) %>% 
    mutate(n = n()) %>% 
    distinct() %>% 
    mutate(size = n * 5) %>% 
    mutate(Country = factor(Country,
                            levels = c('England', 
                                       'Scotland', 
                                       'Belgium', 
                                       'Republic of Ireland', 
                                       'Wales'))) %>% 
    arrange(Country)


library(leaflet)

map_palette <- colorFactor(c('darkorange',  "red", 
                             'green', "navy", 'seagreen'),
                           domain = c('England', 
                                      'Scotland', 
                                      'Belgium', 
                                      'Republic of Ireland', 
                                      'Wales'))

leaflet(map_data) %>%
    addTiles() %>%
    addCircleMarkers(
        radius = ~size, 
        popup = ~htmltools::htmlEscape(City),
        fillColor = ~map_palette(Country), 
        stroke = FALSE, fillOpacity = 0.5) %>% 
    setView(lng = -3, lat = 54.5, zoom = 4)

