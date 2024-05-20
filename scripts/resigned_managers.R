library(rvest)
library(dplyr)
library(ggplot2)

## Get the historic premier league results for the last 6 years

epl_2023_24 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = "2023/24")

epl_2022_23 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = "2022/23")

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = "2021/22")

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = "2020/21")

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 5000 + row_number(),
         season_id = "2019/20")

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 6000 + row_number(),
         season_id = "2018/19")

all_data <- bind_rows(epl_2023_24,
                      epl_2022_23,
                      epl_2021_22, 
                      epl_2020_21, 
                      epl_2019_20,
                      epl_2018_19)

## Get the list of managers and when they left

url <- 'https://en.wikipedia.org/wiki/List_of_Premier_League_managers'

managers <- url %>%
  read_html %>%
  html_table() %>%
  .[[2]] 

## The club name doesn't match to the home/away team names very well so fix that

managers_formatted <- managers %>% 
  janitor::clean_names() %>% 
  mutate(until_formatted = as.Date(until, format = "%d %B %Y")) %>% 
  mutate(club_match = case_when(club == "Brighton & Hove Albion" ~ "Brighton",
                                club == "Cardiff City" ~ "Cardiff",
                                club == "Huddersfield Town" ~ "Huddersfield",
                                club == "Leeds United" ~ "Leeds",
                                club == "Leicester City" ~ "Leicester",
                                club == "Newcastle United" ~ "Newcastle",
                                club == "Manchester City" ~ "Man City",
                                club == "Manchester United" ~ "Man United",
                                club == "Nottingham Forest" ~ "Nott'm Forest",
                                club == "Norwich City" ~ "Norwich",
                                club == "Tottenham Hotspur" ~ "Tottenham",
                                club == "West Ham United" ~ "West Ham",
                                club == "West Bromwich Albion" ~ "West Brom",
                                club == "	Wolverhampton Wanderers" ~ "Wolves",
                                TRUE ~ club))

# managers_formatted %>% count(club) %>% arrange(club) %>% View()

## Join here
## Join on away team and filter with a week of the manager leaving

managers_formatted_join <- managers_formatted %>% 
  inner_join(all_data, by = c("club_match" = "away_team")) %>% 
  mutate(days_diff = date - until_formatted) %>% 
  filter(days_diff >= 0 & days_diff < 8) %>% 
  select(home_team, club_match, date, name, from, until, duration_days, b365h, b365d, b365a, ftr, match_id) %>% 
  arrange(match_id) %>% 
  group_by(match_id) %>% 
  mutate(rownum = row_number()) %>% 
  filter(rownum == 1) %>% 
  ungroup() %>% 
  arrange(date)

## ftr gives the result, so count this - away team wins about 19% of the time
managers_formatted_join %>% count(ftr) %>% mutate(perc = n/sum(n))

## To find out if this is a profitable betting strategy assume we would bet on the home team 
managers_formatted_join %>% 
  arrange(date) %>% 
  mutate(p_l = ifelse(ftr == "H", (b365h-1), -1),
         gamenum = row_number()) %>%
  mutate(c_pl = cumsum(p_l)) %>% 
  ggplot(aes(x = gamenum, y = c_pl)) +
    geom_line()

managers_formatted_join %>% 
  arrange(date) %>% 
  mutate(p_l = ifelse(ftr == "H", (b365h-1), -1),
         gamenum = row_number()) %>%
  summarise(outlay = n(),
            p_l_toal = sum(p_l),
            ROI = sum(p_l)/n())

