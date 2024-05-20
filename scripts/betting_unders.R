library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(janitor)
library(ggplot2)
library(randomForest)
library(caret)
library(gbm)
library(pROC)
library(measures)
library(readxl)
library(rvest)

epl_2023_24 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y")) %>% 
  mutate(match_id = 0 + row_number(),
         season_id = 2024) %>% 
  rename(bet365_o_25 = b365_2_5,
         bet365_u_25 = b365_2_5_2)

epl_2022_23 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2023) %>% 
  rename(bet365_o_25 = b365_2_5,
         bet365_u_25 = b365_2_5_2)

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2022) %>% 
  rename(bet365_o_25 = b365_2_5,
         bet365_u_25 = b365_2_5_2)

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2021) %>% 
  rename(bet365_o_25 = b365_2_5,
         bet365_u_25 = b365_2_5_2)

all <- rbind(epl_2023_24, epl_2022_23, epl_2021_22, epl_2020_21)

months_grp <- all %>% 
  arrange(date) %>%
  mutate(month_num = lubridate::month(date)) %>% 
  mutate(o25 = ifelse(fthg + ftag < 2.5, 1, 0)) %>% 
  mutate(stake = 1,
         profit_loss = ifelse(o25 == 1, bet365_u_25 - 1, -1),
         year = lubridate::year(date)) %>% 
  arrange(month_num, date) %>% 
  group_by(year, month_num) %>% 
  mutate(cumulative_pl = cumsum(profit_loss),
         rownum = row_number())
  

ggplot(months_grp, aes(x = rownum, y = cumulative_pl, colour = factor(year))) + 
    geom_line() +
    facet_wrap( ~ month_num) +
  geom_hline(yintercept = 0)
