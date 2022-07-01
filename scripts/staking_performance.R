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

## how do the different staking methods compare

tracker_all <- read_csv("scripts/tracker.csv") %>%
  clean_names()  %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"))

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2022) %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0)) %>%
  mutate(corner_diff = hc-ac) %>%
  select(home_team, away_team, date, hc, ac, home_corner_winner, corner_diff)

merged <- tracker_all %>%
  inner_join(epl_2021_22, by = c("home_team", "away_team", "date")) %>%
  filter(!is.na(bf_h)) %>%
  filter(bet != "No bet") %>%
  filter(bet != "No Bet") %>%
  mutate(perc_bfh = 1/bf_h,
         perc_bfa = 1/bf_a)

nrow(tracker_all)
nrow(merged)

kelly <- function(b,p){
  a <- ((((b-1)*p) - (1-p))/(b-1))
  
  a[a<0] <-0
  
  return(a)
}

merged <- merged %>%
  mutate(kelly_ratio = case_when(bet == "H" ~ kelly(b = bf_h, p = prob_home_winner),
                                 bet == "A" ~ kelly(b = bf_a, p = prob_away_win))
         ) %>%
  mutate(win_amt_kelly = case_when(bet == "H" & winner == "N" ~ kelly_ratio*-1,
                             bet == "A" & winner == "N" ~ kelly_ratio*-1,
                             bet == "H" & winner == "Y" ~ kelly_ratio*(bf_h-1),
                             bet == "A" & winner == "Y" ~ kelly_ratio*(bf_a-1))
         ) %>%
  mutate(win_amt_fixed = case_when(bet == "H" & winner == "N" ~ -0.3,
                                   bet == "A" & winner == "N" ~ -0.3,
                                   bet == "H" & winner == "Y" ~ 0.3*(bf_h-1),
                                   bet == "A" & winner == "Y" ~ 0.3*(bf_a-1))
         ) %>%
  mutate(kellp_pnl = cumsum(win_amt_kelly),
         fixed_pnl = cumsum(win_amt_fixed)) %>%
  mutate(betid = row_number())

ggplot(merged) +
  geom_line(aes(x = betid, y = kellp_pnl), size = 2, colour = "dodgerblue") +
  geom_line(aes(x = betid, y = fixed_pnl), size = 2, colour = "red")


