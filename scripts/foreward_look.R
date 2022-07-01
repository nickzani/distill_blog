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

# readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%  clean_names() %>% group_by(home_team) %>% count()
# readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%  clean_names() %>% mutate(date = as.Date(date, format="%d/%m/%Y")) %>% summarise(maxdt = max(date))

future_games <- data.frame(home_team = c("Arsenal", "Brentford", "Brighton", "Burnley", "Chelsea", "Crystal Palace", "Leicester", "Liverpool", "Man City", "Norwich"),
                           away_team = c("Everton", "Leeds", "West Ham", "Newcastle", "Watford", "Man United", "Southampton", "Wolves", "Aston Villa", "Tottenham"),
                           date = rep("22/05/2022", 10))

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  bind_rows(future_games) %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2022)

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = 2021)

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = 2020)

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = 2019)

all_data <- bind_rows(epl_2021_22, epl_2020_21, epl_2019_20, epl_2018_19) %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0))

# split up and then stick back together

home_data <- all_data %>%
  dplyr::select(date, match_id, season_id, home_team, ftr, fthg, hc, hs, hst, ac) %>%
  mutate(team_type = "Home",
         win_flag = case_when(ftr == "H" ~ 1, TRUE ~ 0),
         draw_flag = case_when(ftr == "D" ~ 1, TRUE ~ 0)) %>%
  rename(team = home_team,
         full_time_goals = fthg,
         corners = hc,
         shots = hs,
         shots_target = hst,
         corners_conceded = ac)

away_data <- all_data %>%
  dplyr::select(date, match_id, season_id, away_team, ftr, ftag, ac, as, ast, hc) %>%
  mutate(team_type = "Away",
         win_flag = case_when(ftr == "A" ~ 1, TRUE ~ 0),
         draw_flag = case_when(ftr == "D" ~ 1, TRUE ~ 0)) %>%
  rename(team = away_team,
         full_time_goals = ftag,
         corners = ac,
         shots = as,
         shots_target = ast,
         corners_conceded = hc)

long_data = bind_rows(home_data, away_data)

## add features

hc_model_data2 <- home_data %>%
  arrange(team, date) %>%
  group_by(season_id, team) %>%
  mutate(lag_1_home_corner = lag(corners, n = 1),
         lag_2_home_corner = lag(corners, n = 2),
         lag_3_home_corner = lag(corners, n = 3),
         lag_1_home_shots = lag(shots, n = 1),
         lag_2_home_shots = lag(shots, n = 2),
         lag_3_home_shots = lag(shots, n = 3),
         lag_1_home_goals = lag(full_time_goals, n = 1),
         lag_2_home_goals = lag(full_time_goals, n = 2),
         lag_3_home_goals = lag(full_time_goals, n = 3),
         lag_1_home_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_home_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_home_corners_conceded = lag(corners_conceded, n = 3)) %>%
  ungroup() %>%
  filter(!is.na(lag_3_home_corner)) %>%
  select(match_id, starts_with("lag_"))

ac_model_data2 <- away_data %>%
  arrange(team, date) %>%
  group_by(season_id, team) %>%
  mutate(lag_1_away_corner = lag(corners, n = 1),
         lag_2_away_corner = lag(corners, n = 2),
         lag_3_away_corner = lag(corners, n = 3),
         lag_1_away_shots = lag(shots, n = 1),
         lag_2_away_shots = lag(shots, n = 2),
         lag_3_away_shots = lag(shots, n = 3),
         lag_1_away_goals = lag(full_time_goals, n = 1),
         lag_2_away_goals = lag(full_time_goals, n = 2),
         lag_3_away_goals = lag(full_time_goals, n = 3),
         lag_1_away_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_away_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_away_corners_conceded = lag(corners_conceded, n = 3)) %>%
  ungroup() %>%
  filter(!is.na(lag_3_away_corner)) %>%
  select(match_id, starts_with("lag_"))

# merge back together

model_dat <- all_data %>%
  select(match_id, home_corner_winner) %>%
  inner_join(hc_model_data2, by = c("match_id")) %>%
  inner_join(ac_model_data2, by = c("match_id"))

# get pred file

to_predict <- all_data %>%
  filter(is.na(div)) %>%
  select(home_team, away_team, date, match_id) %>%
  inner_join(model_dat)

# predict

modelFitWinner <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model.RDS")

predicted_winner <- as.data.frame(predict(modelFitWinner, newdata = to_predict, "prob"))

to_predict$prob_home_winner <- predicted_winner$`1`

to_predict <- to_predict %>%
  select(home_team, away_team, date, prob_home_winner) %>%
  mutate(prob_away_win = 1- 0.08 - prob_home_winner,
         odds_home_win = 1+((1-prob_home_winner)/prob_home_winner),
         odds_away_win = 1+((1-prob_away_win)/prob_away_win),
         home_odds_plus_10perc = odds_home_win * 1.1,
         away_odds_plus_10perc = odds_away_win * 1.1)

write.csv(to_predict, file = "scripts/corneroutput.csv", row.names = FALSE)

## read in the tracker

tracker <- read_csv("scripts/tracker.csv") %>% clean_names()

tracker <- tracker %>%
  filter(bet %in% c("H", "A")) %>%
  filter(!is.na(winner)) %>%
  mutate(p_and_l = case_when(winner == "N" ~ bet_amt * -1,
                             winner == "Y" ~ bet_amt * (odds -1))) %>%
  mutate(betid = row_number()) %>%
  mutate(fees = case_when(odds_from == "BFE" ~ p_and_l * 0.05,
                          TRUE ~ 0)) %>%
  mutate(adjusted_pl = p_and_l - fees) %>%
  mutate(prob_used = case_when(bet == "H" ~ prob_home_winner,
                               bet == "A" ~ prob_away_win))%>%
  mutate(expected_value = (bet_amt * (odds - 1) * prob_used) - (bet_amt * (1 - prob_used))) %>%
  mutate(cumulative_pl = cumsum(adjusted_pl),
         cumulative_ev = cumsum(expected_value))

tail(tracker %>% select(betid, cumulative_pl, cumulative_ev), 1)

ggplot(tracker) +
  geom_line(aes(x = betid, y = cumulative_pl), size = 2, colour = "dodgerblue") +
  geom_line(aes(x = betid, y = cumulative_ev), size = 2, colour = "black", linetype = "longdash") +
  theme_classic()

sum(tracker$p_and_l)/sum(tracker$bet_amt)

ggplot(tracker, aes(x = betid, y = cumulative_pl)) +
  geom_line(size = 2, colour = "dodgerblue") +
  stat_smooth(method = "lm", colour = "black", linetype = "longdash") +
  labs(x = "Bet Number",
       y = "Total Profit/Loss")

summary(lm(cumulative_pl ~ 0 + betid, data = tracker))
summary(lm(cumulative_pl ~ 0 + cumulative_ev, data = tracker))

cor(tracker$adjusted_pl, tracker$expected_value)
cor(tracker$cumulative_pl, tracker$cumulative_ev)

## profit by source

tracker %>% group_by(odds_from) %>% summarise(pl = sum(adjusted_pl))
  
## compare to actuals

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

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
  inner_join(epl_2021_22, by = c("home_team", "away_team", "date"))

prob_roll_up <- merged %>%
  mutate(rounded_prob_home = round_any(prob_home_winner, 0.05)) %>%
  group_by(rounded_prob_home) %>%
  summarise(cnt = n(),
            winners = sum(home_corner_winner),
            perc_correct = winners/cnt)

ggplot(prob_roll_up) +
  geom_point(aes(x = rounded_prob_home, y = perc_correct)) +
  geom_abline(intercept=0, slope=1)

##################################
# brier score
##################################

Brier(merged$prob_home_winner, merged$home_corner_winner, negative = 0, positive = 1)

##################################

## how does winning margin compare

corner_diff_rollup <- merged %>%
  mutate(rounded_prob_home = round_any(prob_home_winner, 0.05))

ggplot(corner_diff_rollup) +
  geom_boxplot(aes(x = rounded_prob_home, y = corner_diff, group = rounded_prob_home)) 

## how do BF compare

odds_roll_up <- merged %>%
  filter(!is.na(bf_h)) %>%
  mutate(perc_bfh = 1/bf_h) %>%
  mutate(rounded_prob_bfh = round_any(perc_bfh, 0.05)) %>%
  group_by(rounded_prob_bfh) %>%
  summarise(cnt = n(),
            winners = sum(home_corner_winner),
            perc_correct = winners/cnt)

ggplot(odds_roll_up) +
  geom_point(aes(x = rounded_prob_bfh, y = perc_correct)) +
  geom_abline(intercept=0, slope=1)

## how do BF and NZ compare

bf_nz <- merged %>%
  filter(!is.na(bf_h)) %>%
  mutate(perc_bfh = 1/bf_h) %>%
  mutate(perc_bfa = 1/bf_a)

ggplot(bf_nz) +
  geom_point(aes(x = prob_home_winner, y = perc_bfh), colour = "red") +
  geom_point(aes(x = prob_away_win, y = perc_bfa), colour = "blue") +
  geom_abline(intercept=0, slope=1)

## when the model thinks the favourite is the opposite to BF

reverse_faves <- merged %>%
  filter(!is.na(bf_h)) %>%
  mutate(perc_bfh = 1/bf_h) %>%
  filter((prob_home_winner > 0.6 & perc_bfh < 0.5 | prob_home_winner < 0.4 & perc_bfh > 0.5)) %>%
  summarise(cnt = n(),
            winners = sum(home_corner_winner),
            perc_correct = winners/cnt)