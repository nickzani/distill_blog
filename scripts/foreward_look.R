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

# readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%  clean_names() %>% group_by(home_team) %>% count()
# readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%  clean_names() %>% mutate(date = as.Date(date, format="%d/%m/%Y")) %>% summarise(maxdt = max(date))

# "Man City", "Nott'm Forest", "Wolves", "Leeds", "Everton", "Chelsea", "West Ham", "Aston Villa", "Southampton", "Tottenham"
# "Fulham", "Brentford", "Brighton", "Bournemouth", "Leicester", "Arsenal", "Crystal Palace", "Man United", "Newcastle", "Liverpool"

future_games <- data.frame(home_team = c("Man City", "Nott'm Forest", "Liverpool", "Tottenham", "Bournemouth", "West Ham", "Newcastle", "Wolves", "Brighton", "Fulham"),
                           away_team = c("Brentford", "Crystal Palace", "Southampton", "Leeds", "Everton", "Leicester", "Chelsea", "Arsenal", "Aston Villa", "Man United"),
                           date = c(rep("12/11/2022", 8), rep("13/11/2022", 2))
                           )

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  bind_rows(future_games) %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2022)

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = 2021)

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = 2020)

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = 2019)

all_data <- bind_rows(epl_2021_22, epl_2020_21, epl_2019_20, epl_2018_19) %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0))

##################################
# download the xg here
##################################

xg_dat <- readr::read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv') %>%
  clean_names() %>%
  filter(league == "Barclays Premier League")

xg_lkp <- readr::read_csv('C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/xg_team_lkp.csv') %>%
  clean_names()

xg_dat_teams <- xg_dat %>%
  inner_join(xg_lkp, by = c("team1" = "xg_team")) %>%
  rename(home_team = fd_team) %>%
  inner_join(xg_lkp, by = c("team2" = "xg_team")) %>%
  rename(away_team = fd_team) %>%
  select(home_team, away_team, date, spi1, spi2, xg1, xg2, nsxg1, nsxg2, importance1, importance2)

nrow(all_data)

all_data <- all_data %>%
  inner_join(xg_dat_teams, by = c("home_team" = "home_team",
                                  "away_team" = "away_team",
                                  "date" = "date"))

nrow(all_data)

###############################################
# split up and then stick back together
###############################################

home_data <- all_data %>%
  dplyr::select(date, match_id, season_id, home_team, ftr, fthg, ftag, hc, hs, as, hst, ac, hy, spi1, spi2, xg1, xg2, nsxg1, nsxg2, importance1, importance2) %>%
  mutate(team_type = "Home",
         win_flag = case_when(ftr == "H" ~ 1, TRUE ~ 0),
         draw_flag = case_when(ftr == "D" ~ 1, TRUE ~ 0)) %>%
  rename(team = home_team,
         full_time_goals = fthg,
         goals_conceded = ftag,
         corners = hc,
         shots = hs,
         shots_conceded = as,
         shots_target = hst,
         corners_conceded = ac,
         spi = spi1,
         spi_away = spi2,
         xg = xg1,
         xg_conceded = xg2,
         nsxg = nsxg1,
         nsxg_conceded = nsxg2,
         importance = importance1)

away_data <- all_data %>%
  dplyr::select(date, match_id, season_id, away_team, ftr, ftag, fthg, ac, as, hs, ast, hc, ay, spi1, spi2, xg1, xg2, nsxg1, nsxg2, importance1, importance2) %>%
  mutate(team_type = "Away",
         win_flag = case_when(ftr == "A" ~ 1, TRUE ~ 0),
         draw_flag = case_when(ftr == "D" ~ 1, TRUE ~ 0)) %>%
  rename(team = away_team,
         full_time_goals = ftag,
         goals_conceded = fthg,
         corners = ac,
         shots = as,
         shots_conceded = hs,
         shots_target = ast,
         corners_conceded = hc,
         spi = spi2,
         spi_away = spi1,
         xg = xg2,
         xg_conceded = xg1,
         nsxg = nsxg2,
         nsxg_conceded = nsxg1,
         importance = importance2
  )

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
         lag_1_home_yc = lag(hy, n = 1),
         lag_2_home_yc = lag(hy, n = 2),
         lag_3_home_yc = lag(hy, n = 3),
         lag_1_home_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_home_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_home_corners_conceded = lag(corners_conceded, n = 3),
         lag_1_home_shots_conceded = lag(shots_conceded, n = 1),
         lag_2_home_shots_conceded = lag(shots_conceded, n = 2),
         lag_3_home_shots_conceded = lag(shots_conceded, n = 3),
         lag_1_home_goals_conceded = lag(goals_conceded, n = 1),
         lag_2_home_goals_conceded = lag(goals_conceded, n = 2),
         lag_3_home_goals_conceded = lag(goals_conceded, n = 3),
         lag_1_home_spi = lag(spi, n = 1),
         lag_2_home_spi = lag(spi, n = 2),
         lag_3_home_spi = lag(spi, n = 3),
         lag_1_home_against_spi = lag(spi_away, n = 1),
         lag_2_home_against_spi = lag(spi_away, n = 2),
         lag_3_home_against_spi = lag(spi_away, n = 3),
         lag_1_home_xg = lag(xg, n = 1),
         lag_2_home_xg = lag(xg, n = 2),
         lag_3_home_xg = lag(xg, n = 3),
         lag_1_home_xg_conceded = lag(xg_conceded),
         lag_2_home_xg_conceded = lag(xg_conceded, n = 2),
         lag_3_home_xg_conceded = lag(xg_conceded, n = 3),
         lag_1_home_nsxg = lag(nsxg, n = 1),
         lag_2_home_nsxg = lag(nsxg, n = 2),
         lag_3_home_nsxg = lag(nsxg, n = 3),
         lag_1_home_importance = lag(importance, n = 1),
         lag_2_home_importance = lag(importance, n = 2),
         lag_3_home_importance = lag(importance, n = 3),
         lag_1_home_nsxg_conceded = lag(nsxg, n = 1),
         lag_2_home_nsxg_conceded = lag(nsxg, n = 2),
         lag_3_home_nsxg_conceded = lag(nsxg, n = 3),
         current_home_importance = importance,
         current_home_spi = spi) %>%
  ungroup() %>%
  filter(!is.na(lag_3_home_corner)) %>%
  select(match_id, starts_with("lag_"), contains("current"))

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
         lag_1_away_yc = lag(ay, n = 1),
         lag_2_away_yc = lag(ay, n = 2),
         lag_3_away_yc = lag(ay, n = 3),
         lag_1_away_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_away_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_away_corners_conceded = lag(corners_conceded, n = 3),
         lag_1_away_shots_conceded = lag(shots_conceded, n = 1),
         lag_2_away_shots_conceded = lag(shots_conceded, n = 2),
         lag_3_away_shots_conceded = lag(shots_conceded, n = 3),
         lag_1_away_goals_conceded = lag(goals_conceded, n = 1),
         lag_2_away_goals_conceded = lag(goals_conceded, n = 2),
         lag_3_away_goals_conceded = lag(goals_conceded, n = 3),
         lag_1_away_spi = lag(spi, n = 1),
         lag_2_away_spi = lag(spi, n = 2),
         lag_3_away_spi = lag(spi, n = 3),
         lag_1_away_against_spi = lag(spi_away, n = 1),
         lag_2_away_against_spi = lag(spi_away, n = 2),
         lag_3_away_against_spi = lag(spi_away, n = 3),
         lag_1_away_xg = lag(xg, n = 1),
         lag_2_away_xg = lag(xg, n = 2),
         lag_3_away_xg = lag(xg, n = 3),
         lag_1_away_xg_conceded = lag(xg_conceded),
         lag_2_away_xg_conceded = lag(xg_conceded, n = 2),
         lag_3_away_xg_conceded = lag(xg_conceded, n = 3),
         lag_1_away_nsxg = lag(nsxg, n = 1),
         lag_2_away_nsxg = lag(nsxg, n = 2),
         lag_3_away_nsxg = lag(nsxg, n = 3),
         lag_1_away_importance = lag(importance, n = 1),
         lag_2_away_importance = lag(importance, n = 2),
         lag_3_away_importance = lag(importance, n = 3),
         lag_1_away_nsxg_conceded = lag(nsxg_conceded),
         lag_2_away_nsxg_conceded = lag(nsxg_conceded, n = 2),
         lag_3_away_nsxg_conceded = lag(nsxg_conceded, n = 3),
         current_away_importance = importance,
         current_away_spi = spi) %>%
  ungroup() %>%
  filter(!is.na(lag_3_away_corner)) %>%
  select(match_id, starts_with("lag_"), contains("current"))

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

modelFitWinner    <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model.RDS")
modelFitWinnerXG  <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_xg.RDS")
modelFitHC        <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc.RDS")
modelFitAC        <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac.RDS")

predicted_winner <- as.data.frame(predict(modelFitWinner, newdata = to_predict, "prob"))
predicted_winner_xg <- as.data.frame(predict(modelFitWinnerXG, newdata = to_predict, "prob"))

to_predict$prob_home_winner <- predicted_winner$`1`
to_predict$prob_home_winner_xg <- predicted_winner_xg$`1`

to_predict$pred_hc <- predict(modelFitHC, newdata = to_predict)
to_predict$pred_ac <- predict(modelFitAC, newdata = to_predict)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

to_predict <- to_predict %>%
  select(home_team, away_team, date, prob_home_winner, prob_home_winner_xg, pred_hc, pred_ac) %>%
  mutate(prob_away_win = 1- 0.08 - prob_home_winner,
         prob_away_win_xg = 1- 0.08 - prob_home_winner_xg,
         odds_home_win_xg = round_any(1+((1-prob_home_winner_xg)/prob_home_winner_xg), 0.01),
         odds_away_win_xg = round_any(1+((1-prob_away_win_xg)/prob_away_win_xg), 0.01),
         home_odds_plus_10perc = odds_home_win_xg * 1.1,
         away_odds_plus_10perc = odds_away_win_xg * 1.1,
         total_c = pred_hc + pred_ac) %>%
  select(home_team, away_team, date, 
         prob_home_winner, prob_away_win,
         prob_home_winner_xg, prob_away_win_xg,
         pred_hc, pred_ac, total_c,
         odds_home_win_xg,
         odds_away_win_xg,
         home_odds_plus_10perc,
         away_odds_plus_10perc)

write.csv(to_predict, file = "scripts/corneroutput_2023.csv", row.names = FALSE)

#########################################################
## read in the tracker
#########################################################

tracker <- readxl::read_excel(path = "scripts/tracker_2022.xlsx", sheet = "Raw") %>% 
  clean_names() %>%
  mutate(date = as.Date(date),
         rownum = row_number())

# get the recent season data

recent_results <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2022)  %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0)) %>%
  mutate(corner_cnt = hc+ac,
         corner_diff = hc-ac) %>%
  select(home_team, away_team, date, hc, ac, corner_cnt, home_corner_winner, corner_diff)

# merge together. Only use the bet data

bets <- bind_rows(tracker %>% 
                    select(rownum, home_team, away_team, date, winner_bet, winner_odds, winner_odds_from) %>%
                    dplyr::rename(bet = winner_bet, odds = winner_odds, odds_from = winner_odds_from) %>% 
                    filter(!is.na(bet) & bet != "No Bet") %>%
                    mutate(type = "corner_winner"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, h_bet, h_odds, h_odds_from) %>%
                    dplyr::rename(bet = h_bet, odds = h_odds, odds_from = h_odds_from) %>% 
                    filter(!is.na(bet) & bet != "No Bet") %>%
                    mutate(type = "home_count"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, a_bet, a_odds, a_odds_from) %>%
                    dplyr::rename(bet = a_bet, odds = a_odds, odds_from = a_odds_from) %>% 
                    filter(!is.na(bet) & bet != "No Bet") %>%
                    mutate(type = "away_count"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, t_bet, t_odds, t_odds_from) %>%
                    dplyr::rename(bet = t_bet, odds = t_odds, odds_from = t_odds_from) %>% 
                    filter(!is.na(bet) & bet != "No Bet") %>%
                    mutate(type = "total_count"),
                  ) %>%
  arrange(rownum) %>%
  inner_join(recent_results, by = c("home_team", "away_team", "date")) %>%
  mutate(bet_value = case_when(type == "corner_winner" ~ ifelse(bet == "H", 1, 0),
                               TRUE ~ as.numeric(str_remove(bet, "(o|u)"))
                               ),
         bet_type = case_when(type == "corner_winner" ~ "winner",
                               TRUE ~ str_extract(bet, "(o|u)"))
         ) %>%
  mutate(winner_flag = case_when(type == "corner_winner" ~ ifelse(bet_value == home_corner_winner & ac != hc, 1, 0),
                                 type == "home_count" & bet_type == "o" ~ ifelse(hc > bet_value, 1, 0),
                                 type == "home_count" & bet_type == "u" ~ ifelse(hc < bet_value, 1, 0),
                                 type == "away_count" & bet_type == "o" ~ ifelse(ac > bet_value, 1, 0),
                                 type == "away_count" & bet_type == "u" ~ ifelse(ac < bet_value, 1, 0),
                                 type == "total_count" & bet_type == "o" ~ ifelse(corner_cnt > bet_value, 1, 0),
                                 type == "total_count" & bet_type == "u" ~ ifelse(corner_cnt < bet_value, 1, 0)
                                 )
         ) %>%
  mutate(profit = (winner_flag * odds * 10) - 10) %>%
  mutate(cumulative_profit = cumsum(profit)) %>%
  mutate(betnum = row_number()) %>%
  select(-rownum) %>% 
  select(betnum, everything())

perf <- tracker %>%
  select(rownum, home_team, away_team, date, prob_home_winner, prob_home_win_xg, pred_hc, pred_ac) %>%
  inner_join(recent_results, by = c("home_team", "away_team", "date")) %>% 
  mutate(pred_corner_diff = pred_hc - pred_ac) %>% 
  mutate(pred_corner_cross = pred_hc*pred_ac,
         act_corner_corss = hc*ac,
         pred_corner_sq = (pred_hc+pred_ac)*(pred_hc+pred_ac),
         act_corner_sql = (hc+ac)*(hc+ac))

# graph performance

types <- bets %>%
  group_by(type) %>%
  summarise(profit_loss = sum(profit))

types_betnum <- bets %>% 
  select(date, type, profit) %>% 
  arrange(type, date) %>%
  group_by(type)  %>%
  mutate(cumulative_profit = cumsum(profit)) %>%
  mutate(betnum = row_number())

ggplot(bets) +
  geom_line(aes(x = betnum, y = cumulative_profit), size = 2, colour = "dodgerblue") +
  theme_classic()

ggplot(types) +
  geom_col(aes(x = type, y = profit_loss), size = 2, fill = "dodgerblue") +
  theme_classic()

ggplot(types_betnum) +
  geom_line(aes(x = betnum, y = cumulative_profit, group = type, colour = type), size = 2) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  scale_colour_brewer(type = "qual", palette = 2)

# corner cross

ggplot(perf) +
  geom_point(aes(x = pred_corner_cross, y = act_corner_corss), size = 2, colour = "blue") +
  theme_classic() 

# corner sq

ggplot(perf) +
  geom_point(aes(x = pred_corner_sq, y = act_corner_sql), size = 2, colour = "blue") +
  theme_classic() 

cor(perf$pred_corner_sq, perf$act_corner_sql)
cor(perf$pred_corner_cross, perf$act_corner_corss)

# roc curve

roccurve_old <- roc(perf$home_corner_winner ~ perf$prob_home_winner)
auc(roccurve_old)
plot(roccurve_old)

roccurve_xg <- roc(perf$home_corner_winner ~ perf$prob_home_win_xg)
auc(roccurve_xg)
plot(roccurve_xg)

# hc and ac

ggplot(perf) +
  geom_point(aes(x = pred_hc, y = hc), size = 2, colour = "blue") +
  theme_classic() +
  labs(title = "Home Corners") +
  geom_abline(slope = 1, intercept = 0)

ggplot(perf) +
  geom_point(aes(x = pred_ac, y = ac), size = 2, colour = "blue") +
  theme_classic() +
  labs(title = "Away Corners") +
  geom_abline(slope = 1, intercept = 0)

ggplot(perf) +
  geom_point(aes(x = prob_home_win_xg, y = corner_diff), size = 2, colour = "blue") +
  theme_classic() +
  labs(title = "Corner Diff & Prob") 

ggplot(perf) +
  geom_point(aes(x = pred_corner_diff, y = corner_diff), size = 2, colour = "blue") +
  theme_classic() +
  labs(title = "Corner Diff & Prob") +
  geom_abline(slope = 1, intercept = 0)

cor(perf$pred_hc, perf$hc)
cor(perf$pred_ac, perf$ac)
cor(perf$prob_home_win_xg, perf$corner_diff)

# diff models

ggplot(perf) +
  geom_point(aes(x = prob_home_win_xg, y = prob_home_winner), size = 2, colour = "blue") +
  theme_classic() +
  labs(title = "Model Diffs") +
  geom_abline(slope = 1, intercept = 0)

#############################
# what happens if we take the number of hc and ac
# and bet on 1 and 2+ more
#############################

longdata_hc <- tracker %>%
  inner_join(recent_results, by = c("home_team", "away_team", "date")) %>% 
  select(rownum, home_team, away_team, date, pred_hc, hc, starts_with("h_o"), -h_odds, -h_odds_from) %>% 
  pivot_longer(cols = starts_with("h_o")) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(corner_amt = as.numeric(str_remove(name, "h_o"))/10) %>% 
  filter(pred_hc + 2 > corner_amt) %>% 
  filter(value > 2) %>% 
  mutate(did_win = ifelse(hc > corner_amt, 1, 0)) %>% 
  mutate(profit = ifelse(did_win == 1, value -1, -1)) %>% 
  mutate(cumulative_pl = cumsum(profit))

ggplot(longdata_hc) +
  geom_line(aes(x = rownum, y = cumulative_pl), size = 2, colour = "dodgerblue") +
  theme_classic()

longdata_ac <- tracker %>%
  inner_join(recent_results, by = c("home_team", "away_team", "date")) %>% 
  select(rownum, home_team, away_team, date, pred_ac, ac, starts_with("a_o"), -a_odds, -a_odds_from) %>% 
  pivot_longer(cols = starts_with("a_o")) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(corner_amt = as.numeric(str_remove(name, "a_o"))/10) %>% 
  filter(pred_ac + 5 > corner_amt) %>% 
  filter(value > 2) %>% 
  mutate(did_win = ifelse(ac > corner_amt, 1, 0)) %>% 
  mutate(profit = ifelse(did_win == 1, value -1, -1)) %>% 
  mutate(cumulative_pl = cumsum(profit))

ggplot(longdata_ac) +
  geom_line(aes(x = rownum, y = cumulative_pl), size = 2, colour = "dodgerblue") +
  theme_classic()

## other here

merged <- tracker_all %>%
  inner_join(recent_results, by = c("home_team", "away_team", "date"))

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

ggplot(bets) +
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