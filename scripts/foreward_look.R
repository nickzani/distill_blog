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

# readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%  clean_names() %>% group_by(home_team) %>% count()

inner_join(readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%  clean_names() %>% group_by(home_team) %>% count() %>% rename(home = n),
           readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%  clean_names() %>% group_by(away_team) %>% count() %>% rename(away = n),
           by = c("home_team" = "away_team")) %>% 
  mutate(total = home+away)

# readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%  clean_names() %>% mutate(date = as.Date(date, format="%d/%m/%Y")) %>% summarise(maxdt = max(date))

# Arsenal,Aston Villa,Bournemouth,Brentford,Brighton,Burnley,Chelsea,Crystal Palace,Everton,
# Fulham,Liverpool,Luton,Man City,Man United,Newcastle,Nott'm Forest,Sheffield United,Tottenham,West Ham,Wolves


futures <- read_html("https://fbref.com/en/comps/9/2023-2024/schedule/2023-2024-Premier-League-Scores-and-Fixtures") %>% html_table(fill = TRUE)

futures_dat <- bind_rows(futures[1]) %>% 
  filter(Date == as.Date("2023-11-24") | Date == as.Date("2023-11-25") | Date == as.Date("2023-11-26") | Date == as.Date("2023-11-27")) %>% 
  select(Date, Home, Away) %>% 
  clean_names() %>% 
  mutate(date = as.Date(date))

xg_lkp <- readr::read_csv('C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/xg_team_lkp_2.csv') %>%
  clean_names()

future_games <- futures_dat %>%
  inner_join(xg_lkp, by = c("home" = "xg_team")) %>%
  rename(home_team = fd_team) %>%
  inner_join(xg_lkp, by = c("away" = "xg_team")) %>%
  rename(away_team = fd_team) %>%
  select(home_team, away_team, date)

epl_2023_24 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y")) %>% 
  bind_rows(future_games) %>%
  mutate(match_id = 0 + row_number(),
         season_id = 2024)

epl_2022_23 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2023)

all_data <- bind_rows(epl_2023_24,
                      epl_2022_23,)

##################################
# download the xg here
##################################

library(rvest)

xg_22_23 <- read_html("https://fbref.com/en/comps/9/2023-2024/schedule/2023-2024-Premier-League-Scores-and-Fixtures") %>% html_table(fill = TRUE)

xg_dat <- bind_rows(xg_22_23[1]) 

xg_dat <- xg_dat %>% 
  mutate(date = as.Date(Date),
         attendance = as.numeric(str_remove(Attendance, ","))) %>% 
  rename(xg_home = xG...6,
         xg_away = xG...8,
         home = Home,
         away = Away) %>% 
  select(date, home, away, xg_home, xg_away, attendance) %>% 
  clean_names() %>% 
  filter(!is.na(date))

# the expected goals team names are different to the football data ones so will make a lookup between the two

xg_lkp <- readr::read_csv('C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/xg_team_lkp_2.csv') %>%
  clean_names()

xg_dat_teams <- xg_dat %>%
  inner_join(xg_lkp, by = c("home" = "xg_team")) %>%
  rename(home_team = fd_team) %>%
  inner_join(xg_lkp, by = c("away" = "xg_team")) %>%
  rename(away_team = fd_team) %>%
  select(home_team, away_team, date, xg_home, xg_away, attendance)

nrow(all_data)

all_data <- all_data %>%
  left_join(xg_dat_teams, by = c("home_team" = "home_team",
                                  "away_team" = "away_team",
                                  "date" = "date"))

nrow(all_data)

###############################################
# split up and then stick back together
###############################################

home_data <- all_data %>%
  dplyr::select(date, match_id, season_id, home_team, ftr, fthg, ftag, hc, hs, as, hst, ac, hy, xg_home, xg_away, attendance) %>%
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
         xg = xg_home,
         xg_conceded = xg_away)

away_data <- all_data %>%
  dplyr::select(date, match_id, season_id, away_team, ftr, ftag, fthg, ac, as, hs, ast, hc, ay, xg_home, xg_away, attendance) %>%
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
         xg = xg_away,
         xg_conceded = xg_home
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
         lag_1_home_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_home_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_home_corners_conceded = lag(corners_conceded, n = 3),
         lag_1_home_goals_conceded = lag(goals_conceded, n = 1),
         lag_2_home_goals_conceded = lag(goals_conceded, n = 2),
         lag_3_home_goals_conceded = lag(goals_conceded, n = 3),
         lag_1_home_xg = lag(xg, n = 1),
         lag_2_home_xg = lag(xg, n = 2),
         lag_3_home_xg = lag(xg, n = 3),
         lag_1_home_xg_conceded = lag(xg_conceded),
         lag_2_home_xg_conceded = lag(xg_conceded, n = 2),
         lag_3_home_xg_conceded = lag(xg_conceded, n = 3)) %>%
  ungroup() %>%
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
         lag_1_away_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_away_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_away_corners_conceded = lag(corners_conceded, n = 3),
         lag_1_away_goals_conceded = lag(goals_conceded, n = 1),
         lag_2_away_goals_conceded = lag(goals_conceded, n = 2),
         lag_3_away_goals_conceded = lag(goals_conceded, n = 3),
         lag_1_away_xg = lag(xg, n = 1),
         lag_2_away_xg = lag(xg, n = 2),
         lag_3_away_xg = lag(xg, n = 3),
         lag_1_away_xg_conceded = lag(xg_conceded),
         lag_2_away_xg_conceded = lag(xg_conceded, n = 2),
         lag_3_away_xg_conceded = lag(xg_conceded, n = 3)) %>%
  ungroup() %>%
  select(match_id, starts_with("lag_"), contains("current"))

# merge back together

model_dat <- all_data %>%
  select(match_id) %>%
  inner_join(hc_model_data2, by = c("match_id")) %>%
  inner_join(ac_model_data2, by = c("match_id"))

ncol(model_dat)
nrow(model_dat)

# add in some differences

model_dat <- model_dat %>% 
  mutate(d_1_corner = lag_1_home_corner-lag_1_away_corner,
         d_2_corner = lag_2_home_corner-lag_2_away_corner,
         d_3_corner = lag_3_home_corner-lag_3_away_corner,
         d_1_xg = lag_1_home_xg-lag_1_away_xg,
         d_2_xg = lag_2_home_xg-lag_2_away_xg,
         d_3_xg = lag_3_home_xg-lag_3_away_xg,
         d_1_xg_conceded = lag_1_home_xg_conceded-lag_1_away_xg_conceded,
         d_2_xg_conceded = lag_2_home_xg_conceded-lag_2_away_xg_conceded,
         d_3_xg_conceded = lag_3_home_xg_conceded-lag_3_away_xg_conceded
  )

# get pred file

to_predict <- all_data %>%
  filter(is.na(div)) %>%
  select(home_team, away_team, date, match_id) %>%
  inner_join(model_dat)

# predict

modelFitWinner <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_xg_2023.RDS")
modelFitHC <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc_2023.RDS")
modelFitAC <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac_2023.RDS")
modelFitHC45 <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc45_2023.RDS")
modelFitAC45 <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac45_2023.RDS")
modelFitHC55 <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc55_2023.RDS")
modelFitAC55 <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac55_2023.RDS")
modelFitHC65 <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc65_2023.RDS")
modelFitAC65 <- readRDS(file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac65_2023.RDS")

predicted_winner <- as.data.frame(predict(modelFitWinner, newdata = to_predict, "prob"))
to_predict$prob_home_winner <- predicted_winner$`1`

to_predict$pred_hc <- predict(modelFitHC, newdata = to_predict)
to_predict$pred_ac <- predict(modelFitAC, newdata = to_predict)

predicted_hc45 <- as.data.frame(predict(modelFitHC45, newdata = to_predict, "prob"))
to_predict$predicted_hc45 <- predicted_hc45$`1`

predicted_ac45 <- as.data.frame(predict(modelFitAC45, newdata = to_predict, "prob"))
to_predict$predicted_ac45 <- predicted_ac45$`1`

predicted_hc55 <- as.data.frame(predict(modelFitHC55, newdata = to_predict, "prob"))
to_predict$predicted_hc55 <- predicted_hc55$`1`

predicted_ac55 <- as.data.frame(predict(modelFitAC55, newdata = to_predict, "prob"))
to_predict$predicted_ac55 <- predicted_ac55$`1`

predicted_hc65 <- as.data.frame(predict(modelFitHC65, newdata = to_predict, "prob"))
to_predict$predicted_hc65 <- predicted_hc65$`1`

predicted_ac65 <- as.data.frame(predict(modelFitAC65, newdata = to_predict, "prob"))
to_predict$predicted_ac65 <- predicted_ac65$`1`

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

to_predict <- to_predict %>%
  select(home_team, away_team, date, prob_home_winner, pred_hc, pred_ac, 
         predicted_hc45, predicted_ac45, predicted_hc55, predicted_ac55, predicted_hc65, predicted_ac65) %>%
  mutate(prob_away_win = 1- 0.08 - prob_home_winner,
         odds_home_win_xg = round_any(1+((1-prob_home_winner)/prob_home_winner), 0.01),
         odds_away_win_xg = round_any(1+((1-prob_away_win)/prob_away_win), 0.01),
         odd_h45 = round_any(1+((1-predicted_hc45)/predicted_hc45), 0.01),
         odd_a45 = round_any(1+((1-predicted_ac45)/predicted_ac45), 0.01),
         odd_h55 = round_any(1+((1-predicted_hc55)/predicted_hc55), 0.01),
         odd_a55 = round_any(1+((1-predicted_ac55)/predicted_ac55), 0.01),
         odd_h65 = round_any(1+((1-predicted_hc65)/predicted_hc65), 0.01),
         odd_a65 = round_any(1+((1-predicted_ac65)/predicted_ac65), 0.01),
         odds_home_win_plus10 = odds_home_win_xg * 1.1,
         odds_away_win_plus10 = odds_away_win_xg * 1.1,
         total_c = pred_hc + pred_ac) %>%
  select(home_team, away_team, date, 
         prob_home_winner, prob_away_win,
         pred_hc, pred_ac, total_c,
         odds_home_win_plus10,
         odds_away_win_plus10,
         odd_h45,odd_a45,
         odd_h55,odd_a55,
         odd_h65,odd_a65)

write.csv(to_predict, file = "scripts/corneroutput_2023.csv", row.names = FALSE)

#########################################################
## read in the tracker
#########################################################

tracker <- readxl::read_excel(path = "scripts/tracker_2023.xlsx", sheet = "Raw") %>% 
  clean_names() %>%
  mutate(date = as.Date(date),
         rownum = row_number())

# get the recent season data

recent_results <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2023)  %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0)) %>%
  mutate(away_corner_winner = case_when(ac > hc ~ 1, TRUE ~ 0)) %>%
  mutate(corner_cnt = hc+ac,
         corner_diff = hc-ac) %>%
  select(home_team, away_team, date, hc, ac, corner_cnt, home_corner_winner, away_corner_winner, corner_diff)

# merge together. Only use the bet data

bets <- bind_rows(tracker %>% 
                    select(rownum, home_team, away_team, date, corner_winner_bet, corner_winner_odds) %>%
                    dplyr::rename(bet = corner_winner_bet, odds = corner_winner_odds) %>% 
                    filter(!is.na(bet)) %>%
                    mutate(type = "corner_winner"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, h_35bet, h_35odds) %>%
                    dplyr::rename(bet = h_35bet, odds = h_35odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "home_corner_count 3.5"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, h_45bet, h_45odds) %>%
                    dplyr::rename(bet = h_45bet, odds = h_45odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "home_corner_count 4.5"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, h_55bet, h_55odds) %>%
                    dplyr::rename(bet = h_55bet, odds = h_55odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "home_corner_count 5.5"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, h_65bet, h_65odds) %>%
                    dplyr::rename(bet = h_65bet, odds = h_65odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "home_corner_count 6.5"),
                  ############################################
                  ### away corners here
                  ############################################
                  tracker %>% 
                    select(rownum, home_team, away_team, date, a_35bet, a_35odds) %>%
                    dplyr::rename(bet = a_35bet, odds = a_35odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "away_corner_count 3.5"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, a_45bet, a_45odds) %>%
                    dplyr::rename(bet = a_45bet, odds = a_45odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "away_corner_count 4.5"),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, a_55bet, a_55odds) %>%
                    dplyr::rename(bet = a_55bet, odds = a_55odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "away_corner_count 5.5") %>% 
                    mutate(bet = as.character(bet),
                           odds = as.double(odds)),
                  tracker %>% 
                    select(rownum, home_team, away_team, date, a_65bet, a_65odds) %>%
                    dplyr::rename(bet = a_65bet, odds = a_65odds) %>% 
                    filter(!is.na(odds)) %>%
                    mutate(type = "away_corner_count 6.5") %>% 
                    mutate(bet = as.character(bet),
                           odds = as.double(odds)),
                  ) %>%
  arrange(rownum) %>%
  inner_join(recent_results, by = c("home_team", "away_team", "date")) %>%
  mutate(winner_flag = case_when(type == "corner_winner" & bet == "home" ~ ifelse(home_corner_winner == 1, 1, 0),
                                 type == "corner_winner" & bet == "away" ~ ifelse(away_corner_winner == 1, 1, 0),
                                 type == "home_corner_count 3.5" & bet == "over" ~ ifelse(hc > 3.5, 1, 0),
                                 type == "home_corner_count 4.5" & bet == "over" ~ ifelse(hc > 4.5, 1, 0),
                                 type == "home_corner_count 5.5" & bet == "over" ~ ifelse(hc > 5.5, 1, 0),
                                 type == "home_corner_count 6.5" & bet == "over" ~ ifelse(hc > 6.5, 1, 0),
                                 type == "home_corner_count 7.5" & bet == "over" ~ ifelse(hc > 7.5, 1, 0),
                                 type == "home_corner_count 3.5" & bet == "under" ~ ifelse(hc < 3.5, 1, 0),
                                 type == "home_corner_count 4.5" & bet == "under" ~ ifelse(hc < 4.5, 1, 0),
                                 type == "home_corner_count 5.5" & bet == "under" ~ ifelse(hc < 5.5, 1, 0),
                                 type == "home_corner_count 6.5" & bet == "under" ~ ifelse(hc < 6.5, 1, 0),
                                 type == "home_corner_count 7.5" & bet == "under" ~ ifelse(hc < 7.5, 1, 0),
                                 ## away
                                 type == "away_corner_count 3.5" & bet == "over" ~ ifelse(ac > 3.5, 1, 0),
                                 type == "away_corner_count 4.5" & bet == "over" ~ ifelse(ac > 4.5, 1, 0),
                                 type == "away_corner_count 5.5" & bet == "over" ~ ifelse(ac > 5.5, 1, 0),
                                 type == "away_corner_count 6.5" & bet == "over" ~ ifelse(ac > 6.5, 1, 0),
                                 type == "away_corner_count 7.5" & bet == "over" ~ ifelse(ac > 7.5, 1, 0),
                                 type == "away_corner_count 3.5" & bet == "under" ~ ifelse(ac < 3.5, 1, 0),
                                 type == "away_corner_count 4.5" & bet == "under" ~ ifelse(ac < 4.5, 1, 0),
                                 type == "away_corner_count 5.5" & bet == "under" ~ ifelse(ac < 5.5, 1, 0),
                                 type == "away_corner_count 6.5" & bet == "under" ~ ifelse(ac < 6.5, 1, 0),
                                 type == "away_corner_count 7.5" & bet == "under" ~ ifelse(ac < 7.5, 1, 0)
                                 )
         ) %>%
  mutate(profit = (winner_flag * odds * 10) - 10) %>%
  mutate(cumulative_profit = cumsum(profit)) %>%
  mutate(betnum = row_number()) %>%
  select(-rownum) %>% 
  select(betnum, everything())

# graph performance

types <- bets %>%
  group_by(type) %>%
  summarise(bet_num = n(),
            profit_loss = sum(profit))

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