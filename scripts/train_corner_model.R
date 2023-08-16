# Can we predict which team will win more corners based on historic performance?

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

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

##################################
# download the historic football data
##################################

epl_2022_23 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2023)

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = 2022)

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = 2021)

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = 2020)

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 5000 + row_number(),
         season_id = 2019)

# put these all together in one long dataframe and create a flag to show which team had more corners

all_data <- bind_rows(epl_2022_23,
                      epl_2021_22, 
                      epl_2020_21, 
                      epl_2019_20, 
                      epl_2018_19) %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0))

# whats the home/away breakdown

all_data %>% group_by(home_corner_winner) %>% count()

all_data %>% mutate(home_corner_winner = case_when(hc > ac ~ "H", 
                                                   ac > hc ~ "A",
                                                   TRUE ~ "D")) %>%
  group_by(home_corner_winner) %>% count() %>% ungroup() %>%
  mutate(freq = n / sum(n))

all_data %>% mutate(home_corner_winner = case_when(hc > ac ~ "H", 
                                                   ac > hc ~ "A",
                                                   TRUE ~ "D")) %>%
  filter(home_corner_winner == "D") %>%
  group_by(hc, ac) %>% count() %>% ungroup() %>%
  mutate(freq = n / sum(n))

##################################
# download the xg here
##################################

xg_dat <- readr::read_csv('https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv') %>%
  clean_names() %>%
  filter(league == "Barclays Premier League")

# the 538 expected goals team names are different to the football data ones so will make a lookup between the two

xg_lkp <- structure(list(xg_team = c("AFC Bournemouth", "Arsenal", "Aston Villa", 
                           "Brentford", "Brighton and Hove Albion", "Burnley", "Cardiff City", 
                           "Chelsea", "Crystal Palace", "Everton", "Fulham", "Huddersfield Town", 
                           "Leeds United", "Leicester City", "Liverpool", "Manchester City", 
                           "Manchester United", "Newcastle", "Norwich City", "Sheffield United", 
                           "Southampton", "Tottenham Hotspur", "Watford", "West Bromwich Albion", 
                           "West Ham United", "Wolverhampton", "Nottingham Forest"), 
                     fd_team = c("Bournemouth", "Arsenal", "Aston Villa", "Brentford", "Brighton", "Burnley", 
                                 "Cardiff", "Chelsea", "Crystal Palace", "Everton", "Fulham", 
                                  "Huddersfield", "Leeds", "Leicester", "Liverpool", "Man City", 
                                 "Man United", "Newcastle", "Norwich", "Sheffield United", "Southampton", 
                                 "Tottenham", "Watford", "West Brom", "West Ham", "Wolves", "Nott'm Forest")), 
                     class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"), 
                     row.names = c(NA, -26L), 
                     spec = structure(list(cols = list(xg_team = structure(list(), class = c("collector_character", "collector")), 
                                                       fd_team = structure(list(), class = c("collector_character",  "collector"))), 
                                           default = structure(list(), class = c("collector_guess", "collector")), skip = 1L), class = "col_spec")
                     )

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

#filter(all_data, is.na(importance1)) %>% View()
#teams <- all_data %>% count(home_team)
#xg_team <- xg_dat %>% count(team1)
#write.csv(teams, "teams.csv")
#write.csv(xg_team, "xg_team.csv")

# split up and then stick back together

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
         lag_1_home_corners_conceded = lag(corners_conceded, n = 1),
         lag_2_home_corners_conceded = lag(corners_conceded, n = 2),
         lag_3_home_corners_conceded = lag(corners_conceded, n = 3),
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
  na.omit() %>%
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
  na.omit() %>%
  select(match_id, starts_with("lag_"), contains("current"))

# merge back together

model_dat <- all_data %>%
  select(match_id, home_corner_winner) %>%
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
         d_3_xg_conceded = lag_3_home_xg_conceded-lag_3_away_xg_conceded,
         d_1_spi = lag_1_home_spi-lag_1_away_spi,
         d_2_spi = lag_2_home_spi-lag_2_away_spi,
         d_3_spi = lag_3_home_spi-lag_3_away_spi,
         spi_diff = current_away_spi - current_home_spi,
         spi_diff_perc = (current_away_spi - current_home_spi)/current_home_spi,
         imp_diff = current_away_importance - current_home_importance
         )

# output for regression model

regression_dat <- all_data %>%
  select(match_id, home_corner_winner, hc, ac) %>%
  inner_join(hc_model_data2, by = c("match_id")) %>%
  inner_join(ac_model_data2, by = c("match_id")) %>% 
  mutate(d_1_corner = lag_1_home_corner-lag_1_away_corner,
         d_2_corner = lag_2_home_corner-lag_2_away_corner,
         d_3_corner = lag_3_home_corner-lag_3_away_corner,
         d_1_xg = lag_1_home_xg-lag_1_away_xg,
         d_2_xg = lag_2_home_xg-lag_2_away_xg,
         d_3_xg = lag_3_home_xg-lag_3_away_xg,
         d_1_xg_conceded = lag_1_home_xg_conceded-lag_1_away_xg_conceded,
         d_2_xg_conceded = lag_2_home_xg_conceded-lag_2_away_xg_conceded,
         d_3_xg_conceded = lag_3_home_xg_conceded-lag_3_away_xg_conceded,
         d_1_spi = lag_1_home_spi-lag_1_away_spi,
         d_2_spi = lag_2_home_spi-lag_2_away_spi,
         d_3_spi = lag_3_home_spi-lag_3_away_spi,
         spi_diff = current_away_spi - current_home_spi,
         spi_diff_perc = (current_away_spi - current_home_spi)/current_home_spi,
         imp_diff = current_away_importance - current_home_importance
  )

# saveRDS(regression_dat, "regression_dat.rds")

############################
# model
############################

# Define the partition (e.g. 75% of the data for training)
trainIndex <- createDataPartition(model_dat$match_id, p = .75, 
                                  list = FALSE, 
                                  times = 1)

trainIndex_c <- createDataPartition(regression_dat$match_id, p = .75, 
                                  list = FALSE, 
                                  times = 1)

# Split the dataset using the defined partition
train_data <- model_dat[trainIndex, ,drop=FALSE]
train_data <- train_data %>% select(-match_id)
tune_plus_val_data <- model_dat[-trainIndex, ,drop=FALSE]

# Split the dataset using the defined partition for regression
train_data_c <- regression_dat[trainIndex_c, ,drop=FALSE]
train_data_c <- train_data_c %>% select(-match_id)
tune_plus_val_data_c <- regression_dat[-trainIndex_c, ,drop=FALSE]

objControl <- trainControl(method = 'cv',number = 3)
myTuning <- expand.grid(n.trees = c(12000), 
                        interaction.depth = c(10), 
                        shrinkage = c(0.0001), 
                        n.minobsinnode = c(4))

myTuning_c <- expand.grid(n.trees = c(10000), 
                          interaction.depth = c(10), 
                          shrinkage = c(0.005), 
                          n.minobsinnode = c(4))

modelFitWinner <- train(as.factor(home_corner_winner) ~ .,
                        data=train_data,
                        method="gbm",
                        trControl = objControl,
                        tuneGrid = myTuning,
                        distribution="bernoulli",
                        verbose=TRUE)

modelFitHC <- train(hc ~ .,
                    data=train_data_c %>% select(-ac, -home_corner_winner),
                    method="gbm",
                    trControl = objControl,
                    tuneGrid = myTuning_c,
                    verbose=TRUE)

modelFitAC <- train(ac ~ .,
                    data=train_data_c %>% select(-hc, -home_corner_winner),
                    method="gbm",
                    trControl = objControl,
                    tuneGrid = myTuning_c,
                    verbose=TRUE)

modelFitHC45 <- train(hco45 ~ .,
                    data=train_data_c %>% 
                        mutate(hco45 = as.factor(ifelse(hc > 4.5, 1, 0))) %>% 
                        select(-ac, -hc, -home_corner_winner),
                    method="gbm",
                    distribution="bernoulli",
                    trControl = objControl,
                    tuneGrid = myTuning_c,
                    verbose=TRUE)

modelFitAC45 <- train(ac ~ .,
                    data=train_data_c %>% 
                      mutate(aco45 = ifelse(ac > 4.5, 1, 0)) %>% 
                      select(-ac, -hc, -home_corner_winner),
                    method="gbm",
                    trControl = objControl,
                    tuneGrid = myTuning_c,
                    verbose=TRUE)


#############################################
# predict winner here
#############################################

predicted_winner <- as.data.frame(predict(modelFitWinner, newdata = tune_plus_val_data, "prob"))

tune_plus_val_data$probwinner <- predicted_winner$`1`
tune_plus_val_data$predwinner <- ifelse(predicted_winner$`1` > predicted_winner$`0`, 1, 0)
tune_plus_val_data$correct <- ifelse(tune_plus_val_data$home_corner_winner == tune_plus_val_data$predwinner, 1, 0)
sum(tune_plus_val_data$correct)/nrow(tune_plus_val_data)

roccurve <- roc(tune_plus_val_data$home_corner_winner ~ tune_plus_val_data$probwinner)
auc(roccurve)
plot(roccurve)

tune_plus_val_data %>%
  mutate(rounded_prob = round_any(probwinner, 0.05)) %>%
  group_by(rounded_prob) %>%
  summarise(cnt = n(),
            winners = sum(home_corner_winner),
            perc_correct = winners/cnt) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = rounded_prob, y = perc_correct)) +
  geom_abline(intercept=0, slope=1)

winner_perf <- lm(home_corner_winner ~ probwinner, data = tune_plus_val_data)
summary(winner_perf)

# brier score
Brier(tune_plus_val_data$probwinner, tune_plus_val_data$home_corner_winner, negative = 0, positive = 1)

#############################################
# predict home corners
#############################################

predicted_hc <- as.data.frame(predict(modelFitHC, newdata = tune_plus_val_data_c))
names(predicted_hc) <- c("pred_hc")
tune_plus_val_data_c$pred_hc <- predicted_hc$pred_hc

ggplot(tune_plus_val_data_c) +
  geom_point(aes(x = pred_hc, y = hc)) +
  geom_abline(intercept=0, slope=1)

hc_perf <- lm(hc ~ pred_hc, data = tune_plus_val_data_c)
summary(hc_perf)

#############################################
# predict away corners
#############################################

predicted_ac <- as.data.frame(predict(modelFitAC, newdata = tune_plus_val_data_c))
names(predicted_ac) <- c("pred_ac")
tune_plus_val_data_c$pred_ac <- predicted_ac$pred_ac

ggplot(tune_plus_val_data_c) +
  geom_point(aes(x = pred_ac, y = ac)) +
  geom_abline(intercept=0, slope=1)

ac_perf <- lm(ac ~ pred_ac, data = tune_plus_val_data_c)
summary(ac_perf)

#############################################
# predict > 4.5 home corners
#############################################

predicted_winner_45 <- as.data.frame(predict(modelFitHC45, newdata = tune_plus_val_data_c, "prob"))

tune_plus_val_data_c$prob_hc45 <- predicted_winner_45$`1`
tune_plus_val_data_c$pred_hco45 <- ifelse(tune_plus_val_data_c$prob_hc45 > 0.5, 1, 0)
tune_plus_val_data_c$correct_hc45 <- ifelse(tune_plus_val_data_c$pred_hco45 == 1 & tune_plus_val_data_c$hc > 4.5, 1, 0)
sum(tune_plus_val_data_c$correct_hc45)/nrow(tune_plus_val_data_c)

tune_plus_val_data_c$o_hc45 <- ifelse(tune_plus_val_data_c$hc > 4.5, 1, 0)

roccurve <- roc(tune_plus_val_data_c$o_hc45 ~ tune_plus_val_data_c$prob_hc45)
auc(roccurve)
plot(roccurve)

tune_plus_val_data_c %>%
  mutate(rounded_prob = round_any(prob_hc45, 0.05)) %>%
  group_by(rounded_prob) %>%
  summarise(cnt = n(),
            act_corners = sum(hc),
            avg_corners = act_corners/cnt) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = rounded_prob, y = avg_corners)) +
  geom_abline(intercept=0, slope=1)

o45_perf <- lm(hc ~ prob_hc45, data = tune_plus_val_data_c)
summary(o45_perf)

# brier score
Brier(tune_plus_val_data_c$prob_hc45, tune_plus_val_data_c$o_hc45, negative = 0, positive = 1)

## without xg = 0.6504
## xg = 0.7295

# brier score
Brier(tune_plus_val_data$probwinner, tune_plus_val_data$home_corner_winner, negative = 0, positive = 1)

## without xg = 0.233
## xg = 0.2263204
## xg plus additional historic = 0.2183327

## HC here



## AC here



###########################################################
### train the model on the full data here
###########################################################

objControl <- trainControl(method = 'cv',number = 3)

myTuning <- expand.grid(n.trees = c(12000), 
                        interaction.depth = c(10), 
                        shrinkage = c(0.0001), 
                        n.minobsinnode = c(4))

myTuning_c <- expand.grid(n.trees = c(10000), 
                          interaction.depth = c(10), 
                          shrinkage = c(0.005), 
                          n.minobsinnode = c(4))

modelFitWinner <- train(as.factor(home_corner_winner) ~ .,
                        data=model_dat,
                        method="gbm",
                        trControl = objControl,
                        tuneGrid = myTuning,
                        distribution="bernoulli",
                        verbose=TRUE)

modelFitHC <- train(hc ~ .,
                    data=regression_dat %>% 
                      select(-ac, -home_corner_winner),
                    method="gbm",
                    trControl = objControl,
                    tuneGrid = myTuning_c,
                    verbose=TRUE)

modelFitAC <- train(ac ~ .,
                    data=regression_dat %>% 
                      select(-hc, -home_corner_winner),
                    method="gbm",
                    trControl = objControl,
                    tuneGrid = myTuning_c,
                    verbose=TRUE)

modelFitHC45 <- train(hco45 ~ .,
                      data=regression_dat %>% 
                        mutate(hco45 = as.factor(ifelse(hc > 4.5, 1, 0))) %>% 
                        select(-ac, -hc, -home_corner_winner),
                      method="gbm",
                      distribution="bernoulli",
                      trControl = objControl,
                      tuneGrid = myTuning_c,
                      verbose=TRUE)

modelFitAC45 <- train(aco45 ~ .,
                      data=regression_dat %>% 
                        mutate(aco45 = as.factor(ifelse(ac > 4.5, 1, 0))) %>% 
                        select(-ac, -hc, -home_corner_winner),
                      method="gbm",
                      trControl = objControl,
                      tuneGrid = myTuning_c,
                      verbose=TRUE)


saveRDS(modelFitWinner, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_xg_2023.RDS")
saveRDS(modelFitHC, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc_2023.RDS")
saveRDS(modelFitAC, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac_2023.RDS")
saveRDS(modelFitHC45, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_hc45_2023.RDS")
saveRDS(modelFitAC45, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_ac45_2023.RDS")

## try with xgboost

library(xgboost)

cv <- xgb.cv(data = as.matrix(train_data), 
             label = as.factor(train_data$home_corner_winner),
             nrounds = 15000,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.0001,
             max_depth = 10,
             early_stopping_rounds = 5,
             verbose = FALSE   # silent
)

# Get the evaluation log
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))    # find the index of min(test_rmse_mean)

corner_model_xgb <- xgboost(data = as.matrix(train_data %>% select(-home_corner_winner)), 
                          label = train_data$home_corner_winner, 
                          nrounds = 16000,      
                          objective = "binary:logistic", 
                          eta = 0.0005,
                          max_depth = 10,
                          verbose = TRUE,
                          early_stopping_rounds = 10
)

predicted_winner <- predict(corner_model_xgb, newdata = as.matrix(tune_plus_val_data %>% select(-match_id, -home_corner_winner)))

#predicted_winner_prob <- exp(predicted_winner)/(1+exp(predicted_winner))

tune_plus_val_data$probwinner <- predicted_winner_prob
tune_plus_val_data$predwinner <- ifelse(predicted_winner_prob > 0.5, 1, 0)
tune_plus_val_data$correct <- ifelse(tune_plus_val_data$home_corner_winner == tune_plus_val_data$predwinner, 1, 0)

sum(tune_plus_val_data$correct)/nrow(tune_plus_val_data)

roccurve <- roc(tune_plus_val_data$home_corner_winner ~ tune_plus_val_data$probwinner)
auc(roccurve)
plot(roccurve)

## without xg = 0.6504
## xg = 0.7295

# brier score
Brier(tune_plus_val_data$probwinner, tune_plus_val_data$home_corner_winner, negative = 0, positive = 1)