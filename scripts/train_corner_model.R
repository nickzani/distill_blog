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

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
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

xg_lkp <- readr::read_csv('C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/xg_team_lkp.csv') %>%
  clean_names()

xg_dat_teams <- xg_dat %>%
  inner_join(xg_lkp, by = c("team1" = "xg_team")) %>%
  rename(home_team = fd_team) %>%
  inner_join(xg_lkp, by = c("team2" = "xg_team")) %>%
  rename(away_team = fd_team) %>%
  select(home_team, away_team, date, spi1, spi2, xg1, xg2)

nrow(all_data)

all_data <- all_data %>%
  inner_join(xg_dat_teams, by = c("home_team" = "home_team",
                                  "away_team" = "away_team",
                                  "date" = "date"))

nrow(all_data)

#teams <- all_data %>% count(home_team)
#xg_team <- xg_dat %>% count(team1)
#write.csv(teams, "teams.csv")
#write.csv(xg_team, "xg_team.csv")

# split up and then stick back together

home_data <- all_data %>%
  dplyr::select(date, match_id, season_id, home_team, ftr, fthg, hc, hs, hst, ac, spi1, spi2, xg1, xg2) %>%
  mutate(team_type = "Home",
         win_flag = case_when(ftr == "H" ~ 1, TRUE ~ 0),
         draw_flag = case_when(ftr == "D" ~ 1, TRUE ~ 0)) %>%
  rename(team = home_team,
         full_time_goals = fthg,
         corners = hc,
         shots = hs,
         shots_target = hst,
         corners_conceded = ac,
         spi = spi1,
         spi_away = spi2,
         xg = xg1,
         xg_conceded = xg2)

away_data <- all_data %>%
  dplyr::select(date, match_id, season_id, away_team, ftr, ftag, ac, as, ast, hc, spi1, spi2, xg1, xg2) %>%
  mutate(team_type = "Away",
         win_flag = case_when(ftr == "A" ~ 1, TRUE ~ 0),
         draw_flag = case_when(ftr == "D" ~ 1, TRUE ~ 0)) %>%
  rename(team = away_team,
         full_time_goals = ftag,
         corners = ac,
         shots = as,
         shots_target = ast,
         corners_conceded = hc,
         spi = spi2,
         spi_away = spi1,
         xg = xg2,
         xg_conceded = xg1
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
         lag_3_home_xg_conceded = lag(xg_conceded, n = 3)) %>%
  ungroup() %>%
  na.omit() %>%
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
         lag_3_away_corners_conceded = lag(corners_conceded, n = 3),
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
         lag_3_away_xg_conceded = lag(xg_conceded, n = 3)) %>%
  ungroup() %>%
  na.omit() %>%
  select(match_id, starts_with("lag_"))

# merge back together

model_dat <- all_data %>%
  select(match_id, home_corner_winner) %>%
  inner_join(hc_model_data2, by = c("match_id")) %>%
  inner_join(ac_model_data2, by = c("match_id"))

model_dat_diff <- model_dat %>%
  mutate(d1 = lag_1_home_corner - lag_1_away_corner,
         d2 = lag_2_home_corner - lag_2_away_corner,
         d3 = lag_3_home_corner - lag_3_away_corner,
         d4 = lag_1_home_shots - lag_1_away_shots,
         d5 = lag_2_home_shots - lag_2_away_shots,
         d6 = lag_3_home_shots - lag_3_away_shots,
         d7 = lag_1_home_goals - lag_1_away_goals,
         d8 = lag_2_home_goals - lag_2_away_goals,
         d9 = lag_3_home_goals - lag_3_away_goals,
         d10 = lag_1_home_corners_conceded - lag_1_away_corners_conceded,
         d11 = lag_2_home_corners_conceded - lag_2_away_corners_conceded,
         d12 = lag_3_home_corners_conceded - lag_3_away_corners_conceded,
         d13 = lag_1_home_spi - lag_1_away_spi,
         d14 = lag_2_home_spi - lag_2_away_spi,
         d15 = lag_3_home_spi - lag_3_away_spi,
         d16 = lag_1_home_xg - lag_1_away_xg,
         d17 = lag_2_home_xg - lag_2_away_xg,
         d18 = lag_3_home_xg - lag_3_away_xg,
         d19 = lag_1_home_xg_conceded - lag_1_away_xg_conceded,
         d20 = lag_2_home_xg_conceded - lag_2_away_xg_conceded,
         d21 = lag_3_home_xg_conceded - lag_3_away_xg_conceded)

############################
# model
############################

# Define the partition (e.g. 75% of the data for training)
trainIndex <- createDataPartition(model_dat$match_id, p = .75, 
                                  list = FALSE, 
                                  times = 1)

# Split the dataset using the defined partition
train_data <- model_dat[trainIndex, ,drop=FALSE]
train_data <- train_data %>% select(-match_id)
tune_plus_val_data <- model_dat[-trainIndex, ,drop=FALSE]

# diff here
train_data_diff <- model_dat_diff[trainIndex, ,drop=FALSE]
train_data_diff <- train_data_diff %>% select(home_corner_winner, starts_with("d"))
tune_plus_val_data_diff <- model_dat_diff[-trainIndex, ,drop=FALSE]

objControl <- trainControl(method = 'cv',number = 3)
myTuning <- expand.grid(n.trees = c(12000), 
                        interaction.depth = c(10), 
                        shrinkage = c(0.0001), 
                        n.minobsinnode = c(4))



modelFitWinner <- train(as.factor(home_corner_winner) ~ .,
                        data=train_data,
                        method="gbm",
                        trControl = objControl,
                        tuneGrid = myTuning,
                        distribution="bernoulli",
                        verbose=TRUE)

modelFitWinner_diff <- train(as.factor(home_corner_winner) ~ .,
                        data=train_data_diff,
                        method="gbm",
                        trControl = objControl,
                        tuneGrid = myTuning,
                        distribution="bernoulli",
                        verbose=TRUE)
# predict here

predicted_winner <- as.data.frame(predict(modelFitWinner, newdata = tune_plus_val_data, "prob"))
predicted_winner <- as.data.frame(predict(modelFitWinner_diff, newdata = tune_plus_val_data_diff, "prob"))

tune_plus_val_data$probwinner <- predicted_winner$`1`
tune_plus_val_data$predwinner <- ifelse(predicted_winner$`1` > predicted_winner$`0`, 1, 0)
tune_plus_val_data$correct <- ifelse(tune_plus_val_data$home_corner_winner == tune_plus_val_data$predwinner, 1, 0)

sum(tune_plus_val_data$correct)/nrow(tune_plus_val_data)

roccurve <- roc(tune_plus_val_data$home_corner_winner ~ tune_plus_val_data$probwinner)
auc(roccurve)
plot(roccurve)

## without xg = 0.6504
## xg = 0.7295

# brier score
Brier(tune_plus_val_data$probwinner, tune_plus_val_data$home_corner_winner, negative = 0, positive = 1)

## without xg = 0.233
## xg = 0.2263204

mycoords <- coords(roccurve, "all")

plot(mycoords[,"threshold"], mycoords[,"specificity"], type="l", col="red", xlab="Cutoff", ylab="Performance")
lines(mycoords[,"threshold"], mycoords[,"sensitivity"], type="l", col="blue")
legend(100, 0.4, c("Specificity", "Sensitivity"), 
       col=c("red", "blue"), lty=1)

coords(roccurve, "best", best.method="youden")

best.coords <- coords(roccurve, "best", best.method="youden")
abline(v=best.coords["threshold"], lty=2, col="grey")
abline(h=best.coords["specificity"], lty=2, col="red")
abline(h=best.coords["sensitivity"], lty=2, col="blue")

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

tune_plus_val_data %>%
  mutate(rounded_prob = round_any(probwinner, 0.05)) %>%
  group_by(rounded_prob) %>%
  summarise(cnt = n(),
            winners = sum(home_corner_winner),
            perc_correct = winners/cnt) %>%
  ungroup() %>%
  filter(rounded_prob > 0.25) %>%
    ggplot() +
    geom_point(aes(x = rounded_prob, y = perc_correct)) +
    geom_abline(intercept=0, slope=1)

saveRDS(modelFitWinner, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_xg.RDS")