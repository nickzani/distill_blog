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

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E2.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = 2022)

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E2.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = 2021)

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E2.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = 2020)

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1819/E2.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = 2019)

all_data <- bind_rows(epl_2021_22, epl_2020_21, epl_2019_20, epl_2018_19) %>%
  mutate(home_corner_winner = case_when(hc > ac ~ 1, TRUE ~ 0))

# whats the home/away breakdown

all_data %>% group_by(home_corner_winner)

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
         lag_3_away_corners_conceded = lag(corners_conceded, n = 3)) %>%
  ungroup() %>%
  na.omit() %>%
  select(match_id, starts_with("lag_"))

# merge back together

model_dat <- all_data %>%
  select(match_id, home_corner_winner) %>%
  inner_join(hc_model_data2, by = c("match_id")) %>%
  inner_join(ac_model_data2, by = c("match_id"))

############################
# model
############################

# Define the partition (e.g. 75% of the data for training)
trainIndex <- createDataPartition(model_dat$match_id, p = .75, 
                                  list = FALSE, 
                                  times = 1)

# Split the dataset using the defined partition
train_data <- model_dat[trainIndex, ,drop=FALSE]
tune_plus_val_data <- model_dat[-trainIndex, ,drop=FALSE]

train_data <- train_data %>% select(-match_id)

objControl <- trainControl(method = 'cv',number = 3, sampling = "up")
myTuning <- expand.grid(n.trees = c(2000), 
                        interaction.depth = c(10), 
                        shrinkage = c(0.001), 
                        n.minobsinnode = c(4))



modelFitWinner <- train(as.factor(home_corner_winner) ~ .,
                        data=train_data,
                        method="gbm",
                        trControl = objControl,
                        tuneGrid = myTuning,
                        distribution="bernoulli",
                        verbose=TRUE)
# predict here

predicted_winner <- as.data.frame(predict(modelFitWinner, newdata = tune_plus_val_data, "prob"))

tune_plus_val_data$probwinner <- predicted_winner$`1`
tune_plus_val_data$predwinner <- ifelse(predicted_winner$`1` > predicted_winner$`0`, 1, 0)
tune_plus_val_data$correct <- ifelse(tune_plus_val_data$home_corner_winner == tune_plus_val_data$predwinner, 1, 0)

sum(tune_plus_val_data$correct)/nrow(tune_plus_val_data)

roccurve <- roc(tune_plus_val_data$home_corner_winner ~ tune_plus_val_data$probwinner)
auc(roccurve)
plot(roccurve)

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

saveRDS(modelFitWinner, file = "C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/scripts/model_l1.RDS")