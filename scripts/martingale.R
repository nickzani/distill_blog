# Martingale for home team wins

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
library(rvest)
library(quantmod)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

##################################
# download the historic football data
##################################

epl_2023_24 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%
  rename(b365_o_2.5 = `B365C>2.5`,
         b365_u_2.5 = `B365C<2.5`) %>% 
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = "2023/24")

epl_2022_23 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  rename(b365_o_2.5 = `B365C>2.5`,
         b365_u_2.5 = `B365C<2.5`) %>% 
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = "2022/23")

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  rename(b365_o_2.5 = `B365C>2.5`,
         b365_u_2.5 = `B365C<2.5`) %>% 
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = "2021/22")

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv') %>%
  rename(b365_o_2.5 = `B365C>2.5`,
         b365_u_2.5 = `B365C<2.5`) %>% 
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = "2020/21")

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E0.csv') %>%
  rename(b365_o_2.5 = `B365C>2.5`,
         b365_u_2.5 = `B365C<2.5`) %>% 
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 5000 + row_number(),
         season_id = "2019/20")

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv') %>%
  rename(b365_o_2.5 = `B365C>2.5`,
         b365_u_2.5 = `B365C<2.5`) %>% 
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 6000 + row_number(),
         season_id = "2018/19")

# put these all together in one long dataframe

all_data <- bind_rows(epl_2023_24,
                      epl_2022_23,
                      epl_2021_22, 
                      epl_2020_21, 
                      epl_2019_20, 
                      epl_2018_19)

home_wins <- all_data %>% 
  mutate(homewinflag = ifelse(ftr == "H", 1, 0)) %>% 
  group_by(season_id, home_team) %>%
  summarise(home_wins = sum(homewinflag)) %>% 
  ungroup() %>% 
  arrange(season_id, home_wins) %>% 
  group_by(season_id) %>% 
  mutate(rownum = row_number()) %>% 
  filter(rownum == 1)

home_wins %>%
  ggplot(aes(x = season_id, 
             y = home_wins)) +
  geom_col(fill = "orchid3") +
  theme_classic() +
  labs(title = "Number of Home Team Wins for the Team with the Lowest Number of Home Wins", 
       subtitle = "",
       fill = "",
       x = "",
       y = "Count\n") +
  theme(strip.text = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)
  )

# What number game was it

game_ids <- all_data %>% 
  inner_join(home_wins) %>% 
  arrange(date) %>% 
  group_by(season_id, home_team) %>% 
  mutate(rownumber_overall = row_number()) %>% 
  filter(ftr == "H") %>% 
  mutate(rownum = row_number()) %>% 
  filter(rownum == 1) %>% 
  select(season_id, home_team, rownumber_overall, rownum)

game_ids %>%
  ggplot(aes(x = season_id, 
             y = rownumber_overall)) +
  geom_col(fill = "skyblue1") +
  theme_classic() +
  labs(title = "Game Number of First Home Win for the Team with the Lowest Number of Home Wins", 
       subtitle = "",
       fill = "",
       x = "",
       y = "Count\n") +
  theme(strip.text = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)
  )

# list of teams and loop through

teamlist <- all_data %>% 
  group_by(season_id, home_team) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  as.data.frame()

# teamlist <- teamlist[1,]

blank_df <- data.frame(season_id = character(),
                       date = as.Date(character()),
                       home_team = character(),
                       away_team = character(),
                       ftr = character(),
                       b365h = numeric(),
                       stake = numeric(),
                       gamenum = numeric())

for (i in 1:nrow(teamlist)) {
  
  team <- teamlist[i,2]
  season <- teamlist[i,1]
  
  dat_loop <- all_data %>% 
    filter(season_id == season & home_team == team) %>% 
    select(season_id, date, home_team, away_team, ftr, b365h) %>% 
    mutate(stake = NA_real_) %>% 
    mutate(gamenum = row_number())
  
  # set the flags
  
  cumulative_loss <- 0
  has_won <- 0
  winning_amt <- 1
  
  # now loop through the home team games
  
  for (j in 1:nrow(dat_loop)) {
    
    game_odds <- dat_loop[j, "b365h"]
    
    if(has_won == 1){
      stake <- 0
    } else {
      stake <- (winning_amt+cumulative_loss)/(game_odds - 1)
    }
    
    result <- dat_loop[j, "ftr"]
    has_won <- ifelse(result == "H", 1, has_won)
    cumulative_loss <- cumulative_loss + stake
    
    dat_loop[j, "stake"] <- stake
    
  }
  
  blank_df <- bind_rows(blank_df, dat_loop)
  
}

blank_df <- blank_df %>% 
  mutate(p_l = ifelse(ftr == "H", (b365h-1)*stake, stake*-1)) %>%
  arrange(date, home_team) %>% 
  filter(stake != 0) %>% 
  mutate(cumulative_pl = cumsum(p_l)) %>% 
  mutate(gameidoverall = row_number())

blank_df %>%
  ggplot(aes(x = gameidoverall, 
             y = cumulative_pl)) +
  geom_line(colour = "black") +
  theme_classic() +
  labs(title = "Game Number of First Home Win for the Team with the Lowest Number of Home Wins", 
       subtitle = "",
       fill = "",
       x = "",
       y = "Count\n") +
  theme(strip.text = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)
  )

sum(blank_df$stake)
max(blank_df$cumulative_pl)

max(blank_df$cumulative_pl)/sum(blank_df$stake)

