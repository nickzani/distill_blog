library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(janitor)
library(ggplot2)

epl_2023_24 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = "2023/24")

epl_2022_23 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = "2022/23")

epl_2021_22 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 1000 + row_number(),
         season_id = "2021/22")

epl_2020_21 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 2000 + row_number(),
         season_id = "2020/21")

epl_2019_20 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1920/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 3000 + row_number(),
         season_id = "2019/20")

epl_2018_19 <- readr::read_csv('https://www.football-data.co.uk/mmz4281/1819/E0.csv') %>%
  clean_names() %>%
  mutate(date = as.Date(date, format="%d/%m/%Y"),
         match_id = 4000 + row_number(),
         season_id = "2018/19")

all_data <- bind_rows(epl_2023_24, epl_2022_23, epl_2021_22, epl_2020_21, epl_2019_20, epl_2018_19) %>%
  filter(home_team == "Tottenham" | away_team == "Tottenham") %>%
  select(season_id, date , home_team, away_team, ftr, b365h, b365a) %>%
  mutate(did_tottenham_win = case_when(home_team == "Tottenham" & ftr == "H" ~ "Y",
                                       away_team == "Tottenham" & ftr == "A" ~ "Y",
                                       TRUE ~ "N"),
         tottenham_odds = case_when(home_team == "Tottenham" ~ b365h,
                                    away_team == "Tottenham" ~ b365a),
         tottenham_negative_money = case_when(tottenham_odds < 2 ~ "Under Evens",
                                              TRUE ~ "Over Evens"),
         profit_loss = case_when(did_tottenham_win == "N" ~ -1,
                                 TRUE ~ (tottenham_odds -1))
         ) %>%
  arrange(season_id, tottenham_negative_money) %>%
  group_by(season_id, tottenham_negative_money) %>%
  mutate(gamenum = row_number(),
         cumulative_pl = cumsum(profit_loss))

summary <- all_data %>%
  filter(tottenham_negative_money == "Under Evens") %>% 
  group_by(season_id, tottenham_negative_money) %>%
  summarise(total_outlay = n(),
            total_profit = sum(profit_loss),
            ROI = sum(profit_loss)/n()) %>% 
  ungroup() %>%
  mutate(text = paste0("Total profit: ",
                       scales::dollar(total_profit, prefix = "£"),
                       "\nTotal Outlay: ", 
                       scales::dollar(total_outlay, prefix = "£"), 
                       "\nROI: ",
                       scales::percent(ROI)),
         xloc = 20,
         yloc = c(0,3,4,4))

ggplot(all_data %>% filter(tottenham_negative_money == "Under Evens"), 
       aes(x = gamenum,
           y = cumulative_pl)
       ) +
  geom_line(size = 2, colour = "dodgerblue") +
  geom_text(data = summary, aes(x = xloc, y = yloc, label = text), size = 5) +
  facet_grid(rows = vars(season_id)) +
  theme_classic() +
  geom_hline(yintercept = 0, colour = "red", linetype = "longdash") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  labs(title = "Betting Tottenham at Under Evens Odds Would Have Made a Profit in Every Season Except 2019/20", 
       subtitle = "Bet365 Odds\n",
       x = "Game Number",
       y = "Cumulative Profit/Loss\n") +
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

## Overall here

all_data_overall <- bind_rows(epl_2023_24, epl_2022_23, epl_2021_22, epl_2020_21, epl_2019_20, epl_2018_19) %>%
  filter(home_team == "Tottenham" | away_team == "Tottenham") %>%
  select(season_id, date , home_team, away_team, ftr, b365h, b365a) %>%
  mutate(did_tottenham_win = case_when(home_team == "Tottenham" & ftr == "H" ~ "Y",
                                       away_team == "Tottenham" & ftr == "A" ~ "Y",
                                       TRUE ~ "N"),
         tottenham_odds = case_when(home_team == "Tottenham" ~ b365h,
                                    away_team == "Tottenham" ~ b365a),
         tottenham_negative_money = case_when(tottenham_odds < 2 ~ "Under Evens",
                                              TRUE ~ "Over Evens"),
         profit_loss = case_when(did_tottenham_win == "N" ~ -1,
                                 TRUE ~ (tottenham_odds -1))
  ) %>%
  arrange(season_id, tottenham_negative_money) %>%
  group_by(tottenham_negative_money) %>%
  mutate(gamenum = row_number(),
         cumulative_pl = cumsum(profit_loss))

summary_all <- all_data %>%
  filter(tottenham_negative_money == "Under Evens") %>% 
  group_by(tottenham_negative_money) %>%
  summarise(total_outlay = n(),
            total_profit = sum(profit_loss),
            ROI = sum(profit_loss)/n()) %>% 
  ungroup()

ggplot(all_data_overall %>% filter(tottenham_negative_money == "Under Evens"), 
       aes(x = gamenum,
           y = cumulative_pl)
        ) +
  geom_line(size = 2, colour = "dodgerblue") +
  theme_classic() +
  geom_hline(yintercept = 0, colour = "red", linetype = "longdash") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  labs(title = "Betting Tottenham at Under Evens Odds Would Have Returned 6.0% Profit Since 2018", 
       subtitle = "Bet365 Odds\n",
       x = "Bet Number",
       y = "Cumulative Profit/Loss\n") +
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