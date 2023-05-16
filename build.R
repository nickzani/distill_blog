library(distill)
library(rmarkdown)

create_blog(title = "My Blog", dir = "~")

create_post("Making Nice Tables with {gt}")
create_post("Setting up R Markdown")
create_post("Using Expected Goals to Predict Future Matches")
create_post("Scraping Local Election Data")

library(dplyr)
library(readr)
library(tibble)
library(stringr)

file.copy("C:/Users/vi2073/Documents/GitHub/distill_blog/distill_nickzani/_site", 
          "C:/Users/vi2073/Documents/GitHub/nickzani.github.io/blog",
          recursive = TRUE)

as_tibble(read_lines(file = "./_data/apple_health.xml")) %>%
  filter(str_detect(value, 'StepCount')) %>%
  filter(str_detect(value, 'Apple Watch')) %>%
  mutate(value = trimws(value)) %>%
  mutate(value = str_remove_all(value, 'HKDevice: [0-9a-zA-Z]+')) %>%
  mutate(value = str_remove_all(value, 'model:Watch, hardware:Watch5,9, ')) %>%
  write.csv(file = "./_data/apple_health_extract.xml", 
            row.names = FALSE, 
            col.names = NA)
