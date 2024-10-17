## code to prepare `coffee_survey` dataset goes here

library(janitor)
library(tidyverse)
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv'

coffee_survey <- read_csv(url) %>%
  select(prefer_overall, gender, age, where_drink, brew, most_paid)

usethis::use_data(coffee_survey, overwrite = TRUE)
