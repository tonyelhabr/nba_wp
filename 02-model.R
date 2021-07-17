
library(tidyverse)
path_pbp <- file.path('data', 'pbp.rds')
pbp_init <- read_rds(path_pbp)
pbp_init

cols <-
  c("idGame", "nameTeam", "homeTeam", "awayTeam", "eventType", "eventTime", "numberPeriod", "eventDescription", "eventGeneral", "shotResultPoints", "gametime") %>% 
  snakecase::to_snake_case(cols)
cols
pbp_unnest <-
  pbp_init %>% 
  unnest(data) %>% 
  janitor::clean_names()
pbp_unnest
pbp_unnest %>% select(description_play_home)

pbp_unnest %>% 
  select(any_of(cols))
  select(year:slug_score, time_quarter, time_remaining, score_away, score_home, margin_score)
