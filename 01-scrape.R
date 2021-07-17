
library(tidyverse)
library(nbastatR)

seasons <- 2020:2021
f_q <- quietly(nbastatR::game_logs)

logs <- 
  seasons %>% 
  map_dfr(
    ~f_q(.x, result_types = 'team', season_types = 'Regular Season', assign_to_environment = FALSE) %>% pluck('result')
  ) %>% 
  janitor::clean_names() 
logs

logs_slim <-
  logs %>% 
  filter(slug_team == slug_team_winner) %>% 
  select(year = year_season, date = date_game, id_game, tm_w = slug_team_winner, tm_l = slug_team_loser, slug_matchup)
logs_slim
logs_slim %>% distinct(id_game)

do_pbp <- function(game_id) {
  path <- file.path('data', sprintf('game_%d.rds', game_id))
  if(file.exists(path)) {
    return(read_rds(path))
  }
  res <- nbastatR::play_by_play_v2(game_id, return_message = TRUE)
  write_rds(res, path)
  res
}

pbp <-
  logs_slim %>% 
  mutate(
    data = map(id_game, do_pbp)
  )
pbp
path_pbp <- file.path('data', 'pbp.rds')
write_rds(pbp, path_pbp)
