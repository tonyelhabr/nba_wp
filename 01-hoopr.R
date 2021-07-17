
library(tidyverse)
library(hoopR)
# future::plan('multisession')
tictoc::tic()
progressr::with_progress({
  pbp <- load_nba_pbp(2020:2021)
})
tictoc::toc()

playoff_2021_id <- 401326988
mil_atl_g3_id <- 401337342
z <- espn_nba_game_all(game_id = 401337342)$Team %>% as_tibble()
z
z %>% glimpse()
pbp
pbp %>%
  filter(game_id == max(game_id))
pbp

pbp %>%
  select(game_id, season, home_team_abbrev, away_team_abbrev) %>%
  filter(season >= 2021) %>%
  # , home_team_abbrev == 'ATL', away_team_abbrev == 'MIL') %>%
  distinct(game_id, home_team_abbrev, away_team_abbrev) 

df_h <-
  pbp %>%
  mutate(favored_by = -game_spread) %>% 
  select(id, game_id, pts_h = home_score, pts_a = away_score, sec_left = start_game_seconds_remaining, favored_by) %>%
  group_by(game_id) %>%
  mutate(
    pts_diff = pts_h - pts_a,
    # spread = spread * (sec_left / max(sec_left)) ^ pi,
    w = ifelse(max(pts_h) > max(pts_a), 1, 0)
  ) %>%
  ungroup()
df_h

df <-
  bind_rows(
    df_h %>% rename(pts_1 = pts_h, pts_2 = pts_a),
    df_h %>%
      rename(pts_1 = pts_a, pts_2 = pts_h) %>%
      mutate(
        across(c(pts_diff, favored_by), ~ -.x),
        across(w, ~ {
          1 - .x
        })
      )
  )
df

game_ids <- df %>% distinct(game_id)
game_ids

game_ids_trn <- game_ids %>% filter(game_id < playoff_2021_id)
game_ids_tst <- game_ids %>% filter(game_id >= playoff_2021_id)
df_trn <- df %>% semi_join(game_ids_trn)
df_tst <- df %>% semi_join(game_ids_tst)

max_sec <- c(seq(2400, 600, -10), seq(600, 100, -5), seq(100, 10, -1), seq(10, 2, -1), 1)
min_sec <- c(seq(2300, 500, -10), seq(550, 50, -5), seq(95, 5, -1), seq(8, 0, -1), 0)
n <- length(max_sec)

fit_window <- function(i, overwrite = TRUE) {
  path <- file.path('data', sprintf('coefs_%03d.csv', i))
  if (file.exists(path) & !overwrite) {
    return(read_csv(path))
  }
  cat(glue::glue('window {i} of {n}'), sep = '\n')

  df_filt <- df_trn %>% filter(sec_left >= min_sec[i], sec_left <= max_sec[i])
  fit <- glm(w ~ pts_diff + favored_by - 1, data = df_filt, family = binomial(link = 'logit'))

  coefs <-
    fit %>%
    broom::tidy() %>%
    select(
      coefficient = term,
      estimate,
      std_error = std.error,
      z_value = statistic,
      p_value = p.value
    ) %>%
    mutate(min_time = min_sec[i], max_time = max_sec[i])
  write_csv(coefs, path)
}

coefs <- 1:n %>% map_dfr(fit_window)
coefs %>% write_csv(file.path('coefs.csv'))
coefs

library(tonythemes)
tonythemes::theme_set_tony()
p1 <-
  coefs %>%
  mutate(
    across(
      coefficient,
      ~case_when(
        .x == 'pts_diff' ~ 'In-Game Point Differential',
        .x == 'favored_by' ~ 'Pre-Game Spread'
      )
    )
  ) %>% 
  ggplot() +
  aes(x = max_time, y = estimate, group = coefficient) +
  # scale_x_continuous(breaks = seq(2400, 0, -400), limits = c(2400, 0)) + # , labels = scales::comma) +
  scale_x_reverse(labels = scales::comma, breaks = seq(2400, 0, -400), limits = c(2400, 0)) +
  facet_wrap(~coefficient, ncol = 1, scales = 'free_y') +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point(size = 0.5) +
  geom_ribbon(
    fill = 'skyblue',
    aes(ymax = estimate + 2 * std_error, ymin = estimate - 2 * std_error),
    alpha = 0.8
  ) +
  # geom_point(size = 0.35, alpha = 0.2) +
  geom_smooth(
    # color = 'blue',
    method = 'loess',
    formula = 'y~x'
  ) +
  labs(
    x = 'Seconds Left',
    y = 'Coefficient Estimate',
    title = 'Win Probability Model Coefficients Over Time'
  )
p1

ggsave(
  plot = p1,
  filename = file.path('coefs.png'),
  h = 6,
  w = 9,
  type = 'cairo'
)


coefs$estimate[coefs$max_time <= 2 & coefs$coefficient == 'favored_by'] <- 0
coefs

pts_diff_smooth <- 
  loess(
    estimate ~ max_time, 
    data = filter(coefs, coefficient == 'pts_diff'),
    span = 0.5
  )

fb_smooth <- 
  loess(
    estimate ~ max_time, 
    data = filter(coefs, coefficient == 'favored_by'),
    span = 0.5
  )

logit <- function(x) {
  tmp <- exp(x)
  case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    TRUE ~ tmp/(1 + tmp)
  )
}

# secs_to_model <- function(sec, msec) {
#   offset <- msec - 2400
#   if(offset == 300 & sec > offset) {
#     sec <- sec - offset
#   }
#   if(offset == 600) {
#     if(sec > 600) {
#       sec <- sec - offset
#     }
#     else if (sec < 600 & sec > 300) {
#       sec <- sec - 300
#     }
#   }
#   else if(offset == 900) {
#     if(sec > 900) {
#       sec <- sec - offset
#     }
#     else if (sec <= 900 & sec > 600) {
#       sec <- sec - 600
#     }
#     else if (sec <= 600 & sec > 300) {
#       sec <- sec - 300
#     }
#   }
#   else if(offset == 1200) {
#     if(sec > 1200) {
#       sec <- sec - offset
#     }
#     else if (sec <= 1200 & sec > 900) {
#       sec <- sec - 900
#     }
#     else if (sec <= 900 & sec > 600) {
#       sec <- sec - 600
#     }
#     else if (sec <= 600 & sec > 300) {
#       sec <- sec - 300
#     }
#   }
#   else if(offset == 1500) {
#     if(sec > 1500) {
#       sec <- sec - offset
#     }
#     else if (sec <= 1500 & sec > 1200) {
#       sec <- sec - 1200
#     }
#     else if (sec <= 1200 & sec > 900) {
#       sec <- sec - 900
#     }
#     else if (sec <= 900 & sec > 600) {
#       sec <- sec - 600
#     }
#     else if (sec <= 600 & sec > 300) {
#       sec <- sec - 300
#     }
#   }
#   
#   else if(offset == 1800) {
#     if(sec > 1800) {
#       sec <- sec - offset
#     }
#     else if (sec <= 1800 & sec > 1500) {
#       sec <- sec - 1500
#     }
#     else if (sec <= 1500 & sec > 1200) {
#       sec <- sec - 1200
#     }
#     else if (sec <= 1200 & sec > 900) {
#       sec <- sec - 900
#     }
#     else if (sec <= 900 & sec > 600) {
#       sec <- sec - 600
#     }
#     else if (sec <= 600 & sec > 300) {
#       sec <- sec - 300
#     }
#   }
#   
#   if(sec == 0) {
#     m <- 1
#   }
#   else if(sec >= 1 & sec < 5) {
#     m <- 5
#   }
#   else if(sec >= 5 & sec <= 10) {
#     m <- 10
#   }
#   else if(sec > 10 & sec <= 30) {
#     m <- sec + 1
#   }
#   else if(sec > 30 & sec <= 60) {
#     m <- 31 + floor((sec - 30)/2)
#   }
#   else if(sec > 60 & sec < 2400) {
#     m <- 46 + floor((sec - 60)/10)
#   }
#   else{
#     m <- 279
#   }
#   return(c(m, sec))
# }

df_tst_filt <- df_tst %>% filter(game_id == mil_atl_g3_id)
df_tst_filt

compute_wp <- function(x) {
  if(is.na(x$favored_by[1])) {
    x$favored_by <- 0
  }

  pd <- predict(pts_diff_smooth, newdata = x$sec_left)
  fb <- predict(fb_smooth, newdata = x$sec_left)
  
  index <- x$sec_left == 0 & (x$pts_1 != x$pts_2)
  pd[index] <- 20
  fb[index] <- predict(fb_smooth, newdata = 1)
  
  log_odds <- pd * x$pts_diff  + fb * x$favored_by
  
  logit(log_odds)
}

df_tst_filt$win_prob <- df_tst_filt %>% compute_wp()
df_tst_filt$win_prob

df_tst_filt %>% 
  tail(455) %>% 
  ggplot() +
  aes(x = 2400 - sec_left, y = win_prob) +
  scale_x_reverse(labels = scales::comma, breaks = seq(2400, 0, -400), limits = c(2400, 0)) +
  geom_point(size = 0.5) +
  geom_step() +
  labs(
    x = 'Seconds Left',
    y = 'Win Probability',
    title = 'Win Probability Model Coefficients Over Time'
  )
  
# df_tst_filt$secs_remaining_relative <- NA
# msec <- max(df_tst_filt$sec_left)
# for(k in 1:nrow(df_tst_filt)) {
#   df_tst_filt$secs_remaining_relative[k] <-
#     secs_to_model(df_tst_filt$sec_left[k], msec)[2]
# }
# df_tst_filt
# df_tst_filt %>% filter(secs_remaining_relative!=sec_left)


