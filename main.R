
# setup ----
# Some of this code and the ideas are borrowed from https://github.com/lbenz730/Senior-Thesis.
# I make adjustments (and re-factor the code) where I see fit.
library(tidyverse)
library(scales)
library(broom)
library(hoopR) # saiemgilani/hoopR
library(tonythemes) # tonyelhabr/tonythemes
library(ggforce)
library(patchwork)
library(ggtext)
library(glue)
tonythemes::theme_set_tony()

# data prep ----
# Unfortunately this data set doesn't have dates, but one can find the start of the
# 2021 playoffs and the MIL@ATL G3 game pretty easily by browsing ESPN's website. (game id's are from ESPN.)
pbp <- hoopR::load_nba_pbp(2020:2021)

playoff_2021_id <- 401326988 # first game of 2021 NBA playoffs
mil_atl_g3_id <- 401337342 # MIL@ATL G3 game

df_h <-
  pbp %>%
  mutate(
    favored_by = -game_spread,
    quarter_left = ifelse((4 - period) < 0, 0, 4 - period),
    # Have to recalculate the seconds remaining cuz the original data looks wrong to me.
    sec_left = (quarter_left) * (60 * 12) + start_quarter_seconds_remaining
  ) %>% 
  select(
    id,
    game_id,
    sec_left,
    pts_h = home_score,
    pts_a = away_score,
    favored_by
  ) %>%
  group_by(game_id) %>%
  mutate(
    pts_diff = pts_h - pts_a,
    w = ifelse(max(pts_h) > max(pts_a), 1, 0)
  ) %>%
  ungroup()
df_h
# max(df_h$sec_left)

# See https://github.com/lbenz730/Senior-Thesis/blob/master/clean.R#L2
# Basically, repeat the data, making it symmetrical across home and away teams.
# This means there isn't a explicity home variable, but the pre-game spread accounts for both home/away
# and team strength
df <-
  bind_rows(
    df_h %>% 
      rename(pts_1 = pts_h, pts_2 = pts_a) %>% 
      mutate(is_h = TRUE),
    df_h %>%
      rename(pts_1 = pts_a, pts_2 = pts_h) %>%
      mutate(
        is_h = FALSE,
        across(c(pts_diff, favored_by), ~ -.x),
        across(w, ~ {
          1 - .x
        })
      )
  )
df
df %>% arrow::write_parquet('pbp.parquet') # just so y'all can see my data

game_ids <- df %>% distinct(game_id)
game_ids

game_ids_trn <- game_ids %>% filter(game_id < playoff_2021_id)
game_ids_tst <- game_ids %>% filter(game_id >= playoff_2021_id)
df_trn <- df %>% semi_join(game_ids_trn)
df_tst <- df %>% semi_join(game_ids_tst)

# See https://github.com/lbenz730/Senior-Thesis/blob/master/fit_model.R
# This is basically a re-factored version of that code.
max_sec <- c(seq(2880, 720, -10), seq(720, 100, -5), seq(100, 10, -1), seq(10, 2, -1), 1)
min_sec <- c(seq(2780, 620, -10), seq(670, 50, -5), seq(95, 5, -1), seq(8, 0, -1), 0)
max_sec %>% length()
min_sec %>% length()
n <- length(max_sec)

fit_window <- function(i, overwrite = TRUE) {
  # path <- file.path('data', sprintf('coefs_%03d.csv', i))
  # if (file.exists(path) & !overwrite) {
  #   return(read_csv(path))
  # }
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

coefs <- 1:n %>% map_dfr(fit_window, overwrite = T)
coefs %>% write_csv(file.path('coefs.csv'))
coefs

# plot coefs ----
# See https://github.com/lbenz730/Senior-Thesis/blob/master/chapter_3_graphics.R#L9. 
p_coefs <-
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
  scale_x_reverse(labels = scales::comma, breaks = seq(2880, 0, -360), limits = c(2880, 0)) +
  facet_wrap(~coefficient, ncol = 1, scales = 'free_y') +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_point(size = 0.5) +
  geom_ribbon(
    fill = 'skyblue',
    aes(ymax = estimate + 2 * std_error, ymin = estimate - 2 * std_error),
    alpha = 0.8
  ) +
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
p_coefs

ggsave(
  plot = p_coefs,
  filename = file.path('coefs.png'),
  h = 6,
  w = 9,
  type = 'cairo'
)

# various functions ----
coefs$estimate[coefs$max_time <= 2 & coefs$coefficient == 'favored_by'] <- 0
coefs

pts_diff_smooth <- 
  loess(
    estimate ~ max_time, 
    data = filter(coefs, coefficient == 'pts_diff'),
    span = 0.5
  )
pts_diff_smooth

fb_smooth <- 
  loess(
    estimate ~ max_time, 
    data = filter(coefs, coefficient == 'favored_by'),
    span = 0.5
  )
fb_smooth

logit <- function(x) {
  tmp <- exp(x)
  case_when(
    tmp == Inf ~ 1,
    tmp == -Inf ~ 0,
    TRUE ~ tmp/(1 + tmp)
  )
}


compute_wp <- function(x) {
  if(is.na(x$favored_by[1])) {
    x$favored_by <- 0
  }

  # game features
  pd <- predict(pts_diff_smooth, newdata = x$sec_left)
  fb <- predict(fb_smooth, newdata = x$sec_left)
  
  index <- x$sec_left == 0 & (x$pts_1 != x$pts_2)
  pd[index] <- 20
  fb[index] <- predict(fb_smooth, newdata = 1)
  
  # log odds of winning
  log_odds <- pd * x$pts_diff  + fb * x$favored_by
  
  logit(log_odds)
}

log_loss <- function(x, y) {
  eps <- 1e-16
  x[x < eps] <- eps
  x[x > (1 - eps)] <- 1 - eps
  return(-mean(y * log(x) + (1-y) * log(1-x)))
}

mse <- function(x, y) {
  return(mean((x-y)^2))
}

misclass_rate <- function(x, y) {
  return(mean(round(x) != y))
}

# evaluation ----
do_compute_wp <- function(df) {
  df[['win_prob']] <- df %>% compute_wp() %>% coalesce(0.5)
  df
}

df_trn <- df_trn %>% do_compute_wp()
df_tst <- df_tst %>% do_compute_wp()

do_compute_metrics <- function(df, .set) {
  tibble(
    n = nrow(df),
    ll = log_loss(df[['win_prob']], df[['w']]),
    mse = mse(df[['win_prob']], df[['w']]),
    misclass_rate = misclass_rate(df[['win_prob']], df[['w']])
  ) %>% 
    mutate(set = .set)
}

mets <-
  bind_rows(
    do_compute_metrics(df_trn, 'train'),
    do_compute_metrics(df_tst, 'test')
  ) %>% 
  pivot_longer(-set) %>% 
  pivot_wider(names_from = name, values_from = value)
mets


do_compute_metrics_window <- function(df, i, ...) {
  # cat(glue::glue('window {i} of {n}'), sep = '\n')
  df_filt <- df %>% filter(sec_left >= min_sec[i], sec_left <= max_sec[i])
  df_filt %>%
    do_compute_metrics(...) %>%
    mutate(min_time = min_sec[i], max_time = max_sec[i])
}

mets_window <-
  1:n %>% 
  map_dfr(~do_compute_metrics_window(df_tst, .x, 'test'))
mets_window

p_mets <-
  mets_window %>% 
  select(-c(set, n)) %>% 
  pivot_longer(
    -c(min_time, max_time)
  ) %>% 
  ggplot() +
  aes(x = max_time, y = value, group = name) +
  scale_x_reverse(labels = scales::comma, breaks = seq(2880, 0, -360), limits = c(2880, 0)) +
  facet_wrap(~name, ncol = 1, scales = 'free_y') +
  geom_point(size = 0.5) +
  labs(
    x = 'Seconds Left',
    subtitle = 'Test Set',
    title = 'Win Probability Model Error Over Time'
  )
p_mets

ggsave(
  plot = p_mets,
  filename = file.path('error_over_time.png'),
  h = 9,
  w = 9,
  type = 'cairo'
)

# win prob example ----
df_tst_filt <- df_tst %>% filter(game_id == mil_atl_g3_id)
df_tst_filt

# There are 5 events when the game is 44-42 (around 5:40 in Q2), and we know that the third record is when 5:40 actually occurs.
win_prob_filt <-
  df_tst_filt %>% 
  filter(pts_1 == 42, pts_2 == 44) %>% 
  slice(3)
win_prob_filt

# Colors source
# teamcolors::teamcolors %>% filter(league == 'nba') %>% filter(name %>% str_detect('Atlanta|Milwaukee'))
pal <- c('MIL' = '#00471b', 'ATL' = '#e13a3e')
# scales::show_col(pal)
qs <- 
  tibble(q = 1:4) %>% 
  mutate(sec_left = (4 - q) * 12 * 60)

pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

p_win_prob_filt <-
  df_tst_filt %>% 
  ggplot() +
  aes(x = sec_left, y = win_prob) +
  geom_vline(
    data = qs,
    aes(xintercept = sec_left),
    linetype = 2
  ) +
  geom_hline(
    data = tibble(),
    aes(yintercept = 0.5),
    linetype = 1
  ) +
  geom_text(
    inherit.aes = FALSE,
    data = qs,
    family = 'Karla',
    angle = 90,
    size = pts(14),
    hjust = 1,
    nudge_x = -60,
    aes(x = sec_left, y = 0.9, label = sprintf('End of Q%d', q))
  ) +
  geom_step(
    size = 1,
    data = df_tst_filt %>% 
      mutate(
        across(
          win_prob,
          ~case_when(
            .x >= 0.5 & is_h ~ 1 - .x,
            .x >= 0.5 & !is_h ~ .x
          )
        ) 
      ) %>% 
      drop_na(win_prob) %>% 
      arrange(desc(sec_left)),
    color = 'grey50'
  ) +
  geom_step(
    size = 1,
    aes(group = is_h),
    data =
      df_tst_filt %>% 
      mutate(
        across(
          win_prob,
          ~case_when(
            .x >= 0.5 & is_h ~ 1 - .x,
            TRUE ~ NA_real_
          )
        ) 
      ),
    color = pal['ATL']
  ) +
  geom_step(
    size = 1,
    aes(group = is_h),
    data =
      df_tst_filt %>% 
      mutate(
        across(
          win_prob,
          ~case_when(
            .x >= 0.5 & !is_h ~ .x,
            TRUE ~ NA_real_
          )
        ) 
      ),
    color = pal['MIL']
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_reverse(labels = scales::comma, breaks = seq(2880, 0, -360), limits = c(2880, 0)) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor.y = element_line()
  ) +
  labs(
    x = NULL,
    y = 'Win Probability'
  ) +
  ggforce::geom_mark_circle(
    data = win_prob_filt,
    expand =  unit(0.001, 'mm'),
    colour = 'black',
    label.family = 'Karla',
    size = pts(14),
    aes(label = 'Q2 5:40', description = glue::glue('{scales::percent(win_prob)} (MIL {pts_1} - {pts_2} ATL)'))
  )
p_win_prob_filt

p_pts_diff_filt <-
  df_tst_filt %>% 
  ggplot() +
  aes(x = sec_left, y = pts_diff) +
  geom_vline(
    data = qs,
    aes(xintercept = sec_left),
    linetype = 2
  ) +
  geom_hline(
    aes(yintercept = 0L),
    linetype = 1
  ) +
  geom_step(
    size = 1,
    data = 
      df_tst_filt %>% 
      mutate(
        across(
          pts_diff,
          ~case_when(
            .x >= 0L & is_h ~ - .x,
            .x >= 0L & !is_h ~ .x,
            TRUE ~ NA_integer_
          )
        ) 
      ) %>% 
      drop_na(pts_diff) %>% 
      arrange(desc(sec_left)),
    color = 'grey50'
  ) +
  geom_step(
    size = 1,
    aes(group = is_h),
    data =
      df_tst_filt %>% 
      mutate(
        across(
          pts_diff,
          ~case_when(
            .x >= 0L & is_h ~ - .x,
            TRUE ~ NA_integer_
          )
        ) 
      ),
    color = pal['ATL'] # colorspace::desaturate(pal['ATL'], 0.5)
  ) +
  geom_step(
    size = 1,
    aes(group = is_h),
    data =
      df_tst_filt %>% 
      mutate(
        across(
          pts_diff,
          ~case_when(
            .x >= 0L & !is_h ~ .x,
            TRUE ~ NA_integer_
          )
        ) 
      ),
    color = pal['MIL'] # colorspace::desaturate(pal['MIL'], 0.5)
  ) +
  scale_x_reverse(labels = scales::comma, breaks = seq(2880, 0, -360), limits = c(2880, 0)) +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, by = 5), labels = sprintf('%+d', seq(-15, 15, by = 5))) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = 'Score Margin'
  )
p_pts_diff_filt

p <- 
  patchwork::wrap_plots(p_win_prob_filt, p_pts_diff_filt, ncol = 1, heights = c(2, 1)) +
  patchwork::plot_annotation(
    title = glue::glue('<span style="color:{pal["MIL"]}">Milwaukee</span> @ <span style="color:{pal["ATL"]}">Atlanta</span>, Eastern Conference Finals Game 3, 6/27/2021'),
    subtitle = glue::glue('From <span style="color:{pal["MIL"]}">Milwaukee</span>\'s Perspective'),
    caption = glue::glue('<b><span style="color:black">Other win probability models at Q2 5:40</span></b>:<br/>Inpredict: 58.2%, Gambletron2000: 57.2%, ESPN: 52.3%'),
    theme = theme(
      plot.title = ggtext::element_markdown(face = 'bold', size = 18, hjust = 0, color = 'grey20'),
      plot.subtitle = ggtext::element_markdown(face = 'bold', size = 16, hjust = 0, color = 'grey50'),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 0, color = 'grey20')
    )
  )
p

ggsave(
  plot = p,
  filename = file.path('win_prob_example.png'),
  width = 12,
  height = 6,
  type = 'cairo'
)

