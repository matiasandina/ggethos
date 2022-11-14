library(tidyverse)
library(lubridate)
set.seed(1)

generate_behaviours <- function(n) {
  wombat_behaviours <- c("snuffling" = 3, "digging" = 5, "pondering" = 9, "nothing" = 8)
  next_behaviour <- function(last_behaviour, ...) {
    if (runif(1) < 1 / wombat_behaviours[last_behaviour]) {
      last_behaviour <- sample(names(wombat_behaviours), 1)
    }
    last_behaviour
  }
  behaviours <- accumulate(
    rep(sample(names(wombat_behaviours), 1, prob = wombat_behaviours), n),
    next_behaviour
  )
  na_if(behaviours, "nothing")
}

# Behaviours are implied to be presented in order with a fixed interval between
# observations, as if they were recorded from a film. NA represents a
# frame where there was no behaviour observed/assigned
wombats_ordered <- tibble(wombat = c("jerry", "pomelo", "speedy", "gimli")) %>%
  mutate(trial = 1:4) %>%
  expand(wombat, trial) %>%
  mutate(n_observations = round(rnorm(16, mean = 50, sd = 15))) %>%
  mutate(behaviour = lapply(n_observations, generate_behaviours),
         trial_frame = purrr::map(n_observations, ~1:.x)) %>%
  select(-n_observations) %>%
  unnest(c(behaviour, trial_frame))

# Behaviours are recorded at regular 5-second intervals, with 'seconds'
# representing seconds elapsed since the start of the experiment.
# 'seconds' is measured relative to the start of the experiment rather than each trial to allow
# testing of the 'align_trials' argument
sampling_period <- 5
wombats_ordered <- wombats_ordered %>%
  group_by(wombat) %>%
  mutate(frame = 1:n(),
         seconds = seq(from = 0, by = sampling_period, length.out = n()))

# As in `seconds`, except that rather then the time of an observation
# being given as seconds elapsed since the start of the experiment, it is given
# as a datetime
wombats_ordered <- wombats_ordered %>%
  group_by(wombat) %>%
  nest() %>%
  mutate(exp_dt = as_datetime(rnorm(n(), 1577880000, 10000000))) %>%
  unnest(data) %>%
  mutate(
    start_dt = exp_dt + dseconds(seconds),
    end_dt = lead(start_dt, 1, default=last(start_dt) + sampling_period))

wombats <- wombats_ordered %>% ungroup()
usethis::use_data(wombats, overwrite = TRUE)

wombats_summ <-
  wombats_ordered %>%
  group_by(wombat, trial) %>%
  summarise(exp_dt = unique(exp_dt),
            trial_start = first(seconds),
            trial_end = last(seconds))

# Observed behaviours are recorded with their start and end times, given in
# seconds since the start of the trial. There are no NA values; if a NA value
# was to be included it would probably represent missing data
wombats_duration <- wombats_ordered %>%
  ungroup() %>%
  select(wombat, trial, behaviour) %>%
  mutate(behaviour = replace_na(behaviour, "nothing")) %>%
  nest(behaviours = behaviour) %>%
  mutate(behaviours = map(behaviours, ~ .x$behaviour)) %>%
  mutate(rle = map(behaviours, rle)) %>%
  mutate(behaviours = map(rle, ~ data.frame(behaviour = .x$values, duration = .x$lengths))) %>%
  select(-rle) %>%
  unnest(behaviours) %>%
  group_by(wombat, trial) %>%
  mutate(end_seconds = cumsum(duration)) %>%
  mutate(start_seconds = end_seconds - duration) %>%
  ungroup() %>%
  select(wombat, trial, behaviour, start_seconds, end_seconds) %>%
  filter(! behaviour == "nothing")

# As in wombats_duration, except that start and end times are given as datetimes
wombats_duration <- wombats_duration %>%
  left_join(wombats_summ, by=c("wombat", "trial")) %>%
  mutate(start_seconds = start_seconds + trial_start,
         end_seconds = ifelse(trial == 1,
                              end_seconds,
                              end_seconds+trial_end),
         start_dt = exp_dt + dseconds(start_seconds),
         end_dt = exp_dt + dseconds(end_seconds))

usethis::use_data(wombats_duration, overwrite = TRUE)

