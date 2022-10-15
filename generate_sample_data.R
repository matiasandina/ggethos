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
# observations. NA represents an observation where there was no behaviour
# observed
wombats_ordered <- tibble(wombat = c("jerry", "pomelo", "speedy", "gimli")) %>%
  mutate(trial = 1:4) %>%
  expand(wombat, trial) %>%
  mutate(n_observations = round(rnorm(16, mean = 50000, sd = 15000))) %>%
  mutate(behaviour = lapply(n_observations, generate_behaviours)) %>%
  select(-n_observations) %>%
  unnest(behaviour)

# Behaviours are recorded for each consecutive frame of a film. NA represents a
# frame where there was no behaviour observed
wombats_frame <- wombats_ordered %>%
  group_by(wombat, trial) %>%
  mutate(frame = 1:n()) %>%
  ungroup() %>%
  select(wombat, trial, frame, behaviour)

# Behaviours are recorded at regular 5-second intervals, with 'seconds'
# representing seconds elapsed since the start of the trial. NA represents an
# observation where there was no behaviour observed
wombats_seconds <- wombats_frame %>%
  mutate(seconds = 5 * frame) %>%
  select(wombat, trial, seconds, behaviour)

# As in wombats_seconds, except that rather then the time of an observation
# being given as seconds elapsed since the start of the trial, it is given as a
# datetime
wombats_dt <- wombats_seconds %>%
  nest(seconds, behaviour) %>%
  mutate(start_dt = as_datetime(rnorm(n(), 1577880000, 100000000))) %>%
  unnest(data) %>%
  mutate(dt = start_dt + dseconds(seconds)) %>%
  select(wombat, trial, dt, behaviour)

# Observed behaviours are recorded with their start and end times, given in
# seconds since the start of the trial. There are no NA values; if a NA value
# was to be included it would probably represent missing data
wombats_duration <- wombats_ordered %>%
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
wombats_duration_dt <- wombats_duration %>%
  nest(behaviour, start_seconds, end_seconds) %>%
  mutate(trial_start_dt = as_datetime(rnorm(n(), 1577880000, 100000000))) %>%
  unnest(data) %>%
  mutate(start_dt = trial_start_dt + dseconds(start_seconds)) %>%
  mutate(end_dt = trial_start_dt + dseconds(end_seconds)) %>%
  select(wombat, trial, behaviour, start_dt, end_dt)

use_data(wombats_ordered, overwrite = TRUE)
use_data(wombats_frame, overwrite = TRUE)
use_data(wombats_seconds, overwrite = TRUE)
use_data(wombats_dt, overwrite = TRUE)
use_data(wombats_duration, overwrite = TRUE)
use_data(wombats_duration_dt, overwrite = TRUE)
