library(tidyverse)
load_all()

# simulate data
samples <- 50
subjects <- 4
sampling_frequency <- 100 # in Hz
trial_id <- paste0("trial_", sort(rep(1:5, samples/5)))
df <- tibble::tibble(
  sample_n = rep(1:samples, subjects),
  time_sec = sample_n * 1/sampling_frequency,
  duration_sec = sample(1:10, 200, TRUE) * 0.001,
  trial_n = rep(trial_id, subjects),
  id = sort(rep(paste0("id", 1:subjects), samples)),
  target = sample(LETTERS[1:3],
                    size = samples*subjects,
                    replace=TRUE,
                    prob=c(0.75, 0.15, 0.1))
) %>%
  mutate(time_end = time_sec + duration_sec)

# Case with x and xend
ggplot(df, aes(x = time_sec, xend = time_end, y = id, colour = target)) +
  geom_ethogram()

# Case with x but no xend (behaviour assumed to continue until next behaviour
# in the same panel, or until the end of the plot)
ggplot(df, aes(x = time_sec, y = id, color = target)) +
  geom_ethogram()

# Case with no x
ggplot(df, aes(y = id, color = target))+
  geom_ethogram()

# With faceting
ggplot(df, aes(x = time_sec, y = trial_n, color = target))+
  geom_ethogram() +
  facet_wrap(~id)
