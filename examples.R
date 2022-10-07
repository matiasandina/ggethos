source("ggethos.R")
# simulate data
samples <- 50
subjects <- 4
sampling_frequency <- 100 # in Hz
trial_id <- paste0("trial_", sort(rep(1:5, samples/5)))
df <- tibble::tibble(
  sample_n = rep(1:samples, subjects),
  time_sec = sample_n * 1/sampling_frequency,
  trial_n = rep(trial_id, subjects),
  id = sort(rep(paste0("id", 1:subjects), samples)),
  target = sample(LETTERS[1:3],
                    size = samples*subjects,
                    replace=TRUE,
                    prob=c(0.75, 0.15, 0.1))
)

# This is the most basic plot
# no x time is needed, x axis is number of samples
ggplot(df, aes(y=id, behavior=target, color=target))+
  geom_ethogram()


# add x axis in time units ------------------------------------------------
# This will do weird things
# it's because the ethogram() function it's not seeing x
# it get's the character vector
# group_by(y) %>% summarise(ethogram(behavior))  
ggplot(df, aes(x = time_sec, y=id, behavior=target, color=target))+
  geom_ethogram()


# plot behavior on the y axis ---------------------------------------------
# This is not working, because we are using group(y)
# it just messes things up
ggplot(df, aes(y=target, behavior=target, color=target))+
  geom_ethogram()

# align to trial ----
# this will fail because of lengths
# this has to construct a common x axis 
# the idea is to have all trials go from 0 to tmax
# something like group_by() %>% mutate(common_time = time - first(time))
# and after that calculate the ethogram by trial
ggplot(df, aes(x=time_sec, y=trial_id, behavior=target, color=target))+
  geom_ethogram() +
  facet_wrap(~id)
