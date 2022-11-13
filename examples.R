library(ggplot2)
library(ggethos)

# Observations with specified start and end times, expressed in seconds
ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# Observations with specified start and end times, expressed as datetimes
ggplot(wombats_duration, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial, scales = "free")

# Working: Behavior in the y axis
ggplot(wombats, aes(x=seconds, y = behaviour)) +
  geom_ethogram() +
  facet_wrap(~wombat)

ggplot(wombats, aes(x=seconds, y = behaviour)) +
  geom_ethogram() +
  facet_wrap(~wombat)

ggplot(wombats, aes(x=seconds, y = behaviour)) +
  geom_ethogram() +
  facet_wrap(trial~wombat)


# Currently failing -------------------------------------------------------

ggplot(wombats, aes(y=wombat)) +
  geom_ethogram(color="red") +
  facet_wrap(~trial)

ggplot(wombats, aes(y=wombat)) +
  geom_ethogram(aes(color=wombat)) +
  facet_wrap(~trial)

ggplot(wombats, aes(y=wombat)) +
  geom_ethogram(aes(color=behaviour)) +
  facet_wrap(~trial)

# This produces weird behavior, am I not understanding the syntax  ?
# By the way, none of the examples have y=behaviour
ggplot(wombats, aes(x=frame, y=behaviour, color=behaviour)) +
  geom_ethogram() +
  facet_wrap(trial~wombat, scales = "free")



# x scale issues ----------------------------------------------------------
# This gives no error, plot is empty, not sure what's going on
# is this because the range is so large ?!
# > range(wombats_duration$start_dt)
# [1] "2019-08-28 06:37:49 UTC" "2020-09-21 22:29:47 UTC"
# scale="free" fixes the errors
ggplot(wombats_duration, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial) +
  scale_x_datetime(labels=scales::label_time(format = "%H:%M"))

# This also gives no error and has an empty plot
ggplot(wombats_duration_dt, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
     geom_ethogram() +
     facet_wrap( ~ trial) +
  scale_x_datetime(labels=scales::label_time(format = "%H:%M"))

# This only produces the expected output if you add the extra time 1:n() variable
# wombats_seconds %>%
#   group_by(wombat) %>%
#   mutate(time = 1:n()) %>%
ggplot(wombats, aes(y = behaviour)) +
  geom_ethogram() +
  facet_wrap(~wombat)

# does not work as expected
ggplot(wombats, aes(y=wombat)) +
  geom_ethogram()

ggplot(wombats, aes(y=wombat)) +
  geom_ethogram(color="salmon")

# expected result is this, but one color
ggplot(wombats, aes(y=wombat, color=behaviour)) +
  geom_ethogram()

# does not work as expected
ggplot(wombats, aes(y=behaviour))+
  geom_ethogram() +
  facet_wrap(~wombat)

# does not work as expected
# NAs ar e filtered when color is specified, but the Y axis still has them
ggplot(wombats, aes(y=behaviour, color=behaviour))+
  geom_ethogram() +
  facet_wrap(~wombat)

# looks like there's overlap in behaviors
ggplot(wombats, aes(x=frame, y=behaviour, color=behaviour))+
  geom_ethogram() +
  facet_wrap(~wombat)
# same here
ggplot(wombats, aes(x=seconds, y=behaviour, color=behaviour))+
  geom_ethogram() +
  facet_wrap(~wombat, scales="free")

# expected output, although many bins
ggplot(wombats, aes(x=frame, y=behaviour))+
   geom_ethogram() +
  facet_wrap(trial~wombat)

# Produces overlapping intervals when lumping behaviors
ggplot(wombats, aes(x=frame, y=behaviour, color=behaviour))+
  geom_ethogram() +
  facet_wrap(trial~wombat)

ggplot(wombats, aes(x=seconds, y=behaviour, color=behaviour))+
  geom_ethogram() +
  facet_wrap(trial~wombat, scales="free")

# does not work as expected
ggplot(wombats, aes(x = frame, y = wombat)) +
  geom_ethogram() +
  facet_wrap(~ trial)
# expected
ggplot(wombats, aes(x = frame, y = wombat, color=behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# not the same output as the colored one
# is it just filtering of NAs ?
ggplot(wombats, aes(x = seconds, y = wombat)) +
  geom_ethogram() +
  facet_wrap(~ trial)

ggplot(wombats, aes(x = seconds, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# lumps all behaviors and creates overlap
ggplot(wombats, aes(x = seconds, y = behaviour, color=behaviour)) +
  geom_ethogram()

# expected output
ggplot(wombats, aes(x = seconds, y = behaviour)) +
  geom_ethogram()

# does not produce expected output
# is it just the filtering of NAs ?
ggplot(wombats, aes(x = seconds, y = wombat)) +
  geom_ethogram()

ggplot(wombats, aes(x = seconds, y = wombat, color=behaviour)) +
  geom_ethogram()


# issues with scale here
# unlikely people try to plot things like these
ggplot(wombats_duration,
       aes(x = as.numeric(start_dt), y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial , scales = "free")

# only visible when adding wombat to the facet_wrap
# I think it's unintuitive
ggplot(wombats_duration,
       aes(x = start_dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial , scales = "free")

ggplot(wombats_duration,
       aes(x = start_seconds, xend = end_seconds, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = trial, colour = behaviour)) +
  geom_ethogram(align_trials = TRUE) +
  facet_wrap(~ wombat)

# also not sure if it's getting rid of spaces that were previously NAs
ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = trial, colour = behaviour)) +
  geom_ethogram(align_trials = FALSE) +
  facet_wrap(~ wombat)
