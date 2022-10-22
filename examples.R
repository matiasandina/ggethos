library(ggplot2)

# Frames in implied order
load_all()
ggplot(wombats_ordered, aes(y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# Frames of uniform duration 1
load_all()
ggplot(wombats_frame, aes(x = frame, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# Observations at uniform 5-second intervals
load_all()
ggplot(wombats_seconds, aes(x = seconds, y = wombat, colour = behaviour)) +
  geom_ethogram(align_trials = TRUE) +
  facet_wrap(~ trial)

# Observations at specified datetimes with uniform 5-second intervals
load_all()
ggplot(wombats_dt, aes(x = dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial, scales = "free")

# Observations with specified start and end times, expressed in seconds
load_all()
ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# Observations with specified start and end times, expressed as datetimes
load_all()
ggplot(wombats_duration_dt, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial, scales = "free")

# This produces weird behavior, am I not understanding the syntax  ?
# By the way, none of the examples have y=behaviour
load_all()
ggplot(wombats_frame, aes(y=behaviour, x=frame, color=behaviour)) + 
  geom_ethogram() + 
  facet_wrap(~wombat, scales = "free")

# ^Fixed

# Produces weird plot, likely because trials share x value and the stats get confused
# In a real application you would probably have two time columns, one absolute and one within trial, not too worried about this
load_all()
ggplot(wombats_seconds, aes(x=seconds, y = behaviour)) + 
  geom_ethogram() + 
  facet_wrap(~wombat)

# ^Fixed

# Either one of the two lines below work, I'm a bit worried about the package producing wrong outputs but not failing with any errors. 
# These errors are glaring, but subtler errors might be difficult to catch.
load_all()
ggplot(wombats_seconds, aes(x=seconds, y = behaviour)) + 
  geom_ethogram() + 
  facet_wrap(trial~wombat)

# ^ Fixed

# library(tidyverse)
# wombats_seconds %>% 
#   filter(trial==1) %>%  
load_all()
ggplot(wombats_seconds, aes(x=seconds, y = behaviour)) + 
  geom_ethogram() + 
  facet_wrap(~wombat)

# ^ Fixed

# This gives no error, plot is empty, not sure what's going on
# is this because the range is so large ?!
# > range(wombats_duration_dt$start_dt)
#[1] "2016-10-02 02:07:31 UTC" "2028-02-11 18:42:51 UTC"
ggplot(wombats_duration_dt, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial) + 
  scale_x_datetime(labels=scales::label_time(format = "%H:%M"))

# This also gives no error and has an empty plot
ggplot(wombats_duration_dt, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
     geom_ethogram() +
     facet_wrap( ~ trial) + scale_x_datetime(labels=scales::label_time(format = "%H:%M"))

# ^ Above two just need scales = "free"

# This produces the expected output, but you have to add the extra time 1:n() variable
# wombats_seconds %>% 
#   group_by(wombat) %>% 
#   mutate(time = 1:n()) %>%  
load_all()
  ggplot(wombats_seconds, aes(y = behaviour)) + geom_ethogram() + facet_wrap(~wombat)

# ^Fixed

# This does not produce the output above, even though it's supposed to use the order of values on y to compute
# Again, I think the problem is that the split is on behavior because the y is behavior, but that changes the temporal order when it shouldn't
wombats_seconds %>% group_by(wombat) %>% mutate(time = 1:n()) %>%  ggplot(aes(y = behaviour)) + geom_ethogram() + facet_wrap(~wombat)

