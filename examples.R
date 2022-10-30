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


load_all()
ggplot(wombats_frame, aes(y=wombat)) + geom_ethogram(color="red") + facet_wrap(~trial)

load_all()
ggplot(wombats_frame, aes(y=wombat)) + geom_ethogram(aes(color=wombat)) + facet_wrap(~trial)

load_all()
ggplot(wombats_frame, aes(y=wombat)) + geom_ethogram(aes(color=behaviour)) + facet_wrap(~trial)

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


## More examples

# does not work as expected
ggplot(wombats_ordered, aes(y=wombat)) + 
  geom_ethogram()

ggplot(wombats_ordered, aes(y=wombat)) + 
  geom_ethogram(color="salmon")

# expected result is this, but one color
ggplot(wombats_ordered, aes(y=wombat, color=behaviour)) + 
  geom_ethogram()

# does not work as expected
ggplot(wombats_ordered, aes(y=behaviour))+
  geom_ethogram() +
  facet_wrap(~wombat) 

# does not work as expected
# NAs ar e filtered when color is specified, but the Y axis still has them 
ggplot(wombats_ordered, aes(y=behaviour, color=behaviour))+
  geom_ethogram() + 
  facet_wrap(~wombat)

# x axis is correct, multiple bins
# looks like there's overlap in behaviors ?
ggplot(wombats_frame, aes(x=frame, y=behaviour))+
  geom_ethogram() + 
  facet_wrap(~wombat)

# I guess I might be using the wrong x variable
ggplot(wombats_seconds, aes(x=seconds, y=behaviour))+
  geom_ethogram() + 
  facet_wrap(~wombat, scales="free")

# x axis is correct, NAs are filtered but still on y axis 
# looks like there's overlap in behaviors ?
ggplot(wombats_frame, aes(x=frame, y=behaviour, color=behaviour))+
  geom_ethogram() + 
  facet_wrap(~wombat)

# this one is doing weird things
ggplot(wombats_seconds, aes(x=seconds, y=behaviour, color=behaviour))+
  geom_ethogram() + 
  facet_wrap(~wombat, scales="free")

# expected output, although many bins
ggplot(wombats_frame, aes(x=frame, y=behaviour))+
   geom_ethogram() + 
  facet_wrap(trial~wombat)

ggplot(wombats_seconds, aes(x=seconds, y=behaviour))+
  geom_ethogram() + 
  facet_wrap(trial~wombat, scales="free")

  
# Produces overlapping intervals when lumping behaviors
ggplot(wombats_frame, aes(x=frame, y=behaviour, color=behaviour))+
  geom_ethogram() + 
  facet_wrap(trial~wombat)

ggplot(wombats_seconds, aes(x=seconds, y=behaviour, color=behaviour))+
  geom_ethogram() + 
  facet_wrap(trial~wombat, scales="free")

# does not work as expected
ggplot(wombats_frame, aes(x = frame, y = wombat)) +
  geom_ethogram() +
  facet_wrap(~ trial)
# expected
ggplot(wombats_frame, aes(x = frame, y = wombat, color=behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# not the same output as the colored one
# is it just filtering of NAs ?
ggplot(wombats_seconds, aes(x = seconds, y = wombat)) +
  geom_ethogram() +
  facet_wrap(~ trial)

ggplot(wombats_seconds, aes(x = seconds, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# lumps all behaviors and creates overlap
ggplot(wombats_seconds, aes(x = seconds, y = behaviour, color=behaviour)) +
  geom_ethogram()

# expected output
ggplot(wombats_seconds, aes(x = seconds, y = behaviour)) +
  geom_ethogram()

# does not produce expected output
# is it just the filtering of NAs ?
ggplot(wombats_seconds, aes(x = seconds, y = wombat)) +
  geom_ethogram(align_trials = FALSE) 

ggplot(wombats_seconds, aes(x = seconds, y = wombat, color=behaviour)) +
  geom_ethogram(align_trials = FALSE) 


# issues with scale here
# unlikely people try to plot things like these
ggplot(wombats_dt, aes(x = dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial , scales = "free")

# only visible when adding wombat to the facet_wrap
# I think it's unintuitive
ggplot(wombats_dt, aes(x = dt, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(wombat ~ trial , scales = "free")

ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = wombat, colour = behaviour)) +
  geom_ethogram() +
  facet_wrap(~ trial)

# I'm not sure but the behavior of align trials might be wrong here
ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = wombat, colour = behaviour)) +
  geom_ethogram(align_trials = TRUE) +
  facet_wrap(~ trial)

ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = trial, colour = behaviour)) +
  geom_ethogram(align_trials = TRUE) +
  facet_wrap(~ wombat)

# also not sure if it's getting rid of spaces that were previously NAs 
ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = trial, colour = behaviour)) +
  geom_ethogram(align_trials = FALSE) +
  facet_wrap(~ wombat)
