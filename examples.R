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
  geom_ethogram() +
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
