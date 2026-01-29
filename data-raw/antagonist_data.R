antagonist_cases <- list(
  mixed_conditions = data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "b", "b"),
    condition = c(1, 1, 2, 2)
  ),
  multiple_subjects_shared_y = data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "a", "a"),
    subject = c("s1", "s1", "s2", "s2")
  ),
  large_gap_same_behaviour = data.frame(
    x = c(0, 1, 100, 101),
    y = 1,
    behaviour = c("a", "a", "a", "a")
  ),
  time_reset_no_group = data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "b", "b"),
    trial = c("t1", "t1", "t2", "t2")
  ),
  overlapping_intervals = data.frame(
    x = c(0, 1, 1.5),
    xend = c(2, 3, 4),
    y = 1,
    behaviour = c("a", "b", "c")
  ),
  conflicting_labels_same_time = data.frame(
    x = c(1, 1, 2, 2),
    y = 1,
    behaviour = c("a", "b", "a", "b")
  )
)

save(antagonist_cases, file = "data/antagonist_cases.rda")

antagonist_cases$unsorted_datetime <- data.frame(
  x = as.POSIXct(c("2020-01-01 00:00:10",
                   "2020-01-01 00:00:05",
                   "2020-01-01 00:00:20"), tz = "UTC"),
  y = 1,
  behaviour = c("a", "a", "a")
)

antagonist_cases$time_reset_no_group <- data.frame(
  x = c(0, 1, 0, 1),
  y = 1,
  behaviour = c("a", "a", "b", "b"),
  trial = c("t1", "t1", "t2", "t2")
)

save(antagonist_cases, file = "data/antagonist_cases.rda")

antagonist_cases$overlapping_intervals2 <- data.frame(
  x = c(0, 1, 1.2),
  xend = c(2, 2.5, 3.5),
  y = 1,
  behaviour = c("a", "b", "c")
)

antagonist_cases$duplicated_times_conflict <- data.frame(
  x = c(0.2, 0.2, 0.3, 0.3),
  y = 1,
  behaviour = c("a", "b", "a", "b")
)

save(antagonist_cases, file = "data/antagonist_cases.rda")
