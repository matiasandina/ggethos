library(dplyr)
library(rlang)
library(vctrs)
library(cli)
library(assertthat)
library(tidyselect)
library(magrittr)

source("/home/matias/Projects/ggethos/R/compute.R")

run_case <- function(name, expr) {
  cat("\n== ", name, " ==\n", sep = "")
  out <- tryCatch({
    val <- eval(expr)
    print(val)
    "ok"
  }, warning = function(w) {
    cat("warning: ", conditionMessage(w), "\n", sep = "")
    "warning"
  }, error = function(e) {
    cat("error: ", conditionMessage(e), "\n", sep = "")
    "error"
  })
  invisible(out)
}

# Interval inference and ordering
run_case("samples: NA in x", quote({
  df <- data.frame(x = c(1, NA, 3), y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: duplicated x", quote({
  df <- data.frame(x = c(1, 1, 1), y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: irregular intervals", quote({
  df <- data.frame(x = c(0, 5, 15), y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: unsorted x", quote({
  df <- data.frame(x = c(3, 1, 2), y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: explicit interval NA", quote({
  df <- data.frame(x = c(1, 2), y = 1, behaviour = c("a", "a"))
  compute_samples(df, x, y, behaviour, interval = NA_real_, interval_mode = "explicit")
}))

run_case("samples: fractional seconds repeated", quote({
  df <- data.frame(x = c(0.1, 0.2, 0.2, 0.3), y = 1, behaviour = c("a", "a", "b", "b"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: extreme outlier gap", quote({
  df <- data.frame(x = c(0, 1, 2, 1000), y = 1, behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: non-monotonic within group", quote({
  df <- data.frame(x = c(0, 2, 1, 3, 2), y = 1, behaviour = c("a", "a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: time reversal", quote({
  df <- data.frame(x = c(0, 2, 1, 3), y = 1, behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: large gap same behaviour", quote({
  df <- data.frame(x = c(0, 1, 100, 101), y = 1,
                   behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Ambiguous or conflicting inputs
run_case("intervals: negative durations", quote({
  df <- data.frame(x = c(5, 4), xend = c(3, 2), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("intervals: zero duration", quote({
  df <- data.frame(x = c(1, 2), xend = c(1, 2), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("intervals: overlapping segments", quote({
  df <- data.frame(x = c(0, 1, 1.5), xend = c(2, 3, 4), y = 1,
                   behaviour = c("a", "b", "c"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("intervals: NA in xend", quote({
  df <- data.frame(x = c(0, 2), xend = c(1, NA), y = 1,
                   behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("intervals: x/xend as character", quote({
  df <- data.frame(x = c("1", "2"), xend = c("2", "3"), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("auto mode: xend only", quote({
  df <- data.frame(xend = c(1, 2), y = 1, behaviour = c("a", "a"))
  compute_ethogram(df, mode = "auto", y = y, behaviour = behaviour)
}))

run_case("auto mode: x without y", quote({
  df <- data.frame(x = c(0, 1), behaviour = c("a", "a"))
  compute_ethogram(df, mode = "auto", x = x, behaviour = behaviour)
}))

run_case("auto mode: x/xend without y", quote({
  df <- data.frame(x = c(0, 1), xend = c(1, 2), behaviour = c("a", "a"))
  compute_ethogram(df, mode = "auto", x = x, xend = xend, behaviour = behaviour)
}))

# Input type pitfalls
run_case("implied: list behaviour", quote({
  df <- data.frame(y = 1)
  df$behaviour <- list(c("a"), c("a"), c("b"))
  compute_implied(df, y, behaviour)
}))

run_case("samples: behaviour as numeric", quote({
  df <- data.frame(x = c(0, 1, 2), y = 1, behaviour = c(1, 1, 2))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: behaviour as list", quote({
  df <- data.frame(x = c(0, 1), y = 1)
  df$behaviour <- list("a", "b")
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: behaviour factor", quote({
  df <- data.frame(x = c(0, 1, 2), y = 1, behaviour = factor(c("a", "a", "b")))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: x as character", quote({
  df <- data.frame(x = c("1", "2", "3"), y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: NA in y", quote({
  df <- data.frame(x = c(0, 1, 2), y = c(1, NA, 1), behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: explicit negative interval", quote({
  df <- data.frame(x = c(0, 1), y = 1, behaviour = c("a", "a"))
  compute_samples(df, x, y, behaviour, interval = -1, interval_mode = "explicit")
}))

run_case("implied: empty data", quote({
  df <- data.frame(y = numeric(0), behaviour = character(0))
  compute_implied(df, y, behaviour)
}))

# Human mistake patterns
run_case("samples: duplicated x conflicting labels", quote({
  df <- data.frame(x = c(1, 1, 2, 2), y = 1,
                   behaviour = c("a", "b", "a", "b"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: shuffled rows", quote({
  df <- data.frame(x = c(1, 3, 2, 4), y = 1,
                   behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("implied: multiple trials collapsed by y", quote({
  df <- data.frame(y = c(1, 1, 1, 1), behaviour = c("a", "b", "a", "b"),
                   trial = c("t1", "t1", "t2", "t2"))
  compute_implied(df, y, behaviour)
}))

run_case("samples: time reset without group", quote({
  df <- data.frame(x = c(0, 1, 0, 1), y = 1,
                   behaviour = c("a", "a", "b", "b"),
                   trial = c("t1", "t1", "t2", "t2"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: NA in behaviour within run", quote({
  df <- data.frame(x = c(1, 2, 3), y = 1, behaviour = c("a", NA, "a"))
  compute_samples(df, x, y, behaviour, remove_nas = FALSE)
}))

run_case("samples: mixed conditions without group", quote({
  df <- data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "b", "b"),
    condition = c(1, 1, 2, 2)
  )
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: multiple subjects sharing y", quote({
  df <- data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "a", "a"),
    subject = c("s1", "s1", "s2", "s2")
  )
  compute_samples(df, x, y, behaviour)
}))

# Datetime patterns
run_case("samples: datetime unsorted", quote({
  df <- data.frame(x = as.POSIXct(c("2020-01-01 00:00:10",
                                   "2020-01-01 00:00:05",
                                   "2020-01-01 00:00:20"), tz = "UTC"),
                   y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: datetime mixed tz", quote({
  x1 <- as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:10"), tz = "UTC")
  x2 <- as.POSIXct(c("2020-01-01 00:00:20", "2020-01-01 00:00:30"), tz = "America/New_York")
  df <- data.frame(x = c(x1, x2), y = 1, behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Aesthetics conflicts
run_case("samples: colour vs color conflict", quote({
  df <- data.frame(x = c(0, 1), y = 1, behaviour = c("a", "a"),
                   colour = c("red", "red"), color = c("blue", "blue"))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: colour with NA", quote({
  df <- data.frame(x = c(0, 1), y = 1, behaviour = c("a", "a"), colour = c("red", NA))
  compute_samples(df, x, y, behaviour)
}))

# Grouping pitfalls
run_case("samples: group column all NA", quote({
  df <- data.frame(x = c(0, 1, 2), y = 1, behaviour = c("a", "b", "c"),
                   group = NA)
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: column named group but not grouping", quote({
  df <- data.frame(x = c(0, 1, 0, 1), y = 1, behaviour = c("a", "a", "b", "b"),
                   group = c("g1", "g1", "g2", "g2"))
  compute_samples(df, x, y, behaviour)
}))

# Alignment pitfalls
run_case("align: missing x/xend", quote({
  df <- data.frame(y = 1, behaviour = c("a", "b"))
  align_ethogram(df, by = y)
}))

run_case("align: NA in x", quote({
  df <- data.frame(x = c(NA, 1, 2), xend = c(NA, 2, 3), y = 1)
  align_ethogram(df, by = y)
}))

run_case("align: by missing column", quote({
  df <- data.frame(x = c(0, 1), xend = c(1, 2), y = 1)
  align_ethogram(df, by = missing_col)
}))

run_case("align: by columns with NA", quote({
  df <- data.frame(x = c(0, 1, 2), xend = c(1, 2, 3), y = c(1, 1, NA), group = c("g1", "g1", "g1"))
  align_ethogram(df, by = c(y, group))
}))

# PANEL-driven edge cases
run_case("samples: PANEL separates identical y", quote({
  df <- data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "b", "b"),
    PANEL = c(1, 1, 2, 2)
  )
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: PANEL missing in some rows", quote({
  df <- data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "b", "b"),
    PANEL = c(1, NA, 2, NA)
  )
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: PANEL as character", quote({
  df <- data.frame(
    x = c(0, 1, 0, 1),
    y = 1,
    behaviour = c("a", "a", "b", "b"),
    PANEL = c("p1", "p1", "p2", "p2")
  )
  compute_samples(df, x, y, behaviour)
}))

run_case("intervals: PANEL with overlapping segments", quote({
  df <- data.frame(
    x = c(0, 1, 0.5, 1.5),
    xend = c(2, 3, 2, 3),
    y = 1,
    behaviour = c("a", "b", "a", "b"),
    PANEL = c(1, 1, 2, 2)
  )
  compute_intervals(df, x, xend, y, behaviour)
}))

# Timezone/DST edge cases
run_case("samples: DST spring forward gap", quote({
  df <- data.frame(
    x = as.POSIXct(
      c("2020-03-08 01:59:50", "2020-03-08 03:00:00", "2020-03-08 03:00:10"),
      tz = "America/New_York"
    ),
    y = 1,
    behaviour = c("a", "a", "a")
  )
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: DST fall back repeat", quote({
  df <- data.frame(
    x = as.POSIXct(
      c("2020-11-01 01:59:50", "2020-11-01 01:00:05", "2020-11-01 01:00:10"),
      tz = "America/New_York"
    ),
    y = 1,
    behaviour = c("a", "a", "a")
  )
  compute_samples(df, x, y, behaviour)
}))

# Performance/memory stress (intentionally heavy)
run_case("samples: 1e6 rows single group", quote({
  n <- 1e6
  df <- data.frame(
    x = seq_len(n),
    y = 1,
    behaviour = rep(c("a", "b"), length.out = n)
  )
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: many groups", quote({
  n <- 5e5
  df <- data.frame(
    x = rep(1:10, length.out = n),
    y = rep(seq_len(5000), length.out = n),
    behaviour = rep(c("a", "b", "c", "d"), length.out = n),
    group = rep(seq_len(5000), length.out = n)
  )
  compute_samples(df, x, y, behaviour, group = group)
}))

run_case("implied: 1e6 rows", quote({
  n <- 1e6
  df <- data.frame(
    y = 1,
    behaviour = rep(c("a", "b"), length.out = n)
  )
  compute_implied(df, y, behaviour)
}))

# Demo data probing
run_case("demo data: duplicate times conflict", quote({
  load("/home/matias/Projects/ggethos/data/ethogram_demo.rda")
  obj <- NULL
  for (nm in ls()) {
    if (is.data.frame(get(nm))) { obj <- get(nm); break }
  }
  if (is.null(obj)) stop("no data frame found")
  df <- obj
  if (!all(c("x", "y", "behaviour") %in% names(df))) {
    stop("demo data does not have x/y/behaviour")
  }
  df <- df[1:10, , drop = FALSE]
  df$x[1:2] <- df$x[1]
  df$behaviour[1:2] <- c("a", "b")
  compute_samples(df, x, y, behaviour)
}))
