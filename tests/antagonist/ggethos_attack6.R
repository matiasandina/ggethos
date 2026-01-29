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
