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

# Ambiguous or conflicting inputs
run_case("intervals: negative durations", quote({
  df <- data.frame(x = c(5, 4), xend = c(3, 2), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("intervals: zero duration", quote({
  df <- data.frame(x = c(1, 2), xend = c(1, 2), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("auto mode: xend only", quote({
  df <- data.frame(xend = c(1, 2), y = 1, behaviour = c("a", "a"))
  compute_ethogram(df, mode = "auto", y = y, behaviour = behaviour)
}))

# Input type pitfalls
run_case("implied: list behaviour", quote({
  df <- data.frame(y = 1)
  df$behaviour <- list(c("a"), c("a"), c("b"))
  compute_implied(df, y, behaviour)
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
