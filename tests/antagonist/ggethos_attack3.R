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

# Human mistakes: merged conditions without faceting/grouping
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

# Large gaps but same behaviour collapse into one run
run_case("samples: large gap same behaviour", quote({
  df <- data.frame(x = c(0, 1, 100, 101), y = 1,
                   behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Time reversal inside same behaviour
run_case("samples: time reversal", quote({
  df <- data.frame(x = c(0, 2, 1, 3), y = 1,
                   behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Intervals with NA endpoints
run_case("intervals: NA in xend", quote({
  df <- data.frame(x = c(0, 2), xend = c(1, NA), y = 1,
                   behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

# Conflicting colour vs color columns
run_case("samples: colour vs color conflict", quote({
  df <- data.frame(x = c(0, 1), y = 1, behaviour = c("a", "a"),
                   colour = c("red", "red"), color = c("blue", "blue"))
  compute_samples(df, x, y, behaviour)
}))

# Group column with all NA merges everything
run_case("samples: group column all NA", quote({
  df <- data.frame(x = c(0, 1, 2), y = 1, behaviour = c("a", "b", "c"),
                   group = NA)
  compute_samples(df, x, y, behaviour)
}))

# Align by missing column
run_case("align: by missing column", quote({
  df <- data.frame(x = c(0, 1), xend = c(1, 2), y = 1)
  align_ethogram(df, by = missing_col)
}))
