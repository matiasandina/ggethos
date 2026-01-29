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

# Mixed precision and rounding
run_case("samples: fractional seconds repeated", quote({
  df <- data.frame(x = c(0.1, 0.2, 0.2, 0.3), y = 1, behaviour = c("a", "a", "b", "b"))
  compute_samples(df, x, y, behaviour)
}))

# Outliers distort min diff
run_case("samples: extreme outlier gap", quote({
  df <- data.frame(x = c(0, 1, 2, 1000), y = 1, behaviour = c("a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Non-monotonic within groups
run_case("samples: non-monotonic within group", quote({
  df <- data.frame(x = c(0, 2, 1, 3, 2), y = 1, behaviour = c("a", "a", "a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Mixed types in behaviour
run_case("samples: behaviour factor", quote({
  df <- data.frame(x = c(0, 1, 2), y = 1, behaviour = factor(c("a", "a", "b")))
  compute_samples(df, x, y, behaviour)
}))

# Implied mode with duplicated rows
run_case("implied: duplicated rows", quote({
  df <- data.frame(y = c(1, 1, 1), behaviour = c("a", "a", "a"))
  compute_implied(df, y, behaviour)
}))

# compute_ethogram auto with x but no y
run_case("auto mode: x without y", quote({
  df <- data.frame(x = c(0, 1), behaviour = c("a", "a"))
  compute_ethogram(df, mode = "auto", x = x, behaviour = behaviour)
}))

# compute_ethogram auto with xend and x but missing y
run_case("auto mode: x/xend without y", quote({
  df <- data.frame(x = c(0, 1), xend = c(1, 2), behaviour = c("a", "a"))
  compute_ethogram(df, mode = "auto", x = x, xend = xend, behaviour = behaviour)
}))

# compute_intervals with character x/xend
run_case("intervals: x/xend as character", quote({
  df <- data.frame(x = c("1", "2"), xend = c("2", "3"), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

# colour mapping with missing values
run_case("samples: colour with NA", quote({
  df <- data.frame(x = c(0, 1), y = 1, behaviour = c("a", "a"), colour = c("red", NA))
  compute_samples(df, x, y, behaviour)
}))
