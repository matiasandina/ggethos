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

# Missing columns and name collisions
run_case("samples: no behaviour column", quote({
  df <- data.frame(x = c(1, 2), y = 1)
  compute_samples(df, x, y, behaviour)
}))

run_case("intervals: xend missing column", quote({
  df <- data.frame(x = c(1, 2), y = 1, behaviour = c("a", "b"))
  compute_intervals(df, x, xend, y, behaviour)
}))

run_case("samples: column named group but not grouping", quote({
  df <- data.frame(x = c(0, 1, 0, 1), y = 1, behaviour = c("a", "a", "b", "b"),
                   group = c("g1", "g1", "g2", "g2"))
  compute_samples(df, x, y, behaviour)
}))

# Non-atomic or mixed types
run_case("samples: behaviour as numeric", quote({
  df <- data.frame(x = c(0, 1, 2), y = 1, behaviour = c(1, 1, 2))
  compute_samples(df, x, y, behaviour)
}))

run_case("samples: behaviour as list", quote({
  df <- data.frame(x = c(0, 1), y = 1)
  df$behaviour <- list("a", "b")
  compute_samples(df, x, y, behaviour)
}))

# X as character strings (implicit conversion)
run_case("samples: x as character", quote({
  df <- data.frame(x = c("1", "2", "3"), y = 1, behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Missing y values
run_case("samples: NA in y", quote({
  df <- data.frame(x = c(0, 1, 2), y = c(1, NA, 1), behaviour = c("a", "a", "a"))
  compute_samples(df, x, y, behaviour)
}))

# Explicit interval with negative value
run_case("samples: explicit negative interval", quote({
  df <- data.frame(x = c(0, 1), y = 1, behaviour = c("a", "a"))
  compute_samples(df, x, y, behaviour, interval = -1, interval_mode = "explicit")
}))

# Implied mode with empty data
run_case("implied: empty data", quote({
  df <- data.frame(y = numeric(0), behaviour = character(0))
  compute_implied(df, y, behaviour)
}))

# Align by multiple columns with NA values
run_case("align: by columns with NA", quote({
  df <- data.frame(x = c(0, 1, 2), xend = c(1, 2, 3), y = c(1, 1, NA), group = c("g1", "g1", "g1"))
  align_ethogram(df, by = c(y, group))
}))
