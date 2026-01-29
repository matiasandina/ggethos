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

# Human mistake patterns
run_case("samples: duplicated x conflicting labels", quote({
  df <- data.frame(x = c(1, 1, 2, 2), y = 1,
                   behaviour = c("a", "b", "a", "b"))
  compute_samples(df, x, y, behaviour)
}))

run_case("intervals: overlapping segments", quote({
  df <- data.frame(x = c(0, 1, 1.5), xend = c(2, 3, 4), y = 1,
                   behaviour = c("a", "b", "c"))
  compute_intervals(df, x, xend, y, behaviour)
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
