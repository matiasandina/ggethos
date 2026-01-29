library(ggethos)

testthat::test_that("compute_intervals returns segments", {
  out <- compute_intervals(wombats_duration,
                           x = start_seconds,
                           xend = end_seconds,
                           y = wombat,
                           behaviour = behaviour)
  testthat::expect_true(all(c("start_seconds", "end_seconds", "wombat", "wombat_end", "behaviour") %in% names(out)))
  testthat::expect_true(!is.null(attr(out, "ethogram_mapping")))
})

testthat::test_that("compute_samples returns collapsed segments", {
  out <- compute_samples(wombats,
                         x = seconds,
                         y = wombat,
                         behaviour = behaviour,
                         interval = 5)
  testthat::expect_true(all(c("seconds", "seconds_end") %in% names(out)))
  testthat::expect_true(all(out$seconds_end >= out$seconds))
  testthat::expect_true(!is.null(attr(out, "ethogram_mapping")))
})

testthat::test_that("compute_implied uses implied order", {
  out <- compute_implied(wombats,
                         y = wombat,
                         behaviour = behaviour)
  testthat::expect_true(min(out$sample) == 1)
  testthat::expect_true(all(out$sample_end >= out$sample))
  testthat::expect_true(!is.null(attr(out, "ethogram_mapping")))
})

testthat::test_that("align_ethogram aligns by key", {
  out <- compute_samples(wombats,
                         x = seconds,
                         y = wombat,
                         behaviour = behaviour,
                         interval = 5)
  aligned <- align_ethogram(out, by = wombat)
  mins <- dplyr::summarise(dplyr::group_by(aligned, wombat),
                           min_x = min(seconds),
                           .groups = "drop")
  testthat::expect_true(all(mins$min_x == 0))
})

testthat::test_that("demo intervals have no overlaps per subject/trial", {
  seg <- compute_intervals(ethogram_demo_duration,
                           x = start_seconds,
                           xend = end_seconds,
                           y = subject,
                           behaviour = behaviour)
  overlaps <- seg %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::arrange(start_seconds, .by_group = TRUE) %>%
    dplyr::mutate(prev_end = dplyr::lag(end_seconds)) %>%
    dplyr::filter(!is.na(prev_end) & start_seconds < prev_end)
  testthat::expect_equal(nrow(overlaps), 0)
})
