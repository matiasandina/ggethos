library(ggplot2)

test_that("drawing frames in implied order", {

  p <- ggplot(wombats_ordered, aes(y = wombat, colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames in implied order", p)
})

test_that("drawing frames of uniform duration", {

  p <- ggplot(wombats_frame, aes(x = frame, y = wombat, colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames of uniform duration 1", p)

  p <- ggplot(wombats_seconds, aes(x = seconds, y = wombat, colour = behaviour)) +
      geom_ethogram() +
      facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames of uniform duration 5", p)

  p <- ggplot(wombats_seconds, aes(x = seconds, y = wombat, colour = behaviour)) +
    geom_ethogram(align_trials = TRUE) +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames of uniform duration 5, with alignment", p)

  p <- ggplot(wombats_dt, aes(x = dt, y = wombat, colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(wombat ~ trial, scales = "free")
  vdiffr::expect_doppelganger("frames of uniform duration in datetime", p)

  p <- ggplot(wombats_dt, aes(x = dt, y = wombat, colour = behaviour)) +
    geom_ethogram(align_trials = TRUE) +
    facet_wrap(wombat ~ trial, scales = "free")
  vdiffr::expect_doppelganger("frames of uniform duration in datetime, with alignment", p)
})

test_that("explicit start and end", {

  p <- ggplot(wombats_duration, aes(x = start_seconds, xend = end_seconds, y = wombat, colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("explicit start and end, in seconds", p)

  p <- ggplot(wombats_duration_dt, aes(x = start_dt, xend = end_dt, y = wombat, colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(wombat ~ trial, scales = "free")
  vdiffr::expect_doppelganger("explicit start and end, in datetimes", p)
})

test_that("behaviour mapped to y", {

  p <- ggplot(wombats_frame, aes(y = behaviour, x = frame, color = behaviour)) + 
    geom_ethogram() + 
    facet_wrap(~ wombat, scales = "free")
  vdiffr::expect_doppelganger("behaviour mapped to y, with frames of duration 1", p)

  p <- ggplot(wombats_seconds, aes(x = seconds, y = behaviour)) + 
    geom_ethogram() + 
    facet_wrap(~ wombat)
  vdiffr::expect_doppelganger("behaviour mapped to y, with frames of 5 seconds", p)

  p <- ggplot(wombats_seconds, aes(x = seconds, y = behaviour)) + 
    geom_ethogram() + 
    facet_wrap(trial ~ wombat)
  vdiffr::expect_doppelganger("behaviour mapped to y, faceted by trial and wombat", p)

})
