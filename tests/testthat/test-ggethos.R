library(ggplot2)
library(ggethos)

testthat::test_that("drawing frames in implied order salmon color", {

  p <- ggplot(wombats,
              aes(y = wombat,
                  behaviour=behaviour)) +
    geom_ethogram(color="salmon") +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames in implied order salmon color", p)
})

testthat::test_that("drawing frames in implied order behavior color", {

  p <- ggplot(wombats,
              aes(y = wombat,
                  behaviour=behaviour,
                  colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ trial)

  vdiffr::expect_doppelganger("frames in implied order behavior color", p)
})

testthat::test_that("drawing frames of uniform duration", {

  p <- ggplot(wombats,
              aes(x = frame,
                  y = wombat,
                  behaviour = behaviour,
                  colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames of uniform duration 1", p)

  p <- ggplot(wombats,
              aes(x = seconds,
                  y = wombat,
                  behaviour = behaviour,
                  colour = behaviour)) +
      geom_ethogram() +
      facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames of uniform duration 5", p)

  p <- ggplot(wombats,
              aes(x = seconds,
                  y = wombat,
                  behaviour = behaviour,
                  colour = behaviour)) +
    geom_ethogram(align_trials = TRUE) +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("frames of uniform duration 5, with alignment", p)

  p <- ggplot(wombats,
              aes(x = start_dt,
                  y = wombat,
                  behaviour = behaviour,
                  colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(wombat ~ trial, scales = "free")
  vdiffr::expect_doppelganger("frames of uniform duration in datetime", p)

  p <- ggplot(wombats,
              aes(x = start_dt,
                  behaviour = behaviour,
                  y = wombat,
                  colour = behaviour)) +
    geom_ethogram(align_trials = TRUE) +
    facet_wrap(wombat ~ trial, scales = "free")
  vdiffr::expect_doppelganger("frames of uniform duration in datetime, with alignment", p)
})

testthat::test_that("explicit start and end", {

  p <- ggplot(wombats_duration,
              aes(x = start_seconds,
                  xend = end_seconds,
                  y = wombat,
                  behaviour = behaviour,
                  colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ trial)
  vdiffr::expect_doppelganger("explicit start and end, in seconds", p)

  p <- ggplot(wombats_duration,
              aes(x = start_dt,
                  xend = end_dt,
                  y = wombat,
                  behaviour = behaviour,
                  colour = behaviour)) +
    geom_ethogram() +
    facet_wrap(wombat ~ trial, scales = "free")
  vdiffr::expect_doppelganger("explicit start and end, in datetimes", p)
})

testthat::test_that("behaviour mapped to y", {

  p <- ggplot(wombats,
              aes(y = behaviour,
                  x = frame,
                  behaviour = behaviour,
                  color = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ wombat, scales = "free")
  vdiffr::expect_doppelganger("behaviour mapped to y, with frames of duration 1", p)

  p <- ggplot(wombats,
              aes(x = seconds,
                  behaviour = behaviour,
                  y = behaviour)) +
    geom_ethogram() +
    facet_wrap(~ wombat)
  vdiffr::expect_doppelganger("behaviour mapped to y, with frames of 5 seconds", p)

  # With the current interval guessing algorithm, this plot (correctly) emits a
  # warning
  p <- ggplot(wombats,
              aes(x = seconds,
                  behaviour = behaviour,
                  y = behaviour)) +
    geom_ethogram() +
    facet_wrap(trial ~ wombat)

  expect_warning( { print(p) } )
  # vdiffr::expect_doppelganger("behaviour mapped to y, faceted by trial and wombat", p)

})
