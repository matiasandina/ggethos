StatEtho <- ggplot2::ggproto("StatEtho", ggplot2::Stat, 
                    compute_panel = function(data, scales, align_trials) {

                      # yend is always y
                      data$yend <- data$y

                      # If x and xend are provided, these values are passed
                      # directly to GeomSegment
                      if (all(c("x", "xend") %in% names(data))) {


                      # If x is provided but not xend, behaviours are assumed
                      # to represent fixed intervals, which will be guessed
                      # from the smallest interval between provided values.
                      # In future users will be able to manually override
                      # this with an explicit value and thus suppress the
                      # below warning.
                      } else if (("x" %in% names(data)) & (! "xend" %in% names(data))) {

                        data <- do.call("rbind", lapply(split(data, data$y), function(s) {

                            s <- s[order(s$x), ]
                            interval <- min(diff(s$x))
                            message( "No observation interval provided, using guessed interval ",
                              interval
                            )
                            s$xend <- s$x + interval

                            # For efficiency of drawing, runs of identical
                            # behaviours are collapsed
                            runs <- rle(s$colour)
                            s <- do.call("rbind", lapply(1:length(runs$lengths), function(i) {
                                                     x <- sum(runs$lengths[1:i-1])
                                                     d <- s[x + 1, ]
                                                     d$x <- x
                                                     d$xend <- x + runs$lengths[i]
                                                     d
                              }))
                            s
                          }
                        ))

                      # If no x is provided, behaviours are set to unit width
                      # in the order they appear in the data
                      } else if (! "x" %in% names(data)) {
                        data <- do.call("rbind", lapply(split(data, data$y), function(s) {
                                          s$xend <- seq_along(s$y)
                                          s$x <- s$xend - 1

                                          # For efficiency of drawing, runs of identical
                                          # behaviours are collapsed
                                          runs <- rle(s$colour)
                                          s <- do.call("rbind", lapply(1:length(runs$lengths), function(i) {
                                                              x <- sum(runs$lengths[1:i-1])
                                                              d <- s[x + 1, ]
                                                              d$x <- x
                                                              d$xend <- x + runs$lengths[i]
                                                              d
                                          }))
                                          s
                                        }))


                      }

                      # Align trials, if so instructed
                      if (align_trials) {
                        data <- do.call("rbind", lapply(split(data, data$y), function(s) {
                                          zero <- min(s$x)
                                          s$x <- s$x - zero
                                          s$xend <- s$xend - zero
                                          s
                                        }))
                      }

                      # Remove NA values for colour. This is intentionally done
                      # right at the end, as NA values are considered to be
                      # observations where no behaviour was observed (rather
                      # than missing observations).
                      data <- data[which(! is.na(data$colour )), ]

                      return(data)
                    },
                    required_aes = c("y")
)

geom_ethogram <- function(mapping = NULL,
                          data = NULL,
                          stat = "etho",
                          position = "identity",
                          ...,
                          size = 5,
                          arrow = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          align_trials = FALSE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      align_trials = align_trials,
      ...
    )
  )
}
