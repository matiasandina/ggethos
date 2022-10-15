StatEtho <- ggplot2::ggproto("StatEtho", ggplot2::Stat, 
                    compute_panel = function(data, scales) {

                      # yend is always y
                      data$yend <- data$y

                      # If x and xend are provided, these values are passed
                      # directly to GeomSegment
                      if (all(c("x", "xend") %in% names(data))) {
                        return(data)

                      # If x is provided but not xend, behaviours are assumed
                      # to represent fixed intervals, which will be guessed
                      # from the smallest interval between provided values.
                      # In future users will be able to manually override
                      # this with an explicit value and thus suppress the
                      # below warning.
                      } else if (("x" %in% names(data)) & (! "xend" %in% names(data))) {

                        data <- unsplit(lapply(split(data, data$y), function(d) {

                            d <- d[order(d$x), ]
                            interval <- min(diff(d$x))
                            message(
                              "No observation interval provided, using guessed interval ",
                              interval
                            )
                            d$xend <- d$x + interval
                            d
                          }
                        ), data$y)

                      # If no x is provided, behaviours are set to unit width
                      # in the order they appear in the data
                      } else if (! "x" %in% names(data)) {
                        data <- unsplit(lapply(split(data, data$y), function(s) {
                                          s$xend <- seq_along(s$y)
                                          s$x <- s$xend - 1
                                          s
                                        }),
                                        data$y)
                      }

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
                          inherit.aes = TRUE) {
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
      ...
    )
  )
}