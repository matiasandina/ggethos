StatEtho <- ggproto("StatEtho", Stat, 
                    compute_panel = function(data, scales) {

                      # yend is always y
                      data$yend <- data$y

                      # If x and xend are provided, these values are passed
                      # directly to GeomSegment
                      if (all(c("x", "xend") %in% names(data))) {
                        return(data)

                      # If x is provided but not xend, the behavior is assumed
                      # to continue until the next behaviour starts, or until
                      # the x axis max limit in the case of the last
                      # behaviour. This means that some behaviours (those
                      # with an x value equal to the x axis max limit) will
                      # not be drawn.
                      } else if (("x" %in% names(data)) & (! "xend" %in% names(data))) {
                        data <- unsplit(lapply(split(data, data$y), function(s) {
                                          # Sort the data frame by x
                                          s <- s[order(s$x), ]

                                          # xmax is either the start of the
                                          # next behavior, or the x axis limit
                                          s$xend <- c(s$x[-1], scales$x$get_limits()[2]) 
                                          s
                                        }),
                                        data$y)

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
